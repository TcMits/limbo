#![allow(clippy::arc_with_non_send_sync)]

use super::{common, Completion, CompletionInner, File, OpenFlags, IO};
use crate::io::clock::{Clock, Instant};
use crate::storage::wal::CKPT_BATCH_PAGES;
use crate::{turso_assert, LimboError, MemoryIO, Result};
use rustix::fs::{self, FlockOperation, OFlags};
use std::{
    cell::RefCell,
    collections::{HashMap, VecDeque},
    io::ErrorKind,
    ops::Deref,
    os::{fd::AsFd, unix::io::AsRawFd},
    rc::Rc,
    sync::Arc,
};
use tracing::{debug, trace};

const ENTRIES: u32 = 512;
const SQPOLL_IDLE: u32 = 1000;
const FILES: u32 = 8;

pub struct UringIO {
    inner: Rc<RefCell<InnerUringIO>>,
}

unsafe impl Send for UringIO {}
unsafe impl Sync for UringIO {}

struct WrappedIOUring {
    ring: io_uring::IoUring,
    pending_ops: usize,
    writev_states: HashMap<u64, WritevState>,
    pending: [Option<Arc<Completion>>; ENTRIES as usize + 1],
    key: u64,
}

struct InnerUringIO {
    ring: WrappedIOUring,
    free_files: VecDeque<u32>,
}

impl UringIO {
    pub fn new() -> Result<Self> {
        let ring = match io_uring::IoUring::builder()
            .setup_single_issuer()
            .setup_coop_taskrun()
            .setup_sqpoll(SQPOLL_IDLE)
            .build(ENTRIES)
        {
            Ok(ring) => ring,
            Err(_) => io_uring::IoUring::new(ENTRIES)?,
        };
        // we only ever have 2 files open at a time for the moment
        ring.submitter().register_files_sparse(FILES)?;
        let inner = InnerUringIO {
            ring: WrappedIOUring {
                ring,
                pending_ops: 0,
                writev_states: HashMap::new(),
                pending: [const { None }; ENTRIES as usize + 1],
                key: 0,
            },
            free_files: (0..FILES).collect(),
        };
        debug!("Using IO backend 'io-uring'");
        Ok(Self {
            inner: Rc::new(RefCell::new(inner)),
        })
    }
}

macro_rules! with_fd {
    ($file:expr, |$fd:ident| $body:expr) => {
        match $file.id() {
            Some(id) => {
                let $fd = io_uring::types::Fixed(id);
                $body
            }
            None => {
                let $fd = io_uring::types::Fd($file.as_raw_fd());
                $body
            }
        }
    };
}

enum Fd {
    Fixed(u32),
    RawFd(i32),
}

impl Fd {
    fn as_raw_fd(&self) -> i32 {
        match self {
            Fd::RawFd(fd) => *fd,
            _ => unreachable!("only to be called on RawFd variant"),
        }
    }
    fn id(&self) -> Option<u32> {
        match self {
            Fd::Fixed(id) => Some(*id),
            Fd::RawFd(_) => None,
        }
    }
}

struct WritevState {
    // fixed fd slot
    file_id: Fd,
    // absolute file offset for next submit
    file_pos: usize,
    // current buffer index in `bufs`
    current_buffer_idx: usize,
    // intra-buffer offset
    current_buffer_offset: usize,
    total_written: usize,
    total_len: usize,
    bufs: Vec<Arc<RefCell<crate::Buffer>>>,
    // we keep the last iovec allocation alive until CQE:
    // raw ptr to Box<[iovec]>
    last_iov: *mut libc::iovec,
    last_iov_len: usize,
}

impl WritevState {
    fn new(file: &UringFile, pos: usize, bufs: Vec<Arc<RefCell<crate::Buffer>>>) -> Self {
        let file_id = match file.id() {
            Some(id) => Fd::Fixed(id),
            None => Fd::RawFd(file.as_raw_fd()),
        };
        Self {
            file_id,
            file_pos: pos,
            current_buffer_idx: 0,
            current_buffer_offset: 0,
            total_written: 0,
            bufs,
            last_iov: core::ptr::null_mut(),
            last_iov_len: 0,
            total_len: bufs.iter().map(|b| b.borrow().len()).sum(),
        }
    }

    #[inline(always)]
    fn remaining(&self) -> usize {
        self.total_len - self.total_written
    }

    /// Advance (idx, off, pos) after written bytes
    #[inline(always)]
    fn advance(&mut self, written: usize) {
        let mut remaining = written;
        while remaining > 0 {
            let current_buf_len = {
                let r = self.bufs[self.current_buffer_idx].borrow();
                r.len()
            };
            let left = current_buf_len - self.current_buffer_offset;
            if remaining < left {
                self.current_buffer_offset += remaining;
                self.file_pos += remaining;
                remaining = 0;
            } else {
                remaining -= left;
                self.file_pos += left;
                self.current_buffer_idx += 1;
                self.current_buffer_offset = 0;
            }
        }
        self.total_written += written;
    }

    /// Free the allocation that keeps the iovec array alive while writev is ongoing
    #[inline(always)]
    fn free_last_iov(&mut self) {
        if !self.last_iov.is_null() {
            unsafe {
                drop(Box::from_raw(core::slice::from_raw_parts_mut(
                    self.last_iov,
                    self.last_iov_len,
                )))
            };
            self.last_iov = core::ptr::null_mut();
            self.last_iov_len = 0;
        }
    }
}

impl InnerUringIO {
    fn register_file(&mut self, fd: i32) -> Result<u32> {
        if let Some(slot) = self.free_files.pop_front() {
            self.ring
                .ring
                .submitter()
                .register_files_update(slot, &[fd.as_raw_fd()])?;
            return Ok(slot);
        }
        Err(LimboError::UringIOError(
            "unable to register file, no free slots available".to_string(),
        ))
    }
    fn unregister_file(&mut self, id: u32) -> Result<()> {
        self.ring
            .ring
            .submitter()
            .register_files_update(id, &[-1])?;
        self.free_files.push_back(id);
        Ok(())
    }
}

impl WrappedIOUring {
    fn submit_entry(&mut self, entry: &io_uring::squeue::Entry, c: Arc<Completion>) {
        trace!("submit_entry({:?})", entry);
        self.pending[entry.get_user_data() as usize] = Some(c);
        unsafe {
            let mut sub = self.ring.submission_shared();
            match sub.push(entry) {
                Ok(_) => self.pending_ops += 1,
                Err(e) => {
                    tracing::error!("Failed to submit entry: {e}");
                    self.ring.submit().expect("failed to submit entry");
                    sub.push(entry).expect("failed to push entry after submit");
                    self.pending_ops += 1;
                }
            }
        }
    }

    fn wait_for_completion(&mut self) -> Result<()> {
        if self.pending_ops == 0 {
            return Ok(());
        }
        let wants = std::cmp::min(self.pending_ops, 8);
        tracing::trace!("submit_and_wait for {wants} pending operations to complete");
        self.ring.submit_and_wait(wants)?;
        Ok(())
    }

    fn empty(&self) -> bool {
        self.pending_ops == 0
    }

    /// Submit a writev operation for the given key. WritevState MUST exist in the map
    /// of `writev_states`
    fn submit_writev(&mut self, key: u64, mut st: WritevState, c: Arc<Completion>) {
        self.writev_states.insert(key, st);
        // the likelyhood of the whole batch size being contiguous is very low, so lets not pre-allocate more than half
        let max = CKPT_BATCH_PAGES / 2;
        let mut iov = Vec::with_capacity(max);
        for (i, b) in st
            .bufs
            .iter()
            .enumerate()
            .skip(st.current_buffer_idx)
            .take(max)
        {
            let r = b.borrow();
            let s = r.as_slice();
            let slice = if i == st.current_buffer_idx {
                &s[st.current_buffer_offset..]
            } else {
                s
            };
            if slice.is_empty() {
                continue;
            }
            iov.push(libc::iovec {
                iov_base: slice.as_ptr() as *mut _,
                iov_len: slice.len(),
            });
        }

        // keep iov alive until CQE
        let boxed = iov.into_boxed_slice();
        let ptr = boxed.as_ptr() as *mut libc::iovec;
        let len = boxed.len();
        st.free_last_iov();
        st.last_iov = ptr;
        st.last_iov_len = len;
        // leak the iovec array, will be freed when CQE processed
        let _ = Box::into_raw(boxed);
        let entry = with_fd!(st.file_id, |fd| {
            io_uring::opcode::Writev::new(fd, ptr, len as u32)
                .offset(st.file_pos as u64)
                .build()
                .user_data(key)
        });
        self.submit_entry(&entry, c.clone());
    }
}

#[inline(always)]
/// use the callback pointer as the user_data for the operation as is
/// common practice for io_uring to prevent more indirection
fn get_key(c: Arc<Completion>) -> u64 {
    Arc::into_raw(c) as u64
}

#[inline(always)]
/// convert the user_data back to an Arc<Completion> pointer
fn completion_from_key(key: u64) -> Arc<Completion> {
    unsafe { Arc::from_raw(key as *const Completion) }
}

impl IO for UringIO {
    fn open_file(&self, path: &str, flags: OpenFlags, direct: bool) -> Result<Arc<dyn File>> {
        trace!("open_file(path = {})", path);
        let mut file = std::fs::File::options();
        file.read(true);

        if !flags.contains(OpenFlags::ReadOnly) {
            file.write(true);
            file.create(flags.contains(OpenFlags::Create));
        }

        let file = file.open(path)?;
        // Let's attempt to enable direct I/O. Not all filesystems support it
        // so ignore any errors.
        let fd = file.as_fd();
        if direct {
            match fs::fcntl_setfl(fd, OFlags::DIRECT) {
                Ok(_) => {}
                Err(error) => debug!("Error {error:?} returned when setting O_DIRECT flag to read file. The performance of the system may be affected"),
            }
        }
        let id = self.inner.borrow_mut().register_file(file.as_raw_fd()).ok();
        let uring_file = Arc::new(UringFile {
            io: self.inner.clone(),
            file,
            id,
        });
        if std::env::var(common::ENV_DISABLE_FILE_LOCK).is_err() {
            uring_file.lock_file(!flags.contains(OpenFlags::ReadOnly))?;
        }
        Ok(uring_file)
    }

    fn wait_for_completion(&self, c: Completion) -> Result<()> {
        while !c.is_completed() {
            self.run_once()?;
        }
        Ok(())
    }

    fn run_once(&self) -> Result<()> {
        let mut inner = self.inner.borrow_mut();
        let ring = &mut inner.ring;

        if ring.empty() {
            return Ok(());
        }
        ring.wait_for_completion()?;
        const MAX: usize = ENTRIES as usize;
        // to circumvent borrowing rules, collect everything without the heap
        let mut uds = [0u64; MAX];
        let mut ress = [0i32; MAX];
        let mut count = 0;
        {
            let cq = ring.ring.completion();
            for cqe in cq {
                ring.pending_ops -= 1;
                uds[count] = cqe.user_data();
                ress[count] = cqe.result();
                count += 1;
                if count == MAX {
                    break;
                }
            }
        }

        for i in 0..count {
            ring.pending_ops -= 1;
            let user_data = uds[i];
            let result = ress[i];
            turso_assert!(
                    user_data != 0,
                    "user_data must not be zero, we dont submit linked timeouts or cancelations that would cause this"
                );
            if let Some(mut st) = ring.writev_states.remove(&user_data) {
                if result < 0 {
                    st.free_last_iov();
                    completion_from_key(ud).complete(result);
                } else {
                    let written = result as usize;
                    st.free_last_iov();
                    st.advance(written);
                    if st.remaining() == 0 {
                        // write complete
                        c.complete(st.total_written as i32);
                    } else {
                        // partial write, submit next
                        ring.submit_writev(user_data, st, c.clone());
                    }
                }
                continue;
            }
            completion_from_key(user_data).complete(result)
        }
        Ok(())
    }

    fn generate_random_number(&self) -> i64 {
        let mut buf = [0u8; 8];
        getrandom::getrandom(&mut buf).unwrap();
        i64::from_ne_bytes(buf)
    }

    fn get_memory_io(&self) -> Arc<MemoryIO> {
        Arc::new(MemoryIO::new())
    }
}

impl Clock for UringIO {
    fn now(&self) -> Instant {
        let now = chrono::Local::now();
        Instant {
            secs: now.timestamp(),
            micros: now.timestamp_subsec_micros(),
        }
    }
}

#[inline(always)]
/// use the callback pointer as the user_data for the operation as is
/// common practice for io_uring to prevent more indirection
fn get_key(c: Completion) -> u64 {
    Arc::into_raw(c.inner.clone()) as u64
}

#[inline(always)]
/// convert the user_data back to an Completion pointer
fn completion_from_key(key: u64) -> Completion {
    let c_inner = unsafe { Arc::from_raw(key as *const CompletionInner) };
    Completion { inner: c_inner }
}

pub struct UringFile {
    io: Rc<RefCell<InnerUringIO>>,
    file: std::fs::File,
    id: Option<u32>,
}

impl Deref for UringFile {
    type Target = std::fs::File;
    fn deref(&self) -> &Self::Target {
        &self.file
    }
}

impl UringFile {
    fn id(&self) -> Option<u32> {
        self.id
    }
}

unsafe impl Send for UringFile {}
unsafe impl Sync for UringFile {}

impl File for UringFile {
    fn lock_file(&self, exclusive: bool) -> Result<()> {
        let fd = self.file.as_fd();
        // F_SETLK is a non-blocking lock. The lock will be released when the file is closed
        // or the process exits or after an explicit unlock.
        fs::fcntl_lock(
            fd,
            if exclusive {
                FlockOperation::NonBlockingLockExclusive
            } else {
                FlockOperation::NonBlockingLockShared
            },
        )
        .map_err(|e| {
            let io_error = std::io::Error::from(e);
            let message = match io_error.kind() {
                ErrorKind::WouldBlock => {
                    "Failed locking file. File is locked by another process".to_string()
                }
                _ => format!("Failed locking file, {io_error}"),
            };
            LimboError::LockingError(message)
        })?;

        Ok(())
    }

    fn unlock_file(&self) -> Result<()> {
        let fd = self.file.as_fd();
        fs::fcntl_lock(fd, FlockOperation::NonBlockingUnlock).map_err(|e| {
            LimboError::LockingError(format!(
                "Failed to release file lock: {}",
                std::io::Error::from(e)
            ))
        })?;
        Ok(())
    }

    fn pread(&self, pos: usize, c: Completion) -> Result<Completion> {
        let r = c.as_read();
        trace!("pread(pos = {}, length = {})", pos, r.buf().len());
        let mut io = self.io.borrow_mut();
        let read_e = {
            let mut buf = r.buf_mut();
            let len = buf.len();
            let buf = buf.as_mut_ptr();
            with_fd!(self, |fd| {
                io_uring::opcode::Read::new(fd, buf, len as u32)
                    .offset(pos as u64)
                    .build()
                    .user_data(io.ring.get_key())
            })
        };
        io.ring.submit_entry(&read_e, c.clone());
        Ok(c)
    }

    fn pwrite(
        &self,
        pos: usize,
        buffer: Arc<RefCell<crate::Buffer>>,
        c: Completion,
    ) -> Result<Completion> {
        let mut io = self.io.borrow_mut();
        let write = {
            let buf = buffer.borrow();
            trace!("pwrite(pos = {}, length = {})", pos, buf.len());
            with_fd!(self, |fd| {
                io_uring::opcode::Write::new(fd, buf.as_ptr(), buf.len() as u32)
                    .offset(pos as u64)
                    .build()
                    .user_data(io.ring.get_key())
            })
        };
        io.ring.submit_entry(&write, c.clone());
        Ok(c)
    }

    fn sync(&self, c: Completion) -> Result<Completion> {
        let mut io = self.io.borrow_mut();
        trace!("sync()");
        let sync = with_fd!(self, |fd| {
            io_uring::opcode::Fsync::new(fd)
                .build()
                .user_data(get_key(c.clone()))
        });
        io.ring.submit_entry(&sync, c.clone());
        Ok(c)
    }

    fn pwritev(
        &self,
        pos: usize,
        bufs: Vec<Arc<RefCell<crate::Buffer>>>,
        c: Arc<Completion>,
    ) -> Result<Arc<Completion>> {
        // for a single buffer use pwrite directly
        if bufs.len().eq(&1) {
            return self.pwrite(pos, bufs[0].clone(), c.clone());
        }
        tracing::trace!("pwritev(pos = {}, bufs.len() = {})", pos, bufs.len());
        let mut io = self.io.borrow_mut();
        let key = io.ring.get_key();
        // create state to track ongoing writev operation
        let state = WritevState::new(self, pos, bufs);
        io.ring.submit_writev(key, state, c.clone());
        Ok(c)
    }

    fn size(&self) -> Result<u64> {
        Ok(self.file.metadata()?.len())
    }

    fn truncate(&self, len: usize, c: Completion) -> Result<Completion> {
        let mut io = self.io.borrow_mut();
        let truncate = with_fd!(self, |fd| {
            io_uring::opcode::Ftruncate::new(fd, len as u64)
                .build()
                .user_data(get_key(c.clone()))
        });
        io.ring.submit_entry(&truncate);
        Ok(c)
    }
}

impl Drop for UringFile {
    fn drop(&mut self) {
        self.unlock_file().expect("Failed to unlock file");
        if let Some(id) = self.id {
            self.io
                .borrow_mut()
                .unregister_file(id)
                .inspect_err(|e| {
                    debug!("Failed to unregister file: {e}");
                })
                .ok();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::io::common;

    #[test]
    fn test_multiple_processes_cannot_open_file() {
        common::tests::test_multiple_processes_cannot_open_file(UringIO::new);
    }
}
