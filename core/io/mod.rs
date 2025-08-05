use crate::Result;
use bitflags::bitflags;
use cfg_block::cfg_block;
use std::fmt;
use std::ptr::NonNull;
use std::sync::Arc;
use std::{cell::Cell, fmt::Debug, mem::ManuallyDrop, pin::Pin, rc::Rc};

pub trait File: Send + Sync {
    fn lock_file(&self, exclusive: bool) -> Result<()>;
    fn unlock_file(&self) -> Result<()>;
    fn pread(&self, pos: usize, c: Completion) -> Result<Completion>;
    fn pwrite(&self, pos: usize, buffer: Arc<Buffer>, c: Completion) -> Result<Completion>;
    fn sync(&self, c: Completion) -> Result<Completion>;
    fn pwritev(&self, pos: usize, buffers: Vec<Arc<Buffer>>, c: Completion) -> Result<Completion> {
        use std::sync::atomic::{AtomicUsize, Ordering};
        if buffers.is_empty() {
            c.complete(0);
            return Ok(c);
        }
        // naive default implementation can be overridden on backends where it makes sense to
        let mut pos = pos;
        let outstanding = Arc::new(AtomicUsize::new(buffers.len()));
        let total_written = Arc::new(AtomicUsize::new(0));

        for buf in buffers {
            let len = buf.len();
            let child_c = {
                let c_main = c.clone();
                let outstanding = outstanding.clone();
                let total_written = total_written.clone();
                let _cloned = buf.clone();
                Completion::new_write(move |n| {
                    // reference buffer in callback to ensure alive for async io
                    let _buf = _cloned.clone();
                    // accumulate bytes actually reported by the backend
                    total_written.fetch_add(n as usize, Ordering::Relaxed);
                    if outstanding.fetch_sub(1, Ordering::AcqRel) == 1 {
                        // last one finished
                        c_main.complete(total_written.load(Ordering::Acquire) as i32);
                    }
                })
            };
            if let Err(e) = self.pwrite(pos, buf.clone(), child_c) {
                // best-effort: mark as done so caller won't wait forever
                c.complete(-1);
                return Err(e);
            }
            pos += len;
        }
        Ok(c)
    }
    fn size(&self) -> Result<u64>;
    fn truncate(&self, len: usize, c: Completion) -> Result<Completion>;
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct OpenFlags(i32);

bitflags! {
    impl OpenFlags: i32 {
        const None = 0b00000000;
        const Create = 0b0000001;
        const ReadOnly = 0b0000010;
    }
}

impl Default for OpenFlags {
    fn default() -> Self {
        Self::Create
    }
}

pub trait IO: Clock + Send + Sync {
    fn open_file(&self, path: &str, flags: OpenFlags, direct: bool) -> Result<Arc<dyn File>>;

    fn run_once(&self) -> Result<()>;

    fn wait_for_completion(&self, c: Completion) -> Result<()>;

    fn generate_random_number(&self) -> i64;

    fn get_memory_io(&self) -> Arc<MemoryIO>;

    fn register_fixed_buffer(&self, _ptr: NonNull<u8>, _len: usize) -> Result<()> {
        Err(crate::LimboError::InternalError(
            "unsupported operation".to_string(),
        ))
    }
}

pub type Complete = dyn Fn(Arc<Buffer>, i32);
pub type WriteComplete = dyn Fn(i32);
pub type SyncComplete = dyn Fn(i32);
pub type TruncateComplete = dyn Fn(i32);

#[must_use]
#[derive(Clone)]
pub struct Completion {
    inner: Arc<CompletionInner>,
}

struct CompletionInner {
    pub completion_type: CompletionType,
    is_completed: Cell<bool>,
}

pub enum CompletionType {
    Read(ReadCompletion),
    Write(WriteCompletion),
    Sync(SyncCompletion),
    Truncate(TruncateCompletion),
}

pub struct ReadCompletion {
    pub buf: Arc<Buffer>,
    pub complete: Box<Complete>,
}

impl Completion {
    pub fn new(completion_type: CompletionType) -> Self {
        Self {
            inner: Arc::new(CompletionInner {
                completion_type,
                is_completed: Cell::new(false),
            }),
        }
    }

    pub fn new_write<F>(complete: F) -> Self
    where
        F: Fn(i32) + 'static,
    {
        Self::new(CompletionType::Write(WriteCompletion::new(Box::new(
            complete,
        ))))
    }

    pub fn new_read<F>(buf: Arc<Buffer>, complete: F) -> Self
    where
        F: Fn(Arc<Buffer>, i32) + 'static,
    {
        Self::new(CompletionType::Read(ReadCompletion::new(
            buf,
            Box::new(complete),
        )))
    }
    pub fn new_sync<F>(complete: F) -> Self
    where
        F: Fn(i32) + 'static,
    {
        Self::new(CompletionType::Sync(SyncCompletion::new(Box::new(
            complete,
        ))))
    }

    pub fn new_trunc<F>(complete: F) -> Self
    where
        F: Fn(i32) + 'static,
    {
        Self::new(CompletionType::Truncate(TruncateCompletion::new(Box::new(
            complete,
        ))))
    }
    pub fn is_completed(&self) -> bool {
        self.inner.is_completed.get()
    }

    pub fn complete(&self, result: i32) {
        if !self.inner.is_completed.get() {
            match &self.inner.completion_type {
                CompletionType::Read(r) => r.complete(result),
                CompletionType::Write(w) => w.complete(result),
                CompletionType::Sync(s) => s.complete(result), // fix
                CompletionType::Truncate(t) => t.complete(result),
            };
            self.inner.is_completed.set(true);
        }
    }

    /// only call this method if you are sure that the completion is
    /// a ReadCompletion, panics otherwise
    pub fn as_read(&self) -> &ReadCompletion {
        match self.inner.completion_type {
            CompletionType::Read(ref r) => r,
            _ => unreachable!(),
        }
    }

    /// only call this method if you are sure that the completion is
    /// a WriteCompletion, panics otherwise
    pub fn as_write(&self) -> &WriteCompletion {
        match self.inner.completion_type {
            CompletionType::Write(ref w) => w,
            _ => unreachable!(),
        }
    }
}

pub struct WriteCompletion {
    pub complete: Box<WriteComplete>,
}

pub struct SyncCompletion {
    pub complete: Box<SyncComplete>,
}

impl ReadCompletion {
    pub fn new(buf: Arc<Buffer>, complete: Box<Complete>) -> Self {
        Self { buf, complete }
    }

    pub fn buf(&self) -> &Buffer {
        &self.buf
    }

    pub fn complete(&self, bytes_read: i32) {
        (self.complete)(self.buf.clone(), bytes_read);
    }
}

impl WriteCompletion {
    pub fn new(complete: Box<WriteComplete>) -> Self {
        Self { complete }
    }

    pub fn complete(&self, bytes_written: i32) {
        (self.complete)(bytes_written);
    }
}

impl SyncCompletion {
    pub fn new(complete: Box<SyncComplete>) -> Self {
        Self { complete }
    }

    pub fn complete(&self, res: i32) {
        (self.complete)(res);
    }
}

pub struct TruncateCompletion {
    pub complete: Box<TruncateComplete>,
}

impl TruncateCompletion {
    pub fn new(complete: Box<TruncateComplete>) -> Self {
        Self { complete }
    }

    pub fn complete(&self, res: i32) {
        (self.complete)(res);
    }
}

pub type BufferData = Pin<Vec<u8>>;

pub type BufferDropFn = Rc<dyn Fn(BufferData)>;

pub struct Buffer {
    data: ManuallyDrop<BufferData>,
    drop: BufferDropFn,
}

impl Debug for Buffer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.data)
    }
}

impl Drop for Buffer {
    fn drop(&mut self) {
        let data = unsafe { ManuallyDrop::take(&mut self.data) };
        (self.drop)(data);
    }
}

impl Buffer {
    pub fn allocate(size: usize, drop: BufferDropFn) -> Self {
        let data = ManuallyDrop::new(Pin::new(vec![0; size]));
        Self { data, drop }
    }

    pub fn new(data: BufferData, drop: BufferDropFn) -> Self {
        let data = ManuallyDrop::new(data);
        Self { data, drop }
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub fn as_slice(&self) -> &[u8] {
        &self.data
    }

    #[allow(clippy::mut_from_ref)]
    pub fn as_mut_slice(&self) -> &mut [u8] {
        unsafe { std::slice::from_raw_parts_mut(self.as_mut_ptr(), self.data.len()) }
    }

    pub fn as_ptr(&self) -> *const u8 {
        self.data.as_ptr()
    }

    pub fn as_mut_ptr(&self) -> *mut u8 {
        self.data.as_ptr() as *mut u8
    }
}

cfg_block! {
    #[cfg(all(target_os = "linux", feature = "io_uring"))] {
        mod io_uring;
        #[cfg(feature = "fs")]
        pub use io_uring::UringIO;
    }

    #[cfg(target_family = "unix")] {
        mod unix;
        #[cfg(feature = "fs")]
        pub use unix::UnixIO;
        pub use unix::UnixIO as PlatformIO;
        pub use PlatformIO as SyscallIO;
    }

    #[cfg(any(target_os = "android", target_os = "ios"))] {
        mod unix;
        #[cfg(feature = "fs")]
        pub use unix::UnixIO;
        pub use unix::UnixIO as SyscallIO;
        pub use unix::UnixIO as PlatformIO;
    }

     #[cfg(target_os = "windows")] {
        mod windows;
        pub use windows::WindowsIO as PlatformIO;
         pub use PlatformIO as SyscallIO;
    }

    #[cfg(not(any(target_os = "linux", target_os = "macos", target_os = "windows", target_os = "android", target_os = "ios")))] {
        mod generic;
        pub use generic::GenericIO as PlatformIO;
        pub use PlatformIO as SyscallIO;
    }
}

mod memory;
#[cfg(feature = "fs")]
mod vfs;
pub use memory::MemoryIO;
pub mod clock;
mod common;
pub use clock::Clock;
