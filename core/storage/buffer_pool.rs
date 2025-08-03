use crate::fast_lock::SpinLock;
use crate::io::TEMP_BUFFER_CACHE;
use crate::storage::sqlite3_ondisk::WAL_FRAME_HEADER_SIZE;
use crate::{turso_assert, Buffer, LimboError, IO};
use parking_lot::Mutex;
use std::cell::UnsafeCell;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicU32, AtomicUsize, Ordering};
use std::sync::{Arc, OnceLock};

pub static BUFFER_POOL: OnceLock<Arc<BufferPool>> = OnceLock::new();

#[derive(Debug)]
/// A buffer allocated from an arena from `[BufferPool]`
pub struct ArenaBuffer {
    ptr: NonNull<u8>,
    arena_id: u32,
    page_idx: u32,
    len: usize,
}

const REGISTERED_ID: u32 = 0;

impl ArenaBuffer {
    const fn new(ptr: NonNull<u8>, len: usize, arena_id: u32, page_idx: u32) -> Self {
        ArenaBuffer {
            ptr,
            arena_id,
            page_idx,
            len,
        }
    }

    #[inline(always)]
    /// Returns the `id` of the underlying arena if it is registered with `io_uring`
    pub fn fixed_id(&self) -> Option<u32> {
        if self.arena_id == REGISTERED_ID {
            Some(REGISTERED_ID)
        } else {
            None
        }
    }

    /// The requested size of the allocation. The actual size is always rounded up to
    /// the next multiple of the arena.page_size
    pub const fn logical_len(&self) -> usize {
        self.len
    }
    pub fn as_slice(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.ptr.as_ptr(), self.logical_len()) }
    }
    pub fn as_mut_slice(&mut self) -> &mut [u8] {
        unsafe { std::slice::from_raw_parts_mut(self.ptr.as_ptr(), self.logical_len()) }
    }
}

impl Drop for ArenaBuffer {
    fn drop(&mut self) {
        let pool = BUFFER_POOL
            .get()
            .expect("BufferPool not initialized, cannot free ArenaBuffer");
        pool.free(self.logical_len(), self.arena_id, self.page_idx);
    }
}

impl std::ops::Deref for ArenaBuffer {
    type Target = [u8];
    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

impl std::ops::DerefMut for ArenaBuffer {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut_slice()
    }
}

/// Static Buffer pool managing multiple arenas
pub struct BufferPool {
    inner: UnsafeCell<PoolInner>,
}
unsafe impl Sync for BufferPool {}
unsafe impl Send for BufferPool {}

struct PoolInner {
    io: Option<Arc<dyn IO>>,
    arena: Option<Arena>,
    init_lock: Mutex<()>,
    arena_size: AtomicUsize,
    db_page_size: AtomicUsize,
}

unsafe impl Sync for PoolInner {}
unsafe impl Send for PoolInner {}

impl Default for BufferPool {
    fn default() -> Self {
        Self::new(Self::DEFAULT_ARENA_SIZE)
    }
}

impl BufferPool {
    pub const DEFAULT_ARENA_SIZE: usize = 4 * 1024 * 1024; // 4MB arena
    pub const TEST_AREA_SIZE: usize = 512 * 1024; // 512KB arena for testing
    pub const DEFAULT_PAGE_SIZE: usize = 4096; // 4KB default page size
    const MAX_ARENA_SIZE: usize = 32 * 1024 * 1024; // 32MB max arena

    pub fn new(arena_size: usize) -> Self {
        turso_assert!(
            arena_size < Self::MAX_ARENA_SIZE,
            "Arena size cannot exceed {} bytes",
            Self::MAX_ARENA_SIZE
        );
        Self {
            inner: UnsafeCell::new(PoolInner {
                arena: None,
                arena_size: arena_size.into(),
                db_page_size: Self::DEFAULT_PAGE_SIZE.into(),
                init_lock: Mutex::new(()),
                io: None,
            }),
        }
    }

    pub fn allocate(len: usize) -> Buffer {
        let pool = BUFFER_POOL.get().expect("BufferPool must be initialized");
        pool.inner().allocate(len)
    }

    pub fn get_page(&self) -> Buffer {
        let inner = self.inner();
        inner.allocate(inner.db_page_size.load(Ordering::Relaxed))
    }

    fn inner(&self) -> &PoolInner {
        unsafe { &*self.inner.get() }
    }

    #[allow(clippy::mut_from_ref)]
    fn inner_mut(&self) -> &mut PoolInner {
        unsafe { &mut *self.inner.get() }
    }

    /// Create a static `BufferPool` initialize the pool to the default page size, **without**
    /// creating an arena. Arena will be created when `[BufferPool::finalize_page_size]` is called.
    /// Until then the pool will return temporary buffers to prevent reallocation of the
    /// arena if the page size is set to something other than the default value.
    pub fn begin_init(io: &Arc<dyn IO>, arena_size: usize) -> Arc<Self> {
        let pool = BUFFER_POOL.get_or_init(|| Arc::new(BufferPool::new(arena_size)));
        let inner = pool.inner_mut();
        // Just store the IO handle, don't create arena yet
        if inner.io.is_none() {
            inner.io = Some(Arc::clone(io));
        }
        pool.clone()
    }

    /// Call when `[Database::db_state]` is initialized, providing the `page_size` to allocate
    /// an arena for the pool. Before this call, the pool will use temporary buffers
    pub fn finalize_with_page_size(&self, page_size: usize) -> crate::Result<Arc<Self>> {
        let pool = BUFFER_POOL.get().expect("BufferPool must be initialized");
        let inner = pool.inner_mut();
        tracing::trace!("finalize page size called with size {page_size}");
        if page_size != BufferPool::DEFAULT_PAGE_SIZE {
            // so far we have handed out some temporary buffers, since the page size is not
            // default, we need to clear the cache so they aren't reused for other operations.
            TEMP_BUFFER_CACHE.with(|cache| {
                cache.borrow_mut().reinit_cache(page_size);
            });
        }
        if inner.arena.is_some() {
            return Ok(pool.clone());
        }
        inner.db_page_size.store(page_size, Ordering::Relaxed);
        inner.init_arena()?;
        Ok(pool.clone())
    }

    #[inline]
    pub fn free(&self, size: usize, arena_id: u32, page_id: u32) {
        self.inner_mut().free(size, arena_id, page_id);
    }
}

impl PoolInner {
    /// Allocate a buffer of the given length from the pool, falling back to
    /// temporary thread local buffers if the pool is not initialized or full
    pub fn allocate(&self, len: usize) -> Buffer {
        turso_assert!(len > 0, "Cannot allocate zero-length buffer");
        let Some(arena) = self.arena.as_ref() else {
            // pool isn't fully initialized, return temporary buffer
            return Buffer::new_temporary(len);
        };
        if let Some(FreeEntry { ptr, first_idx }) = arena.try_alloc(len) {
            tracing::trace!(
                "Allocated buffer of length {} from arena {} at index {}",
                len,
                arena.id,
                first_idx
            );
            return Buffer::new_pooled(ArenaBuffer::new(ptr, len, arena.id, first_idx));
        }
        Buffer::new_temporary(len)
    }

    /// Allocate a new arena for the pool to use
    fn init_arena(&mut self) -> crate::Result<()> {
        // Prevent concurrent growth
        let Some(_guard) = self.init_lock.try_lock() else {
            tracing::debug!("Buffer pool is already growing, skipping initialization");
            return Ok(()); // Already in progress
        };
        let arena_size = self.arena_size.load(Ordering::Relaxed);
        let io = self.io.as_ref().expect("Pool not initialized").clone();
        match Arena::new(
            self.db_page_size.load(Ordering::Relaxed) + WAL_FRAME_HEADER_SIZE,
            arena_size,
            &io,
        ) {
            Ok(arena) => {
                tracing::trace!(
                    "added arena {} with size {} MB",
                    arena.id,
                    arena_size / (1024 * 1024)
                );
                self.arena = Some(arena);
                Ok(())
            }
            Err(e) => {
                tracing::error!("Failed to create new arena: {:?}", e);
                Err(LimboError::InternalError(format!(
                    "Failed to create new arena: {e}",
                )))
            }
        }
    }

    pub fn free(&mut self, size: usize, arena_id: u32, page_idx: u32) {
        let arena = self
            .arena
            .as_mut()
            .expect("pool arena not initialized, cannot free buffer");
        let pages = size.div_ceil(arena.page_size);
        tracing::trace!("Freeing {} pages from arena {}", pages, arena_id);
        turso_assert!(
            arena_id == arena.id,
            "should not free from different arena. {arena_id} != {}",
            arena.id
        );
        arena.free(page_idx, pages);
    }
}

/// A single memory arena
struct Arena {
    /// Identifier to tie allocations back to the arena
    id: u32,
    /// base pointer to the arena returned by `mmap`
    base: NonNull<u8>,
    allocated_pages: AtomicUsize,
    free_pages: SpinLock<Vec<u32>>,
    arena_size: usize,
    page_size: usize,
}

impl Drop for Arena {
    fn drop(&mut self) {
        unsafe { arena::dealloc(self.base.as_ptr(), self.arena_size) };
    }
}

struct FreeEntry {
    ptr: NonNull<u8>,
    first_idx: u32,
}

const UNREGISTERED: u32 = 1;

/// For an arena which isn't registered with `io_uring`
/// registered arena will always have id = 0
static NEXT_ID: AtomicU32 = AtomicU32::new(UNREGISTERED);

impl Arena {
    /// Create a new arena with the given size and page size.
    fn new(page_size: usize, arena_size: usize, io: &Arc<dyn IO>) -> Result<Self, String> {
        let ptr = unsafe { arena::alloc(arena_size) };
        let base = NonNull::new(ptr).ok_or("Failed to allocate arena")?;
        let total_pages = arena_size / page_size;
        let id = if io.register_fixed_buffer(base, arena_size).is_ok() {
            REGISTERED_ID
        } else {
            NEXT_ID.fetch_add(1, Ordering::Relaxed)
        };

        Ok(Self {
            id,
            base,
            free_pages: SpinLock::new((0..total_pages as u32).collect()),
            allocated_pages: AtomicUsize::new(0),
            page_size,
            arena_size,
        })
    }

    pub fn try_alloc(&self, size: usize) -> Option<FreeEntry> {
        let pages = size.div_ceil(self.page_size);
        let mut free = self.free_pages.lock();
        if free.len() < pages {
            return None;
        }
        if pages == 1 {
            // fast path: for now, most/all allocations are single pages
            if let Some(page_idx) = free.pop() {
                self.allocated_pages.fetch_add(pages, Ordering::Relaxed);
                let offset = page_idx as usize * self.page_size;
                let ptr = unsafe { NonNull::new_unchecked(self.base.as_ptr().add(offset)) };
                tracing::trace!(
                    "Allocated single page at index {} from arena {}",
                    page_idx,
                    self.id
                );
                return Some(FreeEntry {
                    ptr,
                    first_idx: page_idx,
                });
            }
        } else {
            return self.try_alloc_many(pages);
        }
        None
    }

    /// Free pages back to this arena
    pub fn free(&self, page_idx: u32, count: usize) {
        let mut free = self.free_pages.lock();
        // Add pages back to freelist
        for i in 0..count {
            free.push(page_idx + i as u32);
        }
        self.allocated_pages.fetch_sub(count, Ordering::Relaxed);
    }

    #[cold]
    fn try_alloc_many(&self, pages: usize) -> Option<FreeEntry> {
        // TODO (preston): we can optimize this further when we start allocating larger
        // contiguous blocks for coalescing. this is 'unused' for now
        let mut free = self.free_pages.lock();
        if pages <= 3 && free.len() >= pages {
            let start = free.len() - pages;
            let first_idx = free[start];

            let mut consecutive = true;
            for j in 1..pages {
                if free[start + j] != first_idx + j as u32 {
                    consecutive = false;
                    break;
                }
            }
            if consecutive {
                free.truncate(start);
                self.allocated_pages.fetch_add(pages, Ordering::Relaxed);
                let offset = first_idx as usize * self.page_size;
                let ptr = unsafe { NonNull::new_unchecked(self.base.as_ptr().add(offset)) };
                return Some(FreeEntry { ptr, first_idx });
            }
        }
        // Fall back to searching from the beginning
        for i in 0..free.len().saturating_sub(pages - 1) {
            let first_idx = free[i];
            let mut consecutive = true;
            for j in 1..pages {
                if free[i + j] != first_idx + j as u32 {
                    consecutive = false;
                    break;
                }
            }

            if consecutive {
                free.drain(i..i + pages);
                self.allocated_pages.fetch_add(pages, Ordering::Relaxed);
                let offset = first_idx as usize * self.page_size;
                let ptr = unsafe { NonNull::new_unchecked(self.base.as_ptr().add(offset)) };
                return Some(FreeEntry { ptr, first_idx });
            }
        }
        None
    }
}

#[cfg(unix)]
mod arena {
    #[cfg(target_os = "macos")]
    use libc::MAP_ANON as MAP_ANONYMOUS;
    #[cfg(target_os = "linux")]
    use libc::MAP_ANONYMOUS;
    use libc::{mmap, munmap, MAP_PRIVATE, PROT_READ, PROT_WRITE};
    use std::ffi::c_void;

    pub unsafe fn alloc(len: usize) -> *mut u8 {
        let ptr = mmap(
            std::ptr::null_mut(),
            len,
            PROT_READ | PROT_WRITE,
            MAP_PRIVATE | MAP_ANONYMOUS,
            -1,
            0,
        );
        if ptr == libc::MAP_FAILED {
            panic!("mmap failed: {}", std::io::Error::last_os_error());
        }
        #[cfg(target_os = "linux")]
        {
            libc::madvise(ptr, len, libc::MADV_HUGEPAGE);
        }
        ptr as *mut u8
    }

    pub unsafe fn dealloc(ptr: *mut u8, len: usize) {
        let result = munmap(ptr as *mut c_void, len);
        if result != 0 {
            panic!("munmap failed: {}", std::io::Error::last_os_error());
        }
    }
}

#[cfg(not(unix))]
mod arena {
    pub fn alloc(len: usize) -> *mut u8 {
        let layout = std::alloc::Layout::from_size_align(len, std::mem::size_of::<u8>()).unwrap();
        unsafe { std::alloc::alloc(layout) }
    }
    pub fn dealloc(ptr: *mut u8, len: usize) {
        let layout = std::alloc::Layout::from_size_align(len, std::mem::size_of::<u8>()).unwrap();
        unsafe { std::alloc::dealloc(ptr, layout) };
    }
}
