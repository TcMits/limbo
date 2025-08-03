use crate::fast_lock::SpinLock;
use crate::io::TEMP_BUFFER_CACHE;
use crate::storage::page_bitmap::PageBitmap;
use crate::storage::sqlite3_ondisk::WAL_FRAME_HEADER_SIZE;
use crate::{turso_assert, Buffer, LimboError, IO};
use parking_lot::Mutex;
use std::cell::UnsafeCell;
use std::ptr::NonNull;
use std::sync::atomic::{AtomicU32, AtomicUsize, Ordering};
use std::sync::{Arc, OnceLock, Weak};

pub static BUFFER_POOL: OnceLock<Arc<BufferPool>> = OnceLock::new();

#[derive(Debug)]
/// A buffer allocated from an arena from `[BufferPool]`
pub struct ArenaBuffer {
    ptr: NonNull<u8>,
    arena_id: u32,
    page_idx: u32,
    len: usize,
}

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
    /// Returns the `id` of the underlying arena, only if it was registered with `io_uring`
    pub fn fixed_id(&self) -> Option<u32> {
        if self.arena_id < UNREGISTERED_START {
            Some(self.arena_id)
        } else {
            None
        }
    }

    /// The requested size of the allocation, the actual size of the underlying buffer is rounded up to
    /// the next multiple of the arena's page_size
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
        let inner = pool.inner_mut();
        inner.free(self.logical_len(), self.arena_id, self.page_idx);
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
    page_arena: Option<Arena>,
    wal_frame_arena: Option<Arena>,
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
                page_arena: None,
                wal_frame_arena: None,
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
        let inner = self.inner_mut();
        inner.get_page()
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
    /// an arena for the pool. Before this call, the pool will use temporary buffers which are
    /// cached in thread local storage.
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
        if inner.page_arena.is_some() {
            return Ok(pool.clone());
        }
        inner.db_page_size.store(page_size, Ordering::Relaxed);
        inner.init_arenas()?;
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

        let db_page_size = self.db_page_size.load(Ordering::Relaxed);
        let wal_frame_size = db_page_size + WAL_FRAME_HEADER_SIZE;

        // Check if this is exactly a WAL frame size allocation
        if len == wal_frame_size {
            let Some(wal_arena) = self.wal_frame_arena.as_ref() else {
                return Buffer::new_temporary(len);
            };
            if let Some(FreeEntry { ptr, first_idx }) = wal_arena.try_alloc(len) {
                tracing::trace!(
                    "Allocated WAL frame buffer of length {} from arena {} at index {}",
                    len,
                    wal_arena.id,
                    first_idx
                );
                return Buffer::new_pooled(ArenaBuffer::new(ptr, len, wal_arena.id, first_idx));
            }
        }
        // For all other sizes, use regular arena
        let Some(arena) = self.page_arena.as_ref() else {
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

    fn get_page(&mut self) -> Buffer {
        let db_page_size = self.db_page_size.load(Ordering::Relaxed);
        let Some(arena) = self.page_arena.as_ref() else {
            return Buffer::new_temporary(db_page_size);
        };
        if let Some(FreeEntry { ptr, first_idx }) = arena.try_alloc(db_page_size) {
            tracing::trace!(
                "Allocated page buffer of size {} from arena {} at index {}",
                db_page_size,
                arena.id,
                first_idx
            );
            return Buffer::new_pooled(ArenaBuffer::new(ptr, db_page_size, arena.id, first_idx));
        }
        Buffer::new_temporary(db_page_size)
    }

    /// Allocate a new arena for the pool to use
    fn init_arenas(&mut self) -> crate::Result<()> {
        // Prevent concurrent growth
        let Some(_guard) = self.init_lock.try_lock() else {
            tracing::debug!("Buffer pool is already growing, skipping initialization");
            return Ok(()); // Already in progress
        };
        let arena_size = self.arena_size.load(Ordering::Relaxed);
        let db_page_size = self.db_page_size.load(Ordering::Relaxed);
        let io = self.io.as_ref().expect("Pool not initialized").clone();

        // Create regular page arena
        match Arena::new(db_page_size, arena_size, &io) {
            Ok(arena) => {
                tracing::trace!(
                    "added arena {} with size {} MB and page size {}",
                    arena.id,
                    arena_size / (1024 * 1024),
                    db_page_size
                );
                self.page_arena = Some(arena);
            }
            Err(e) => {
                tracing::error!("Failed to create arena: {:?}", e);
                return Err(LimboError::InternalError(format!(
                    "Failed to create arena: {e}",
                )));
            }
        }

        // Create WAL frame arena
        let wal_frame_size = db_page_size + WAL_FRAME_HEADER_SIZE;
        match Arena::new(wal_frame_size, arena_size, &io) {
            Ok(arena) => {
                tracing::trace!(
                    "added WAL frame arena {} with size {} MB and page size {}",
                    arena.id,
                    arena_size / (1024 * 1024),
                    wal_frame_size
                );
                self.wal_frame_arena = Some(arena);
            }
            Err(e) => {
                tracing::error!("Failed to create WAL frame arena: {:?}", e);
                return Err(LimboError::InternalError(format!(
                    "Failed to create WAL frame arena: {e}",
                )));
            }
        }

        Ok(())
    }

    pub fn free(&self, size: usize, arena_id: u32, page_idx: u32) {
        // Check WAL frame arena first
        if let Some(wal_arena) = self.wal_frame_arena.as_ref() {
            if arena_id == wal_arena.id {
                let pages = size.div_ceil(wal_arena.page_size);
                tracing::trace!("Freeing {} pages from WAL frame arena {}", pages, arena_id);
                wal_arena.free(page_idx, pages);
                return;
            }
        }

        // Otherwise use regular arena
        let arena = self
            .page_arena
            .as_ref()
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

/// Preallocated block of memory used by the pool to distribute Buffers
struct Arena {
    /// Identifier to tie allocations back to the arena
    id: u32,
    /// base pointer to the arena returned by `mmap`
    base: NonNull<u8>,
    /// Total number of pages currently allocated from this arena
    allocated_pages: AtomicUsize,
    /// Currently free pages
    free_pages: SpinLock<PageBitmap>,
    /// Total size of the arena in bytes
    arena_size: usize,
    /// Page size the total arena is divided into.
    /// Because most allocations are of size `[Database::page_size]`, with an
    /// additional 24 byte wal frame header, we treat this as the `page_size` to reduce
    /// fragmentation to 24 bytes for regular pages
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

const UNREGISTERED_START: u32 = 2;

/// For an arena which isn't registered with `io_uring`
/// registered arena will always have id = 0..=1
static NEXT_ID: AtomicU32 = AtomicU32::new(UNREGISTERED_START);

impl Arena {
    /// Create a new arena with the given size and page size.
    fn new(page_size: usize, arena_size: usize, io: &Arc<dyn IO>) -> Result<Self, String> {
        let ptr = unsafe { arena::alloc(arena_size) };
        let base = NonNull::new(ptr).ok_or("Failed to allocate arena")?;
        let total_pages = arena_size / page_size;
        let id = io
            .register_fixed_buffer(base, arena_size)
            .unwrap_or_else(|_| {
                // Register with io_uring if possible, otherwise use next available ID
                let next_id = NEXT_ID.fetch_add(1, Ordering::Relaxed);
                tracing::trace!("Allocating arena with id {}", next_id);
                next_id
            });

        Ok(Self {
            id,
            base,
            free_pages: SpinLock::new(PageBitmap::new(total_pages as u32)),
            allocated_pages: AtomicUsize::new(0),
            page_size,
            arena_size,
        })
    }

    pub fn try_alloc(&self, size: usize) -> Option<FreeEntry> {
        let pages = size.div_ceil(self.page_size) as u32;
        let mut freemap = self.free_pages.lock();

        let first_idx = if pages == 1 {
            freemap.alloc_one()?
        } else {
            freemap.alloc_run(pages)?
        };

        self.allocated_pages
            .fetch_add(pages as usize, Ordering::Relaxed);
        let offset = first_idx as usize * self.page_size;
        let ptr = unsafe { NonNull::new_unchecked(self.base.as_ptr().add(offset)) };
        Some(FreeEntry { ptr, first_idx })
    }

    /// Allocate a contiguous run of `pages`. Returns the first page index.
    #[inline]
    pub fn alloc_run(&self, pages: u32) -> Option<u32> {
        let mut bm = self.free_pages.lock();
        bm.alloc_run(pages)
    }

    pub fn free(&self, page_idx: u32, count: usize) {
        let mut bm = self.free_pages.lock();
        bm.free_run(page_idx, count as u32);
        self.allocated_pages.fetch_sub(count, Ordering::Relaxed);
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
