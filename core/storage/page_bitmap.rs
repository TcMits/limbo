use crate::turso_assert;

#[derive(Debug)]
/// Immutable-size bitmap for use in tracking allocated pages from an arena.
pub(super) struct PageBitmap {
    /// 1 = free, 0 = allocated
    words: Box<[u64]>,
    /// current number of available pages in the arena
    n_pages: u32,
    /// where single allocations search downward from
    scan_one_high: u32,
    /// where runs search upward from
    scan_run_low: u32,
}

///
/// ```text
///                          bitmap
/// -----------|----------------------------------------------|-----
///        low ^ scan_run_low                   scan_one_high ^ high
/// ```
///
/// There are two hints or 'pointers' where we begin scanning for free pages,
/// one on each end of the map. This strategy preserves contiguous free space
/// at the low end of the arena, which is beneficial for allocating many contiguous
/// buffers that can be coalesced into fewer I/O operations.
///
/// The high end will be plucking individual pages
/// so we just subtract one from the last page index to get the next hint.
/// scanning begins for larger sequential runs from the 'low' end, so a separate
/// hint is maintained there.
///
/// After 'freeing' a run, we update the appropriate hint depending on which end
/// of which pointer it falls into.
impl PageBitmap {
    /// 64 bits per word, so shift by 6 to get page index
    const WORD_SHIFT: u32 = 6;
    const WORD_BITS: u32 = 64;
    const WORD_MASK: u32 = 63;
    const ALL_FREE: u64 = u64::MAX;
    const ALL_ALLOCATED: u64 = 0u64;

    /// Creates a new `PageBitmap` capable of tracking `n_pages` pages.
    ///
    /// If `n_pages` is not a multiple of 64, the trailing bits in the last
    /// word are marked as allocated to prevent out-of-bounds allocations.
    pub fn new(n_pages: u32) -> Self {
        turso_assert!(n_pages > 0, "PageBitmap must have at least one page");
        let n_words = n_pages.div_ceil(Self::WORD_BITS) as usize;
        let mut words = vec![Self::ALL_FREE; n_words].into_boxed_slice();
        // Mask out bits beyond n_pages as allocated (=0)
        if let Some(last_word_mask) = Self::last_word_mask(n_pages) {
            words[n_words - 1] &= last_word_mask;
        }
        Self {
            words,
            n_pages,
            scan_run_low: 0,
            scan_one_high: n_pages.saturating_sub(1),
        }
    }

    #[inline]
    /// Convert word index and bit offset to page index
    const fn word_and_bit_to_page(word_idx: usize, bit: u32) -> u32 {
        (word_idx as u32) << Self::WORD_SHIFT | bit
    }

    #[inline]
    /// Get mask for valid bits in the last word
    const fn last_word_mask(n_pages: u32) -> Option<u64> {
        let valid_bits = (n_pages as usize) & (Self::WORD_MASK as usize);
        if valid_bits != 0 {
            Some((1u64 << valid_bits) - 1)
        } else {
            None
        }
    }

    #[inline]
    /// Convert page index to word index and bit offset
    const fn page_to_word_and_bit(page_idx: u32) -> (usize, u32) {
        (
            (page_idx >> Self::WORD_SHIFT) as usize,
            page_idx & Self::WORD_MASK,
        )
    }

    /// Allocates a single free page from the bitmap.
    ///
    /// This method scans from high to low addresses to preserve contiguous
    /// runs of free pages at the low end of the bitmap.
    pub fn alloc_one(&mut self) -> Option<u32> {
        if self.n_pages == 0 {
            return None;
        }

        // Scan from high to low to preserve contiguous runs at the front
        for (word_idx, word) in self.words.iter_mut().enumerate().rev() {
            if *word != Self::ALL_ALLOCATED {
                // Find the highest set bit (rightmost free page in this word)
                let bit = 63 - word.leading_zeros();
                let page_idx = Self::word_and_bit_to_page(word_idx, bit);

                if page_idx < self.n_pages {
                    *word &= !(1u64 << bit);
                    // store hint for next single allocation
                    self.scan_one_high = page_idx.saturating_sub(1);
                    return Some(page_idx);
                }
            }
        }
        None
    }

    /// Allocates a contiguous run of `need` pages from the bitmap.
    /// This method scans from low to high addresses, starting from the
    /// `scan_run_low` pointer.
    pub fn alloc_run(&mut self, need: u32) -> Option<u32> {
        if need == 1 {
            return self.alloc_one();
        }
        if need == 0 || need > self.n_pages {
            return None;
        }

        // Two-pass search with scan_hint optimization
        let mut search_start = self.scan_run_low;
        for pass in 0..2 {
            if pass == 1 {
                search_start = 0;
            }

            if let Some(found) = self.find_free_run(search_start, need) {
                self.mark_run(found, need, false);
                self.scan_run_low = found + need;
                return Some(found);
            }

            if search_start == 0 {
                // Already searched from beginning
                break;
            }
        }
        None
    }

    /// Search for a free run of `need` pages beginning from `start`
    fn find_free_run(&self, start: u32, need: u32) -> Option<u32> {
        let mut pos = start;
        let limit = self.n_pages.saturating_sub(need - 1);

        while pos < limit {
            if let Some(next_free) = self.next_free_bit_from(pos) {
                if next_free + need > self.n_pages {
                    break;
                }
                if self.check_run_free(next_free, need) {
                    return Some(next_free);
                }
                pos = next_free + 1;
            } else {
                break;
            }
        }
        None
    }

    /// Frees a contiguous run of pages, marking them as available for reallocation
    /// and update the scan hints to potentially reuse the freed space in future allocations.
    pub fn free_run(&mut self, start: u32, count: u32) {
        if count == 0 {
            return;
        }
        self.mark_run(start, count, true);
        // Update scan hint to potentially reuse this space
        if start < self.scan_run_low {
            self.scan_run_low = start;
        } else if start > self.scan_one_high {
            // If we freed a run beyond the current scan hint, adjust it
            self.scan_one_high = start.saturating_add(count).saturating_sub(1);
        }
    }

    /// Checks whether a contiguous run of pages is completely free.
    pub fn check_run_free(&self, start: u32, len: u32) -> bool {
        if start.saturating_add(len) > self.n_pages {
            return false;
        }
        self.inspect_run(start, len, |word, mask| (word & mask) == mask)
    }

    /// Marks a contiguous run of pages as either free or allocated.
    pub fn mark_run(&mut self, start: u32, len: u32, free: bool) {
        let (mut word_idx, bit_offset) = Self::page_to_word_and_bit(start);
        let mut remaining = len as usize;
        let mut pos_in_word = bit_offset as usize;

        while remaining > 0 {
            let bits_in_word = (Self::WORD_BITS as usize).saturating_sub(pos_in_word);
            let bits_to_process = remaining.min(bits_in_word);

            let mask = if bits_to_process == Self::WORD_BITS as usize {
                Self::ALL_FREE
            } else {
                (1u64 << bits_to_process).saturating_sub(1) << pos_in_word
            };

            if free {
                self.words[word_idx] |= mask;
            } else {
                self.words[word_idx] &= !mask;
            }

            remaining -= bits_to_process;
            // move to the next word/reset position
            word_idx += 1;
            pos_in_word = 0;
        }
    }

    /// Process a run of bits with a read-only operation
    fn inspect_run<F>(&self, start: u32, len: u32, mut check: F) -> bool
    where
        F: FnMut(u64, u64) -> bool,
    {
        let (mut word_idx, bit_offset) = Self::page_to_word_and_bit(start);
        let mut remaining = len as usize;
        let mut pos_in_word = bit_offset as usize;

        while remaining > 0 {
            let bits_in_word = (Self::WORD_BITS as usize).saturating_sub(pos_in_word);
            let bits_to_process = remaining.min(bits_in_word);

            let mask = if bits_to_process == Self::WORD_BITS as usize {
                Self::ALL_FREE
            } else {
                (1u64 << bits_to_process).saturating_sub(1) << pos_in_word
            };

            if !check(self.words[word_idx], mask) {
                return false;
            }

            remaining -= bits_to_process;
            word_idx += 1;
            pos_in_word = 0;
        }
        true
    }

    /// Try to find next free bit (1) at or after `from` page index.
    pub fn next_free_bit_from(&self, from: u32) -> Option<u32> {
        if from >= self.n_pages {
            return None;
        }

        let (mut word_idx, bit_offset) = Self::page_to_word_and_bit(from);

        // Check current word from bit_offset onward
        let mask = u64::MAX << bit_offset;
        let current = self.words[word_idx] & mask;
        if current != 0 {
            let bit = current.trailing_zeros();
            return Some(Self::word_and_bit_to_page(word_idx, bit));
        }

        // Check remaining words
        word_idx += 1;
        while word_idx < self.words.len() {
            if self.words[word_idx] != Self::ALL_ALLOCATED {
                let bit = self.words[word_idx].trailing_zeros();
                return Some(Self::word_and_bit_to_page(word_idx, bit));
            }
            word_idx += 1;
        }
        None
    }
}
