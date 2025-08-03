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

#[cfg(test)]
pub mod tests {
    use super::*;
    use rand::{rngs::StdRng, Rng, SeedableRng};

    fn pb_free_vec(pb: &PageBitmap) -> Vec<bool> {
        let mut v = vec![false; pb.n_pages as usize];
        v.iter_mut()
            .enumerate()
            .take(pb.n_pages as usize)
            .map(|(i, v)| {
                let w = pb.words[i >> 6];
                let bit = i & 63;
                *v = (w & (1u64 << bit)) != 0;
                *v
            })
            .collect()
    }

    /// Count free bits in the reference model.
    fn ref_count_free(model: &[bool]) -> usize {
        model.iter().filter(|&&b| b).count()
    }

    /// Find the first free index >= from in the reference model.
    fn ref_first_free_from(model: &[bool], from: u32) -> Option<u32> {
        let from = from as usize;
        model
            .iter()
            .enumerate()
            .skip(from)
            .find_map(|(i, &f)| if f { Some(i as u32) } else { None })
    }

    /// Check whether [start, start+len) are all free in the reference model.
    fn ref_check_run_free(model: &[bool], start: u32, len: u32) -> bool {
        let s = start as usize;
        let l = len as usize;
        if s + l > model.len() {
            return false;
        }
        model[s..s + l].iter().all(|&b| b)
    }

    /// Mark [start, start+len) free(=true) or allocated(=false) in the reference model.
    fn ref_mark_run(model: &mut [bool], start: u32, len: u32, free: bool) {
        let st = start as usize;
        let len = len as usize;
        for page in &mut model[st..st + len] {
            *page = free;
        }
    }

    /// Returns `true` if the bitmap's notion of free bits equals the reference model exactly.
    fn assert_equivalent(pb: &PageBitmap, model: &[bool]) {
        let pv = pb_free_vec(pb);
        assert_eq!(pv, model, "bitmap bits disagree with reference model");
    }

    #[test]
    fn new_masks_trailing_bits() {
        // test weird page counts
        for n_pages in [1, 63, 64, 65, 127, 128, 129, 255, 256, 1023] {
            let pb = PageBitmap::new(n_pages);
            // All valid pages must be free.
            let free = pb_free_vec(&pb);
            assert_eq!(free.len(), n_pages as usize);
            assert!(free.iter().all(|&b| b), "all pages should start free");

            // Bits beyond n_pages must be treated as allocated (masked out).
            // We check the last word explicitly.
            if n_pages > 0 {
                let words_len = pb.words.len();
                let valid_bits = (n_pages as usize) & 63;
                if valid_bits != 0 {
                    let last = pb.words[words_len - 1];
                    let mask = (1u64 << valid_bits) - 1;
                    assert_eq!(last & !mask, 0, "bits beyond n_pages must be 0");
                }
            }
        }
    }

    #[test]
    fn alloc_one_exhausts_all() {
        let mut pb = PageBitmap::new(257);
        let mut model = vec![true; 257];

        let mut count = 0;
        while let Some(idx) = pb.alloc_one() {
            assert!(model[idx as usize], "must be free in model");
            model[idx as usize] = false;
            count += 1;
        }
        assert_eq!(count, 257, "should allocate all pages once");
        assert!(pb.alloc_one().is_none(), "no pages left");
        assert_equivalent(&pb, &model);
    }

    #[test]
    fn alloc_run_basic_and_free() {
        let mut pb = PageBitmap::new(200);
        let mut model = vec![true; 200];

        // Allocate a run of 70 crossing word boundaries
        let need = 70;
        let start = pb.alloc_run(need).expect("expected a run");
        assert!(ref_check_run_free(&model, start, need));
        ref_mark_run(&mut model, start, need, false);
        assert!(!pb.check_run_free(start, need));
        assert_equivalent(&pb, &model);

        // Free it and verify again
        pb.free_run(start, need);
        ref_mark_run(&mut model, start, need, true);
        assert!(pb.check_run_free(start, need));
        assert_equivalent(&pb, &model);
    }

    #[test]
    fn alloc_run_none_when_only_smaller_gaps() {
        let n = 128u32;
        let mut pb = PageBitmap::new(n);

        // Allocate everything in one go.
        let start = pb.alloc_run(n).expect("whole arena should be free");
        assert_eq!(start, 0);
        let mut model = vec![false; n as usize];
        assert_equivalent(&pb, &model);

        // heavily fragment, free exactly every other page to create isolated 1-page holes
        for i in (0..n).step_by(2) {
            pb.free_run(i, 1);
            model[i as usize] = true;
        }
        assert_equivalent(&pb, &model);

        // no run of 2 should exist
        assert!(
            pb.alloc_run(2).is_none(),
            "no free run of length 2 should exist"
        );
        assert_equivalent(&pb, &model);
    }

    #[test]
    fn next_free_bit_from_matches_reference() {
        let mut pb = PageBitmap::new(130);
        let mut model = vec![true; 130];

        // Allocate a few arbitrary runs/singles
        let r1 = pb.alloc_run(10).unwrap();
        ref_mark_run(&mut model, r1, 10, false);

        let r2 = pb.alloc_run(1).unwrap();
        model[r2 as usize] = false;

        let r3 = pb.alloc_run(50).unwrap();
        ref_mark_run(&mut model, r3, 50, false);

        // Check random 'from' points
        for from in [0, 1, 5, 9, 10, 59, 60, 61, 64, 100, 129] {
            let expect = ref_first_free_from(&model, from);
            let got = pb.next_free_bit_from(from);
            assert_eq!(got, expect, "from={from}");
        }
        assert_equivalent(&pb, &model);
    }

    #[test]
    fn singles_from_tail_preserve_front_run() {
        // This asserts the desirable policy, single-page allocations should
        // not destroy large runs at the front
        let mut pb = PageBitmap::new(512);
        let mut model = vec![true; 512];

        // Take 100 single pages, model updates at returned indices
        for _ in 0..100 {
            let idx = pb.alloc_one().unwrap();
            model[idx as usize] = false;
        }
        assert_equivalent(&pb, &model);

        // There should still be a long free run near the beginning.
        // Request 64, it should succeed
        let r = pb.alloc_run(64).expect("64-run should still be available");
        assert!(ref_check_run_free(&model, r, 64));
        ref_mark_run(&mut model, r, 64, false);
        assert_equivalent(&pb, &model);
    }

    #[test]
    fn fuzz_rand_compare_with_reference_model() {
        let seeds: &[u64] = &[
            std::time::SystemTime::UNIX_EPOCH
                .elapsed()
                .unwrap_or_default()
                .as_secs(),
            1234567890,
            0x69420,
            94822,
            165029,
        ];
        for &seed in seeds {
            let mut rng = StdRng::seed_from_u64(seed);
            // random size including tricky boundaries
            let n_pages = match rng.gen_range(0..6) {
                0 => 777,
                1 => 1,
                2 => 63,
                3 => 64,
                4 => 65,
                _ => rng.gen_range(1..=2048),
            } as u32;

            let mut pb = PageBitmap::new(n_pages);
            let mut model = vec![true; n_pages as usize];

            let iters = 2000usize;
            for _ in 0..iters {
                let op = rng.gen_range(0..100);
                match op {
                    0..=49 => {
                        // alloc_one
                        let before_free = ref_count_free(&model);
                        let got = pb.alloc_one();
                        if let Some(i) = got {
                            assert!(i < n_pages, "index in range");
                            assert!(model[i as usize], "bit must be free");
                            model[i as usize] = false;
                            assert_eq!(ref_count_free(&model), before_free - 1);
                        } else {
                            // Then model must have no free bits
                            assert_eq!(before_free, 0, "no free bits if None returned");
                        }
                    }
                    50..=79 => {
                        // alloc_run with random length
                        let need =
                            rng.gen_range(1..=std::cmp::max(1, (n_pages as usize).min(128))) as u32;
                        let got = pb.alloc_run(need);
                        if let Some(start) = got {
                            assert!(start + need <= n_pages, "within bounds");
                            assert!(
                                ref_check_run_free(&model, start, need),
                                "run must be free in model"
                            );
                            ref_mark_run(&mut model, start, need, false);
                        } else {
                            // If None, assert there is no free run of 'need' in the model.
                            let mut exists = false;
                            for s in 0..=n_pages.saturating_sub(need) {
                                if ref_check_run_free(&model, s, need) {
                                    exists = true;
                                    break;
                                }
                            }
                            assert!(!exists, "allocator returned None but a free run exists");
                        }
                    }
                    _ => {
                        // free_run on a random valid range
                        let len =
                            rng.gen_range(1..=std::cmp::max(1, (n_pages as usize).min(128))) as u32;
                        let max_start = n_pages.saturating_sub(len);
                        let start = if max_start == 0 {
                            0
                        } else {
                            rng.gen_range(0..=max_start)
                        };
                        pb.free_run(start, len);
                        ref_mark_run(&mut model, start, len, true);
                    }
                }
                // Occasionally check next_free_bit_from correctness against model
                if rng.gen_bool(0.2) {
                    let from = rng.gen_range(0..n_pages);
                    let got = pb.next_free_bit_from(from);
                    let expect = ref_first_free_from(&model, from);
                    assert_eq!(got, expect, "next_free_bit_from(from={from})");
                }
                // Keep both representations in sync
                assert_equivalent(&pb, &model);
            }
        }
    }
}
