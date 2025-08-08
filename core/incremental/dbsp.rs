// Simplified DBSP integration for incremental view maintenance
// For now, we'll use a basic approach and can expand to full DBSP later

use std::collections::HashMap;

/// A simplified ZSet for incremental computation
/// Each element has a weight: positive for additions, negative for deletions
#[derive(Clone, Debug, Default)]
pub struct SimpleZSet<T> {
    data: HashMap<T, isize>,
}

impl<T: std::hash::Hash + Eq + Clone> SimpleZSet<T> {
    pub fn new() -> Self {
        Self {
            data: HashMap::new(),
        }
    }

    pub fn insert(&mut self, item: T, weight: isize) {
        let current = self.data.get(&item).copied().unwrap_or(0);
        let new_weight = current + weight;
        if new_weight == 0 {
            self.data.remove(&item);
        } else {
            self.data.insert(item, new_weight);
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&T, isize)> {
        self.data.iter().map(|(k, &v)| (k, v))
    }

    /// Get all items with positive weights
    pub fn to_vec(&self) -> Vec<T> {
        self.data
            .iter()
            .filter(|(_, &weight)| weight > 0)
            .map(|(item, _)| item.clone())
            .collect()
    }

    pub fn merge(&mut self, other: &SimpleZSet<T>) {
        for (item, weight) in other.iter() {
            self.insert(item.clone(), weight);
        }
    }
}

/// A simplified stream for incremental computation
#[derive(Clone, Debug)]
pub struct SimpleStream<T> {
    current: SimpleZSet<T>,
}

impl<T: std::hash::Hash + Eq + Clone> SimpleStream<T> {
    pub fn from_zset(zset: SimpleZSet<T>) -> Self {
        Self { current: zset }
    }

    /// Apply a delta (change) to the stream
    pub fn apply_delta(&mut self, delta: &SimpleZSet<T>) {
        self.current.merge(delta);
    }

    /// Get the current state as a vector of items (only positive weights)
    pub fn to_vec(&self) -> Vec<T> {
        self.current.to_vec()
    }
}

// Type aliases for convenience
use super::hashable_row::HashableRow;

pub type RowKey = HashableRow;
pub type RowKeyZSet = SimpleZSet<RowKey>;
pub type RowKeyStream = SimpleStream<RowKey>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_zset_merge_with_weights() {
        let mut zset1 = SimpleZSet::new();
        zset1.insert(1, 1); // Row 1 with weight +1
        zset1.insert(2, 1); // Row 2 with weight +1

        let mut zset2 = SimpleZSet::new();
        zset2.insert(2, -1); // Row 2 with weight -1 (delete)
        zset2.insert(3, 1); // Row 3 with weight +1 (insert)

        zset1.merge(&zset2);

        // Row 1: weight 1 (unchanged)
        // Row 2: weight 0 (deleted)
        // Row 3: weight 1 (inserted)
        assert_eq!(zset1.iter().count(), 2); // Only rows 1 and 3
        assert!(zset1.iter().any(|(k, _)| *k == 1));
        assert!(zset1.iter().any(|(k, _)| *k == 3));
        assert!(!zset1.iter().any(|(k, _)| *k == 2)); // Row 2 removed
    }

    #[test]
    fn test_zset_represents_updates_as_delete_plus_insert() {
        let mut zset = SimpleZSet::new();

        // Initial state
        zset.insert(1, 1);

        // Update row 1: delete old + insert new
        zset.insert(1, -1); // Delete old version
        zset.insert(1, 1); // Insert new version

        // Weight should be 1 (not 2)
        let weight = zset.iter().find(|(k, _)| **k == 1).map(|(_, w)| w);
        assert_eq!(weight, Some(1));
    }
}
