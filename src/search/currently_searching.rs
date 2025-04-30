use std::cell::UnsafeCell;
use std::sync::Arc;

use super::search_thread::Depth;
use crate::ply::Ply;
use crate::zobrist_hash::ZobristHash;

pub const CS_SIZE: usize = 1 << 15;
pub const CS_BUCKET_SIZE: usize = 4;
// TODO: we probably need to lower defer depth with q-search.
pub const DEFER_DEPTH: Depth = Depth::ONE.wrapping_add(Depth::ONE).wrapping_add(Depth::ONE);

// TODO: move Arc out of here?
#[derive(Clone)]
pub struct CurrentlySearching(Arc<[[UnsafeCell<u64>; CS_BUCKET_SIZE]; CS_SIZE]>);

// This data structure is not intended to be 100% correct all the time, and its
// consumers should be aware of this.
// This is okay because it is not intended for synchronization or access to
// resources, but for coordination. It only needs to give the correct answer
// _most_ of the time to be useful.
unsafe impl Send for CurrentlySearching {}
unsafe impl Sync for CurrentlySearching {}

impl Default for CurrentlySearching {
    fn default() -> Self {
        Self::new()
    }
}

impl CurrentlySearching {
    pub fn new() -> CurrentlySearching {
        let cs: [[UnsafeCell<u64>; CS_BUCKET_SIZE]; CS_SIZE] = unsafe { std::mem::zeroed() };

        CurrentlySearching(cs.into())
    }

    pub fn clear(&self) {
        for bucket in self.0.iter() {
            for slot in bucket {
                unsafe { *slot.get() = 0 };
            }
        }
    }

    fn get_bucket(&self, hash: ZobristHash, ply: Ply) -> &[UnsafeCell<u64>; CS_BUCKET_SIZE] {
        // TODO: is the multiplication here mathematically necessary?
        &self.0[Self::combine(hash, ply) as usize % CS_SIZE]
    }

    fn combine(hash: ZobristHash, ply: Ply) -> u64 {
        hash.as_u64().wrapping_add(3 * ply.as_u16() as u64)
    }

    fn get(&self, hash: ZobristHash, ply: Ply) -> bool {
        let content = Self::combine(hash, ply);
        let bucket = self.get_bucket(hash, ply);

        for cell in bucket {
            let p = cell.get();
            let val = unsafe { p.read_volatile() };
            if val == content {
                return true;
            }
        }
        false
    }

    fn put(&self, hash: ZobristHash, ply: Ply) {
        let bucket = self.get_bucket(hash, ply);
        let content = Self::combine(hash, ply);

        for cell in bucket {
            let p = cell.get();
            let val = unsafe { p.read_volatile() };
            if val == content {
                return;
            }
            if val == 0 {
                unsafe { p.write_volatile(content) };
                return;
            }
        }

        unsafe { bucket[0].get().write_volatile(content) };
    }

    fn remove(&self, hash: ZobristHash, ply: Ply) {
        let bucket = self.get_bucket(hash, ply);
        let content = Self::combine(hash, ply);

        for cell in bucket {
            let p = cell.get();
            let val = unsafe { p.read_volatile() };

            // Due to potential race conditions we can't quit after zeroing one
            // hash.
            if val == content {
                // Zero the cell.
                unsafe { p.write_volatile(0) };
            }
        }
    }

    pub fn defer_move(&self, hash: ZobristHash, ply: Ply, depth: Depth) -> bool {
        if depth < DEFER_DEPTH {
            return false;
        }

        self.get(hash, ply)
    }

    pub fn starting_search(&self, hash: ZobristHash, ply: Ply, depth: Depth) {
        if depth < DEFER_DEPTH {
            return;
        }

        self.put(hash, ply);
    }

    pub fn finished_search(&self, hash: ZobristHash, ply: Ply, depth: Depth) {
        if depth < DEFER_DEPTH {
            return;
        }

        self.remove(hash, ply);
    }

    pub fn num_buckets_filled(&self) -> usize {
        let mut set = std::collections::HashSet::new();

        for bucket in self.0.iter() {
            for hash in bucket {
                let hash = unsafe { *hash.get() };
                if hash == 0 {
                    continue;
                }

                set.insert(hash);
            }
        }

        set.len()
    }
}
