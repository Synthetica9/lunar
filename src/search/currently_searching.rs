use std::cell::UnsafeCell;
use std::sync::Arc;

use crate::zobrist_hash::ZobristHash;

pub const CS_SIZE: usize = 1 << 15;
pub const CS_BUCKET_SIZE: usize = 4;
pub const DEFER_DEPTH: usize = 3;

// TODO: move Arc out of here?
#[derive(Clone)]
pub struct CurrentlySearching(Arc<[[UnsafeCell<ZobristHash>; CS_BUCKET_SIZE]; CS_SIZE]>);

// This data structure is not intended to be 100% correct all the time, and its
// consumers should be aware of this.
// This is okay because it is not intended for synchronization or access to
// resources, but for coordination. It only needs to give the correct answer
// _most_ of the time to be useful.
unsafe impl Send for CurrentlySearching {}
unsafe impl Sync for CurrentlySearching {}

impl CurrentlySearching {
    pub fn new() -> CurrentlySearching {
        // Use maybe_uninit
        let cs: [[UnsafeCell<ZobristHash>; CS_BUCKET_SIZE]; CS_SIZE] =
            unsafe { std::mem::uninitialized() };
        for bucket in &cs {
            for cell in bucket {
                unsafe {
                    *cell.get() = ZobristHash::new();
                }
            }
        }

        CurrentlySearching(cs.into())
    }

    fn get_bucket<'a>(
        &'a self,
        hash: ZobristHash,
    ) -> &'a [UnsafeCell<ZobristHash>; CS_BUCKET_SIZE] {
        &self.0[hash.as_u64() as usize % CS_SIZE]
    }

    fn get(&self, hash: ZobristHash) -> bool {
        let bucket = self.get_bucket(hash);
        for cell in bucket {
            let p = cell.get();
            let val = unsafe { p.read_volatile() };
            if val == hash {
                return true;
            }
        }
        return false;
    }

    fn put(&self, hash: ZobristHash) {
        let bucket = self.get_bucket(hash);
        for cell in bucket {
            let p = cell.get();
            let val = unsafe { p.read_volatile() };
            if val == hash {
                return;
            }
            if val.as_u64() == 0 {
                unsafe { p.write_volatile(hash) };
                return;
            }
        }
    }

    fn remove(&self, hash: ZobristHash) {
        let bucket = self.get_bucket(hash);
        for cell in bucket {
            let p = cell.get();
            let val = unsafe { p.read_volatile() };

            // Due to potential race conditions we can't quit after zeroing one
            // hash.
            if val == hash {
                // Zero the cell.
                unsafe { p.write_volatile(ZobristHash::new()) };
            }
        }
    }

    pub fn defer_move(&self, hash: ZobristHash, depth: usize) -> bool {
        if depth < DEFER_DEPTH {
            return false;
        }

        self.get(hash)
    }

    pub fn starting_search(&self, hash: ZobristHash, depth: usize) {
        if depth < DEFER_DEPTH {
            return;
        }

        self.put(hash)
    }

    pub fn finished_search(&self, hash: ZobristHash, depth: usize) {
        if depth < DEFER_DEPTH {
            return;
        }

        self.remove(hash)
    }
}
