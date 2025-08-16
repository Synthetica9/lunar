use std::cell::UnsafeCell;
use std::mem::MaybeUninit;
use std::simd::cmp::SimdPartialEq;
use std::sync::atomic::{AtomicIsize, AtomicU8, Ordering};

use not_empty::NonEmptySlice;
use static_assertions::*;
use std::num::NonZeroUsize;

use crate::game::Game;
use crate::millipawns::Millipawns;
use crate::ply::Ply;
use crate::zobrist_hash::ZobristHash;

#[cfg(unix)]
use crate::hugepages_mmap_alloc::HugePagesAlloc;

pub struct TranspositionTable {
    #[cfg(not(unix))]
    table: Box<NonEmptySlice<UnsafeCell<TranspositionLine>>>,

    #[cfg(unix)]
    table: Box<NonEmptySlice<UnsafeCell<TranspositionLine>>, HugePagesAlloc>,

    occupancy: AtomicIsize,
    age: AtomicU8,
}

unsafe impl Sync for TranspositionTable {}

const ITEMS_PER_BUCKET: usize = 3;
type HashBucket = std::simd::u16x4;

const_assert!(ITEMS_PER_BUCKET <= HashBucket::LEN);
assert_eq_size!(TranspositionEntry, u64);
const_assert!(std::mem::size_of::<TranspositionLine>() <= 32);

#[repr(align(32))]
struct TranspositionLine(HashBucket, [TranspositionEntry; ITEMS_PER_BUCKET]);

impl TranspositionLine {
    fn empty() -> Self {
        unsafe { std::mem::zeroed() }
    }
}

// TODO: rename to ValueType?
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TranspositionEntryType {
    Exact,
    LowerBound,
    UpperBound,
}

const AGE_BITS: usize = 6;
const MAX_AGE: u8 = (1 << AGE_BITS) - 1;

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct TranspositionEntry {
    pub depth: u8,
    pub value: Millipawns,
    best_move: Ply,
    // LSB 0, 1: value_type
    // LSB 2-7: age
    age_value_type: u8,
}

pub enum PutResult {
    ValueReplaced,
    ValueAdded,
    Noop,
}

impl TranspositionEntry {
    fn as_u64(self) -> u64 {
        unsafe { std::mem::transmute(self) }
    }

    fn from_u64(val: u64) -> TranspositionEntry {
        unsafe { std::mem::transmute(val) }
    }

    fn is_some(self) -> bool {
        self.as_u64() != 0
    }

    pub fn value_type(&self) -> TranspositionEntryType {
        use TranspositionEntryType::*;
        let value_type_u8 = self.age_value_type % 4;
        match value_type_u8 {
            0 | 3 => Exact,
            1 => LowerBound,
            2 => UpperBound,
            _ => unreachable!(),
        }
    }

    pub fn best_move(&self) -> Option<Ply> {
        self.best_move.wrap_null()
    }

    pub fn age(&self) -> u8 {
        self.age_value_type / 4
    }

    pub fn new(
        depth: u8,
        best_move: Option<Ply>,
        value: Millipawns,
        value_type: TranspositionEntryType,
        age: u8,
    ) -> TranspositionEntry {
        debug_assert!(age <= MAX_AGE);
        TranspositionEntry {
            depth,
            value,
            best_move: Ply::unwrap_null(best_move),
            age_value_type: age << (8 - AGE_BITS) | value_type as u8,
        }
    }
}

impl TranspositionTable {
    pub fn new(bytes: usize) -> TranspositionTable {
        assert!(bytes > 0, "empty tt not supported");

        let needed_lines = bytes / std::mem::size_of::<TranspositionLine>();

        // TODO: should each thread do their part of this?
        #[cfg(unix)]
        let boxed: Box<[MaybeUninit<TranspositionLine>], HugePagesAlloc> =
            Box::new_uninit_slice_in(needed_lines, HugePagesAlloc);

        #[cfg(not(unix))]
        let boxed: Box<[MaybeUninit<TranspositionLine>]> = Box::new_uninit_slice(needed_lines);

        assert!(!boxed.is_empty(), "tt was found empty!");
        assert!(boxed.len().is_power_of_two(), "must be power of two");

        // TODO: can this be done better?
        // SAFETY: Box<[MaybeUninit<T>], A> and Box<NonEmptySlice<T>, A> have the same memory layout
        let boxed_nonempty = unsafe { std::mem::transmute(boxed) };

        let mut res = Self {
            table: boxed_nonempty,
            occupancy: 0.into(),
            age: (MAX_AGE / 2).into(),
        };

        res.madv_random();
        res
    }

    fn madv_random(&mut self) {
        #[cfg(unix)]
        {
            use nix::sys::mman::{madvise, MmapAdvise};
            use std::os::raw::c_void;
            let addr = self.table.as_mut_ptr().cast::<c_void>();
            let length = self.num_bytes();

            let advises = [
                MmapAdvise::MADV_RANDOM,
                MmapAdvise::MADV_DONTDUMP,
                MmapAdvise::MADV_WILLNEED,
            ];

            let addr = std::ptr::NonNull::new(addr)
                .expect("addr is not null (we initialized the hash table)");

            for advise in advises {
                unsafe {
                    madvise(addr, length, advise).expect("madvise failed");
                }
            }
        }
    }

    pub fn inc_age(&self) {
        self.age.fetch_add(1, Ordering::Acquire);
        self.age.fetch_and(MAX_AGE, Ordering::Release);
        self.occupancy.store(0, Ordering::Release);
    }

    pub fn age(&self) -> u8 {
        self.age.load(Ordering::Relaxed)
    }

    pub fn occupancy(&self) -> isize {
        self.occupancy.load(Ordering::Acquire)
    }

    pub fn occupancy_mil(&self) -> isize {
        (self.occupancy() * 1000) / self.num_entries() as isize
    }

    pub fn add_occupancy(&self, extra: usize) -> isize {
        self.occupancy.fetch_add(extra as isize, Ordering::AcqRel)
    }

    pub fn clear(&self) {
        for item in self.table.iter() {
            unsafe { item.get().write(TranspositionLine::empty()) };
        }
    }

    pub fn num_buckets(&self) -> usize {
        self.table.len().into()
    }

    pub fn num_entries(&self) -> usize {
        self.num_buckets() * ITEMS_PER_BUCKET
    }

    pub fn num_bytes(&self) -> usize {
        self.num_buckets() * std::mem::size_of::<TranspositionLine>()
    }

    fn bucket(&self, hash: ZobristHash) -> &UnsafeCell<TranspositionLine> {
        &self.table[self.bucket_idx(hash)]
    }

    pub fn bucket_idx(&self, hash: ZobristHash) -> usize {
        let num_buckets: NonZeroUsize = self.table.len();
        let n: usize = num_buckets.into();

        #[cfg(feature = "intrinsics")]
        {
            debug_assert!(n > 0);
            // NonZeroUsize doesn't propagate this static claim. This is important to make some
            // other things non-panicking.
            // SAFTEY: guaranteed by NonZeroUsize, see also assert above.
            unsafe { std::intrinsics::assume(n > 0) }
        }
        debug_assert!(n.is_power_of_two());
        hash.to_usize() & (n - 1)
    }

    pub fn get(&self, hash: ZobristHash) -> Option<TranspositionEntry> {
        let bucket_p = self.bucket(hash).get();
        let bucket = { unsafe { bucket_p.read() } };

        let hash_upper_16 = hash.upper_16();
        let hash_splat = HashBucket::splat(hash_upper_16);
        let idx = bucket
            .0
            .simd_eq(hash_splat)
            .first_set()
            .filter(|x| *x < ITEMS_PER_BUCKET)?;

        Some(bucket.1[idx])
    }

    #[allow(unreachable_code, clippy::needless_return)]
    pub fn prefetch_read(&self, hash: ZobristHash) {
        let ptr = self.bucket(hash).get();

        #[cfg(target_arch = "x86_64")]
        {
            use core::arch::x86_64::*;
            const FLAG: i32 = _MM_HINT_T0;
            unsafe { _mm_prefetch(ptr.cast(), FLAG) };
            return;
        }

        #[cfg(feature = "intrinsics")]
        {
            unsafe { core::intrinsics::prefetch_read_data(ptr, 3) }
            return;
        }
    }

    #[allow(unreachable_code, clippy::needless_return)]
    pub fn prefetch_write(&self, hash: ZobristHash) {
        let ptr = self.bucket(hash).get();

        #[cfg(target_arch = "x86_64")]
        {
            use core::arch::x86_64::*;
            const FLAG: i32 = _MM_HINT_ET0;
            unsafe { _mm_prefetch(ptr.cast(), FLAG) };
            return;
        }

        #[cfg(feature = "intrinsics")]
        {
            unsafe { core::intrinsics::prefetch_write_data(ptr, 3) }
            return;
        }
    }

    fn replacement_order_key(
        &self,
        entry: TranspositionEntry,
        old_hash: u16,
        new_hash: u16,
    ) -> impl Ord {
        // Effective age is actually a multi-purpose filter:
        // - Age is initialized to MAX_AGE / 2, which makes 0 age for unoccpied entries
        //   a pretty high age at the start.
        // - Invalid entries quickly get aged out.
        // If we include is_none before this, we run into a problem! We store every hash 4 times...!
        // Effectively, this means we discard ~3/4 puts!

        // h000 0000 0000 0000
        // 0aaa aaa0 0000 0000
        // 0000 000d dddd ddd0
        // 0000 0000 0000 000t
        // ------------------- |
        // haaa aaad dddd dddt

        #[allow(clippy::identity_op)]
        {
            (((old_hash != new_hash) as u16) << 15) // 1 bit
                | (((MAX_AGE - self.effective_age(entry)) as u16) << 9) // 6 bits
                | ((entry.depth as u16) << 1) // 8 bits
                | (((entry.value_type() == TranspositionEntryType::Exact) as u16) << 0) // 1 bit
                | 0
            // TOTAL: 16 bits.
        }
    }

    pub fn put(&self, hash: ZobristHash, value: TranspositionEntry) -> PutResult {
        self.prefetch_write(hash);

        let bucket_p = self.bucket(hash).get();
        let mut bucket = unsafe { bucket_p.read() };

        let new_hash = hash.upper_16();
        let kv_pairs = bucket.0.to_array().into_iter().zip(bucket.1.iter());

        let idx = kv_pairs
            .enumerate()
            .min_by_key(|(_, (old_hash, entry))| {
                self.replacement_order_key(**entry, *old_hash, new_hash)
            })
            .map(|x| x.0);

        debug_assert!(idx.is_some());

        // SAFETY: We're iterating over a set-length slice, so there will always be _some_ entry.
        // See also the above debug.
        let idx = unsafe { idx.unwrap_unchecked() };

        // What we're plugging in would be actively worse.
        if new_hash == bucket.0[idx] && value.depth < bucket.1[idx].depth {
            return PutResult::Noop;
        }

        let replaced_age = bucket.1[idx].age();
        bucket.0[idx] = new_hash;
        bucket.1[idx] = value;

        unsafe { bucket_p.write(bucket) }

        if replaced_age == value.age() {
            PutResult::ValueReplaced
        } else {
            PutResult::ValueAdded
        }
    }

    pub fn print_cache_stats(&self) {
        let mut empty = 0;
        let mut lower = 0;
        let mut upper = 0;
        let mut exact = 0;
        let mut slots = [0_i64; ITEMS_PER_BUCKET];
        let mut ages = [0_i64; MAX_AGE as usize];
        let mut depths = std::collections::BTreeMap::new();
        for bucket in self.table.as_ref() {
            let bucket = unsafe { bucket.get().read() };
            for (i, slot) in bucket.1.iter().enumerate() {
                if !slot.is_some() {
                    empty += 1;
                } else {
                    use TranspositionEntryType::*;
                    *depths.entry(slot.depth).or_insert(0) += 1;
                    slots[i] += 1;
                    ages[self.effective_age(*slot) as usize] += 1;

                    match slot.value_type() {
                        Exact => exact += 1,
                        LowerBound => lower += 1,
                        UpperBound => upper += 1,
                    }
                }
            }
        }
        println!("Entry types:");
        println!("Exact: {exact}");
        println!("Upper: {upper}");
        println!("Lower: {lower}");
        println!("Empty: {empty}");
        println!();
        println!("Slots: {slots:?}");
        println!("Ages:  {ages:?}");
        println!("Depth: {depths:?}");
    }

    fn effective_age(&self, entry: TranspositionEntry) -> u8 {
        let global_age = self.age();

        global_age.wrapping_sub(entry.age()) & MAX_AGE
    }
}

// TODO: move to other file
pub fn pv_string(game: &Game, pv: &[Ply]) -> String {
    use crate::basic_enums::Color::*;

    let mut res = String::new();
    let mut is_first = true;
    let mut game = game.clone();

    for ply in pv {
        if is_first || game.to_move() == White {
            res.push_str(&game.full_move().to_string());
            res.push_str(". ");
        }

        if is_first && game.to_move() == Black {
            res.push_str("... ");
        }

        res.push_str(&game.ply_name(*ply));
        res.push(' ');

        game.apply_ply(*ply);
        is_first = false;
    }

    res
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::game::Game;

    #[test]
    fn read_write() {
        let tt = TranspositionTable::new(1024 * 1024 * 16);
        let game = Game::new();
        let entry = TranspositionEntry::new(
            123,
            None,
            Millipawns(456),
            TranspositionEntryType::Exact,
            tt.age(),
        );

        tt.put(game.hash(), entry);
        let entry2 = tt.get(game.hash());

        assert_eq!(Some(entry), entry2);
    }
}
