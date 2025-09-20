use std::cell::UnsafeCell;
use std::mem::MaybeUninit;
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
    table: Box<NonEmptySlice<TranspositionLine>>,

    #[cfg(unix)]
    table: Box<NonEmptySlice<TranspositionLine>, HugePagesAlloc>,

    occupancy: AtomicIsize,
    age: AtomicU8,
}

unsafe impl Sync for TranspositionTable {}

const CACHE_LINE_SIZE: usize = 64;
const ENTRY_SIZE: usize = std::mem::size_of::<TranspositionPair>();
const ITEMS_PER_BUCKET: usize = CACHE_LINE_SIZE / ENTRY_SIZE;

assert_eq_size!(TranspositionEntry, u64);
const_assert!(std::mem::size_of::<TranspositionLine>() <= CACHE_LINE_SIZE);

// TODO: depth + always replace strategy.

#[repr(align(64))]
struct TranspositionLine([UnsafeCell<TranspositionPair>; ITEMS_PER_BUCKET]);

#[derive(Copy, Clone)]
struct TranspositionPair {
    key: u64,
    value: u64,
}

impl TranspositionPair {
    fn hash(&self) -> ZobristHash {
        ZobristHash(self.key ^ self.value)
    }

    pub fn new(hash: ZobristHash, value: TranspositionEntry) -> Self {
        Self {
            key: hash.as_u64() ^ value.as_u64(),
            value: value.as_u64(),
        }
    }

    pub fn value(self) -> TranspositionEntry {
        TranspositionEntry::from_u64(self.value)
    }

    pub fn none() -> Self {
        Self { key: 0, value: 0 }
    }
}

// TODO: rename to ValueType?
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TranspositionEntryType {
    Exact,
    LowerBound,
    UpperBound,
}

const AGE_BITS: usize = 5;
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
        self.age_value_type >> (8 - AGE_BITS)
    }

    pub fn ttpv(self) -> bool {
        (self.age_value_type >> 2) & 1 != 0
    }

    pub fn new(
        depth: u8,
        best_move: Option<Ply>,
        value: Millipawns,
        value_type: TranspositionEntryType,
        ttpv: bool,
        age: u8,
    ) -> TranspositionEntry {
        debug_assert!(age <= MAX_AGE);
        TranspositionEntry {
            depth,
            value,
            best_move: Ply::unwrap_null(best_move),
            age_value_type: age << (8 - AGE_BITS) | (ttpv as u8) << 2 | value_type as u8,
        }
    }
}

impl TranspositionTable {
    pub fn new(bytes: usize) -> TranspositionTable {
        assert!(bytes > 0, "empty tt not supported");

        let needed_entries = TranspositionTable::needed_entries(bytes);
        let needed_lines = needed_entries / ITEMS_PER_BUCKET;

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
        #[allow(clippy::missing_transmute_annotations)]
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
            for val in item.0.iter() {
                unsafe { val.get().write(TranspositionPair::none()) };
            }
        }
    }

    pub const fn needed_entries(bytes: usize) -> usize {
        bytes / ENTRY_SIZE
    }

    pub fn num_buckets(&self) -> usize {
        self.table.len().into()
    }

    pub fn num_entries(&self) -> usize {
        self.num_buckets() * ITEMS_PER_BUCKET
    }

    pub fn num_bytes(&self) -> usize {
        self.num_entries() * std::mem::size_of::<TranspositionPair>()
    }

    fn bucket(&self, hash: ZobristHash) -> &TranspositionLine {
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
        self.bucket(hash)
            .0
            .iter()
            .map(|p| unsafe { p.get().read() })
            .find(|x| x.hash() == hash)
            .map(TranspositionPair::value)
    }

    #[allow(unreachable_code, clippy::needless_return)]
    pub fn prefetch_read(&self, hash: ZobristHash) {
        let ptr = self.bucket(hash).0[0].get();

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
        let ptr = self.bucket(hash).0[0].get();

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

    fn replacement_order_key(&self, entry: &TranspositionPair, new_hash: ZobristHash) -> impl Ord {
        let value = entry.value();
        let hash = entry.hash();

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
            (((hash != new_hash) as u16) << 15) // 1 bit
                | (((MAX_AGE - self.effective_age(value)) as u16) << 9) // 6 bits
                | ((value.depth as u16) << 1) // 8 bits
                | (((value.value_type() == TranspositionEntryType::Exact) as u16) << 0) // 1 bit
                | 0
            // TOTAL: 16 bits.
        }
    }

    pub fn put(&self, hash: ZobristHash, value: TranspositionEntry) -> PutResult {
        self.prefetch_write(hash);

        let bucket = self.bucket(hash);

        let to_replace = bucket
            .0
            .iter()
            .min_by_key(|entry| self.replacement_order_key(&unsafe { entry.get().read() }, hash));

        debug_assert!(to_replace.is_some());

        // SAFETY: We're iterating over a set-length slice, so there will always be _some_ entry.
        // See also the above debug.
        let to_replace = unsafe { to_replace.unwrap_unchecked() };
        let replaced = unsafe { to_replace.get().read() };

        let entry = TranspositionPair::new(hash, value);

        // What we're plugging in would be actively worse.
        if hash == replaced.hash() && value.depth < replaced.value().depth {
            return PutResult::Noop;
        }

        unsafe {
            to_replace.get().write(entry);
        }

        if replaced.value().age() == value.age() {
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
            for (i, slot) in bucket.0.iter().enumerate() {
                let slot = unsafe { slot.get().read() };
                if !slot.value().is_some() {
                    empty += 1;
                } else {
                    use TranspositionEntryType::*;
                    let value = slot.value();
                    *depths.entry(value.depth).or_insert(0) += 1;
                    slots[i] += 1;
                    ages[self.effective_age(value) as usize] += 1;

                    match value.value_type() {
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
            true,
            12,
        );

        tt.put(game.hash(), entry);
        let entry2 = tt.get(game.hash());

        debug_assert_eq!(entry.age(), 12);
        debug_assert!(entry.ttpv());
        debug_assert_eq!(entry.value_type(), TranspositionEntryType::Exact);

        assert_eq!(Some(entry), entry2);
    }
}
