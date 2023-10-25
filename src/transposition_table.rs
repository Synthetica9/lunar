use std::cell::UnsafeCell;

use no_panic::no_panic;
use static_assertions::*;

use crate::game::Game;
use crate::millipawns::Millipawns;
use crate::ply::Ply;
use crate::zobrist_hash::ZobristHash;

pub struct TranspositionTable {
    table: Vec<UnsafeCell<TranspositionLine>>,
    occupancy: usize,
}

unsafe impl Sync for TranspositionTable {}

const CACHE_LINE_SIZE: usize = 64;
const ENTRY_SIZE: usize = std::mem::size_of::<TranspositionPair>();
const ITEMS_PER_BUCKET: usize = CACHE_LINE_SIZE / ENTRY_SIZE;
const N_MERIT_ENTRIES: usize = ITEMS_PER_BUCKET / 2;
const N_FIFO_ENTRIES: usize = ITEMS_PER_BUCKET - N_MERIT_ENTRIES;

assert_eq_size!(TranspositionEntry, u64);
const_assert!(std::mem::size_of::<TranspositionLine>() <= CACHE_LINE_SIZE);

// TODO: depth + always replace strategy.

#[repr(align(64))]
struct TranspositionLine([TranspositionPair; ITEMS_PER_BUCKET]);

impl TranspositionLine {
    // These two are no-panic because the unrwap _could_ produce panicking code,
    // but statically it should be verified that the size is right.
    #[no_panic]
    pub fn merit_entries(&mut self) -> &mut [TranspositionPair; N_MERIT_ENTRIES] {
        (&mut self.0[..N_MERIT_ENTRIES]).try_into().unwrap()
    }

    #[no_panic]
    pub fn fifo_entries(&mut self) -> &mut [TranspositionPair; N_FIFO_ENTRIES] {
        (&mut self.0[N_MERIT_ENTRIES..]).try_into().unwrap()
    }
}

#[derive(Copy, Clone)]
struct TranspositionPair {
    key: u64,
    value: u64,
}

impl TranspositionPair {
    fn hash(&self) -> ZobristHash {
        ZobristHash {
            hash: self.key ^ self.value,
        }
    }
}

// TODO: rename to ValueType?
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum TranspositionEntryType {
    Exact,
    LowerBound,
    UpperBound,
}

// TODO: aging?
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct TranspositionEntry {
    pub depth: u8,
    pub best_move: Ply,
    pub value: Millipawns,
    pub value_type: TranspositionEntryType,
}

impl TranspositionEntry {
    fn as_u64(self) -> u64 {
        unsafe { std::mem::transmute(self) }
    }

    fn from_u64(val: u64) -> TranspositionEntry {
        unsafe { std::mem::transmute(val) }
    }
}

impl TranspositionTable {
    pub fn new(bytes: usize) -> TranspositionTable {
        let needed_entries = TranspositionTable::needed_entries(bytes);
        let mut table = Vec::with_capacity(needed_entries);
        while table.len() < needed_entries {
            table.push(
                TranspositionLine([TranspositionPair { key: 0, value: 0 }; ITEMS_PER_BUCKET])
                    .into(),
            );
        }

        TranspositionTable {
            table,
            occupancy: 0,
        }
    }

    pub fn clear(&self) {
        for item in self.table.iter() {
            unsafe { item.get().write(std::mem::zeroed()) };
        }
    }

    pub const fn needed_entries(bytes: usize) -> usize {
        bytes / ENTRY_SIZE
    }

    pub fn num_entries(&self) -> usize {
        self.table.len()
    }

    pub fn slot(&self, hash: ZobristHash) -> usize {
        hash.as_u64() as usize % self.num_entries()
    }

    pub fn get(&self, hash: ZobristHash) -> Option<TranspositionEntry> {
        // TODO: check if hash matches current value. If not, replace with zeros.
        let slot = self.slot(hash);
        let ptr: *mut TranspositionLine = self.table[slot].get();
        let content: TranspositionLine = unsafe {
            // TODO: does this need read_volatile?
            ptr.read_volatile()
        };

        for entry in content.0 {
            let checksum = entry.value;
            let computed_key = checksum ^ hash.as_u64();

            if computed_key == entry.key {
                return Some(TranspositionEntry::from_u64(entry.value));
            }
        }

        None
    }

    pub fn put(&self, hash: ZobristHash, value: TranspositionEntry) {
        let entry = TranspositionPair {
            key: value.as_u64() ^ hash.as_u64(),
            value: value.as_u64(),
        };

        let slot = self.slot(hash);
        let ptr = self.table[slot].get();
        let mut bucket = unsafe { ptr.read_volatile() };

        for other in bucket.0.iter_mut() {
            // Evict anything that seems off...
            if self.slot(other.hash()) != slot {
                *other = TranspositionPair { key: 0, value: 0 }
            }
        }

        // Merit-based entries:
        let mut have_replaced = false;
        for other in bucket.merit_entries().iter_mut() {
            let other_entry = TranspositionEntry::from_u64(other.value);
            if self.should_replace(&value, &other_entry) || other.value == 0 {
                have_replaced = true;
                *other = entry;
                break;
            }
        }

        // Otherwise, do one of the FIFO entries
        if !have_replaced {
            let fifo = bucket.fifo_entries();
            fifo.rotate_right(1);
            fifo[0] = entry;
        }

        // Lastly, make sure we have only one value for our current hash.
        let mut found = false;
        for other in bucket.0.iter_mut() {
            if other.hash() == hash {
                if !found {
                    found = true;
                } else {
                    *other = TranspositionPair { key: 0, value: 0 }
                }
            }
        }

        unsafe {
            ptr.write_volatile(bucket);
        }
    }

    pub fn principle_variation(&self, game: &Game) -> Vec<Ply> {
        let mut game = *game;
        let mut hashes_seen = Vec::new();
        let mut res = Vec::new();
        loop {
            let hash = game.hash();
            if let Some(tte) = &self.get(hash) {
                if let Some(ply) = tte.best_move.wrap_null() {
                    res.push(ply);
                    let is_repetition = hashes_seen.contains(&hash);
                    if !is_repetition {
                        game.apply_ply(&ply);
                        hashes_seen.push(hash);
                        continue;
                    }
                }
            }
            return res;
        }
    }

    pub fn update_pv(&self, game: &Game, old_pv: &[Ply]) -> Vec<Ply> {
        let mut game = *game;
        let mut res = Vec::new();
        let _old_pv_relevant = true;

        for entry in old_pv.iter() {
            let from_tt = self.get(game.hash());

            let next = match from_tt {
                Some(tte) => tte.best_move.wrap_null().filter(|&ply| ply == *entry),
                None => Some(*entry),
            };

            match next {
                Some(ply) => {
                    res.push(ply);
                    game.apply_ply(&ply);
                }
                None => {
                    break;
                }
            }
        }
        res
    }

    pub fn print_cache_stats(&self) {
        let mut empty = 0;
        let mut lower = 0;
        let mut upper = 0;
        let mut exact = 0;
        for bucket in &self.table {
            let bucket = unsafe { bucket.get().read() };
            for slot in bucket.0 {
                if slot.value == 0 {
                    empty += 1;
                } else {
                    use TranspositionEntryType::*;
                    let value = TranspositionEntry::from_u64(slot.value);
                    match value.value_type {
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
    }

    fn should_replace(&self, cand: &TranspositionEntry, old: &TranspositionEntry) -> bool {
        // if old.hash != self.hash {
        //     return true;
        // }

        use std::cmp::Ordering::*;
        match Ord::cmp(&cand.depth, &old.depth) {
            Less => false,
            Equal => cand.value_type == TranspositionEntryType::Exact,
            Greater => true,
        }
    }
}

// TODO: move to other file
pub fn pv_string(game: &Game, pv: &[Ply]) -> String {
    use crate::basic_enums::Color::*;

    let mut res = String::new();
    let mut is_first = true;
    let mut game = *game;

    for ply in pv {
        if is_first || game.to_move() == White {
            res.push_str(&game.full_move().to_string());
            res.push_str(". ");
        }

        if is_first && game.to_move() == Black {
            res.push_str("... ");
        }

        res.push_str(&game.ply_name(ply));
        res.push(' ');

        game.apply_ply(ply);
        is_first = false;
    }

    res
}

pub fn pv_uci(pv: &[Ply]) -> String {
    let mut res = String::new();
    let mut is_first = true;
    for ply in pv {
        if !is_first {
            res.push(' ');
        }
        res.push_str(&ply.long_name());
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
        let tt = TranspositionTable::new(512);
        let game = Game::new();
        let entry = TranspositionEntry {
            depth: 123,
            value: Millipawns(456),
            best_move: Ply::null(),
            value_type: TranspositionEntryType::Exact,
        };

        tt.put(game.hash(), entry);
        let entry2 = tt.get(game.hash());

        assert_eq!(Some(entry), entry2);
    }
}
