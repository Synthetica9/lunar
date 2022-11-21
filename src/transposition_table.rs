use std::cell::UnsafeCell;

use crate::game::Game;
use crate::millipawns::Millipawns;
use crate::ply::Ply;
use crate::zobrist_hash::ZobristHash;

pub struct TranspositionTable {
    table: Vec<UnsafeCell<TranspositionLine>>,
    occupancy: usize,
}

unsafe impl Sync for TranspositionTable {}

// TODO: depth + always replace strategy.
struct TranspositionLine {
    key: u64,
    value: TranspositionEntry,
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
    pub best_move: Option<Ply>,
    pub value: Millipawns,
    pub value_type: TranspositionEntryType,
}

impl TranspositionEntry {
    fn checksum(&self) -> u64 {
        (self.depth as u64)
            ^ ((self.value_type as u64) << 8)
            ^ (self.best_move.map_or(0, |x| x.as_u16() as u64) << 16)
            ^ ((self.value.0 as u64) << 32)
    }
}

const ZERO_ENTRY: TranspositionEntry = TranspositionEntry {
    depth: 0,
    value: Millipawns(0),
    best_move: None,
    value_type: TranspositionEntryType::Exact,
};

const ENTRY_SIZE: usize = std::mem::size_of::<TranspositionLine>();

impl TranspositionTable {
    pub fn new(bytes: usize) -> TranspositionTable {
        let needed_entries = TranspositionTable::needed_entries(bytes);
        let mut table = Vec::new();
        table.reserve(needed_entries);
        while table.len() < needed_entries {
            table.push(
                TranspositionLine {
                    key: 0,
                    value: ZERO_ENTRY,
                }
                .into(),
            );
        }

        TranspositionTable {
            table,
            occupancy: 0,
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

        let checksum = content.value.checksum();
        let computed_key = checksum ^ hash.as_u64();

        if computed_key != content.key {
            None
        } else {
            Some(content.value)
        }
    }

    pub fn put(&self, hash: ZobristHash, value: TranspositionEntry) {
        let curr_occupant = self.get(hash);

        let should_replace = curr_occupant.map_or(true, |old| self.should_replace(&value, &old));

        if should_replace {
            // TODO: occupancy

            let key = value.checksum() ^ hash.as_u64();
            let content = TranspositionLine { key, value };

            let slot = self.slot(hash);
            let ptr: *mut TranspositionLine = self.table[slot].get();
            unsafe {
                // Is this needed?
                ptr.write_volatile(content);
            }
        }
    }

    pub fn principle_variation(&self, game: &Game) -> Vec<Ply> {
        let mut game = *game;
        let mut hashes_seen = Vec::new();
        let mut res = Vec::new();
        loop {
            let hash = game.hash();
            if let Some(tte) = &self.get(hash) {
                if let Some(ply) = tte.best_move {
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
                Some(tte) => tte.best_move.filter(|&ply| ply == *entry),
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

    fn should_replace(&self, cand: &TranspositionEntry, old: &TranspositionEntry) -> bool {
        // if old.hash != self.hash {
        //     return true;
        // }

        if old.depth < cand.depth {
            return true;
        }

        // TODO: replace upper/lower bounds with exact values.

        false
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
            best_move: None,
            value_type: TranspositionEntryType::Exact,
        };

        tt.put(game.hash(), entry);
        let entry2 = tt.get(game.hash());

        assert_eq!(Some(entry), entry2);
    }
}
