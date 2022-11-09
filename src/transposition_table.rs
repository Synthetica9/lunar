use crate::game::Game;
use crate::millipawns::Millipawns;
use crate::ply::Ply;
use crate::zobrist_hash::ZobristHash;

pub struct TranspositionTable {
    table: Vec<Option<TranspositionEntry>>,
    occupancy: usize,
}

// TODO: drop the option, use zero hash if not present.
#[derive(Copy, Clone, Debug)]
pub struct TranspositionEntry {
    pub hash: ZobristHash,
    pub depth: u8,
    pub alpha: Millipawns,
    pub beta: Millipawns,
    pub best_move: Option<Ply>,
}

const ENTRY_SIZE: usize = std::mem::size_of::<Option<TranspositionEntry>>();

impl TranspositionTable {
    pub fn new(bytes: usize) -> TranspositionTable {
        let num_entries = TranspositionTable::num_entries(bytes);
        TranspositionTable {
            table: vec![None; num_entries],
            occupancy: 0,
        }
    }

    pub const fn num_entries(bytes: usize) -> usize {
        bytes / ENTRY_SIZE
    }

    pub fn get_num_entries(&self) -> usize {
        self.table.len()
    }

    pub fn slot(&self, hash: ZobristHash) -> usize {
        hash.as_u64() as usize % self.get_num_entries()
    }

    pub fn get(&self, hash: ZobristHash) -> Option<TranspositionEntry> {
        let slot = self.slot(hash);
        if let Some(entry) = &self.table[slot] {
            if entry.hash == hash {
                return Some(*entry);
            }
        }
        None
    }

    pub fn put(&mut self, entry: TranspositionEntry) {
        let hash = entry.hash;
        let slot = self.slot(hash);
        let curr_occupant = self.get(hash);
        let should_replace = match curr_occupant {
            None => true,
            Some(old) => entry.should_replace(&old),
        };

        if should_replace {
            if curr_occupant.is_none() {
                self.occupancy += 1;
            }

            self.table[slot] = Some(entry);
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

    pub fn pv_string(&self, game: &Game) -> String {
        use crate::basic_enums::Color::*;

        let pv = self.principle_variation(game);
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

            res.push_str(&game.ply_name(&ply));
            res.push(' ');

            game.apply_ply(&ply);
            is_first = false;
        }

        res
    }
}

impl TranspositionEntry {
    fn should_replace(&self, old: &TranspositionEntry) -> bool {
        if old.hash != self.hash {
            return true;
        }

        if old.depth < self.depth {
            return true;
        }

        // Check if the window is narrower
        if (self.alpha - self.beta) <= (old.alpha - old.beta)
        // And if it overlaps with the old window...
            && (old.alpha <= self.alpha || old.beta >= self.beta)
        {
            return true;
        }

        return false;
    }
}
