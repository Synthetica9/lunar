use crate::ply::Ply;
use crate::game::Game;

pub const HASH_TABLE_SIZE: usize = 1 << 14;

#[derive(Clone, Debug)]
pub struct History {
    first: Game,
    states: Vec<(Game, Ply)>,
    // TODO: stack or heap allocation?
    hash_table: Box<[u8; HASH_TABLE_SIZE]>,
}

impl History {
    pub fn new(first: Game) -> Self {
        let mut res = Self {
            first,
            states: Vec::new(),
            hash_table: Box::new([0; 1 << 14]),
        };
        res.hash_table[first.hash().hash as usize % HASH_TABLE_SIZE] = 1;
        res
    }

    pub fn last<'a>(&'a self) -> &'a Game {
        self.states.last().map(|x| &x.0).unwrap_or(&self.first)
    }

    pub fn push(&mut self, ply: &Ply) {
        let mut game = self.last().clone();
        debug_assert!(game.is_pseudo_legal(ply));
        game.apply_ply(ply);
        self.states.push((game, *ply));
        self.hash_table[game.hash().hash as usize % HASH_TABLE_SIZE] += 1;
    }

    pub fn pop(&mut self) {
        let res = self.states.pop().unwrap();
        let game = res.0;
        self.hash_table[game.hash().hash as usize % HASH_TABLE_SIZE] -= 1;
    }

    pub fn repetition_count_at_least_3(&self) -> bool {
        let last = self.last();
        let hash_count = self.hash_table[last.hash().hash as usize % HASH_TABLE_SIZE];
        debug_assert!(hash_count != 0);
        if hash_count < 3 {
            // No false positives possible.
            false
        } else {
            let mut count = 0;
            for (game, _) in self.states.iter().rev() {
                // TODO: Avoid hash collisions? (unlikely)
                count += (game.hash() == last.hash()) as u8;
                // if game.hash() != last.hash() {
                //     continue;
                // }
                // count += (game == last) as u8;
                if count >= 3 {
                    break;
                }
            }
            count >= 3
        }
    }
}
