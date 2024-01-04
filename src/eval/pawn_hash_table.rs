use crate::basic_enums::Color::{self};
use crate::bitboard::Bitboard;
use crate::eval;
use crate::game::Game;
use crate::millipawns::Millipawns;
use crate::piece::Piece::{self};
use crate::square::files;
use crate::zobrist_hash::ZobristHash;

use std::cell::RefCell;

// const PHT_SIZE: usize = 1 << 5;
const PHT_SIZE: usize = 1 << 13;

#[derive(Copy, Clone)]
pub struct SidedPHTEntry {
    // pawns: Bitboard,
    // files: Bitboard,
    isolated: Bitboard,
    // attacks: Bitboard,
    // holes: Bitboard,
    // weak: Bitboard,
    protected: Bitboard,
    // ram: Bitboard,
    doubled: Bitboard,
    passed: Bitboard,
    // outposts: Bitboard,
}

#[derive(Copy, Clone)]
#[repr(align(64))]
pub struct PHTEntry {
    hash: ZobristHash,

    // open_files: Bitboard,
    // half_open_files: Bitboard,
    white: SidedPHTEntry,

    // STORED FROM BLACK's PERSPECTIVE!
    black: SidedPHTEntry,

    mg: Millipawns,
    eg: Millipawns,
}

struct PawnHashTable([PHTEntry; PHT_SIZE]);

impl PHTEntry {
    pub fn new(game: &Game, evaluator: &eval::Evaluator) -> PHTEntry {
        use Color::*;
        use Piece::*;

        let [white_pawns, black_pawns] =
            [White, Black].map(|color| game.board().get(&color, &Pawn));

        let white = SidedPHTEntry::new(white_pawns, black_pawns);
        let black = SidedPHTEntry::new(black_pawns.flip_vertical(), white_pawns.flip_vertical());

        // let open_files = !(white.files | black.files);
        // let half_open_files = white.files | black.files & !open_files;

        let mut res = PHTEntry {
            hash: game.pawn_hash(),

            // open_files,
            // half_open_files,
            white,
            black,

            mg: Millipawns(0),
            eg: Millipawns(0),
        };

        let (mg, eg) = evaluator._evaluate_inline(eval::Evaluator::PAWN_TERMS, &res, game);

        res.mg = mg;
        res.eg = eg;

        res
    }

    pub fn get(&self, color: &Color) -> &SidedPHTEntry {
        match color {
            Color::White => &self.white,
            Color::Black => &self.black,
        }
    }

    pub fn mg(&self) -> Millipawns {
        self.mg
    }

    pub fn eg(&self) -> Millipawns {
        self.eg
    }
}

impl SidedPHTEntry {
    fn new(pawns: Bitboard, enemies: Bitboard) -> SidedPHTEntry {
        // Initialize from white's perspective

        use crate::direction::directions::*;

        let files = pawns.fill_cols();
        let isolated = pawns & !(files.shift(E) | files.shift(W));
        let attacks = pawns.shift(NE) | pawns.shift(NW);
        let protected = pawns & attacks;
        let doubled = files::ALL
            .iter()
            .map(|x| x.as_bitboard())
            .filter(|x| (*x & pawns).popcount() >= 2)
            .fold(Bitboard::new(), Bitboard::or)
            .and(pawns);
        // let ram = pawns & enemies.shift(S);
        // let holes = !attacks.fill(N);
        let enemy_attacks = enemies.shift(SE) | enemies.shift(SW);
        // let enemy_holes = !enemy_attacks.fill(S);
        // let outposts = enemy_holes & attacks;
        let pass_prevention = (enemy_attacks | enemies).fill(S);
        let passed = pawns & !pass_prevention;
        // let weak = pawns & holes;

        SidedPHTEntry {
            // pawns,
            // files,
            isolated,
            // attacks,
            // holes,
            // weak,
            protected,
            // ram,
            passed,
            doubled,
            // outposts,
        }
    }

    pub(crate) fn isolated(&self) -> Bitboard {
        self.isolated
    }

    pub(crate) fn protected(&self) -> Bitboard {
        self.protected
    }

    pub(crate) fn doubled(&self) -> Bitboard {
        self.doubled
    }

    // pub(crate) fn pawns(&self) -> Bitboard {
    //     self.pawns
    // }

    // pub(crate) fn files(&self) -> Bitboard {
    //     self.files
    // }

    // pub(crate) fn attacks(&self) -> Bitboard {
    //     self.attacks
    // }

    // pub(crate) fn holes(&self) -> Bitboard {
    //     self.holes
    // }

    // pub(crate) fn weak(&self) -> Bitboard {
    //     self.weak
    // }

    // pub(crate) fn ram(&self) -> Bitboard {
    //     self.ram
    // }

    pub(crate) fn passed(&self) -> Bitboard {
        self.passed
    }

    // pub fn outposts(&self) -> Bitboard {
    //     self.outposts
    // }
}

impl PawnHashTable {
    pub fn new() -> PawnHashTable {
        unsafe { std::mem::zeroed() }
    }

    pub fn get(&self, game: &Game) -> Option<&PHTEntry> {
        let entry = self.0.get(game.pawn_hash().to_usize() % PHT_SIZE).unwrap();

        if entry.hash != game.pawn_hash()
        // TODO: are both needed?
        // || [entry.white.pawns, entry.black.pawns.flip_vertical()]
        //     != [White, Black].map(|color| game.board().get(&color, &Pawn))
        {
            None
        } else {
            Some(entry)
        }
    }

    pub fn insert(&mut self, game: &Game) -> &PHTEntry {
        let idx = game.pawn_hash().to_usize() % PHT_SIZE;
        self.0[idx] = PHTEntry::new(game, &eval::STATIC_EVALUATOR);
        &self.0[idx]
    }
}

impl Default for PawnHashTable {
    fn default() -> Self {
        PawnHashTable::new()
    }
}

thread_local! {
    static PAWN_HASH_TABLE: Box<RefCell<PawnHashTable>> = Box::default();
}

pub fn get(game: &Game) -> PHTEntry {
    let res = PAWN_HASH_TABLE.with(|x| x.as_ref().borrow().get(game).copied());

    match res {
        None => PAWN_HASH_TABLE.with(|x| *x.as_ref().borrow_mut().insert(game)),
        Some(result) => result,
    }
}
