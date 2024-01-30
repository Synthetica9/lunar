#![cfg_attr(feature = "simd", feature(portable_simd))]
#![cfg_attr(feature = "hugepages", feature(allocator_api, slice_ptr_get))]
#![cfg_attr(feature = "intrinsics", feature(core_intrinsics))]

pub mod basic_enums;
pub mod bitboard;
pub mod bitboard_map;
pub mod board;
pub mod byteboard;
pub mod castlerights;
pub mod direction;
pub mod eval;
pub mod game;
pub mod history;
pub mod legality;
pub mod millipawns;
pub mod piece;
pub mod ply;
pub mod plyset;
pub mod poison_drop;
pub mod search;
pub mod square;
pub mod transposition_table;
pub mod uci;
pub mod values;
pub mod zobrist_hash;

#[cfg(feature = "hugepages")]
pub mod hugepages_mmap_alloc;

// pub mod build;

mod generated;
