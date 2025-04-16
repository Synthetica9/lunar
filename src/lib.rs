#![cfg_attr(feature = "simd", feature(portable_simd))]
#![cfg_attr(feature = "hugepages", feature(allocator_api, slice_ptr_get))]
#![cfg_attr(feature = "intrinsics", feature(core_intrinsics))]
#![feature(generic_const_exprs)]
#![feature(new_uninit)]
#![feature(thread_local)]

pub mod basic_enums;
pub mod bitboard;
pub mod bitboard_map;
pub mod board;
pub mod castlerights;
pub mod direction;
pub mod eval;
pub mod game;
pub mod history;

#[cfg(feature = "hugepages")]
pub mod hugepages_mmap_alloc;

pub mod legality;
pub mod millipawns;
pub mod piece;
pub mod ply;
pub mod plyset;
pub mod search;
pub mod square;
pub mod transposition_table;
pub mod uci;
pub mod values;
pub mod zobrist_hash;

// pub mod build;

mod generated;
