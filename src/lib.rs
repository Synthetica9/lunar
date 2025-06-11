#![cfg_attr(feature = "simd", feature(portable_simd))]
#![cfg_attr(unix, feature(allocator_api, slice_ptr_get))]
#![cfg_attr(feature = "intrinsics", feature(core_intrinsics))]
#![feature(generic_const_exprs)]
#![feature(new_zeroed_alloc)]
#![feature(thread_local)]
#![feature(const_precise_live_drops)]

pub mod basic_enums;
pub mod bitboard;
pub mod bitboard_map;
pub mod board;
pub mod castlerights;
pub mod direction;
pub mod eval;
pub mod game;
pub mod history;

#[cfg(unix)]
pub mod hugepages_mmap_alloc;

pub mod legality;
pub mod millipawns;
pub mod piece;
pub mod ply;
pub mod plyset;
pub mod polyglot;
pub mod search;
pub mod small_finite_enum;
pub mod square;
pub mod transposition_table;
pub mod uci;
pub mod zero_init;
pub mod zobrist_hash;

// pub mod build;

mod generated;
