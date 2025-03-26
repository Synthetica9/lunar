use std::io::Write;

// Ported from http://www.talkchess.com/forum3/viewtopic.php?topic_view=threads&p=175834&t=19699

type Bitboard = u64;
type Square = isize;
type Direction = (isize, isize);

fn pop_bit(board: &mut Bitboard) -> Bitboard {
    let res = 1 << board.trailing_zeros();
    assert!(res & *board != 0);
    *board &= !res;
    res
}

fn index_to_bitboard(index: usize, mask: Bitboard) -> Bitboard {
    let mut bitboard = 0;
    let mut mask = mask;

    for i in 0..mask.count_ones() {
        let m = pop_bit(&mut mask);
        if index & (1 << i) != 0 {
            bitboard |= m;
        }
    }
    bitboard
}

fn from_sq(sq: Square) -> (isize, isize) {
    (sq % 8, sq / 8)
}

fn to_sq(x: isize, y: isize) -> Square {
    x + y * 8
}

fn create_mask(
    src: Square,
    directions: &[Direction],
    include_edges: bool,
    blocker_mask: Bitboard,
) -> Bitboard {
    let (min, max) = if include_edges { (0, 7) } else { (1, 6) };

    let mut mask = 0;

    for &dir in directions {
        let (dx, dy) = dir;
        let (mut x, mut y) = from_sq(src);

        let file_bounds = dx != 0;
        let rank_bounds = dy != 0;
        assert!(file_bounds || rank_bounds);

        loop {
            x += dx;
            y += dy;

            if ((x < min || x > max) && file_bounds) || ((y < min || y > max) && rank_bounds) {
                break;
            }
            let sq_bit = 1 << to_sq(x, y) as Bitboard;
            mask |= sq_bit;
            if blocker_mask & sq_bit != 0 {
                break;
            }
        }
    }
    mask
}

const ROOK_DIRECTIONS: [Direction; 4] = [(1, 0), (-1, 0), (0, 1), (0, -1)];
const BISHOP_DIRECTIONS: [Direction; 4] = [(1, 1), (-1, 1), (1, -1), (-1, -1)];

pub fn rook_premask(square: Square) -> Bitboard {
    create_mask(square, &ROOK_DIRECTIONS, false, 0)
}

pub fn bishop_premask(square: Square) -> Bitboard {
    create_mask(square, &BISHOP_DIRECTIONS, false, 0)
}

pub fn rook_postmask(square: Square) -> Bitboard {
    create_mask(square, &ROOK_DIRECTIONS, true, 0)
}

pub fn bishop_postmask(square: Square) -> Bitboard {
    create_mask(square, &BISHOP_DIRECTIONS, true, 0)
}

pub fn rook_attacks(square: Square, blockers: Bitboard) -> Bitboard {
    create_mask(square, &ROOK_DIRECTIONS, true, blockers)
}

pub fn bishop_attacks(square: Square, blockers: Bitboard) -> Bitboard {
    create_mask(square, &BISHOP_DIRECTIONS, true, blockers)
}

pub fn transform(blockers: Bitboard, magic: u64, bits: isize) -> usize {
    // Regular * causes overflow
    ((blockers.wrapping_mul(magic)) >> (64 - bits)) as usize
}

pub fn random_u64_fewbits(rng: &mut rand::rngs::SmallRng) -> u64 {
    use rand::Rng;
    let mut res = !0;
    for _ in 0..3 {
        res &= rng.gen::<u64>();
    }
    res
}

pub fn find_magic(square: Square, bits: isize, is_bishop: bool) -> (u64, Vec<Bitboard>) {
    use rand::SeedableRng;

    let mask = if is_bishop {
        bishop_premask(square)
    } else {
        rook_premask(square)
    };
    let n = mask.count_ones();

    let blockers_attacks = {
        let mut ba = Vec::new();
        for i in 0..(1 << n) {
            let b = index_to_bitboard(i, mask);
            let a = if is_bishop {
                bishop_attacks(square, b)
            } else {
                rook_attacks(square, b)
            };
            ba.push((b, a));
        }
        ba
    };

    let mut rng = rand::rngs::SmallRng::seed_from_u64(1);
    'outer: for _ in 0..100000000 {
        let magic = random_u64_fewbits(&mut rng);

        if (transform(mask, magic, 8)).count_ones() < 6 {
            // Not sure why this is, probably to discard surely bad magics.
            continue;
        }

        // used: (bits used, result)
        let mut used = vec![0; 1 << bits];
        for &(blockers, attacks) in &blockers_attacks {
            let j = transform(blockers, magic, bits);
            let existing = used[j];
            if existing != 0 && existing != attacks {
                continue 'outer;
            }
            used[j] = attacks;
        }
        return (magic, used);
    }
    panic!(
        "Failed to find {} magic for square {}",
        if is_bishop { "bishop" } else { "rook" },
        square
    );
}

// Empty comments to keep rustfmt from messing up the formatting.

#[allow(clippy::zero_prefixed_literal)]
const ROOK_BITS: [isize; 64] = [
    12, 11, 11, 11, 11, 11, 11, 12, //
    11, 10, 10, 10, 10, 10, 10, 11, //
    11, 10, 10, 10, 10, 10, 10, 11, //
    11, 10, 10, 10, 10, 10, 10, 11, //
    11, 10, 10, 10, 10, 10, 10, 11, //
    11, 10, 10, 10, 10, 10, 10, 11, //
    11, 10, 10, 10, 10, 10, 10, 11, //
    12, 11, 11, 11, 11, 11, 11, 12, //
];

// Original:
#[allow(clippy::zero_prefixed_literal)]
const ROOK_SHARING: [usize; 64] = [
    00, 01, 02, 03, 04, 05, 06, 07, //
    01, 00, 03, 02, 05, 04, 07, 06, //
    08, 09, 10, 11, 12, 13, 14, 15, //
    09, 08, 11, 10, 13, 12, 15, 14, //
    16, 17, 18, 19, 20, 21, 22, 23, //
    17, 16, 19, 18, 21, 20, 23, 22, //
    24, 25, 26, 27, 28, 29, 30, 31, //
    25, 24, 27, 26, 29, 28, 31, 30, //
];

// Mine:
// const ROOK_SHARING: [usize; 64] = [
//     00, 01, 02, 03, 04, 05, 06, 07, //
//     01, 02, 03, 04, 05, 06, 08, 14, //
//     09, 10, 11, 12, 13, 08, 14, 15, //
//     10, 09, 12, 11, 32, 13, 15, 16, //
//     20, 21, 22, 23, 24, 25, 16, 17, //
//     21, 20, 23, 22, 25, 24, 17, 18, //
//     26, 31, 27, 28, 29, 30, 18, 19, //
//     07, 26, 28, 27, 30, 29, 19, 00, //
// ];

#[allow(clippy::zero_prefixed_literal)]
const BISHOP_BITS: [isize; 64] = [
    6, 5, 5, 5, 5, 5, 5, 6, //
    5, 5, 5, 5, 5, 5, 5, 5, //
    5, 5, 7, 7, 7, 7, 5, 5, //
    5, 5, 7, 9, 9, 7, 5, 5, //
    5, 5, 7, 9, 9, 7, 5, 5, //
    5, 5, 7, 7, 7, 7, 5, 5, //
    5, 5, 5, 5, 5, 5, 5, 5, //
    6, 5, 5, 5, 5, 5, 5, 6, //
];

#[allow(clippy::zero_prefixed_literal)]
const BISHOP_SHARING: [usize; 64] = [
    00, 02, 04, 04, 04, 04, 12, 14, //
    00, 02, 05, 05, 05, 05, 12, 14, //
    00, 02, 06, 06, 06, 06, 12, 14, //
    00, 02, 07, 07, 07, 07, 12, 14, //
    01, 03, 08, 08, 08, 08, 13, 15, //
    01, 03, 09, 09, 09, 09, 13, 15, //
    01, 03, 10, 10, 10, 10, 13, 15, //
    01, 03, 11, 11, 11, 11, 13, 15, //
];

pub fn gen_magics_file(file: &mut std::fs::File) -> std::io::Result<()> {
    println!("cargo:warning=Generating magics.rs");

    for is_bishop in &[false, true] {
        let bits = if *is_bishop { BISHOP_BITS } else { ROOK_BITS };
        let name = if *is_bishop { "BISHOP" } else { "ROOK" };
        let sharing_idx = if *is_bishop {
            BISHOP_SHARING
        } else {
            ROOK_SHARING
        };

        // TODO: explicit struct
        writeln!(file, "// Magic, premask, postmask, LUT for {name}")?;
        writeln!(
            file,
            "pub const {name}_MAGICS: [(u64, u64, u64, &[u64]); 64] = [",
        )?;

        // init sharing db
        let mut sharing_db = vec![vec![]; sharing_idx.iter().max().unwrap() + 1];
        for square in 0..64i64 {
            let target = 1 << bits[square as usize];
            let group = sharing_idx[square as usize];
            let db = &mut sharing_db[group];
            println!("{square}. Current length: {}", db.len());
            while db.len() < target {
                db.push(0);
            }
        }

        for square in 0..64 {
            let sharing = sharing_idx[square as usize];
            let db = &mut sharing_db[sharing];
            let (magic, used) = find_magic(
                square,
                (db.len().trailing_zeros()).try_into().unwrap(),
                *is_bishop,
            );

            // merge into sharing db
            for (i, &u) in used.iter().enumerate() {
                db[i] |= u;
            }

            let (premask, postmask) = if *is_bishop {
                (bishop_premask(square), bishop_postmask(square))
            } else {
                (rook_premask(square), rook_postmask(square))
            };

            writeln!(file, "    (")?;
            writeln!(file, "        0x{magic:016x},")?;
            writeln!(file, "        0x{premask:016x},")?;
            writeln!(file, "        0x{postmask:016x},")?;
            writeln!(file, "        SHARED_{name}_MAGIC_{sharing}")?;
            writeln!(file, "    ),")?;
        }
        writeln!(file, "];\n")?;

        // write sharing db
        for (i, db) in sharing_db.iter().enumerate() {
            writeln!(file, "const SHARED_{name}_MAGIC_{i}: &[u64] = &[")?;
            for &entry in db.iter() {
                writeln!(file, "    0x{entry:016x},")?;
            }
            writeln!(file, "];")?;
        }
    }
    Ok(())
}
