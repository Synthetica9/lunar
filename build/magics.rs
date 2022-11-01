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
    let min = if include_edges { 0 } else { 1 };
    let max = if include_edges { 7 } else { 6 };

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

        let mut used = vec![None; 1 << bits];
        for &(blockers, attacks) in &blockers_attacks {
            let j = transform(blockers, magic, bits);
            if used[j].is_some() && used[j].unwrap() != attacks {
                // Collision.
                continue 'outer;
            }
            used[j] = Some(attacks);
        }
        return (
            magic,
            used.iter()
                .map(|x| x.unwrap_or(0xDEADBEEFDEADBEEF))
                .collect(),
        );
    }
    panic!(
        "Failed to find {} magic for square {}",
        if is_bishop { "bishop" } else { "rook" },
        square
    );
}

// Empty comments to keep rustfmt from messing up the formatting.

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

pub fn dump_table(
    file: &mut std::fs::File,
    name: &str,
    content: &[u64],
    public: bool,
) -> std::io::Result<()> {
    writeln!(
        file,
        "{}const {}: [u64; {}] = [",
        if public { "pub " } else { "" },
        name,
        content.len()
    )?;
    for i in content {
        writeln!(file, "  0x{:016x},", i)?;
    }
    writeln!(file, "];\n")?;
    Ok(())
}

pub fn gen_magics_file(file: &mut std::fs::File) -> std::io::Result<()> {
    println!("cargo:warning=Generating magics.rs");

    for is_bishop in &[false, true] {
        let bits = if *is_bishop { BISHOP_BITS } else { ROOK_BITS };
        let name = if *is_bishop { "BISHOP" } else { "ROOK" };

        let mut magics = Vec::new();
        let mut masks = Vec::new();

        for square in 0..64 {
            let (magic, used) = find_magic(square, bits[square as usize], *is_bishop);
            magics.push(magic);
            masks.push(if *is_bishop {
                bishop_premask(square)
            } else {
                rook_premask(square)
            });

            dump_table(file, &format!("{}_{}_ATTACKS", name, square), &used, false)?;
        }

        // TODO: Array-of-structs?
        dump_table(file, &format!("{}_MAGIC", name), &magics, true)?;
        dump_table(file, &format!("{}_MASKS", name), &masks, true)?;

        writeln!(file, "pub const {}_ATTACKS: [&[u64]; 64] = [", name)?;
        for square in 0..64 {
            writeln!(file, "  &{},", format!("{}_{}_ATTACKS", name, square))?;
        }
        writeln!(file, "];\n")?;
    }
    Ok(())
}
