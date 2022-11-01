use rand;
use std::fs::File;
use std::io::Write;

pub mod magics;
use magics::gen_magics_file;

fn main() -> std::io::Result<()> {
    println!("cargo:rerun-if-changed=build");

    gen_squares();
    gen_hashes();

    gen_magics_file(&mut open("magics"))?;

    // std::thread::sleep(std::time::Duration::from_secs(10));
    Ok(())
}

fn open(name: &str) -> File {
    let out_dir = std::env::var("OUT_DIR").unwrap();
    let dest_path = format!("{}/{}.rs", out_dir, name);

    File::create(&dest_path).unwrap()
}

fn squares() -> Vec<String> {
    (0..64)
        .map(|i| {
            let file = i % 8;
            let rank = i / 8;

            let file = 'A' as u8 + file;
            let rank = '1' as u8 + rank;

            format!("{}{}", file as char, rank as char)
        })
        .collect()
}

fn gen_squares() {
    let lines: Vec<_> = squares()
        .iter()
        .zip(0..64)
        .map(|(sq, i)| format!("pub const {}: Square = Square::from_index({});\n", sq, i))
        .collect();

    let mut file = open("squares");

    file.write_all("use crate::square::Square;\n\n".as_bytes());
    for line in lines {
        file.write_all(line.as_bytes()).unwrap();
    }
}

fn gen_hashes() {
    use rand::rngs::SmallRng as Rng;
    use rand::RngCore;
    use rand::SeedableRng;

    fn hash_array(name: &str, len: usize, rng: &mut Rng) -> String {
        let mut res = String::new();

        res.push_str(&format!("pub const {}: [u64; {}] = [\n", name, len));

        for _ in 0..len {
            res.push_str(&format!("    {:#018x},\n", rng.next_u64()));
        }

        res.push_str("];\n\n");

        res
    }

    let mut rng = Rng::seed_from_u64(0);

    let mut file = open("hashes");
    let mut write = |name: &str, len: usize| {
        file.write_all(hash_array(name, len, &mut rng).as_bytes())
            .unwrap();
    };

    write("CASTLE_RIGHTS", 4);
    write("EN_PASSANT", 8);
    write("SIDE_TO_MOVE", 1);
    write("COLOR_PIECE_SQUARE", 64 * 6 * 2);
}
