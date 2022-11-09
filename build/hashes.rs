use rand::rngs::SmallRng as Rng;
use rand::RngCore;
use rand::SeedableRng;
use std::io::Write;

fn hash_array(name: &str, len: usize, rng: &mut Rng) -> String {
    let mut res = String::new();

    res.push_str(&format!("pub const {}: [u64; {}] = [\n", name, len));

    for _ in 0..len {
        res.push_str(&format!("    {:#018x},\n", rng.next_u64()));
    }

    res.push_str("];\n\n");

    res
}

pub fn gen_hashes_file(file: &mut std::fs::File) -> std::io::Result<()> {
    let mut rng = Rng::seed_from_u64(0);

    let mut write = |name: &str, len: usize| -> std::io::Result<()> {
        file.write_all(hash_array(name, len, &mut rng).as_bytes())?;
        Ok(())
    };

    write("CASTLE_RIGHTS", 4)?;
    write("EN_PASSANT", 8)?;
    write("SIDE_TO_MOVE", 1)?;
    write("COLOR_PIECE_SQUARE", 64 * 6 * 2)?;

    Ok(())
}
