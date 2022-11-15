use std::fs::File;
use std::io::Write;

pub mod hashes;
use hashes::gen_hashes_file;

pub mod magics;
use magics::gen_magics_file;

fn main() -> std::io::Result<()> {
    println!("cargo:rerun-if-changed=build");

    gen_squares();

    gen_magics_file(&mut open("magics")?)?;
    gen_hashes_file(&mut open("hashes")?)?;

    // std::thread::sleep(std::time::Duration::from_secs(10));
    Ok(())
}

fn open(name: &str) -> std::io::Result<File> {
    let out_dir = std::env::var("OUT_DIR").unwrap();
    let dest_path = format!("{}/{}.rs", out_dir, name);

    File::create(&dest_path)
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

fn gen_squares() -> std::io::Result<()> {
    let lines: Vec<_> = squares()
        .iter()
        .zip(0..64)
        .map(|(sq, i)| format!("pub const {}: Square = Square::from_index({});\n", sq, i))
        .collect();

    let mut file = open("squares")?;

    file.write_all("use crate::square::Square;\n\n".as_bytes())?;
    for line in lines {
        file.write_all(line.as_bytes())?;
    }
    Ok(())
}
