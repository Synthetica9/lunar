use std::fs::File;
use std::io::Write;

pub mod magics;
use magics::gen_magics_file;

pub fn main() -> std::io::Result<()> {
    println!("cargo:rerun-if-changed=build");

    gen_squares()?;

    gen_magics_file(&mut open("magics")?)?;

    let evalfile = match std::env::var("EVALFILE") {
        Ok(name) => name,
        Err(_) => "./nets/screlu1024_hm.nnue".to_owned(),
    };

    let evalfile = std::path::PathBuf::from(evalfile);
    let canon = match std::fs::canonicalize(&evalfile) {
        Ok(p) => p,
        Err(err) => panic!(
            "Could not load EVALFILE {}: {err}",
            evalfile.to_str().unwrap()
        ),
    };

    let string_path = canon.to_str().unwrap();
    println!("cargo:rustc-env=NETWORK={string_path}");
    println!("cargo:rerun-if-changed={string_path}");
    println!("cargo:rerun-if-env-changed=EVALFILE");

    // std::thread::sleep(std::time::Duration::from_secs(10));
    Ok(())
}

fn open(name: &str) -> std::io::Result<File> {
    let out_dir = std::env::var("OUT_DIR").unwrap();
    let dest_path = format!("{out_dir}/{name}.rs");

    File::create(dest_path)
}

fn squares() -> Vec<String> {
    (0..64)
        .map(|i| {
            let file = i % 8;
            let rank = i / 8;

            let file = b'A' + file;
            let rank = b'1' + rank;

            format!("{}{}", file as char, rank as char)
        })
        .collect()
}

fn gen_squares() -> std::io::Result<()> {
    let mut file = open("squares")?;
    writeln!(file, "#[derive(Debug, Clone, Copy, Eq, PartialEq)]")?;
    writeln!(file, "pub enum Square {{")?;
    for sq in squares() {
        writeln!(file, "    {sq},")?;
    }
    writeln!(file, "}}")?;

    writeln!(file, "impl Square {{")?;
    writeln!(file, "    pub const fn from_u8(n: u8) -> Square {{")?;
    writeln!(file, "        match n {{")?;
    for (i, sq) in squares().iter().enumerate() {
        writeln!(file, "            {i} => Square::{sq},")?;
    }
    writeln!(file, "            _ => panic!()")?;
    writeln!(file, "        }}")?;
    writeln!(file, "    }}")?;
    writeln!(file, "}}")?;

    Ok(())
}
