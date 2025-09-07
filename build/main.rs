use std::fs::File;
use std::io::{Read, Write};

pub mod magics;
use magics::gen_magics_file;

fn lunar_version() -> String {
    let from_env = std::env::var("LUNAR_VERSION");
    if let Ok(ver) = from_env {
        return ver;
    }

    let from_git = std::process::Command::new("git")
        .arg("describe")
        .arg("--tags")
        .arg("--dirty")
        .output();
    if let Ok(output) = from_git {
        if output.status.success() {
            println!("cargo:rerun-if-changed=.git/HEAD");
            let revision = std::fs::File::open(".git/HEAD");
            if let Ok(mut revision) = revision {
                let mut content = String::new();
                revision.read_to_string(&mut content).unwrap();

                if let Some(r) = content.strip_prefix("ref: ") {
                    content = r.to_string();
                }

                if let Some(r) = content.strip_suffix("\n") {
                    content = r.to_string();
                }

                println!("cargo:rerun-if-changed=.git/{content}");
            }

            return String::from_utf8(output.stdout).unwrap();
        }
    }

    let from_cargo = std::env::var("CARGO_PKG_VERSION");
    if let Ok(ver) = from_cargo {
        return ver;
    }

    "unknown".to_owned()
}

pub fn main() -> std::io::Result<()> {
    println!("cargo:rerun-if-changed=build");

    gen_squares()?;

    gen_magics_file(&mut open("magics")?)?;

    let evalfile = match std::env::var("EVALFILE") {
        Ok(name) => name,
        Err(_) => "./nets/screlu512_hm.nnue".to_owned(),
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

    println!("cargo:rustc-env=LUNAR_VERSION={}", lunar_version());
    println!("cargo:rerun-if-env-changed=LUNAR_VERSION");
    println!("cargo:rerun-if-env-changed=CARGO_PKG_VERSION");

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
