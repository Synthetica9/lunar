// Hardcodes tuning table

use std::{fs::File, io::Write};

use serde::Deserialize;
use serde_yaml::Deserializer;

use parameters::*;

pub fn gen_tuning_file(file: &mut File) -> std::io::Result<()> {
    println!("cargo:rerun-if-changed=parameters.yaml");
    let input = File::open("parameters.yaml")?;
    let yaml = Deserializer::from_reader(input);
    let result = Parameters::deserialize(yaml).unwrap();
    println!("{result:#?}");
    writeln!(file, "use parameters::*;")?;
    write!(
        file,
        "pub const STATIC_PARAMETERS: Parameters = {result:#?};"
    )?;
    Ok(())
}
