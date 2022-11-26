// Hardcodes tuning table

use std::collections::HashSet;
use std::io::Read;
use std::{fs::File, io::Write};

use yaml_rust::{Yaml, YamlLoader};

fn parse_scalar(yaml: &Yaml) -> Result<(i32, bool), String> {
    match yaml {
        Yaml::Integer(x) => Ok((*x as i32, true)),
        Yaml::String(x) => {
            if !x.starts_with('$') {
                Err("expected `$`".to_string())?;
            }
            let stripped = &x[1..];
            let value = stripped.parse::<i32>().map_err(|x| x.to_string())?;
            Ok((value, false))
        }
        x => Err(format!("Bad value for scalar {x:?}")),
    }
}

fn parse_square_table(yaml: &Yaml) -> Result<(bool, Vec<(i32, bool)>), String> {
    match yaml {
        Yaml::Array(xs) => {
            if xs.len() != 64 {
                Err(format!("Wrong length for array {}", xs.len()))?;
            }

            let res = xs.iter().map(parse_scalar).collect::<Result<Vec<_>, _>>()?;
            // It's nicer if we can just view the array from white's perspective in the file.
            let res = (0..64).map(|i| res[i ^ 56]).collect();
            Ok((true, res))
        }
        x => {
            // parse scalar
            Ok((false, vec![parse_scalar(x)?]))
        }
    }
}

const PIECES: &[&str] = &["pawn", "knight", "bishop", "rook", "queen", "king"];

fn parse_piece_map(yaml: &Yaml) -> Result<(bool, bool, Vec<(i32, bool)>), String> {
    // Try to parse inner
    if let Ok((a, b)) = parse_square_table(yaml) {
        return Ok((false, a, b));
    }

    // Okay, if this is to be valid it should be a piece map.
    let map = yaml.as_hash().ok_or(format!(
        "Expected hashmap with piece values, found {yaml:?}"
    ))?;
    let keys: HashSet<_> = map.keys().map(|x| x.as_str()).collect();
    let expected_keys = PIECES.iter().map(|x| Some(*x)).collect();
    if keys != expected_keys {
        Err("Wrong key set...")?;
    }

    let res = PIECES
        .iter()
        .map(|x| &yaml[*x])
        .map(parse_square_table)
        .collect::<Result<Vec<_>, _>>()?;

    let diff_modes = res.iter().map(|x| x.0).collect::<HashSet<bool>>();

    if diff_modes.len() != 1 {
        Err("Inconsistent members")?;
    }

    let a = res[0].0;
    let b = res.iter().flat_map(|x| &x.1).copied().collect::<Vec<_>>();

    Ok((true, a, b))
}

const GAME_PHASES: &[&str] = &["mg", "eg"];

fn parse_phase_map(yaml: &Yaml) -> Result<(bool, bool, bool, Vec<(i32, bool)>), String> {
    if let Ok((a, b, c)) = parse_piece_map(yaml) {
        return Ok((false, a, b, c));
    }

    let res = GAME_PHASES
        .iter()
        .map(|x| &yaml[*x])
        .map(parse_piece_map)
        .collect::<Result<Vec<_>, _>>()?;

    let diff_modes = res.iter().map(|x| x.0).collect::<HashSet<bool>>();

    if diff_modes.len() != 1 {
        Err("Inconsistent members")?;
    }

    let a = res[0].0;
    let b = res[0].1;
    let c = res.iter().flat_map(|x| &x.2).copied().collect::<Vec<_>>();

    Ok((true, a, b, c))
}

pub fn gen_tuning_file(file: &mut std::fs::File) -> Result<(), std::io::Error> {
    writeln!(file, "// Tuning values from yaml\n")?;
    writeln!(
        file,
        "use crate::eval::parameters::{{EvaluationTerm, Parameters, Parameter, ToYaml}};"
    )?;

    println!("cargo:rerun-if-changed=parameters.yaml");

    let yaml = {
        let mut raw_contents = String::new();
        File::open("parameters.yaml")?.read_to_string(&mut raw_contents)?;
        &YamlLoader::load_from_str(&raw_contents).unwrap()[0]
    };

    let xs = yaml.as_hash().ok_or("Couldn't parse hash").unwrap();

    let res = xs
        .iter()
        .map(|(k, v)| {
            (
                k.as_str().ok_or(format!("Key must be string, found {k:?}")),
                parse_phase_map(v),
            )
        })
        .map(|(k, v)| (k.unwrap(), v.unwrap()))
        .collect::<Vec<_>>();

    // Output  data structures: one as a layout guide, one with the data, and
    // one with the "mutability" (can this parameter be tuned? Prefixed with $ if it can't)

    writeln!(file, "impl<'a> Parameters<'a> {{")?;
    writeln!(
        file,
        "    pub fn yaml_terms(&self) -> [(String, bool, String); {}] {{",
        res.len()
    )?;
    writeln!(file, "        [")?;
    for (name, line) in res.iter() {
        let (a, b, c, _) = line;

        writeln!(
            file,
            "            ({name:?}.to_string(), {}, self.{name}().to_yaml()),",
            *a || *b || *c
        )?;
    }
    writeln!(file, "        ]")?;
    writeln!(file, "    }}\n")?;

    let mut offset = 0;
    for (name, line) in res.iter() {
        let (a, b, c, vals) = line;
        writeln!(
            file,
            "    pub fn {name}(self) -> Parameter<'a, {a}, {b}, {c}> {{",
        )?;
        writeln!(file, "        Parameter::new({offset}, self)")?;
        writeln!(file, "    }}\n")?;
        offset += vals.len();
    }
    writeln!(file, "}}\n")?;

    // write values and mutability
    let values = res.iter().flat_map(|(_, x)| &x.3).collect::<Vec<_>>();
    assert!(offset == values.len());

    writeln!(file, "pub const N_PARAMETERS: usize = {};", values.len())?;
    writeln!(file, "pub const MUTABILITY: [bool; N_PARAMETERS] = [")?;
    for (_, p) in values.iter() {
        writeln!(file, "  {p},")?;
    }
    writeln!(file, "];\n")?;

    writeln!(
        file,
        "pub const STATIC_EVALUATOR_VALUES: [i32; N_PARAMETERS] = ["
    )?;
    for (val, _) in values.iter() {
        writeln!(file, "  {val},")?;
    }
    writeln!(file, "];")?;

    Ok(())
}
