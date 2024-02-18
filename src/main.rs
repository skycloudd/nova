#![forbid(unsafe_code)]
#![warn(clippy::pedantic)]
#![warn(clippy::nursery)]

use ariadne::{Color, FileCache, Fmt};
use clap::Parser;
use nova::{report, run};
use std::{
    fs::read_to_string,
    io::{Cursor, Read, Write},
    path::{Path, PathBuf},
};

#[derive(clap::Parser)]
struct Args {
    filename: PathBuf,

    /// The .exolvl file to modify
    #[clap(short = 'i', long = "input")]
    level: Option<PathBuf>,

    /// Output file
    #[clap(short, long = "out")]
    output: PathBuf,

    /// Force-overwrite the output file if it exists
    #[clap(long)]
    force: bool,
}

fn main() {
    let args = Args::parse();

    let level = level(args.level);

    let out = output(args.output, args.force);

    match run(
        &read_to_string(&args.filename).unwrap(),
        &args.filename,
        level,
        out,
    ) {
        Ok(()) => {}
        Err(errors) => {
            for error in &errors {
                report(&args.filename, error)
                    .eprint(FileCache::default())
                    .unwrap();
            }

            eprintln!(
                "{} error{} found",
                errors.len(),
                if errors.len() == 1 { "" } else { "s" }
            );
        }
    }
}

fn level(input: Option<PathBuf>) -> Box<dyn Read> {
    input.map_or_else(
        || Box::new(Cursor::new(include_bytes!("default.exolvl"))) as Box<dyn Read>,
        |path| Box::new(std::fs::File::open(path).unwrap()),
    )
}

fn output<P: AsRef<Path>>(out: P, overwrite: bool) -> Box<dyn Write> {
    let out = out.as_ref();

    Box::new(match out.try_exists() {
        Ok(true) | Err(_) if out.extension() != Some("exolvl".as_ref()) || overwrite => {
            panic!(
                "output file `{}` already exists",
                out.display().fg(Color::Yellow)
            )
        }
        _ => std::fs::File::create(out).unwrap(),
    })
}
