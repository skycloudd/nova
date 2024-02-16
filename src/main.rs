use ariadne::{Color, FileCache, Fmt};
use clap::Parser;
use nova::{report, run};
use std::{
    fs::read_to_string,
    io::{Cursor, Read, Write},
    path::PathBuf,
};

#[derive(clap::Parser)]
struct Args {
    filename: PathBuf,

    #[clap(short, long)]
    level: Option<PathBuf>,

    #[clap(short, long)]
    out: Option<PathBuf>,

    #[clap(long)]
    overwrite: bool,
}

fn main() {
    let args = Args::parse();

    let level = level(args.level);

    let out = output(args.out, args.overwrite);

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
    match input {
        Some(path) => Box::new(std::fs::File::open(path).unwrap()),
        None => Box::new(Cursor::new(include_bytes!("default.exolvl"))),
    }
}

fn output(out: Option<PathBuf>, overwrite: bool) -> Box<dyn Write> {
    match out {
        Some(path) => Box::new(match path.try_exists() {
            Ok(true) | Err(_) if path.extension() != Some("exolvl".as_ref()) || overwrite => {
                panic!(
                    "output file `{}` already exists",
                    path.display().fg(Color::Yellow)
                )
            }
            _ => std::fs::File::create(path).unwrap(),
        }),
        None => Box::new(std::io::stdout()),
    }
}
