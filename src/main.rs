use ariadne::FileCache;
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
}

fn main() {
    let args = Args::parse();

    let level = match args.level {
        Some(path) => Box::new(std::fs::File::open(path).unwrap()) as Box<dyn Read>,
        None => Box::new(Cursor::new(include_bytes!("default.exolvl"))) as Box<dyn Read>,
    };

    let out = match args.out {
        Some(path) => Box::new(std::fs::File::create(path).unwrap()) as Box<dyn Write>,
        None => Box::new(std::io::stdout()) as Box<dyn Write>,
    };

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
