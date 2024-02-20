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
    output: Option<PathBuf>,

    /// Force-overwrite the output file if it exists
    #[clap(long)]
    force: bool,
}

fn main() {
    let args = Args::parse();

    let level = level(args.level);

    let out = output(args.output, args.force);

    eprintln!(
        "{} {}",
        "Compiling".fg(Color::Green),
        args.filename.display()
    );

    let exit_code = match run(
        &read_to_string(&args.filename).unwrap(),
        &args.filename,
        level,
        out,
    ) {
        (true, warnings, _) => {
            for warning in &warnings {
                report(&args.filename, warning, ariadne::ReportKind::Warning)
                    .eprint(FileCache::default())
                    .unwrap();
            }

            if !warnings.is_empty() {
                eprintln!(
                    "{} warning{} generated",
                    warnings.len(),
                    if warnings.len() == 1 { "" } else { "s" }
                );
            }

            eprintln!(
                "{} {}",
                "Finished".fg(Color::Green),
                args.filename.display()
            );

            0
        }
        (false, warnings, errors) => {
            for warning in &warnings {
                report(&args.filename, warning, ariadne::ReportKind::Warning)
                    .eprint(FileCache::default())
                    .unwrap();
            }

            if !warnings.is_empty() {
                eprintln!(
                    "{} warning{} generated",
                    warnings.len(),
                    if warnings.len() == 1 { "" } else { "s" }
                );
            }

            for error in &errors {
                report(&args.filename, error, ariadne::ReportKind::Error)
                    .eprint(FileCache::default())
                    .unwrap();
            }

            eprintln!(
                "{} error{} found",
                errors.len(),
                if errors.len() == 1 { "" } else { "s" }
            );

            1
        }
    };

    std::process::exit(exit_code);
}

fn level(input: Option<PathBuf>) -> Box<dyn Read> {
    input.map_or_else(
        || Box::new(Cursor::new(include_bytes!("default.exolvl"))) as Box<dyn Read>,
        |path| Box::new(std::fs::File::open(path).unwrap()),
    )
}

fn output<P: AsRef<Path>>(out: Option<P>, overwrite: bool) -> Box<dyn Write> {
    out.map_or_else(
        || Box::new(std::io::stdout()) as Box<dyn Write>,
        |out| {
            let out = out.as_ref();

            match out.try_exists() {
                Ok(true) | Err(_) if out.extension() != Some("exolvl".as_ref()) || overwrite => {
                    panic!(
                        "output file `{}` already exists",
                        out.display().fg(Color::Yellow)
                    )
                }
                _ => match out.as_os_str().to_str() {
                    Some("-") => Box::new(std::io::stdout()) as Box<dyn Write>,
                    _ => Box::new(std::fs::File::create(out).unwrap()),
                },
            }
        },
    )
}
