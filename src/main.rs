#![forbid(unsafe_code)]
#![warn(clippy::pedantic)]
#![warn(clippy::nursery)]

use ariadne::{Color, FileCache, Fmt};
use clap::Parser;
use nova::{report, run, CompileResult};
use std::{fs::read_to_string, path::PathBuf, process::ExitCode};

#[derive(clap::Parser)]
struct Args {
    filename: PathBuf,
}

fn main() -> ExitCode {
    let args = Args::parse();

    eprintln!(
        "{} {}",
        "Compiling".fg(Color::Green),
        args.filename.display()
    );

    match run(&read_to_string(&args.filename).unwrap(), &args.filename) {
        CompileResult::Success { warnings } => {
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

            ExitCode::SUCCESS
        }
        CompileResult::Failure { warnings, errors } => {
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

            ExitCode::FAILURE
        }
    }
}
