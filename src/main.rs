#![forbid(unsafe_code)]
#![warn(clippy::pedantic)]
#![warn(clippy::nursery)]
#![warn(clippy::arithmetic_side_effects)]
#![deny(clippy::float_arithmetic)]
#![deny(clippy::float_cmp)]
#![warn(clippy::alloc_instead_of_core)]
#![warn(clippy::std_instead_of_core)]
#![warn(clippy::std_instead_of_alloc)]
#![warn(clippy::print_stdout)]
#![warn(clippy::print_stderr)]

use camino::Utf8PathBuf;
use clap::Parser;
use codespan_reporting::{
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};
use log::info;
use nova::{report, run, CompileResult};
use simple_logger::SimpleLogger;
use std::{fs::read_to_string, process::ExitCode};

#[derive(clap::Parser)]
struct Args {
    filename: Utf8PathBuf,
}

fn main() -> ExitCode {
    SimpleLogger::new().init().unwrap();

    let args = Args::parse();

    let mut files = SimpleFiles::new();

    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = codespan_reporting::term::Config::default();

    match run(
        &mut files,
        &read_to_string(&args.filename).unwrap(),
        &args.filename,
    ) {
        CompileResult::Success { warnings } => {
            for warning in &warnings {
                let diag = report(warning);

                term::emit(&mut writer.lock(), &config, &files, &diag).unwrap();
            }

            if !warnings.is_empty() {
                info!(
                    "{} warning{} generated",
                    warnings.len(),
                    if warnings.len() == 1 { "" } else { "s" }
                );
            }

            ExitCode::SUCCESS
        }
        CompileResult::Failure { warnings, errors } => {
            for warning in &warnings {
                let diag = report(warning);

                term::emit(&mut writer.lock(), &config, &files, &diag).unwrap();
            }

            for error in &errors {
                let diag = report(error);

                term::emit(&mut writer.lock(), &config, &files, &diag).unwrap();
            }

            if !warnings.is_empty() {
                info!(
                    "{} warning{} generated",
                    warnings.len(),
                    if warnings.len() == 1 { "" } else { "s" }
                );
            }

            info!(
                "{} error{} found",
                errors.len(),
                if errors.len() == 1 { "" } else { "s" }
            );

            ExitCode::FAILURE
        }
    }
}
