use ariadne::FileCache;
use nova::{report, run, RunError};
use std::{fs::read_to_string, path::PathBuf};

#[derive(clap::Parser)]
struct Args {
    filename: PathBuf,

    #[clap(short, long)]
    with: Option<PathBuf>,

    #[clap(short, long)]
    out: Option<PathBuf>,
}

fn main() {
    let args = <Args as clap::Parser>::parse();

    match run(
        &read_to_string(&args.filename).unwrap(),
        &args.filename,
        args.with.as_deref(),
    ) {
        Ok(out_bytes) => match args.out {
            Some(out) => std::fs::write(out, out_bytes).unwrap(),
            None => std::io::Write::write_all(&mut std::io::stdout(), &out_bytes).unwrap(),
        },
        Err(RunError::Io(error)) => {
            eprintln!("Io error: {}", error);
        }
        Err(RunError::LevelFile(error)) => {
            eprintln!("Error reading level file: {}", error);
        }
        Err(RunError::Compile(errors)) => {
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
