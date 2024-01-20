#![forbid(unsafe_code)]
#![warn(clippy::pedantic)]
#![allow(clippy::too_many_lines)]
#![allow(clippy::float_cmp)]
#![warn(clippy::nursery)]
#![allow(clippy::cognitive_complexity)]
#![allow(clippy::type_complexity)]

use ariadne::{ColorGenerator, FileCache, Label, Report};
use chumsky::prelude::*;
use error::{convert, Error};
use span::{Span, Spanned};
use std::{
    fmt::Display,
    fs::read_to_string,
    io::Cursor,
    path::{Path, PathBuf},
};

mod ast;
mod codegen;
mod const_eval;
mod error;
mod lexer;
mod low_ir;
mod mir;
mod mir_no_span;
mod optimiser;
mod parser;
mod scopes;
mod span;
mod typecheck;

type IntTy = i32;
type FloatTy = f32;

#[derive(clap::Parser)]
struct Args {
    filename: PathBuf,

    #[clap(short, long)]
    with: Option<PathBuf>,

    #[clap(short, long)]
    out: Option<PathBuf>,

    #[clap(short, long)]
    eval: bool,
}

fn main() {
    let args = <Args as clap::Parser>::parse();

    match run(
        &read_to_string(&args.filename).unwrap(),
        &args.filename,
        args.with.as_deref(),
        args.eval,
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

pub fn run<'file>(
    input: &str,
    filename: &'file Path,
    with: Option<&Path>,
    eval: bool,
) -> Result<Vec<u8>, RunError<'file>> {
    let low_ir = run_inner(input, filename).map_err(RunError::Compile)?;

    if eval {
        low_ir::eval::evaluate(&low_ir);
    }

    let mut reader = Cursor::new(match with {
        Some(with) => std::fs::read(with).map_err(RunError::Io)?,
        None => include_bytes!("default.exolvl").to_vec(),
    });

    let mut exolvl = levelfile::read(&mut reader).map_err(RunError::LevelFile)?;

    if !exolvl.level_data.nova_scripts.0.is_empty() {
        return Err(RunError::Compile(vec![Error::LevelFileHasScripts]));
    }

    codegen::codegen(&low_ir, &mut exolvl);

    Ok(levelfile::write(&exolvl).map_err(RunError::Io)?)
}

fn run_inner<'file>(
    input: &str,
    filename: &'file Path,
) -> Result<Vec<Option<low_ir::BasicBlock>>, Vec<error::Error<'file>>> {
    let mut errors = vec![];

    let (tokens, lex_errors) = lexer::lexer()
        .parse(input.with_context(filename))
        .into_output_errors();

    errors.extend(map_errors(lex_errors));

    let (ast, parse_errors) = tokens.as_ref().map_or((None, vec![]), |tokens| {
        let eof = input.chars().count() - 1;
        let eoi = Span::new(filename, eof..eof);

        parser::parser()
            .parse(tokens.spanned(eoi))
            .into_output_errors()
    });

    errors.extend(map_errors(parse_errors));

    let (typed_ast, type_errors) = ast.map_or((Err(()), vec![]), typecheck::typecheck);

    errors.extend(map_boxed_errors(type_errors));

    let mir = typed_ast.map(mir::build);

    let (mir, const_eval_errors) = mir.map_or((Err(()), vec![]), |mir| const_eval::const_eval(mir));

    errors.extend(map_boxed_errors(const_eval_errors));

    if errors.is_empty() {
        let mir = mir_no_span::mir_remove_span(mir.unwrap());

        let low_ir = low_ir::lower(mir);

        let low_ir = optimiser::optimise(&low_ir);

        Ok(low_ir)
    } else {
        Err(errors)
    }
}

fn report<'file, 'a, Id>(filename: Id, error: &'file error::Error<'file>) -> Report<'a, Span<'file>>
where
    Id: Into<<<Span<'file> as ariadne::Span>::SourceId as ToOwned>::Owned>,
{
    let mut color_generator = ColorGenerator::new();

    let message = error.message();
    let spans = error.spans();
    let note = error.note();

    let offset = spans.iter().map(|s| s.1.start()).min().unwrap_or(0);

    let mut report = Report::build(ariadne::ReportKind::Error, filename, offset);

    report.set_message(message);

    for Spanned(message, span) in spans {
        let mut label = Label::new(span).with_color(color_generator.next());

        if let Some(message) = message {
            label = label.with_message(message);
        }

        report.add_label(label);
    }

    if let Some(note) = note {
        report.set_note(note);
    }

    report.finish()
}

fn map_errors<'file, T: Clone + Display>(
    errors: Vec<Rich<'_, T, Span<'file>>>,
) -> Vec<error::Error<'file>> {
    errors
        .into_iter()
        .map(|e| e.map_token(|t| t.to_string()))
        .flat_map(|e| convert(&e))
        .collect()
}

fn map_boxed_errors(errors: Vec<Box<Error>>) -> Vec<error::Error> {
    errors.into_iter().map(|e| *e).collect()
}

pub enum RunError<'file> {
    Io(std::io::Error),
    LevelFile(binread::Error),
    Compile(Vec<Error<'file>>),
}
