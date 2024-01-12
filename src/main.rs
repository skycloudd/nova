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
use span::Span;
use std::{
    fmt::Display,
    fs::read_to_string,
    path::{Path, PathBuf},
};

mod ast;
mod const_eval;
mod error;
mod lexer;
mod mir;
mod mir_no_span;
mod parser;
mod scopes;
mod span;
mod typecheck;

#[derive(clap::Parser)]
struct Args {
    filename: PathBuf,
}

fn main() -> std::io::Result<()> {
    let args = <Args as clap::Parser>::parse();

    run_filename(&args.filename)
}

fn run_filename(filename: &Path) -> std::io::Result<()> {
    let input = read_to_string(filename)?;

    if let Err(errors) = run(&input, filename) {
        for error in &errors {
            report(filename, error).eprint(FileCache::default())?;
        }

        eprintln!("{} errors found", errors.len());
    }

    Ok(())
}

fn run<'file>(input: &str, filename: &'file Path) -> Result<(), Vec<error::Error<'file>>> {
    let mut errors = vec![];

    let (tokens, lex_errors) = lexer::lexer()
        .parse(input.with_context(filename))
        .into_output_errors();

    errors.extend(map_errors(lex_errors));

    let (ast, parse_errors) = tokens.as_ref().map_or((None, vec![]), |tokens| {
        let eoi = Span::new(filename, input.len()..input.len());

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
        if let Ok(mir) = mir {
            let mir = mir_no_span::mir_remove_span(mir);

            println!("{mir:?}");
        }

        Ok(())
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

    for span in spans {
        let mut label = Label::new(span.1).with_color(color_generator.next());

        if let Some(message) = span.0 {
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
