#![forbid(unsafe_code)]
#![warn(clippy::pedantic)]
#![allow(clippy::too_many_lines)]
#![allow(clippy::float_cmp)]
#![warn(clippy::nursery)]
#![allow(clippy::cognitive_complexity)]
#![allow(clippy::type_complexity)]
#![allow(clippy::missing_errors_doc)]

use ariadne::{ColorGenerator, Label, Report};
use chumsky::prelude::*;
use error::{convert, Error};
use flate2::{bufread::GzDecoder, write::GzEncoder, Compression};
use span::{Span, Spanned};
use std::{
    fmt::Display,
    io::{Cursor, Read as _, Write as _},
    path::Path,
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

pub fn run<'src: 'file, 'file>(
    input: &'src str,
    filename: &'file Path,
    with: Option<&Path>,
) -> Result<Vec<u8>, RunError<'file>> {
    let low_ir = run_inner(input, filename).map_err(RunError::Compile)?;

    let mut exolvl = read_exolvl(with)?;

    if !exolvl.level_data.nova_scripts.0.is_empty() {
        return Err(RunError::Compile(vec![Error::LevelFileHasScripts]));
    }

    codegen::codegen(&low_ir, &mut exolvl);

    write_exolvl(&exolvl).map_err(RunError::Io)
}

fn run_inner<'src: 'file, 'file>(
    input: &'src str,
    filename: &'file Path,
) -> Result<Vec<low_ir::BasicBlock>, Vec<error::Error<'file>>> {
    let mut errors = vec![];

    let (tokens, lex_errors) = lexer::lexer()
        .parse(input.with_context(filename))
        .into_output_errors();

    errors.extend(map_errors(lex_errors));

    let (ast, parse_errors) = tokens.as_ref().map_or((None, vec![]), |tokens| {
        let eof = input.chars().count() - 1;
        let eoi_span = Span::new(filename, eof..eof);

        parser::parser()
            .parse(tokens.spanned(eoi_span))
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

        let mut low_ir = low_ir::lower(mir);

        optimiser::optimise(&mut low_ir);

        for block in &low_ir {
            eprintln!("{block}\n");
        }

        Ok(low_ir)
    } else {
        Err(errors)
    }
}

pub fn report<'file, 'a, Id>(
    filename: Id,
    error: &'file error::Error<'file>,
) -> Report<'a, Span<'file>>
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

fn read_exolvl<'file, P: AsRef<Path>>(
    with: Option<P>,
) -> Result<levelfile::Exolvl, RunError<'file>> {
    let level_bytes = match with {
        Some(with) => {
            let read_bytes = std::fs::read(with).map_err(RunError::Io)?;

            decode_exolvl(&read_bytes).map_err(RunError::Io)?
        }
        None => include_bytes!("default.exolvl").to_vec(),
    };

    levelfile::read(&mut Cursor::new(level_bytes)).map_err(RunError::LevelFile)
}

fn write_exolvl(exolvl: &levelfile::Exolvl) -> std::io::Result<Vec<u8>> {
    let data = levelfile::write(exolvl)?;

    encode_exolvl(&data)
}

fn decode_exolvl(bytes: &[u8]) -> std::io::Result<Vec<u8>> {
    let mut decoder = GzDecoder::new(bytes);

    let mut buf = Vec::new();

    decoder.read_to_end(&mut buf)?;

    Ok(buf)
}

fn encode_exolvl(bytes: &[u8]) -> std::io::Result<Vec<u8>> {
    let mut encoder = GzEncoder::new(Vec::new(), Compression::default());

    encoder.write_all(bytes)?;

    encoder.finish()
}
