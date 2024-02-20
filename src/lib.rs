#![forbid(unsafe_code)]
#![warn(clippy::pedantic)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::missing_panics_doc)]
#![allow(clippy::too_many_lines)]
#![warn(clippy::nursery)]

use ariadne::{ColorGenerator, FileCache, Label, Report};
use chumsky::prelude::*;
use error::{convert, Error};
use exolvl::{Exolvl, Read as _, Write as _};
use flate2::write::GzEncoder;
use span::{Span, Spanned};
use std::{
    fmt::Display,
    io::{Read, Write},
    path::Path,
};

mod ast;
mod codegen;
mod error;
mod lexer;
mod low_ir;
mod mir;
mod parser;
mod scopes;
mod span;
mod typecheck;

type IntTy = i32;
type FloatTy = f32;

pub fn run<'src: 'file, 'file>(
    input: &'src str,
    filename: &'file Path,
    level: impl Read,
    out: impl Write,
) -> (bool, Vec<Error<'file>>, Vec<Error<'file>>) {
    let mut warnings = vec![];
    let mut errors = vec![];

    let (tokens, lex_errors) = lexer::lexer()
        .parse(input.with_context(filename))
        .into_output_errors();

    errors.extend(map_errors(lex_errors));

    let (ast, parse_errors) = tokens.as_ref().map_or_else(
        || (None, vec![]),
        |tokens| {
            let eof = input.chars().count() - 1;
            let end_of_input = Span::new(filename, eof..eof);

            parser::parser()
                .parse(tokens.spanned(end_of_input))
                .into_output_errors()
        },
    );

    errors.extend(map_errors(parse_errors));

    let (typed_ast, typecheck_warnings, typecheck_errs) =
        ast.map_or_else(|| (vec![], vec![], vec![]), typecheck::typecheck);

    warnings.extend(typecheck_warnings);
    errors.extend(typecheck_errs);

    if errors.is_empty() {
        let mir = mir::build(&typed_ast);

        let low_ir = low_ir::lower(&mir);

        // for toplevel in &low_ir {
        //     println!("{toplevel}");
        // }

        let mut decoder = flate2::read::GzDecoder::new(level);

        let mut exolvl = Exolvl::read(&mut decoder).unwrap();

        codegen::codegen(&low_ir, &mut exolvl);

        // println!("{exolvl:#?}");

        let mut encoder = GzEncoder::new(out, flate2::Compression::default());

        exolvl.write(&mut encoder).unwrap();

        encoder.finish().unwrap();

        (true, warnings, errors)
    } else {
        (false, warnings, errors)
    }
}

pub fn report_warnings_errors<'file, Id>(filename: Id, warnings: Vec<Error>, errors: Vec<Error>)
where
    Id: Into<<<Span<'file> as ariadne::Span>::SourceId as ToOwned>::Owned> + Copy,
{
    for (error, kind) in warnings
        .iter()
        .map(|warning| (warning, ariadne::ReportKind::Warning))
        .chain(
            errors
                .iter()
                .map(|error| (error, ariadne::ReportKind::Error)),
        )
    {
        report(filename, error, kind)
            .eprint(FileCache::default())
            .unwrap();
    }
}

pub fn report<'a: 'file, 'file, Id>(
    filename: Id,
    error: &'file Error<'file>,
    kind: ariadne::ReportKind<'a>,
) -> Report<'file, Span<'file>>
where
    Id: Into<<<Span<'file> as ariadne::Span>::SourceId as ToOwned>::Owned>,
{
    let mut color_generator = ColorGenerator::new();

    let message = error.message();
    let spans = error.spans();
    let note = error.note();

    let offset = spans.iter().map(|s| s.1.start()).min().unwrap_or(0);

    let mut report = Report::build(kind, filename, offset);

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
) -> Vec<Error<'file>> {
    errors
        .into_iter()
        .map(|e| e.map_token(|t| t.to_string()))
        .flat_map(|e| convert(&e))
        .collect()
}

#[derive(Default)]
struct IdGen {
    next_id: usize,
}

impl IdGen {
    fn next(&mut self) -> usize {
        let id = self.next_id;
        self.next_id += 1;
        id
    }

    fn next_i32(&mut self) -> i32 {
        i32::try_from(self.next()).unwrap()
    }
}
