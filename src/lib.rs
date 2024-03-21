#![forbid(unsafe_code)]
#![warn(clippy::pedantic)]
#![warn(clippy::nursery)]
#![warn(clippy::arithmetic_side_effects)]

use ariadne::{ColorGenerator, Label, Report};
use chumsky::prelude::*;
use error::{convert, Diagnostic, Error, Warning};
use span::{Span, Spanned};
use std::{fmt::Display, path::Path};

mod ast;
mod codegen;
mod control_flow;
mod error;
mod lexer;
mod low_ir;
mod mir;
mod mir_no_span;
mod parser;
mod scopes;
mod span;
mod typecheck;

type IntTy = i32;
type FloatTy = f32;

pub enum CompileResult<'file> {
    Success {
        warnings: Vec<Warning<'file>>,
    },
    Failure {
        warnings: Vec<Warning<'file>>,
        errors: Vec<Error<'file>>,
    },
}

pub fn run<'file>(input: &str, filename: &'file Path) -> CompileResult<'file> {
    let mut warnings = vec![];
    let mut errors = vec![];

    let (tokens, lex_errors) = lexer::lexer()
        .parse(input.with_context(filename))
        .into_output_errors();

    errors.extend(map_errors(lex_errors));

    let (ast, parse_errors) = tokens.as_ref().map_or_else(
        || (None, vec![]),
        |tokens| {
            let eof = input.chars().count().saturating_sub(1);
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

    let mir = mir::build(typed_ast);

    // let (cf_warnings, cf_errors) = control_flow::check(&mir);

    if errors.is_empty() {
        let mir = mir_no_span::build(mir);

        let low_ir = low_ir::lower(mir);

        codegen::codegen(&low_ir);

        CompileResult::Success { warnings }
    } else {
        CompileResult::Failure { warnings, errors }
    }
}

pub fn report<'a: 'file, 'file, Id>(
    filename: Id,
    diagnostic: &'file (impl Diagnostic<'file> + ?Sized),
    kind: ariadne::ReportKind<'a>,
) -> Report<'file, Span<'file>>
where
    Id: Into<<<Span<'file> as ariadne::Span>::SourceId as ToOwned>::Owned>,
{
    let mut color_generator = ColorGenerator::new();

    let message = diagnostic.message();
    let spans = diagnostic.spans();
    let note = diagnostic.note();

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

#[derive(Debug, Default)]
struct IdGen {
    next_id: usize,
}

impl IdGen {
    const fn new() -> Self {
        Self { next_id: 0 }
    }

    fn next(&mut self) -> usize {
        let id = self.next_id;
        self.next_id += 1;
        id
    }
}
