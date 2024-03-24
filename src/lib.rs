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

use camino::Utf8Path;
use chumsky::prelude::*;
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
};
use core::fmt::Display;
use error::{convert, Diag, Error, ErrorSpan, Warning};
use log::{error, info};
use span::{Ctx, Span};

mod analysis;
mod ast;
mod codegen;
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

pub enum CompileResult {
    Success {
        warnings: Vec<Warning>,
    },
    Failure {
        warnings: Vec<Warning>,
        errors: Vec<Error>,
    },
}

pub fn run<'src, 'file>(
    files: &mut SimpleFiles<&'file Utf8Path, &'src str>,
    input: &'src str,
    filename: &'file Utf8Path,
) -> CompileResult {
    info!("compiling {}", filename);

    let file_id = files.add(filename, input);

    let file_ctx = Ctx(file_id);

    let mut warnings = vec![];
    let mut errors = vec![];

    let (tokens, lex_errors) = lexer::lexer()
        .parse(input.with_context(file_ctx))
        .into_output_errors();

    errors.extend(map_errors(lex_errors));

    let (ast, parse_errors) = tokens.as_ref().map_or_else(
        || (None, vec![]),
        |tokens| {
            let eof = input.chars().count().saturating_sub(1);
            let end_of_input = Span::new(file_ctx, eof..eof);

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

    // println!("{:#?}", mir);

    let (analyse_warnings, analyse_errors) = analysis::analyse(&mir);

    warnings.extend(analyse_warnings);
    errors.extend(analyse_errors);

    if errors.is_empty() {
        let mir = mir_no_span::build(mir);

        let low_ir = low_ir::lower(mir);

        codegen::codegen(&low_ir);

        info!("compilation successful");

        CompileResult::Success { warnings }
    } else {
        error!("compilation failed");

        CompileResult::Failure { warnings, errors }
    }
}

pub fn report(diagnostic: &impl Diag) -> Diagnostic<usize> {
    codespan_reporting::diagnostic::Diagnostic::new(diagnostic.kind())
        .with_message(diagnostic.message())
        .with_labels(
            diagnostic
                .spans()
                .into_iter()
                .map(|span| match span {
                    ErrorSpan::Primary(message, span) => {
                        let mut label = Label::primary(span.context().0, span.start()..span.end());

                        if let Some(message) = message {
                            label = label.with_message(message);
                        }

                        label
                    }
                    ErrorSpan::Secondary(message, span) => {
                        let mut label =
                            Label::secondary(span.context().0, span.start()..span.end());

                        if let Some(message) = message {
                            label = label.with_message(message);
                        }

                        label
                    }
                })
                .collect(),
        )
        .with_notes(diagnostic.notes())
}

fn map_errors<T: Clone + Display>(errors: Vec<Rich<'_, T, Span>>) -> Vec<Error> {
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
}

impl Iterator for IdGen {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        let id = self.next_id;
        self.next_id = self.next_id.checked_add(1)?;
        Some(id)
    }
}
