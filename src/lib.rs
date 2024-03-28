#![forbid(unsafe_code)]
#![warn(clippy::pedantic)]
#![warn(clippy::nursery)]
#![warn(clippy::arithmetic_side_effects)]
#![warn(clippy::float_arithmetic)]
#![warn(clippy::float_cmp)]
#![warn(clippy::alloc_instead_of_core)]
#![warn(clippy::std_instead_of_core)]
#![warn(clippy::std_instead_of_alloc)]
#![warn(clippy::print_stdout)]
#![warn(clippy::print_stderr)]

use crate::span::Spanned;
use camino::Utf8Path;
use chumsky::prelude::*;
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
};
use core::fmt::Display;
use cranelift_object::ObjectProduct;
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
        object: Box<ObjectProduct>,
        warnings: Vec<Warning>,
    },
    Failure {
        warnings: Vec<Warning>,
        errors: Vec<Error>,
    },
}

pub fn run<'file>(
    files: &mut SimpleFiles<&'file Utf8Path, &str>,
    input: &str,
    filename: &'file Utf8Path,
    print_timing: bool,
) -> CompileResult {
    info!("compiling {}", filename);

    // leak the input
    let input: &'static str = Box::leak(input.into());

    let file_id = files.add(filename, input);

    let file_ctx = Ctx(file_id);

    let mut timing = TimingStats::new(print_timing);

    let mut warnings = vec![];
    let mut errors = vec![];

    let (tokens, lex_errors) = timing.time("lexing", || {
        lexer::lexer()
            .parse(input.with_context(file_ctx))
            .into_output_errors()
    });

    errors.extend(map_errors(lex_errors));

    let (ast, parse_errors) = timing.time("parsing", || {
        tokens.as_ref().map_or_else(
            || (None, vec![]),
            |tokens| {
                let eof = input.chars().count().saturating_sub(1);
                let end_of_input = Span::new(file_ctx, eof..eof);

                parser::parser()
                    .parse(tokens.spanned(end_of_input))
                    .into_output_errors()
            },
        )
    });

    errors.extend(map_errors(parse_errors));

    let (typed_ast, typecheck_warnings, typecheck_errs) = timing.time("typechecking", || {
        ast.map_or_else(|| (vec![], vec![], vec![]), typecheck::typecheck)
    });

    warnings.extend(typecheck_warnings);
    errors.extend(typecheck_errs);

    let mir = timing.time("building MIR", || mir::build(typed_ast));

    let (analyse_warnings, analyse_errors) = timing.time("analysis", || analysis::analyse(&mir));

    warnings.extend(analyse_warnings);
    errors.extend(analyse_errors);

    if errors.is_empty() && !has_correct_main(&mir) {
        errors.push(Error::MissingMainFunction);
    }

    let res = if errors.is_empty() {
        let mir = timing.time("removing spans", || mir_no_span::build(mir));

        let low_ir = timing.time("building LIR", || low_ir::lower(mir));

        let object = timing.time("codegen", || codegen::codegen(low_ir));

        info!("compilation successful");

        CompileResult::Success {
            object: Box::new(object),
            warnings,
        }
    } else {
        error!("compilation failed");

        CompileResult::Failure { warnings, errors }
    };

    timing.print();

    res
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

#[allow(clippy::match_like_matches_macro)]
#[allow(clippy::match_wildcard_for_single_variants)]
fn has_correct_main(mir: &[Spanned<mir::TopLevel>]) -> bool {
    mir.iter().any(|top_level| match top_level.0 {
        mir::TopLevel::Function(Spanned(
            mir::Function {
                name: Spanned("main", _),
                params: Spanned(ref params, _),
                return_ty: Spanned(mir::Type::Integer, _),
                ..
            },
            _,
        )) if params.is_empty() => true,
        _ => false,
    })
}

struct TimingStats {
    stats: Vec<(&'static str, std::time::Duration)>,
    flag: bool,
}

impl TimingStats {
    fn new(flag: bool) -> Self {
        Self {
            stats: vec![],
            flag,
        }
    }

    fn push(&mut self, name: &'static str, duration: std::time::Duration) {
        self.stats.push((name, duration));
    }

    fn time<F, T>(&mut self, name: &'static str, f: F) -> T
    where
        F: FnOnce() -> T,
    {
        let start = std::time::Instant::now();
        let res = f();
        let duration = start.elapsed();

        self.push(name, duration);

        res
    }

    fn print(&self) {
        if !self.flag {
            return;
        }

        for (name, duration) in &self.stats {
            info!("{:<15}: {:>8.2?}", name, duration);
        }
    }
}
