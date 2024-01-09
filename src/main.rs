#![forbid(unsafe_code)]

use ariadne::{ColorGenerator, FileCache, Label, Report};
use chumsky::prelude::*;
use error::convert_error;
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
mod typecheck;

#[derive(clap::Parser)]
struct Args {
    /// Input file
    filename: PathBuf,

    /// Print tokens
    #[clap(short, long)]
    tokens: bool,

    /// Print AST
    #[clap(short, long)]
    ast: bool,

    /// Print typechecked AST
    #[clap(short, long)]
    checked_ast: bool,

    /// Print MIR
    #[clap(short, long)]
    mir: bool,
}

fn main() -> std::io::Result<()> {
    let args = <Args as clap::Parser>::parse();

    run(&args)
}

fn run(args: &Args) -> std::io::Result<()> {
    let input = read_to_string(&args.filename)?;

    let mut errors = vec![];

    let (tokens, lex_errors) = lexer::lexer()
        .parse(input.map_span(|s| Span::new(&args.filename, s.into_range())))
        .into_output_errors();

    errors.extend(map_errors(lex_errors));

    if args.tokens {
        if let Some(tokens) = &tokens {
            dbg!(tokens);
        }
    }

    let (ast, parse_errors) = tokens.as_ref().map_or((None, vec![]), |tokens| {
        let eoi_span = Span::new(&args.filename, input.len()..input.len());

        parser::parser()
            .parse(tokens.spanned(eoi_span))
            .into_output_errors()
    });

    errors.extend(map_errors(parse_errors));

    if args.ast {
        if let Some(ast) = &ast {
            dbg!(ast);
        }
    }

    let (typed_ast, type_errors) = ast.map_or((None, vec![]), typecheck::typecheck);

    errors.extend(type_errors.into_iter().map(|e| *e));

    if args.checked_ast {
        if let Some(typed_ast) = &typed_ast {
            dbg!(typed_ast);
        }
    }

    let mut mir = typed_ast.map(mir::build_mir);

    if let Some(mir) = mir.as_mut() {
        if let Err(errs) = const_eval::const_eval(mir) {
            errors.extend(errs.into_iter().map(|e| *e));
        }
    };

    if args.mir {
        if let Some(mir) = &mir {
            dbg!(mir);
        }
    }

    if errors.is_empty() {
        if let Some(mir) = mir {
            let mir = mir_no_span::mir_remove_span(mir);

            println!("{:?}", mir);
        }
    } else {
        for error in &errors {
            print_error(&args.filename, error)?;
        }

        eprintln!("{} errors found", errors.len());
    }

    Ok(())
}

fn print_error(filename: &Path, error: &error::Error) -> std::io::Result<()> {
    let mut color_generator = ColorGenerator::new();

    let message = error.message();
    let spans = error.spans();
    let note = error.note();

    let offset = spans.iter().map(|s| s.1 .0.start()).min().unwrap_or(0);

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

    report.finish().eprint(FileCache::default())
}

fn map_errors<'file, T: Clone + Display>(
    errors: Vec<Rich<'_, T, Span<'file>>>,
) -> Vec<error::Error<'file>> {
    errors
        .into_iter()
        .map(|e| e.map_token(|t| t.to_string()))
        .flat_map(convert_error)
        .collect()
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Span<'file>(SimpleSpan<usize, &'file Path>);
pub type Spanned<'file, T> = (T, Span<'file>);

impl<'file> Span<'file> {
    pub fn new(context: &'file Path, range: std::ops::Range<usize>) -> Span<'file> {
        Span(SimpleSpan::<usize, &'file Path>::new(context, range))
    }

    pub fn union(self, other: Span<'file>) -> Span<'file> {
        Span(self.0.union(other.0))
    }
}

impl<'file> chumsky::span::Span for Span<'file> {
    type Context = &'file Path;

    type Offset = usize;

    fn new(context: Self::Context, range: std::ops::Range<Self::Offset>) -> Self {
        Span::new(context, range)
    }

    fn context(&self) -> Self::Context {
        self.0.context()
    }

    fn start(&self) -> Self::Offset {
        self.0.start()
    }

    fn end(&self) -> Self::Offset {
        self.0.end()
    }
}

impl<'file> ariadne::Span for Span<'file> {
    type SourceId = Path;

    fn source(&self) -> &Self::SourceId {
        self.0.context()
    }

    fn start(&self) -> usize {
        self.0.start()
    }

    fn end(&self) -> usize {
        self.0.end()
    }
}
