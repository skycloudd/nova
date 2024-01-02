#![forbid(unsafe_code)]

use ariadne::{ColorGenerator, Label, Report, Source};
use chumsky::prelude::*;
use error::convert_error;
use std::{fmt::Display, fs::read_to_string, path::PathBuf};

mod ast;
mod const_eval;
mod error;
mod lexer;
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
    #[clap(short = 'c', long)]
    typechecked_ast: bool,
}

fn main() -> std::io::Result<()> {
    let args = <Args as clap::Parser>::parse();

    let input = read_to_string(&args.filename)?;

    run(&input, &args)
}

fn run(input: &str, args: &Args) -> std::io::Result<()> {
    let mut errors = vec![];

    let (tokens, lex_errors) = lexer::lexer().parse(input).into_output_errors();

    errors.extend(map_errors(lex_errors));

    if args.tokens {
        if let Some(tokens) = &tokens {
            println!("{:#?}", tokens);
        }
    }

    let (ast, parse_errors) = tokens.as_ref().map_or((None, vec![]), |tokens| {
        parser::parser()
            .parse(tokens.spanned((input.len()..input.len()).into()))
            .into_output_errors()
    });

    errors.extend(map_errors(parse_errors));

    if args.ast {
        if let Some(ast) = &ast {
            println!("{:#?}", ast);
        }
    }

    let (mut typed_ast, type_errors) = ast
        .as_ref()
        .map_or((None, vec![]), |ast| typecheck::typecheck(ast));

    errors.extend(type_errors);

    if args.typechecked_ast {
        if let Some(typed_ast) = &typed_ast {
            println!("{:#?}", typed_ast);
        }
    }

    if let Some(typed_ast) = &mut typed_ast {
        if let Err(errs) = const_eval::const_eval(typed_ast) {
            errors.extend(errs);
        }
    }

    if errors.is_empty() {
        if let Some(typed_ast) = typed_ast {
            println!("{:#?}", typed_ast);
        }
    } else {
        for error in errors {
            print_error(input, error)?;
        }
    }

    Ok(())
}

fn print_error(input: &str, error: error::Error) -> std::io::Result<()> {
    let mut color_generator = ColorGenerator::new();

    let message = error.message();
    let spans = error.spans();
    let note = error.note();

    let offset = spans.iter().map(|s| s.1.start).min().unwrap_or(0);

    let mut report = Report::build(ariadne::ReportKind::Error, (), offset);

    report.set_message(message);

    for span in spans {
        let mut label = Label::new(span.1.into_range()).with_color(color_generator.next());

        if let Some(message) = span.0 {
            label = label.with_message(message);
        }

        report.add_label(label);
    }

    if let Some(note) = note {
        report.set_note(note);
    }

    report.finish().eprint(Source::from(input))
}

fn map_errors<T: Clone + Display>(errors: Vec<Rich<'_, T, Span>>) -> Vec<error::Error> {
    errors
        .into_iter()
        .map(|e| e.map_token(|t| t.to_string()))
        .flat_map(convert_error)
        .collect()
}

pub type Span = SimpleSpan<usize>;
pub type Spanned<T> = (T, Span);
