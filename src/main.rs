use ariadne::{ColorGenerator, Label, Report, Source};
use chumsky::prelude::*;
use error::convert_error;
use std::{fmt::Display, fs::read_to_string};

mod error;
mod lexer;
mod parser;

fn main() {
    // let input = read_to_string(std::env::args().nth(1).unwrap()).unwrap();

    let input = "- ";

    run(&input);
}

fn run(input: &str) {
    let mut errors = vec![];

    let tokens = lexer::lexer().parse(input).into_output_errors();

    println!("tokens: {:#?}", tokens);

    errors.extend(map_errors(tokens.1));

    let ast = tokens.0.as_ref().map_or((None, vec![]), |tokens| {
        parser::parser()
            .parse(tokens.spanned((input.len()..input.len()).into()))
            .into_output_errors()
    });

    println!("parsing errors: {:#?}", ast.1);

    errors.extend(map_errors(ast.1));

    if !errors.is_empty() {
        print_errors(input, errors);
    }
}

fn print_errors(input: &str, errors: Vec<error::Error>) {
    for e in errors {
        print_error(input, e);
    }
}

fn print_error(input: &str, error: error::Error) {
    let mut color_generator = ColorGenerator::new();

    let message = error.message();
    // let spans = error.fixed_spans();
    let spans = error.spans();
    let note = error.note();

    let offset = spans.iter().map(|s| s.1.start).min().unwrap_or(0);

    let mut report = Report::build(ariadne::ReportKind::Error, (), offset);

    report.set_message(message);

    for span in spans {
        let label = Label::new(span.1.into_range())
            .with_message(span.0)
            .with_color(color_generator.next());

        report.add_label(label);
    }

    if let Some(note) = note {
        report.set_note(note);
    }

    report.finish().eprint(Source::from(input)).unwrap();
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
