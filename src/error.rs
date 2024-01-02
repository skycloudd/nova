use crate::{Span, Spanned};
use ariadne::{Color, Fmt};
use chumsky::error::{Rich, RichReason};
use std::borrow::Cow;

#[derive(Debug)]
pub enum Error {
    ExpectedFound {
        expected: Vec<String>,
        found: Option<String>,
        span: Span,
    },
    Custom {
        message: String,
        span: Span,
    },
}

impl Error {
    pub fn message(&self) -> Cow<'_, str> {
        match self {
            Error::ExpectedFound {
                expected,
                found,
                span: _,
            } => {
                let found = found
                    .as_ref()
                    .map(|f| f.to_string())
                    .unwrap_or("end of input".into())
                    .fg(Color::Yellow);

                if expected.is_empty() {
                    format!("Found {}, expected something else", found).into()
                } else {
                    let expected_string = expected
                        .iter()
                        .map(|expected| expected.fg(Color::Yellow).to_string())
                        .collect::<Vec<_>>()
                        .join(", ");

                    format!(
                        "Expected {}{}, found {}",
                        if expected.len() > 1 { "one of " } else { "" },
                        expected_string,
                        found
                    )
                    .into()
                }
            }
            Error::Custom { message, span: _ } => message.into(),
        }
    }

    pub fn spans(&self) -> Vec<Spanned<Option<Cow<'_, str>>>> {
        let spans = match self {
            Error::ExpectedFound {
                expected: _,
                found,
                span,
            } => vec![(
                Some(Cow::from(format!(
                    "Found {}",
                    found.as_ref().unwrap_or(&"EOF".into())
                ))),
                *span,
            )],
            Error::Custom { message: _, span } => vec![(None, *span)],
        };

        spans
            .into_iter()
            .map(|(s, span)| {
                let start = span.start;
                let end = span.end;

                (s, Span::new(start, end))
            })
            .collect()
    }

    pub fn note(&self) -> Option<String> {
        match self {
            Error::ExpectedFound { .. } => None,
            Error::Custom { .. } => None,
        }
    }
}

pub fn convert_error(error: Rich<'_, String, Span>) -> Vec<Error> {
    fn convert(reason: &RichReason<'_, String>, span: Span) -> Vec<Error> {
        match reason {
            RichReason::ExpectedFound { expected, found } => {
                let expected = expected.iter().map(|e| e.to_string()).collect();

                let found = found.as_ref().map(|f| f.to_string());

                vec![Error::ExpectedFound {
                    expected,
                    found,
                    span,
                }]
            }
            RichReason::Custom(message) => vec![Error::Custom {
                message: message.to_string(),
                span,
            }],
            RichReason::Many(reasons) => reasons.iter().flat_map(|r| convert(r, span)).collect(),
        }
    }

    convert(error.reason(), *error.span())
}
