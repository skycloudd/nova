use crate::{Span, Spanned};
use ariadne::{Color, Fmt};
use chumsky::error::{Rich, RichReason};

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
    BinaryExpressionTypeMismatch {
        op: String,
        left: Spanned<String>,
        right: Spanned<String>,
    },
    UnaryExpressionTypeMismatch {
        op: String,
        operand: Spanned<String>,
    },
}

impl Error {
    pub fn message(&self) -> String {
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
                    format!("Found {}, expected something else", found)
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
                }
            }
            Error::Custom { message, span: _ } => message.clone(),
            Error::BinaryExpressionTypeMismatch { op, left, right } => format!(
                "Cannot apply operator {} to {} and {}",
                op,
                left.0.clone().fg(Color::Yellow),
                right.0.clone().fg(Color::Yellow)
            ),
            Error::UnaryExpressionTypeMismatch { op, operand } => format!(
                "Cannot apply operator {} to {}",
                op,
                operand.0.clone().fg(Color::Yellow)
            ),
        }
    }

    pub fn spans(&self) -> Vec<Spanned<String>> {
        let spans = match self {
            Error::ExpectedFound {
                expected: _,
                found,
                span,
            } => vec![(
                format!("Found {}", found.as_ref().unwrap_or(&"EOF".into())),
                *span,
            )],
            Error::Custom { message: _, span } => vec![(String::new(), *span)],
            Error::BinaryExpressionTypeMismatch { op: _, left, right } => vec![
                (format!("{}", left.0.clone()), left.1),
                (format!("{}", right.0.clone()), right.1),
            ],
            Error::UnaryExpressionTypeMismatch { op, operand } => vec![(
                format!("Cannot apply operator {} to {}", op, operand.0.clone()),
                operand.1,
            )],
        };

        spans
            .into_iter()
            .map(|(s, span)| {
                let start = span.start;
                let end = span.end;

                if start > end {
                    (s, Span::new(end, start))
                } else {
                    (s, span)
                }
            })
            .collect()
    }

    pub fn note(&self) -> Option<String> {
        match self {
            Error::ExpectedFound { .. } => None,
            Error::Custom { .. } => None,
            Error::BinaryExpressionTypeMismatch { .. } => None,
            Error::UnaryExpressionTypeMismatch { .. } => None,
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
