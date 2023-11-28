use crate::{Span, Spanned};
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
}

impl Error {
    pub fn message(&self) -> String {
        match self {
            Error::ExpectedFound {
                expected,
                found,
                span: _,
            } => {
                let expected = expected.join(", ");

                let found = found
                    .as_ref()
                    .map(|f| f.to_string())
                    .unwrap_or_else(|| "EOF".into());

                format!("Expected one of {}, found {}", expected, found,)
            }
            Error::Custom { message, span: _ } => message.clone(),
        }
    }

    pub fn spans(&self) -> Vec<Spanned<String>> {
        match self {
            Error::ExpectedFound {
                expected: _,
                found,
                span,
            } => vec![(
                format!("Found {}", found.as_ref().unwrap_or(&"EOF".into())),
                *span,
            )],
            Error::Custom { message: _, span } => vec![(String::new(), *span)],
        }
    }

    pub fn fixed_spans(&self) -> Vec<Spanned<String>> {
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
