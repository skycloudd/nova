use crate::{
    span::{Span, Spanned},
    typecheck::TypeInfo,
};
use ariadne::{Color, Fmt};
use chumsky::error::{Rich, RichReason};
use heck::ToSnakeCase;
use std::borrow::Cow;

pub trait Diagnostic<'file> {
    fn message(&self) -> Cow<'_, str>;
    fn spans(&self) -> Vec<Spanned<Option<Cow<'_, str>>>>;
    fn note(&self) -> Option<String>;
}

#[derive(Debug)]
pub enum Error<'file> {
    ExpectedFound {
        expected: Vec<String>,
        found: Option<String>,
        span: Span<'file>,
    },
    Custom {
        message: String,
        span: Span<'file>,
    },
    TypeMismatch {
        expected: TypeInfo,
        found: TypeInfo,
        span: Span<'file>,
    },
    FunctionArgumentCountMismatch {
        expected: usize,
        found: usize,
        expected_span: Span<'file>,
        found_span: Span<'file>,
    },
    UndefinedFunction {
        name: String,
        span: Span<'file>,
    },
}

impl Diagnostic<'_> for Error<'_> {
    fn message(&self) -> Cow<'_, str> {
        match self {
            Error::ExpectedFound {
                expected,
                found,
                span: _,
            } => format!(
                "expected {}, found `{}`",
                if expected.is_empty() {
                    "something else".to_string()
                } else if expected.len() == 1 {
                    format!("`{}`", (&expected[0]).fg(Color::Yellow))
                } else {
                    format!(
                        "one of {}",
                        expected
                            .iter()
                            .map(|e| format!("`{}`", e.fg(Color::Yellow)))
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                },
                found.as_deref().unwrap_or("EOF").fg(Color::Yellow)
            )
            .into(),
            Error::Custom { message, span: _ } => message.into(),
            Error::TypeMismatch {
                expected,
                found,
                span: _,
            } => format!(
                "expected `{}`, found `{}`",
                expected.to_string().fg(Color::Yellow),
                found.to_string().fg(Color::Yellow)
            )
            .into(),
            Error::FunctionArgumentCountMismatch {
                expected,
                found,
                expected_span: _,
                found_span: _,
            } => format!(
                "expected {} arguments, found {}",
                expected.to_string().fg(Color::Yellow),
                found.to_string().fg(Color::Yellow)
            )
            .into(),
            Error::UndefinedFunction { name, span: _ } => {
                format!("undefined function `{}`", name.fg(Color::Yellow)).into()
            }
        }
    }

    fn spans(&self) -> Vec<Spanned<Option<Cow<'_, str>>>> {
        match self {
            Error::ExpectedFound {
                expected,
                found,
                span,
            } => vec![Spanned(
                Some(
                    format!(
                        "expected {}, found `{}`",
                        if expected.is_empty() {
                            "something else".to_string()
                        } else if expected.len() <= 1 {
                            format!("`{}`", (&expected[0]).fg(Color::Yellow))
                        } else {
                            format!(
                                "one of {}",
                                expected
                                    .iter()
                                    .map(|e| format!("`{}`", e.fg(Color::Yellow)))
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            )
                        },
                        found.as_deref().unwrap_or("EOF").fg(Color::Yellow)
                    )
                    .into(),
                ),
                *span,
            )],
            Error::Custom {
                message: _,
                span: _,
            } => vec![],
            Error::TypeMismatch {
                expected,
                found,
                span,
            } => vec![Spanned(
                Some(
                    format!(
                        "expected `{}`, found `{}`",
                        expected.to_string().fg(Color::Yellow),
                        found.to_string().fg(Color::Yellow)
                    )
                    .into(),
                ),
                *span,
            )],
            Error::FunctionArgumentCountMismatch {
                expected,
                found,
                expected_span,
                found_span,
            } => vec![
                Spanned(
                    Some(
                        format!(
                            "expected {} arguments here",
                            expected.to_string().fg(Color::Yellow)
                        )
                        .into(),
                    ),
                    *expected_span,
                ),
                Spanned(
                    Some(
                        format!(
                            "function defined with {} arguments",
                            found.to_string().fg(Color::Yellow)
                        )
                        .into(),
                    ),
                    *found_span,
                ),
            ],
            Error::UndefinedFunction { name, span } => vec![Spanned(
                Some(format!("undefined function `{}`", name.fg(Color::Yellow)).into()),
                *span,
            )],
        }
    }

    fn note(&self) -> Option<String> {
        #[allow(clippy::match_same_arms)]
        match self {
            Error::ExpectedFound { .. } => None,
            Error::Custom { .. } => None,
            Error::TypeMismatch { .. } => None,
            Error::FunctionArgumentCountMismatch { .. } => None,
            Error::UndefinedFunction { .. } => None,
        }
    }
}

#[derive(Debug)]
pub enum Warning<'file> {
    BadName { name: String, span: Span<'file> },
}

impl Diagnostic<'_> for Warning<'_> {
    fn message(&self) -> Cow<'_, str> {
        match self {
            Warning::BadName { name, span: _ } => format!(
                "identifier `{}` should be in snake case",
                name.fg(Color::Yellow)
            )
            .into(),
        }
    }

    fn spans(&self) -> Vec<Spanned<Option<Cow<'_, str>>>> {
        match self {
            Warning::BadName { name, span } => vec![Spanned(
                Some(
                    format!(
                        "consider renaming to `{}`",
                        name.to_snake_case().fg(Color::Yellow)
                    )
                    .into(),
                ),
                *span,
            )],
        }
    }

    fn note(&self) -> Option<String> {
        match self {
            Warning::BadName { name, span: _ } => Some(format!(
                "identifiers should be in snake case, e.g. `{}`",
                name.to_snake_case().fg(Color::Yellow)
            )),
        }
    }
}

pub fn convert<'file>(error: &Rich<'_, String, Span<'file>>) -> Vec<Error<'file>> {
    fn convert_inner<'file>(reason: &RichReason<String>, span: Span<'file>) -> Vec<Error<'file>> {
        match reason {
            RichReason::ExpectedFound { expected, found } => vec![Error::ExpectedFound {
                expected: expected.iter().map(ToString::to_string).collect(),
                found: found.as_ref().map(|f| f.to_string()),
                span,
            }],
            RichReason::Custom(message) => vec![Error::Custom {
                message: message.clone(),
                span,
            }],
            RichReason::Many(reasons) => reasons
                .iter()
                .flat_map(|r| convert_inner(r, span))
                .collect(),
        }
    }

    convert_inner(error.reason(), *error.span())
}
