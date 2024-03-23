use crate::{
    span::{Span, Spanned},
    typecheck::TypeInfo,
};
use chumsky::error::{Rich, RichReason};
use codespan_reporting::diagnostic::Severity;
use heck::ToSnakeCase;
use std::borrow::Cow;

pub trait Diag {
    fn message(&self) -> Cow<'_, str>;
    fn spans(&self) -> Vec<Spanned<Option<Cow<'_, str>>>>;
    fn notes(&self) -> Vec<String>;
    fn kind(&self) -> Severity;
}

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
    TypeMismatch {
        expected: TypeInfo,
        found: TypeInfo,
        span: Span,
    },
    ReturnTypeMismatch {
        expected: TypeInfo,
        found: TypeInfo,
        expected_span: Span,
        found_span: Span,
    },
    FunctionArgumentCountMismatch {
        expected: usize,
        found: usize,
        expected_span: Span,
        found_span: Span,
    },
    UndefinedFunction {
        name: String,
        span: Span,
    },
    MissingReturn {
        span: Span,
    },
}

impl Diag for Error {
    fn message(&self) -> Cow<'_, str> {
        match self {
            Self::ExpectedFound {
                expected,
                found,
                span: _,
            } => format!(
                "expected one of {}, but found {}",
                expected.join(", "),
                found
                    .as_deref()
                    .map(|found| format!("`{found}`"))
                    .unwrap_or("something else".to_string())
            )
            .into(),
            Self::Custom { message, span: _ } => message.into(),
            Self::TypeMismatch {
                expected,
                found,
                span: _,
            } => format!("expected type `{expected}`, but found type `{found}`").into(),
            Self::ReturnTypeMismatch {
                expected,
                found,
                expected_span: _,
                found_span: _,
            } => format!("returning an expression of type `{found}`, but expected `{expected}`")
                .into(),
            Self::FunctionArgumentCountMismatch {
                expected,
                found,
                expected_span: _,
                found_span: _,
            } => format!("expected {expected} function arguments, but found {found}",).into(),
            Self::UndefinedFunction { name, span: _ } => {
                format!("undefined function `{name}`").into()
            }
            Self::MissingReturn { span: _ } => {
                "this function does not return a value on all code paths".into()
            }
        }
    }

    fn spans(&self) -> Vec<Spanned<Option<Cow<'_, str>>>> {
        #[allow(clippy::match_same_arms)]
        match self {
            Self::ExpectedFound {
                expected: _,
                found: _,
                span,
            } => vec![Spanned(Some("unexpected token".into()), *span)],
            Self::Custom { message: _, span } => vec![Spanned(None, *span)],
            Self::TypeMismatch {
                expected,
                found,
                span,
            } => vec![
                Spanned(Some(format!("expected `{expected}`").into()), *span),
                Spanned(Some(format!("found `{found}`").into()), *span),
            ],
            Self::ReturnTypeMismatch {
                expected,
                found,
                expected_span,
                found_span,
            } => vec![
                Spanned(
                    Some(format!("expected `{expected}` because of the return type").into()),
                    *expected_span,
                ),
                Spanned(Some(format!("found `{found}`").into()), *found_span),
            ],
            Self::FunctionArgumentCountMismatch {
                expected,
                found,
                expected_span,
                found_span,
            } => vec![
                Spanned(
                    Some(
                        format!(
                            "expected {} argument{}",
                            expected,
                            if *expected == 1 { "" } else { "s" },
                        )
                        .into(),
                    ),
                    *expected_span,
                ),
                Spanned(
                    Some(
                        format!(
                            "found {} argument{}",
                            found,
                            if *found == 1 { "" } else { "s" }
                        )
                        .into(),
                    ),
                    *found_span,
                ),
            ],
            Self::UndefinedFunction { name, span } => {
                vec![Spanned(Some(format!("`{name}` called here").into()), *span)]
            }
            Self::MissingReturn { span } => vec![Spanned(None, *span)],
        }
    }

    fn notes(&self) -> Vec<String> {
        #[allow(clippy::match_same_arms)]
        match self {
            Self::ExpectedFound { .. } => vec![],
            Self::Custom { .. } => vec![],
            Self::TypeMismatch { .. } => vec![],
            Self::ReturnTypeMismatch { .. } => vec![],
            Self::FunctionArgumentCountMismatch { .. } => vec![],
            Self::UndefinedFunction { .. } => vec![],
            Self::MissingReturn { .. } => {
                vec!["consider adding a return statement at the end of this function".into()]
            }
        }
    }

    fn kind(&self) -> Severity {
        Severity::Error
    }
}

#[derive(Debug)]
pub enum Warning {
    BadName {
        name: String,
        span: Span,
    },
    Lint {
        span: Span,
        message: String,
        note: Option<String>,
    },
}

impl Diag for Warning {
    fn message(&self) -> Cow<'_, str> {
        match self {
            Self::BadName { name, span: _ } => {
                format!("identifier `{name}` should be snake case").into()
            }
            Self::Lint {
                span: _,
                message,
                note: _,
            } => message.into(),
        }
    }

    fn spans(&self) -> Vec<Spanned<Option<Cow<'_, str>>>> {
        #[allow(clippy::match_same_arms)]
        match self {
            Self::BadName { name: _, span } => vec![Spanned(None, *span)],
            Self::Lint {
                span,
                message: _,
                note: _,
            } => vec![Spanned(None, *span)],
        }
    }

    fn notes(&self) -> Vec<String> {
        match self {
            Self::BadName { name, span: _ } => {
                vec![format!("rename it to `{}`", name.to_snake_case())]
            }
            Self::Lint {
                span: _,
                message: _,
                note,
            } => note
                .as_ref()
                .map_or_else(Vec::new, |note| vec![format!("note: {}", note)]),
        }
    }

    fn kind(&self) -> Severity {
        Severity::Warning
    }
}

pub fn convert(error: &Rich<'_, String, Span>) -> Vec<Error> {
    fn convert_inner(reason: &RichReason<String>, span: Span) -> Vec<Error> {
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
