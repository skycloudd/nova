use crate::{
    span::{Span, Spanned},
    typecheck::TypeInfo,
};
use chumsky::error::{Rich, RichReason};
use codespan_reporting::diagnostic::Severity;
use heck::ToSnakeCase;

pub trait Diag {
    fn message(&self) -> String;
    fn spans(&self) -> Vec<Spanned<Option<String>>>;
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
    BreakOrContinueOutsideLoop {
        span: Span,
    },
    UndefinedVariable {
        name: String,
        span: Span,
        closest: Option<(String, usize)>,
    },
}

impl Diag for Error {
    fn message(&self) -> String {
        match self {
            Self::ExpectedFound {
                expected,
                found,
                span: _,
            } => format!(
                "expected {}{}, but found {}",
                if expected.len() == 1 { "" } else { "one of " },
                expected.join(", "),
                found
                    .as_deref()
                    .map(|found| format!("`{found}`"))
                    .unwrap_or("something else".to_string())
            ),
            Self::Custom { message, span: _ } => message.clone(),
            Self::TypeMismatch {
                expected,
                found,
                span: _,
            } => format!("expected type `{expected}`, but found type `{found}`"),
            Self::ReturnTypeMismatch {
                expected,
                found,
                expected_span: _,
                found_span: _,
            } => format!("returning an expression of type `{found}`, but expected `{expected}`"),
            Self::FunctionArgumentCountMismatch {
                expected,
                found,
                expected_span: _,
                found_span: _,
            } => format!("expected {expected} function arguments, but found {found}"),
            Self::UndefinedFunction { name, span: _ } => format!("undefined function `{name}`"),
            Self::MissingReturn { span: _ } => {
                "this function does not return a value on all code paths".to_string()
            }
            Self::BreakOrContinueOutsideLoop { span: _ } => {
                "a `break` or `continue` statement was used outside of a loop".to_string()
            }
            Self::UndefinedVariable {
                name,
                span: _,
                closest: _,
            } => {
                format!("no variable `{name}` was found in this scope")
            }
        }
    }

    fn spans(&self) -> Vec<Spanned<Option<String>>> {
        #[allow(clippy::match_same_arms)]
        match self {
            Self::ExpectedFound {
                expected: _,
                found: _,
                span,
            } => vec![Spanned(Some("unexpected token".to_string()), *span)],
            Self::Custom { message: _, span } => vec![Spanned(None, *span)],
            Self::TypeMismatch {
                expected,
                found,
                span,
            } => vec![
                Spanned(Some(format!("expected `{expected}`")), *span),
                Spanned(Some(format!("found `{found}`")), *span),
            ],
            Self::ReturnTypeMismatch {
                expected,
                found,
                expected_span,
                found_span,
            } => vec![
                Spanned(
                    Some(format!("expected `{expected}` because of the return type")),
                    *expected_span,
                ),
                Spanned(Some(format!("found `{found}`")), *found_span),
            ],
            Self::FunctionArgumentCountMismatch {
                expected,
                found,
                expected_span,
                found_span,
            } => vec![
                Spanned(
                    Some(format!(
                        "expected {} argument{}",
                        expected,
                        if *expected == 1 { "" } else { "s" },
                    )),
                    *expected_span,
                ),
                Spanned(
                    Some(format!(
                        "found {} argument{}",
                        found,
                        if *found == 1 { "" } else { "s" }
                    )),
                    *found_span,
                ),
            ],
            Self::UndefinedFunction { name, span } => {
                vec![Spanned(Some(format!("`{name}` called here")), *span)]
            }
            Self::MissingReturn { span } => vec![Spanned(None, *span)],
            Self::BreakOrContinueOutsideLoop { span } => vec![Spanned(None, *span)],
            Self::UndefinedVariable {
                name,
                span,
                closest: _,
            } => {
                vec![Spanned(Some(format!("`{name}` used here")), *span)]
            }
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
            Self::BreakOrContinueOutsideLoop { .. } => {
                vec!["you can only use this inside of a loop".into()]
            }
            Self::UndefinedVariable { closest, .. } => closest
                .as_ref()
                .filter(|(_, dist)| *dist <= 3)
                .map_or_else(Vec::new, |(closest, _)| {
                    vec![format!("did you mean `{}`?", closest)]
                }),
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
    fn message(&self) -> String {
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

    fn spans(&self) -> Vec<Spanned<Option<String>>> {
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
