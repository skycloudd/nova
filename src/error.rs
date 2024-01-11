use crate::{Span, Spanned};
use ariadne::{Color, Fmt};
use chumsky::error::{Rich, RichReason};
use std::borrow::Cow;

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
    UnknownConstVariable {
        name: String,
        span: Span<'file>,
    },
    UndefinedVariable {
        name: String,
        span: Span<'file>,
    },
    UnknownType {
        span: Span<'file>,
    },
    IncompatibleTypes {
        a: String,
        a_span: Span<'file>,
        b: String,
        b_span: Span<'file>,
    },
    BinaryOp {
        lhs: String,
        lhs_span: Span<'file>,
        rhs: String,
        rhs_span: Span<'file>,
        op: String,
        op_span: Span<'file>,
    },
    UnaryOp {
        ty: String,
        ty_span: Span<'file>,
        op: String,
        op_span: Span<'file>,
    },
    ConstAlreadyDefined {
        name: String,
        span: Span<'file>,
    },
}

impl Error<'_> {
    pub fn message(&self) -> Cow<'_, str> {
        match self {
            Error::ExpectedFound {
                expected,
                found,
                span: _,
            } => {
                let found = found
                    .as_ref()
                    .map_or("end of input".into(), ToString::to_string)
                    .fg(Color::Yellow);

                if expected.is_empty() {
                    format!("Found {found}, expected something else").into()
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
            Error::UnknownConstVariable { name, span: _ } => {
                format!("Unknown const variable `{name}`").into()
            }
            Error::UndefinedVariable { name, span: _ } => {
                format!("Undefined variable `{name}`").into()
            }
            Error::UnknownType { span: _ } => "Unknown type".into(),
            Error::IncompatibleTypes {
                a,
                a_span: _,
                b,
                b_span: _,
            } => format!("Incompatible types `{a}` and `{b}`").into(),
            Error::BinaryOp {
                lhs,
                lhs_span: _,
                rhs,
                rhs_span: _,
                op,
                op_span: _,
            } => format!("Cannot apply `{op}` to `{lhs}` and `{rhs}`").into(),
            Error::UnaryOp {
                ty,
                ty_span: _,
                op,
                op_span: _,
            } => format!("Cannot apply `{op}` to `{ty}`").into(),
            Error::ConstAlreadyDefined { name, span: _ } => {
                format!("Const `{name}` already defined").into()
            }
        }
    }

    pub fn spans(&self) -> Vec<Spanned<Option<Cow<'_, str>>>> {
        match self {
            Error::ExpectedFound {
                expected: _,
                found,
                span,
            } => vec![Spanned(
                Some(format!("Found {}", found.as_ref().unwrap_or(&"EOF".into())).into()),
                *span,
            )],
            Error::Custom { message: _, span } => vec![Spanned(None, *span)],
            Error::UnknownConstVariable { name, span } => {
                vec![Spanned(
                    Some(format!("Unknown const variable `{name}`").into()),
                    *span,
                )]
            }
            Error::UndefinedVariable { name, span } => {
                vec![Spanned(
                    Some(format!("Undefined variable `{name}`").into()),
                    *span,
                )]
            }
            Error::UnknownType { span } => vec![Spanned(Some("Unknown type".into()), *span)],
            Error::IncompatibleTypes {
                a,
                a_span,
                b,
                b_span,
            } => vec![
                Spanned(Some(format!("`{a}`").into()), *a_span),
                Spanned(Some(format!("`{b}`").into()), *b_span),
            ],
            Error::BinaryOp {
                lhs,
                lhs_span,
                rhs,
                rhs_span,
                op,
                op_span,
            } => vec![
                Spanned(Some(format!("`{lhs}`").into()), *lhs_span),
                Spanned(Some(format!("`{rhs}`").into()), *rhs_span),
                Spanned(Some(format!("`{op}`").into()), *op_span),
            ],
            Error::UnaryOp {
                ty,
                ty_span,
                op,
                op_span,
            } => vec![
                Spanned(Some(format!("`{ty}`").into()), *ty_span),
                Spanned(Some(format!("`{op}`").into()), *op_span),
            ],
            Error::ConstAlreadyDefined { name, span } => {
                vec![Spanned(
                    Some(format!("Const `{name}` already defined").into()),
                    *span,
                )]
            }
        }
    }

    pub const fn note(&self) -> Option<String> {
        #[allow(clippy::match_same_arms)]
        match self {
            Error::ExpectedFound { .. } => None,
            Error::Custom { .. } => None,
            Error::UnknownConstVariable { .. } => None,
            Error::UndefinedVariable { .. } => None,
            Error::UnknownType { .. } => None,
            Error::IncompatibleTypes { .. } => None,
            Error::BinaryOp { .. } => None,
            Error::UnaryOp { .. } => None,
            Error::ConstAlreadyDefined { .. } => None,
        }
    }
}

pub fn convert<'file>(error: &Rich<'_, String, Span<'file>>) -> Vec<Error<'file>> {
    fn convert_inner<'file>(
        reason: &RichReason<'_, String>,
        span: Span<'file>,
    ) -> Vec<Error<'file>> {
        match reason {
            RichReason::ExpectedFound { expected, found } => {
                let expected = expected.iter().map(ToString::to_string).collect();

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
            RichReason::Many(reasons) => reasons
                .iter()
                .flat_map(|r| convert_inner(r, span))
                .collect(),
        }
    }

    convert_inner(error.reason(), *error.span())
}
