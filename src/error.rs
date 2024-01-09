use crate::{Span, Spanned};
use ariadne::{Color, Fmt};
use chumsky::error::{Rich, RichReason};
use std::borrow::Cow;

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
            Error::UnknownConstVariable { name, span: _ } => {
                format!("Unknown const variable `{}`", name).into()
            }
            Error::UndefinedVariable { name, span: _ } => {
                format!("Undefined variable `{}`", name).into()
            }
            Error::UnknownType { span: _ } => "Unknown type".into(),
            Error::IncompatibleTypes {
                a,
                a_span: _,
                b,
                b_span: _,
            } => format!("Incompatible types `{}` and `{}`", a, b).into(),
            Error::BinaryOp {
                lhs,
                lhs_span: _,
                rhs,
                rhs_span: _,
                op,
                op_span: _,
            } => format!("Cannot apply `{}` to `{}` and `{}`", op, lhs, rhs).into(),
            Error::UnaryOp {
                ty,
                ty_span: _,
                op,
                op_span: _,
            } => format!("Cannot apply `{}` to `{}`", op, ty).into(),
            Error::ConstAlreadyDefined { name, span: _ } => {
                format!("Const `{}` already defined", name).into()
            }
        }
    }

    pub fn spans(&self) -> Vec<Spanned<Option<Cow<'_, str>>>> {
        match self {
            Error::ExpectedFound {
                expected: _,
                found,
                span,
            } => vec![(
                Some(format!("Found {}", found.as_ref().unwrap_or(&"EOF".into())).into()),
                *span,
            )],
            Error::Custom { message: _, span } => vec![(None, *span)],
            Error::UnknownConstVariable { name, span } => {
                vec![(
                    Some(format!("Unknown const variable `{}`", name).into()),
                    *span,
                )]
            }
            Error::UndefinedVariable { name, span } => {
                vec![(Some(format!("Undefined variable `{}`", name).into()), *span)]
            }
            Error::UnknownType { span } => vec![(Some("Unknown type".into()), *span)],
            Error::IncompatibleTypes {
                a,
                a_span,
                b,
                b_span,
            } => vec![
                (Some(format!("`{}`", a).into()), *a_span),
                (Some(format!("`{}`", b).into()), *b_span),
            ],
            Error::BinaryOp {
                lhs,
                lhs_span,
                rhs,
                rhs_span,
                op,
                op_span,
            } => vec![
                (Some(format!("`{}`", lhs).into()), *lhs_span),
                (Some(format!("`{}`", rhs).into()), *rhs_span),
                (Some(format!("`{}`", op).into()), *op_span),
            ],
            Error::UnaryOp {
                ty,
                ty_span,
                op,
                op_span,
            } => vec![
                (Some(format!("`{}`", ty).into()), *ty_span),
                (Some(format!("`{}`", op).into()), *op_span),
            ],
            Error::ConstAlreadyDefined { name, span } => {
                vec![(
                    Some(format!("Const `{}` already defined", name).into()),
                    *span,
                )]
            }
        }
    }

    pub fn note(&self) -> Option<String> {
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

pub fn convert_error<'file>(error: Rich<'_, String, Span<'file>>) -> Vec<Error<'file>> {
    fn convert<'file>(reason: &RichReason<'_, String>, span: Span<'file>) -> Vec<Error<'file>> {
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
