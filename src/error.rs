use crate::span::{Span, Spanned};
use ariadne::{Color, Fmt};
use chumsky::error::{Rich, RichReason};
use heck::ToSnakeCase;
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
    WrongNumberOfActionArguments {
        expected: i32,
        got: usize,
        span: Span<'file>,
    },
    UnknownAction {
        name: String,
        span: Span<'file>,
    },
    ProcedureRedefinition {
        name: String,
        old_span: Span<'file>,
        new_span: Span<'file>,
    },
    UndefinedProcedure {
        name: String,
        span: Span<'file>,
    },
    WrongNumberOfProcedureArguments {
        expected: usize,
        got: usize,
        span: Span<'file>,
    },
    RunFunctionHasArgs {
        got: usize,
        span: Span<'file>,
        run_span: Span<'file>,
    },
    NameWarning {
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
                    format!("found {found}, expected something else").into()
                } else {
                    let expected_string = expected
                        .iter()
                        .map(|expected| expected.fg(Color::Yellow).to_string())
                        .collect::<Vec<_>>()
                        .join(", ");

                    format!(
                        "expected {}{}, found {}",
                        if expected.len() > 1 { "one of " } else { "" },
                        expected_string,
                        found
                    )
                    .into()
                }
            }
            Error::Custom { message, span: _ } => message.into(),
            Error::UndefinedVariable { name, span: _ } => {
                format!("undefined variable `{}`", name.fg(Color::Yellow)).into()
            }
            Error::UnknownType { span: _ } => "unknown type".into(),
            Error::IncompatibleTypes {
                a,
                a_span: _,
                b,
                b_span: _,
            } => format!(
                "incompatible types `{}` and `{}`",
                a.fg(Color::Yellow),
                b.fg(Color::Yellow)
            )
            .into(),
            Error::BinaryOp {
                lhs,
                lhs_span: _,
                rhs,
                rhs_span: _,
                op,
                op_span: _,
            } => format!(
                "cannot apply `{}` to `{}` and `{}`",
                op.fg(Color::Yellow),
                lhs.fg(Color::Yellow),
                rhs.fg(Color::Yellow)
            )
            .into(),
            Error::UnaryOp {
                ty,
                ty_span: _,
                op,
                op_span: _,
            } => format!(
                "cannot apply `{}` to `{}`",
                op.fg(Color::Yellow),
                ty.fg(Color::Yellow)
            )
            .into(),
            Error::WrongNumberOfActionArguments {
                expected,
                got,
                span: _,
            } => format!(
                "expected {} arguments, got {}",
                expected.fg(Color::Yellow),
                got.fg(Color::Yellow)
            )
            .into(),
            Error::UnknownAction { name, span: _ } => format!("unknown action `{name}`").into(),
            Error::ProcedureRedefinition {
                name,
                old_span: _,
                new_span: _,
            } => format!("procedure `{}` redefined", name.fg(Color::Yellow)).into(),
            Error::UndefinedProcedure { name, span: _ } => {
                format!("undefined procedure `{name}`").into()
            }
            Error::WrongNumberOfProcedureArguments {
                expected,
                got,
                span: _,
            } => format!("expected {expected} arguments, got {got}").into(),
            Error::RunFunctionHasArgs {
                got,
                span: _,
                run_span: _,
            } => format!(
                "function in a run statement takes {got} argument{}, but it should not take any",
                if *got == 1 { "" } else { "s" }
            )
            .into(),
            Error::NameWarning { name, span: _ } => {
                format!("identifier `{name}` should have a snake case name").into()
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
            Error::WrongNumberOfActionArguments {
                expected,
                got,
                span,
            } => vec![Spanned(
                Some(format!("Expected {expected} arguments, got {got}").into()),
                *span,
            )],
            Error::UnknownAction { name, span } => {
                vec![Spanned(
                    Some(format!("Unknown action `{name}`").into()),
                    *span,
                )]
            }
            Error::ProcedureRedefinition {
                name,
                old_span,
                new_span,
            } => {
                vec![
                    Spanned(
                        Some(format!("Procedure `{name}` redefined here").into()),
                        *new_span,
                    ),
                    Spanned(Some("Previous definition here".into()), *old_span),
                ]
            }
            Error::UndefinedProcedure { name, span } => {
                vec![Spanned(
                    Some(format!("Undefined procedure `{name}`").into()),
                    *span,
                )]
            }
            Error::WrongNumberOfProcedureArguments {
                expected,
                got,
                span,
            } => vec![Spanned(
                Some(format!("Expected {expected} arguments, got {got}").into()),
                *span,
            )],
            Error::RunFunctionHasArgs {
                got,
                span,
                run_span,
            } => {
                vec![
                    Spanned(
                        Some(
                            format!(
                                "Function takes {got} argument{}",
                                if *got == 1 { "" } else { "s" }
                            )
                            .into(),
                        ),
                        *span,
                    ),
                    Spanned(Some("Run statement here".into()), *run_span),
                ]
            }
            Error::NameWarning { name: _, span } => {
                vec![Spanned(None, *span)]
            }
        }
    }

    pub fn note(&self) -> Option<String> {
        #[allow(clippy::match_same_arms)]
        match self {
            Error::ExpectedFound { .. } => None,
            Error::Custom { .. } => None,
            Error::UndefinedVariable { .. } => None,
            Error::UnknownType { .. } => None,
            Error::IncompatibleTypes { .. } => None,
            Error::BinaryOp { .. } => None,
            Error::UnaryOp { .. } => None,
            Error::WrongNumberOfActionArguments { .. } => None,
            Error::UnknownAction { .. } => None,
            Error::ProcedureRedefinition { .. } => {
                Some("Two procedures cannot share the same name".into())
            }
            Error::UndefinedProcedure { .. } => None,
            Error::WrongNumberOfProcedureArguments { .. } => None,
            Error::RunFunctionHasArgs { .. } => {
                Some("Functions passed to run statements cannot take any arguments. Consider creating a wrapper function.".into())
            }
            Error::NameWarning { name, span:_ } => {
                Some(format!("convert the identifier to a snake case: `{}`", name.to_snake_case()))
            }
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
