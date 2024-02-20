use crate::{
    ast::Action,
    span::{Span, Spanned},
};
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
        def_span: Span<'file>,
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
    ExpectedTypeFound {
        found: String,
        found_span: Span<'file>,
        expected: String,
        expected_span: Span<'file>,
    },
    InvalidConversion {
        from: String,
        from_span: Span<'file>,
        to: String,
        to_span: Span<'file>,
    },
    ExpectedOneOfTypeFound {
        found: String,
        found_span: Span<'file>,
        expected: Vec<String>,
        expected_span: Span<'file>,
        action: Option<Action>,
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
                def_span: _,
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
            Error::ExpectedTypeFound {
                found,
                found_span: _,
                expected,
                expected_span: _,
            } => format!(
                "found type `{}`, expected `{}`",
                found.fg(Color::Yellow),
                expected.fg(Color::Yellow),
            )
            .into(),
            Error::InvalidConversion {
                from,
                from_span: _,
                to,
                to_span: _,
            } => format!(
                "cannot convert `{}` to `{}`",
                from.fg(Color::Yellow),
                to.fg(Color::Yellow)
            )
            .into(),
            Error::ExpectedOneOfTypeFound {
                found,
                found_span: _,
                expected,
                expected_span: _,
                action: _,
            } => if expected.len() == 1 {
                format!(
                    "found type `{}`, expected `{}`",
                    found.fg(Color::Yellow),
                    (&expected[0]).fg(Color::Yellow)
                )
                .into()
            } else {
                format!(
                    "found type `{}`, expected one of: {}",
                    found.fg(Color::Yellow),
                    expected
                        .iter()
                        .map(|expected| format!("`{}`", expected.fg(Color::Yellow)))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            .into(),
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
                Spanned(
                    Some(format!("found `{}` here", a.fg(Color::Yellow)).into()),
                    *a_span,
                ),
                Spanned(
                    Some(format!("found `{}` here", b.fg(Color::Yellow)).into()),
                    *b_span,
                ),
            ],
            Error::BinaryOp {
                lhs,
                lhs_span,
                rhs,
                rhs_span,
                op: _,
                op_span: _,
            } => vec![
                Spanned(
                    Some(format!("found `{}` here", lhs.fg(Color::Yellow)).into()),
                    *lhs_span,
                ),
                Spanned(
                    Some(format!("found `{}` here", rhs.fg(Color::Yellow)).into()),
                    *rhs_span,
                ),
            ],
            Error::UnaryOp {
                ty,
                ty_span,
                op: _,
                op_span: _,
            } => vec![Spanned(
                Some(format!("found `{}` here", ty.fg(Color::Yellow)).into()),
                *ty_span,
            )],
            Error::WrongNumberOfActionArguments {
                expected,
                got,
                span,
            } => vec![Spanned(
                Some(format!("Expected {expected} arguments, got {got}").into()),
                *span,
            )],
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
                def_span,
            } => vec![
                Spanned(
                    Some(format!("Expected {expected} arguments, got {got}").into()),
                    *span,
                ),
                Spanned(Some("Procedure definition here".into()), *def_span),
            ],
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
            Error::ExpectedTypeFound {
                found,
                found_span,
                expected,
                expected_span,
            } => vec![
                Spanned(Some(format!("Expected {expected}").into()), *expected_span),
                Spanned(Some(format!("Found {found}").into()), *found_span),
            ],
            Error::InvalidConversion {
                from,
                from_span,
                to,
                to_span,
            } => vec![
                Spanned(Some(format!("`{from}` here").into()), *from_span),
                Spanned(Some(format!("`{to}` here").into()), *to_span),
            ],
            Error::ExpectedOneOfTypeFound {
                found,
                found_span: _,
                expected,
                expected_span,
                action: _,
            } => vec![
                Spanned(
                    Some(format!("Found type `{found}` here").into()),
                    *expected_span,
                ),
                Spanned(
                    Some(if expected.len() == 1 {
                        format!("Expected a value of type `{}`", expected[0]).into()
                    } else {
                        format!(
                            "Expected one of: {}",
                            expected
                                .iter()
                                .map(|expected| format!("`{expected}`"))
                                .collect::<Vec<_>>()
                                .join(", ")
                        )
                        .into()
                    }),
                    *expected_span,
                ),
            ],
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
            Error::ProcedureRedefinition { .. } => {
                Some("Two procedures cannot share the same name".into())
            }
            Error::UndefinedProcedure { .. } => None,
            Error::WrongNumberOfProcedureArguments { .. } => None,
            Error::RunFunctionHasArgs { .. } => Some(
                "Functions passed to run statements cannot take any arguments. \
                Consider creating a wrapper function."
                    .into(),
            ),
            Error::NameWarning { name, span: _ } => Some(format!(
                "convert the identifier to a snake case: `{}`",
                name.to_snake_case().fg(Color::Red)
            )),
            Error::ExpectedTypeFound { .. } => None,
            Error::InvalidConversion { .. } => None,
            Error::ExpectedOneOfTypeFound {
                found,
                found_span: _,
                expected,
                expected_span: _,
                action: _,
            } => {
                if expected.len() == 1
                    && expected[0] == "str"
                    && (found == "int" || found == "float")
                {
                    Some(format!(
                        "Try wrapping this value in a converter: `{}`",
                        "@str(...)".fg(Color::Red)
                    ))
                } else {
                    None
                }
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
