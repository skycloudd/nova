use crate::{
    ast::{BinaryOp, UnaryOp},
    span::Span,
    typecheck::TypeInfo,
    IntTy,
};
use chumsky::error::{Rich, RichReason};
use codespan_reporting::diagnostic::Severity;
use heck::ToSnakeCase;

pub trait Diag {
    fn message(&self) -> String;
    fn spans(&self) -> Vec<ErrorSpan>;
    fn notes(&self) -> Vec<String>;
    fn kind(&self) -> Severity;
}

#[derive(Debug)]
#[allow(clippy::module_name_repetitions)]
pub enum ErrorSpan {
    Primary(Option<String>, Span),
    Secondary(Option<String>, Span),
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
        closest: Option<(String, usize)>,
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
    IntegerArithmetic {
        name: String,
        effect: String,
        result: Option<IntTy>,
        span: Span,
    },
    FunctionAlreadyDefined {
        name: String,
        span: Span,
        already_defined_span: Span,
    },
    InvalidBinaryOperation {
        lhs: TypeInfo,
        lhs_span: Span,
        rhs: TypeInfo,
        rhs_span: Span,
        op: BinaryOp,
        op_span: Span,
        full_span: Span,
    },
    InvalidUnaryOperation {
        ty: TypeInfo,
        ty_span: Span,
        op: UnaryOp,
        op_span: Span,
        full_span: Span,
    },
    InvalidConversion {
        from: TypeInfo,
        from_span: Span,
        into: TypeInfo,
        into_span: Span,
        full_span: Span,
    },
    CantInferType {
        span: Span,
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
                found.as_deref().map_or_else(
                    || "something else".to_string(),
                    |found| format!("`{found}`")
                )
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
            Self::UndefinedFunction {
                name,
                span: _,
                closest: _,
            } => format!("the function `{name}` does not exist"),
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
            Self::IntegerArithmetic {
                name,
                effect,
                result: _,
                span: _,
            } => {
                format!("this {name} unconditionally results in {effect}")
            }
            Self::FunctionAlreadyDefined {
                name,
                span: _,
                already_defined_span: _,
            } => {
                format!("a function called `{name}` has already been defined in this file")
            }
            Self::InvalidBinaryOperation {
                lhs,
                lhs_span: _,
                rhs,
                rhs_span: _,
                op,
                op_span: _,
                full_span: _,
            } => {
                format!("invalid binary operation `{op}` between types `{lhs}` and `{rhs}`")
            }
            Self::InvalidUnaryOperation {
                ty,
                ty_span: _,
                op,
                op_span: _,
                full_span: _,
            } => {
                format!("invalid unary operator `{op}` for type `{ty}`")
            }
            Self::InvalidConversion {
                from,
                from_span: _,
                into,
                into_span: _,
                full_span: _,
            } => {
                format!("cannot convert an expression of type `{from}` to `{into}`")
            }
            Self::CantInferType { span: _ } => {
                "cannot infer the type of this expression".to_string()
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    fn spans(&self) -> Vec<ErrorSpan> {
        #[allow(clippy::match_same_arms)]
        match self {
            Self::ExpectedFound {
                expected: _,
                found: _,
                span,
            } => vec![ErrorSpan::Primary(
                Some("unexpected token".to_string()),
                *span,
            )],
            Self::Custom { message: _, span } => vec![ErrorSpan::Primary(None, *span)],
            Self::TypeMismatch {
                expected,
                found,
                span,
            } => vec![
                ErrorSpan::Primary(Some(format!("expected `{expected}`")), *span),
                ErrorSpan::Primary(Some(format!("found `{found}`")), *span),
            ],
            Self::ReturnTypeMismatch {
                expected,
                found,
                expected_span,
                found_span,
            } => vec![
                ErrorSpan::Secondary(
                    Some(format!("expected `{expected}` because of the return type")),
                    *expected_span,
                ),
                ErrorSpan::Primary(Some(format!("found `{found}`")), *found_span),
            ],
            Self::FunctionArgumentCountMismatch {
                expected,
                found,
                expected_span,
                found_span,
            } => vec![
                ErrorSpan::Primary(
                    Some(format!(
                        "expected {} argument{}",
                        expected,
                        if *expected == 1 { "" } else { "s" },
                    )),
                    *expected_span,
                ),
                ErrorSpan::Primary(
                    Some(format!(
                        "found {} argument{}",
                        found,
                        if *found == 1 { "" } else { "s" }
                    )),
                    *found_span,
                ),
            ],
            Self::UndefinedFunction {
                name,
                span,
                closest: _,
            } => {
                vec![ErrorSpan::Primary(
                    Some(format!("`{name}` called here")),
                    *span,
                )]
            }
            Self::MissingReturn { span } => vec![ErrorSpan::Primary(None, *span)],
            Self::BreakOrContinueOutsideLoop { span } => vec![ErrorSpan::Primary(None, *span)],
            Self::UndefinedVariable {
                name,
                span,
                closest: _,
            } => {
                vec![ErrorSpan::Primary(
                    Some(format!("`{name}` used here")),
                    *span,
                )]
            }
            Self::IntegerArithmetic {
                name,
                effect: _,
                result: _,
                span,
            } => {
                vec![ErrorSpan::Primary(Some(format!("{name} here")), *span)]
            }
            Self::FunctionAlreadyDefined {
                name,
                span,
                already_defined_span,
            } => {
                vec![
                    ErrorSpan::Secondary(
                        Some("first definition here".to_string()),
                        *already_defined_span,
                    ),
                    ErrorSpan::Primary(Some(format!("`{name}` redefined here")), *span),
                ]
            }
            Self::InvalidBinaryOperation {
                lhs,
                lhs_span,
                rhs,
                rhs_span,
                op: _,
                op_span,
                full_span: _,
            } => vec![
                ErrorSpan::Primary(Some(format!("`{lhs}`")), *lhs_span),
                ErrorSpan::Primary(Some(format!("`{rhs}`")), *rhs_span),
                ErrorSpan::Secondary(None, *op_span),
            ],
            Self::InvalidUnaryOperation {
                ty,
                ty_span,
                op,
                op_span,
                full_span: _,
            } => vec![
                ErrorSpan::Primary(Some(format!("expression of type `{ty}`")), *ty_span),
                ErrorSpan::Secondary(Some(format!("operator `{op}`")), *op_span),
            ],
            Self::InvalidConversion {
                from,
                from_span,
                into,
                into_span,
                full_span: _,
            } => vec![
                ErrorSpan::Primary(Some(format!("converting from `{from}`")), *from_span),
                ErrorSpan::Primary(Some(format!("into type `{into}`")), *into_span),
            ],
            Self::CantInferType { span } => vec![ErrorSpan::Primary(None, *span)],
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
            Self::UndefinedFunction { closest, .. } => closest
                .as_ref()
                .filter(|(_, dist)| *dist <= 3)
                .map_or_else(Vec::new, |(closest, _)| {
                    vec![format!("help: did you mean `{}`?", closest)]
                }),
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
                    vec![format!("help: did you mean `{}`?", closest)]
                }),
            Self::IntegerArithmetic { result, .. } => result.map_or_else(Vec::new, |result| {
                vec![format!(
                    "replace it with `{}` if this was intended, but it likely wasn't",
                    result
                )]
            }),
            Self::FunctionAlreadyDefined { .. } => vec![
                "functions cannot be redefined".into(),
                "consider removing one of these function definitions".into(),
            ],
            Self::InvalidBinaryOperation { lhs, rhs, .. } => {
                if lhs == rhs {
                    vec!["this operation is not valid for these types".into()]
                } else {
                    vec!["both expressions must be of the same type".into()]
                }
            }
            Self::InvalidUnaryOperation { .. } => vec![],
            Self::InvalidConversion { .. } => vec![],
            Self::CantInferType { .. } => vec![],
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
            Self::BadName { name, span: _ } => format!("identifier `{name}` should be snake case"),
            Self::Lint {
                span: _,
                message,
                note: _,
            } => message.into(),
        }
    }

    fn spans(&self) -> Vec<ErrorSpan> {
        #[allow(clippy::match_same_arms)]
        match self {
            Self::BadName { name: _, span } => vec![ErrorSpan::Primary(None, *span)],
            Self::Lint {
                span,
                message: _,
                note: _,
            } => vec![ErrorSpan::Primary(None, *span)],
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
