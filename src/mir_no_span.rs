use crate::{mir, Spanned};

#[derive(Debug)]
pub enum TypedStatement<'src> {
    Expr(TypedExpression<'src>),
    BuiltinPrint(TypedExpression<'src>),
    Loop(Vec<TypedStatement<'src>>),
    If {
        condition: TypedExpression<'src>,
        then_branch: Vec<TypedStatement<'src>>,
        else_branch: Option<Vec<TypedStatement<'src>>>,
    },
    Let {
        name: &'src str,
        value: TypedExpression<'src>,
    },
    Const {
        name: &'src str,
        value: TypedExpression<'src>,
    },
    Assign {
        name: &'src str,
        value: TypedExpression<'src>,
    },
    Break,
    Continue,
}

#[derive(Debug)]
pub struct TypedExpression<'src> {
    pub expr: Expression<'src>,
    pub ty: Type,
}

#[derive(Debug)]
pub enum Expression<'src> {
    Variable(&'src str),
    Boolean(bool),
    Integer(i32),
    Float(f32),
    Colour {
        r: u8,
        g: u8,
        b: u8,
    },
    Vector {
        x: Box<TypedExpression<'src>>,
        y: Box<TypedExpression<'src>>,
    },
    Operation(Box<Operation<'src>>),
}

#[derive(Debug)]
pub enum Operation<'src> {
    IntegerEquals(TypedExpression<'src>, TypedExpression<'src>),
    IntegerNotEquals(TypedExpression<'src>, TypedExpression<'src>),
    IntegerPlus(TypedExpression<'src>, TypedExpression<'src>),
    IntegerMinus(TypedExpression<'src>, TypedExpression<'src>),
    IntegerMultiply(TypedExpression<'src>, TypedExpression<'src>),
    IntegerDivide(TypedExpression<'src>, TypedExpression<'src>),
    IntegerGreaterThanEquals(TypedExpression<'src>, TypedExpression<'src>),
    IntegerLessThanEquals(TypedExpression<'src>, TypedExpression<'src>),
    IntegerGreaterThan(TypedExpression<'src>, TypedExpression<'src>),
    IntegerLessThan(TypedExpression<'src>, TypedExpression<'src>),

    FloatEquals(TypedExpression<'src>, TypedExpression<'src>),
    FloatNotEquals(TypedExpression<'src>, TypedExpression<'src>),
    FloatPlus(TypedExpression<'src>, TypedExpression<'src>),
    FloatMinus(TypedExpression<'src>, TypedExpression<'src>),
    FloatMultiply(TypedExpression<'src>, TypedExpression<'src>),
    FloatDivide(TypedExpression<'src>, TypedExpression<'src>),
    FloatGreaterThanEquals(TypedExpression<'src>, TypedExpression<'src>),
    FloatLessThanEquals(TypedExpression<'src>, TypedExpression<'src>),
    FloatGreaterThan(TypedExpression<'src>, TypedExpression<'src>),
    FloatLessThan(TypedExpression<'src>, TypedExpression<'src>),

    BooleanEquals(TypedExpression<'src>, TypedExpression<'src>),
    BooleanNotEquals(TypedExpression<'src>, TypedExpression<'src>),

    IntegerNegate(TypedExpression<'src>),
    FloatNegate(TypedExpression<'src>),
    BooleanNot(TypedExpression<'src>),
}

#[derive(Debug)]
pub enum Type {
    Boolean,
    Integer,
    Float,
    Colour,
    Vector,
}

impl From<mir::Type> for Type {
    fn from(ty: mir::Type) -> Self {
        match ty {
            mir::Type::Boolean => Self::Boolean,
            mir::Type::Integer => Self::Integer,
            mir::Type::Float => Self::Float,
            mir::Type::Colour => Self::Colour,
            mir::Type::Vector => Self::Vector,
        }
    }
}

pub fn mir_remove_span<'src>(
    mir: Vec<Spanned<mir::TypedStatement<'src, '_>>>,
) -> Vec<TypedStatement<'src>> {
    mir.into_iter().map(|s| statement_remove_span(s)).collect()
}

fn statement_remove_span<'src>(
    statement: Spanned<mir::TypedStatement<'src, '_>>,
) -> TypedStatement<'src> {
    match statement.0 {
        mir::TypedStatement::Expr(expr) => TypedStatement::Expr(expression_remove_span(expr)),
        mir::TypedStatement::BuiltinPrint(expr) => {
            TypedStatement::BuiltinPrint(expression_remove_span(expr))
        }
        mir::TypedStatement::Loop(statements) => TypedStatement::Loop(
            statements
                .0
                .into_iter()
                .map(|s| statement_remove_span(s))
                .collect(),
        ),
        mir::TypedStatement::If {
            condition,
            then_branch,
            else_branch,
        } => TypedStatement::If {
            condition: expression_remove_span(condition),
            then_branch: then_branch
                .0
                .into_iter()
                .map(|s| statement_remove_span(s))
                .collect(),
            else_branch: else_branch.map(|statements| {
                statements
                    .0
                    .into_iter()
                    .map(|s| statement_remove_span(s))
                    .collect()
            }),
        },
        mir::TypedStatement::Let { name, value } => TypedStatement::Let {
            name: name.0,
            value: expression_remove_span(value),
        },
        mir::TypedStatement::Const { name, value } => TypedStatement::Const {
            name: name.0,
            value: expression_remove_span(value),
        },
        mir::TypedStatement::Assign { name, value } => TypedStatement::Assign {
            name: name.0,
            value: expression_remove_span(value),
        },
        mir::TypedStatement::Break => TypedStatement::Break,
        mir::TypedStatement::Continue => TypedStatement::Continue,
    }
}

fn expression_remove_span<'src>(
    expression: Spanned<mir::TypedExpression<'src, '_>>,
) -> TypedExpression<'src> {
    TypedExpression {
        expr: match expression.0.expr {
            mir::Expression::Variable(name) => Expression::Variable(name),
            mir::Expression::Boolean(value) => Expression::Boolean(value),
            mir::Expression::Integer(value) => Expression::Integer(value),
            mir::Expression::Float(value) => Expression::Float(value),
            mir::Expression::Colour { r, g, b } => Expression::Colour { r, g, b },
            mir::Expression::Vector { x, y } => Expression::Vector {
                x: Box::new(expression_remove_span(*x)),
                y: Box::new(expression_remove_span(*y)),
            },
            mir::Expression::Operation(operation) => {
                Expression::Operation(Box::new(operation_remove_span(*operation)))
            }
        },
        ty: expression.0.ty.into(),
    }
}

fn operation_remove_span<'src>(operation: mir::Operation<'src, '_>) -> Operation<'src> {
    match operation {
        mir::Operation::IntegerEquals(lhs, rhs) => {
            Operation::IntegerEquals(expression_remove_span(lhs), expression_remove_span(rhs))
        }
        mir::Operation::IntegerNotEquals(lhs, rhs) => {
            Operation::IntegerNotEquals(expression_remove_span(lhs), expression_remove_span(rhs))
        }
        mir::Operation::IntegerPlus(lhs, rhs) => {
            Operation::IntegerPlus(expression_remove_span(lhs), expression_remove_span(rhs))
        }
        mir::Operation::IntegerMinus(lhs, rhs) => {
            Operation::IntegerMinus(expression_remove_span(lhs), expression_remove_span(rhs))
        }
        mir::Operation::IntegerMultiply(lhs, rhs) => {
            Operation::IntegerMultiply(expression_remove_span(lhs), expression_remove_span(rhs))
        }
        mir::Operation::IntegerDivide(lhs, rhs) => {
            Operation::IntegerDivide(expression_remove_span(lhs), expression_remove_span(rhs))
        }
        mir::Operation::IntegerGreaterThanEquals(lhs, rhs) => Operation::IntegerGreaterThanEquals(
            expression_remove_span(lhs),
            expression_remove_span(rhs),
        ),
        mir::Operation::IntegerLessThanEquals(lhs, rhs) => Operation::IntegerLessThanEquals(
            expression_remove_span(lhs),
            expression_remove_span(rhs),
        ),
        mir::Operation::IntegerGreaterThan(lhs, rhs) => {
            Operation::IntegerGreaterThan(expression_remove_span(lhs), expression_remove_span(rhs))
        }
        mir::Operation::IntegerLessThan(lhs, rhs) => {
            Operation::IntegerLessThan(expression_remove_span(lhs), expression_remove_span(rhs))
        }

        mir::Operation::FloatEquals(lhs, rhs) => {
            Operation::FloatEquals(expression_remove_span(lhs), expression_remove_span(rhs))
        }
        mir::Operation::FloatNotEquals(lhs, rhs) => {
            Operation::FloatNotEquals(expression_remove_span(lhs), expression_remove_span(rhs))
        }
        mir::Operation::FloatPlus(lhs, rhs) => {
            Operation::FloatPlus(expression_remove_span(lhs), expression_remove_span(rhs))
        }
        mir::Operation::FloatMinus(lhs, rhs) => {
            Operation::FloatMinus(expression_remove_span(lhs), expression_remove_span(rhs))
        }
        mir::Operation::FloatMultiply(lhs, rhs) => {
            Operation::FloatMultiply(expression_remove_span(lhs), expression_remove_span(rhs))
        }
        mir::Operation::FloatDivide(lhs, rhs) => {
            Operation::FloatDivide(expression_remove_span(lhs), expression_remove_span(rhs))
        }
        mir::Operation::FloatGreaterThanEquals(lhs, rhs) => Operation::FloatGreaterThanEquals(
            expression_remove_span(lhs),
            expression_remove_span(rhs),
        ),
        mir::Operation::FloatLessThanEquals(lhs, rhs) => {
            Operation::FloatLessThanEquals(expression_remove_span(lhs), expression_remove_span(rhs))
        }
        mir::Operation::FloatGreaterThan(lhs, rhs) => {
            Operation::FloatGreaterThan(expression_remove_span(lhs), expression_remove_span(rhs))
        }
        mir::Operation::FloatLessThan(lhs, rhs) => {
            Operation::FloatLessThan(expression_remove_span(lhs), expression_remove_span(rhs))
        }

        mir::Operation::BooleanEquals(lhs, rhs) => {
            Operation::BooleanEquals(expression_remove_span(lhs), expression_remove_span(rhs))
        }
        mir::Operation::BooleanNotEquals(lhs, rhs) => {
            Operation::BooleanNotEquals(expression_remove_span(lhs), expression_remove_span(rhs))
        }

        mir::Operation::IntegerNegate(value) => {
            Operation::IntegerNegate(expression_remove_span(value))
        }
        mir::Operation::FloatNegate(value) => Operation::FloatNegate(expression_remove_span(value)),
        mir::Operation::BooleanNot(value) => Operation::BooleanNot(expression_remove_span(value)),
    }
}
