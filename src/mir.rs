use crate::{
    ast::{
        typed::{self, Expr, TypedExpr, TypedStatement as Statement},
        BinaryOp, UnaryOp,
    },
    Spanned,
};

#[derive(Debug)]
pub enum TypedStatement<'src> {
    Expr(Spanned<TypedExpression<'src>>),
    BuiltinPrint(Spanned<TypedExpression<'src>>),
    Loop(Spanned<Vec<Spanned<TypedStatement<'src>>>>),
    If {
        condition: Spanned<TypedExpression<'src>>,
        then_branch: Spanned<Vec<Spanned<TypedStatement<'src>>>>,
        else_branch: Option<Spanned<Vec<Spanned<TypedStatement<'src>>>>>,
    },
    Let {
        name: Spanned<&'src str>,
        value: Spanned<TypedExpression<'src>>,
    },
    Const {
        name: Spanned<&'src str>,
        value: Spanned<TypedExpression<'src>>,
    },
    Assign {
        name: Spanned<&'src str>,
        value: Spanned<TypedExpression<'src>>,
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
    Null,
    Colour {
        r: u8,
        g: u8,
        b: u8,
    },
    Vector {
        x: Box<Spanned<TypedExpression<'src>>>,
        y: Box<Spanned<TypedExpression<'src>>>,
    },
    Operation(Box<Operation<'src>>),
}

#[derive(Debug)]
pub enum Operation<'src> {
    IntegerEquals(
        Spanned<TypedExpression<'src>>,
        Spanned<TypedExpression<'src>>,
    ),
    IntegerNotEquals(
        Spanned<TypedExpression<'src>>,
        Spanned<TypedExpression<'src>>,
    ),
    IntegerPlus(
        Spanned<TypedExpression<'src>>,
        Spanned<TypedExpression<'src>>,
    ),
    IntegerMinus(
        Spanned<TypedExpression<'src>>,
        Spanned<TypedExpression<'src>>,
    ),
    IntegerMultiply(
        Spanned<TypedExpression<'src>>,
        Spanned<TypedExpression<'src>>,
    ),
    IntegerDivide(
        Spanned<TypedExpression<'src>>,
        Spanned<TypedExpression<'src>>,
    ),
    IntegerGreaterThanEquals(
        Spanned<TypedExpression<'src>>,
        Spanned<TypedExpression<'src>>,
    ),
    IntegerLessThanEquals(
        Spanned<TypedExpression<'src>>,
        Spanned<TypedExpression<'src>>,
    ),
    IntegerGreaterThan(
        Spanned<TypedExpression<'src>>,
        Spanned<TypedExpression<'src>>,
    ),
    IntegerLessThan(
        Spanned<TypedExpression<'src>>,
        Spanned<TypedExpression<'src>>,
    ),

    BooleanEquals(
        Spanned<TypedExpression<'src>>,
        Spanned<TypedExpression<'src>>,
    ),
    BooleanNotEquals(
        Spanned<TypedExpression<'src>>,
        Spanned<TypedExpression<'src>>,
    ),

    NullEquals(
        Spanned<TypedExpression<'src>>,
        Spanned<TypedExpression<'src>>,
    ),
    NullNotEquals(
        Spanned<TypedExpression<'src>>,
        Spanned<TypedExpression<'src>>,
    ),

    IntegerNegate(Spanned<TypedExpression<'src>>),
    BooleanNot(Spanned<TypedExpression<'src>>),
}

#[derive(Clone, Copy, Debug)]
pub enum Type {
    Boolean,
    Integer,
    Null,
    Colour,
    Vector,
}

impl From<typed::Type> for Type {
    fn from(ty: typed::Type) -> Self {
        match ty {
            typed::Type::Boolean => Type::Boolean,
            typed::Type::Integer => Type::Integer,
            typed::Type::Null => Type::Null,
            typed::Type::Colour => Type::Colour,
            typed::Type::Vector => Type::Vector,
        }
    }
}

pub fn build_mir(ast: Vec<Spanned<Statement<'_>>>) -> Vec<Spanned<TypedStatement<'_>>> {
    ast.into_iter().map(build_mir_statement).collect()
}

fn build_mir_statement(statement: Spanned<Statement<'_>>) -> Spanned<TypedStatement<'_>> {
    (
        match statement.0 {
            Statement::Expr(expr) => TypedStatement::Expr(build_mir_expr(expr)),
            Statement::BuiltinPrint(expr) => TypedStatement::BuiltinPrint(build_mir_expr(expr)),
            Statement::Loop(statements) => TypedStatement::Loop((
                statements.0.into_iter().map(build_mir_statement).collect(),
                statements.1,
            )),
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => TypedStatement::If {
                condition: build_mir_expr(condition),
                then_branch: (
                    then_branch.0.into_iter().map(build_mir_statement).collect(),
                    then_branch.1,
                ),
                else_branch: else_branch
                    .map(|s| (s.0.into_iter().map(build_mir_statement).collect(), s.1)),
            },
            Statement::Let { name, value } => TypedStatement::Let {
                name,
                value: build_mir_expr(value),
            },
            Statement::Const { name, value } => TypedStatement::Const {
                name,
                value: build_mir_expr(value),
            },
            Statement::Assign { name, value } => TypedStatement::Assign {
                name,
                value: build_mir_expr(value),
            },
            Statement::Break => TypedStatement::Break,
            Statement::Continue => TypedStatement::Continue,
        },
        statement.1,
    )
}

fn build_mir_expr(expr: Spanned<TypedExpr<'_>>) -> Spanned<TypedExpression<'_>> {
    (
        TypedExpression {
            expr: match expr.0.expr {
                Expr::Variable(name) => Expression::Variable(name),
                Expr::Boolean(value) => Expression::Boolean(value),
                Expr::Integer(value) => Expression::Integer(value),
                Expr::Null => Expression::Null,
                Expr::Colour { r, g, b } => Expression::Colour { r, g, b },
                Expr::Vector { x, y } => Expression::Vector {
                    x: Box::new(build_mir_expr(*x)),
                    y: Box::new(build_mir_expr(*y)),
                },
                Expr::Binary(lhs, op, rhs) => {
                    let lhs = build_mir_expr(*lhs);
                    let rhs = build_mir_expr(*rhs);

                    Expression::Operation(Box::new(match (&lhs.0.ty, &rhs.0.ty) {
                        (Type::Integer, Type::Integer) => match op.0 {
                            BinaryOp::Equals => Operation::IntegerEquals(lhs, rhs),
                            BinaryOp::NotEquals => Operation::IntegerNotEquals(lhs, rhs),
                            BinaryOp::Plus => Operation::IntegerPlus(lhs, rhs),
                            BinaryOp::Minus => Operation::IntegerMinus(lhs, rhs),
                            BinaryOp::Multiply => Operation::IntegerMultiply(lhs, rhs),
                            BinaryOp::Divide => Operation::IntegerDivide(lhs, rhs),
                            BinaryOp::GreaterThanEquals => {
                                Operation::IntegerGreaterThanEquals(lhs, rhs)
                            }
                            BinaryOp::LessThanEquals => Operation::IntegerLessThanEquals(lhs, rhs),
                            BinaryOp::GreaterThan => Operation::IntegerGreaterThan(lhs, rhs),
                            BinaryOp::LessThan => Operation::IntegerLessThan(lhs, rhs),
                        },

                        (Type::Boolean, Type::Boolean) => match op.0 {
                            BinaryOp::Equals => Operation::BooleanEquals(lhs, rhs),
                            BinaryOp::NotEquals => Operation::BooleanNotEquals(lhs, rhs),

                            _ => unreachable!(),
                        },

                        (Type::Null, Type::Null) => match op.0 {
                            BinaryOp::Equals => Operation::NullEquals(lhs, rhs),
                            BinaryOp::NotEquals => Operation::NullNotEquals(lhs, rhs),

                            _ => unreachable!(),
                        },

                        _ => unreachable!(),
                    }))
                }
                Expr::Unary(op, rhs) => {
                    let rhs = build_mir_expr(*rhs);

                    Expression::Operation(Box::new(match &rhs.0.ty {
                        Type::Integer => match op.0 {
                            UnaryOp::Negate => Operation::IntegerNegate(rhs),

                            _ => unreachable!(),
                        },
                        Type::Boolean => match op.0 {
                            UnaryOp::Not => Operation::BooleanNot(rhs),

                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    }))
                }
            },
            ty: expr.0.ty.into(),
        },
        expr.1,
    )
}
