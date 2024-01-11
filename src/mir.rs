use crate::{
    ast::{
        typed::{self, Expr, TypedExpr, TypedStatement as Statement},
        BinaryOp, UnaryOp,
    },
    Spanned,
};

#[derive(Debug)]
pub enum TypedStatement<'src, 'file> {
    Expr(Spanned<'file, TypedExpression<'src, 'file>>),
    BuiltinPrint(Spanned<'file, TypedExpression<'src, 'file>>),
    Loop(Spanned<'file, Vec<Spanned<'file, TypedStatement<'src, 'file>>>>),
    If {
        condition: Spanned<'file, TypedExpression<'src, 'file>>,
        then_branch: Spanned<'file, Vec<Spanned<'file, TypedStatement<'src, 'file>>>>,
        else_branch: Option<Spanned<'file, Vec<Spanned<'file, TypedStatement<'src, 'file>>>>>,
    },
    Let {
        name: Spanned<'file, &'src str>,
        value: Spanned<'file, TypedExpression<'src, 'file>>,
    },
    Const {
        name: Spanned<'file, &'src str>,
        value: Spanned<'file, TypedExpression<'src, 'file>>,
    },
    Assign {
        name: Spanned<'file, &'src str>,
        value: Spanned<'file, TypedExpression<'src, 'file>>,
    },
    Break,
    Continue,
}

#[derive(Debug)]
pub struct TypedExpression<'src, 'file> {
    pub expr: Expression<'src, 'file>,
    pub ty: Type,
}

#[derive(Debug)]
pub enum Expression<'src, 'file> {
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
        x: Box<Spanned<'file, TypedExpression<'src, 'file>>>,
        y: Box<Spanned<'file, TypedExpression<'src, 'file>>>,
    },
    Operation(Box<Operation<'src, 'file>>),
}

#[derive(Debug)]
pub enum Operation<'src, 'file> {
    IntegerEquals(
        Spanned<'file, TypedExpression<'src, 'file>>,
        Spanned<'file, TypedExpression<'src, 'file>>,
    ),
    IntegerNotEquals(
        Spanned<'file, TypedExpression<'src, 'file>>,
        Spanned<'file, TypedExpression<'src, 'file>>,
    ),
    IntegerPlus(
        Spanned<'file, TypedExpression<'src, 'file>>,
        Spanned<'file, TypedExpression<'src, 'file>>,
    ),
    IntegerMinus(
        Spanned<'file, TypedExpression<'src, 'file>>,
        Spanned<'file, TypedExpression<'src, 'file>>,
    ),
    IntegerMultiply(
        Spanned<'file, TypedExpression<'src, 'file>>,
        Spanned<'file, TypedExpression<'src, 'file>>,
    ),
    IntegerDivide(
        Spanned<'file, TypedExpression<'src, 'file>>,
        Spanned<'file, TypedExpression<'src, 'file>>,
    ),
    IntegerGreaterThanEquals(
        Spanned<'file, TypedExpression<'src, 'file>>,
        Spanned<'file, TypedExpression<'src, 'file>>,
    ),
    IntegerLessThanEquals(
        Spanned<'file, TypedExpression<'src, 'file>>,
        Spanned<'file, TypedExpression<'src, 'file>>,
    ),
    IntegerGreaterThan(
        Spanned<'file, TypedExpression<'src, 'file>>,
        Spanned<'file, TypedExpression<'src, 'file>>,
    ),
    IntegerLessThan(
        Spanned<'file, TypedExpression<'src, 'file>>,
        Spanned<'file, TypedExpression<'src, 'file>>,
    ),

    FloatEquals(
        Spanned<'file, TypedExpression<'src, 'file>>,
        Spanned<'file, TypedExpression<'src, 'file>>,
    ),
    FloatNotEquals(
        Spanned<'file, TypedExpression<'src, 'file>>,
        Spanned<'file, TypedExpression<'src, 'file>>,
    ),
    FloatPlus(
        Spanned<'file, TypedExpression<'src, 'file>>,
        Spanned<'file, TypedExpression<'src, 'file>>,
    ),
    FloatMinus(
        Spanned<'file, TypedExpression<'src, 'file>>,
        Spanned<'file, TypedExpression<'src, 'file>>,
    ),
    FloatMultiply(
        Spanned<'file, TypedExpression<'src, 'file>>,
        Spanned<'file, TypedExpression<'src, 'file>>,
    ),
    FloatDivide(
        Spanned<'file, TypedExpression<'src, 'file>>,
        Spanned<'file, TypedExpression<'src, 'file>>,
    ),
    FloatGreaterThanEquals(
        Spanned<'file, TypedExpression<'src, 'file>>,
        Spanned<'file, TypedExpression<'src, 'file>>,
    ),
    FloatLessThanEquals(
        Spanned<'file, TypedExpression<'src, 'file>>,
        Spanned<'file, TypedExpression<'src, 'file>>,
    ),
    FloatGreaterThan(
        Spanned<'file, TypedExpression<'src, 'file>>,
        Spanned<'file, TypedExpression<'src, 'file>>,
    ),
    FloatLessThan(
        Spanned<'file, TypedExpression<'src, 'file>>,
        Spanned<'file, TypedExpression<'src, 'file>>,
    ),

    BooleanEquals(
        Spanned<'file, TypedExpression<'src, 'file>>,
        Spanned<'file, TypedExpression<'src, 'file>>,
    ),
    BooleanNotEquals(
        Spanned<'file, TypedExpression<'src, 'file>>,
        Spanned<'file, TypedExpression<'src, 'file>>,
    ),

    IntegerNegate(Spanned<'file, TypedExpression<'src, 'file>>),
    FloatNegate(Spanned<'file, TypedExpression<'src, 'file>>),
    BooleanNot(Spanned<'file, TypedExpression<'src, 'file>>),
}

#[derive(Clone, Copy, Debug)]
pub enum Type {
    Boolean,
    Integer,
    Float,
    Colour,
    Vector,
}

impl From<typed::Type> for Type {
    fn from(ty: typed::Type) -> Self {
        match ty {
            typed::Type::Boolean => Type::Boolean,
            typed::Type::Integer => Type::Integer,
            typed::Type::Float => Type::Float,
            typed::Type::Colour => Type::Colour,
            typed::Type::Vector => Type::Vector,
        }
    }
}

pub fn build<'src, 'file>(
    ast: Vec<Spanned<'file, Statement<'src, 'file>>>,
) -> Vec<Spanned<'file, TypedStatement<'src, 'file>>> {
    ast.into_iter().map(build_mir_statement).collect()
}

fn build_mir_statement<'src, 'file>(
    statement: Spanned<'file, Statement<'src, 'file>>,
) -> Spanned<'file, TypedStatement<'src, 'file>> {
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

fn build_mir_expr<'src, 'file>(
    expr: Spanned<'file, TypedExpr<'src, 'file>>,
) -> Spanned<'file, TypedExpression<'src, 'file>> {
    (
        TypedExpression {
            expr: match expr.0.expr {
                Expr::Variable(name) => Expression::Variable(name),
                Expr::Boolean(value) => Expression::Boolean(value),
                Expr::Integer(value) => Expression::Integer(value),
                Expr::Float(value) => Expression::Float(value),
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

                        (Type::Float, Type::Float) => match op.0 {
                            BinaryOp::Equals => Operation::FloatEquals(lhs, rhs),
                            BinaryOp::NotEquals => Operation::FloatNotEquals(lhs, rhs),
                            BinaryOp::Plus => Operation::FloatPlus(lhs, rhs),
                            BinaryOp::Minus => Operation::FloatMinus(lhs, rhs),
                            BinaryOp::Multiply => Operation::FloatMultiply(lhs, rhs),
                            BinaryOp::Divide => Operation::FloatDivide(lhs, rhs),
                            BinaryOp::GreaterThanEquals => {
                                Operation::FloatGreaterThanEquals(lhs, rhs)
                            }
                            BinaryOp::LessThanEquals => Operation::FloatLessThanEquals(lhs, rhs),
                            BinaryOp::GreaterThan => Operation::FloatGreaterThan(lhs, rhs),
                            BinaryOp::LessThan => Operation::FloatLessThan(lhs, rhs),
                        },

                        (Type::Boolean, Type::Boolean) => match op.0 {
                            BinaryOp::Equals => Operation::BooleanEquals(lhs, rhs),
                            BinaryOp::NotEquals => Operation::BooleanNotEquals(lhs, rhs),

                            _ => unreachable!(),
                        },

                        _ => unreachable!(),
                    }))
                }
                Expr::Unary(op, rhs) => {
                    let rhs = build_mir_expr(*rhs);

                    // TODO: Remove this when more unary operators are added
                    #[allow(clippy::match_wildcard_for_single_variants)]
                    Expression::Operation(Box::new(match &rhs.0.ty {
                        Type::Integer => match op.0 {
                            UnaryOp::Negate => Operation::IntegerNegate(rhs),

                            _ => unreachable!(),
                        },
                        Type::Float => match op.0 {
                            UnaryOp::Negate => Operation::FloatNegate(rhs),

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
