use crate::{
    error::Error,
    mir::{Expression, Operation, Type, TypedExpression, TypedStatement, VarId},
    scopes::Scopes,
    Spanned,
};

pub fn const_eval<'src, 'file>(
    ast: Vec<Spanned<'file, TypedStatement<'file>>>,
) -> (
    Option<Vec<Spanned<'file, TypedStatement<'file>>>>,
    Vec<Box<Error<'file>>>,
) {
    let mut errors = vec![];

    let mut const_vars = Scopes::new();

    let mut statements = vec![];

    for statement in ast {
        if let Some(statement) = const_eval_statement(&mut const_vars, &mut errors, statement) {
            statements.push(statement);
        }
    }

    if errors.is_empty() {
        (Some(statements), errors)
    } else {
        (None, errors)
    }
}

fn const_eval_statement<'src, 'file>(
    const_vars: &mut Scopes<VarId, Spanned<'file, ConstValue<'file>>>,
    errors: &mut Vec<Box<Error<'file>>>,
    statement: Spanned<'file, TypedStatement<'file>>,
) -> Option<Spanned<'file, TypedStatement<'file>>> {
    match statement.0 {
        TypedStatement::Expr(expr) => Some(TypedStatement::Expr(propagate_const(const_vars, expr))),
        TypedStatement::BuiltinPrint(expr) => Some(TypedStatement::BuiltinPrint(propagate_const(
            const_vars, expr,
        ))),
        TypedStatement::Loop(statements) => {
            let mut stmts = vec![];

            for statement in statements.0 {
                if let Some(statement) = const_eval_statement(const_vars, errors, statement) {
                    stmts.push(statement);
                }
            }

            Some(TypedStatement::Loop(Spanned(stmts, statements.1)))
        }
        TypedStatement::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let condition = propagate_const(const_vars, condition);

            let mut then = vec![];

            for statement in then_branch.0 {
                if let Some(statement) = const_eval_statement(const_vars, errors, statement) {
                    then.push(statement);
                }
            }

            let else_ = else_branch.map(|else_branch| {
                let mut else_ = vec![];

                for statement in else_branch.0 {
                    if let Some(statement) = const_eval_statement(const_vars, errors, statement) {
                        else_.push(statement);
                    }
                }

                Spanned(else_, else_branch.1)
            });

            Some(TypedStatement::If {
                condition,
                then_branch: Spanned(then, then_branch.1),
                else_branch: else_,
            })
        }
        TypedStatement::Let { name, value } => {
            let value = propagate_const(const_vars, value);

            Some(TypedStatement::Let { name, value })
        }
        TypedStatement::Const { name, value: expr } => const_eval_expr(const_vars, errors, expr)
            .map(|value| {
                const_vars.insert(*name, value.clone());

                TypedStatement::Const {
                    name,
                    value: Spanned(value.0.into(), value.1),
                }
            }),
        TypedStatement::Assign { name, value } => {
            let value = propagate_const(const_vars, value);

            Some(TypedStatement::Assign { name, value })
        }
        TypedStatement::Break => Some(TypedStatement::Break),
        TypedStatement::Continue => Some(TypedStatement::Continue),
    }
    .map(|stmt| Spanned(stmt, statement.1))
}

fn const_eval_expr<'file>(
    const_vars: &mut Scopes<VarId, Spanned<ConstValue<'file>>>,
    errors: &mut Vec<Box<Error<'file>>>,
    expr: Spanned<'file, TypedExpression<'file>>,
) -> Option<Spanned<'file, ConstValue<'file>>> {
    match expr.0.expr {
        Expression::Variable(name) => {
            if let Some(value) = const_vars.get(&name) {
                Some(value.0.clone())
            } else {
                errors.push(Box::new(Error::UnknownConstVariable {
                    name: name.to_string(),
                    span: expr.1,
                }));

                None
            }
        }
        Expression::Boolean(value) => Some(ConstValue::Boolean(value)),
        Expression::Integer(value) => Some(ConstValue::Integer(value)),
        Expression::Float(value) => Some(ConstValue::Float(value)),
        Expression::Colour { r, g, b } => Some(ConstValue::Colour { r, g, b }),
        Expression::Vector { x, y } => {
            let x = const_eval_expr(const_vars, errors, *x);
            let y = const_eval_expr(const_vars, errors, *y);

            match (x, y) {
                (Some(x), Some(y)) => Some(ConstValue::Vector {
                    x: Box::new(x),
                    y: Box::new(y),
                }),
                _ => None,
            }
        }
        Expression::Operation(operation) => Some(match *operation {
            Operation::IntegerEquals(lhs, rhs) => {
                const_eval_binary_operation!(
                    const_vars, errors,
                    lhs, rhs,
                    ==,
                    Integer, Integer => Boolean
                )
            }
            Operation::IntegerNotEquals(lhs, rhs) => {
                const_eval_binary_operation!(
                    const_vars, errors,
                    lhs, rhs,
                    !=,
                    Integer, Integer => Boolean
                )
            }
            Operation::IntegerPlus(lhs, rhs) => {
                const_eval_binary_operation!(
                    const_vars, errors,
                    lhs, rhs,
                    +,
                    Integer, Integer => Integer
                )
            }
            Operation::IntegerMinus(lhs, rhs) => {
                const_eval_binary_operation!(
                    const_vars, errors,
                    lhs, rhs,
                    -,
                    Integer, Integer => Integer
                )
            }
            Operation::IntegerMultiply(lhs, rhs) => {
                const_eval_binary_operation!(
                    const_vars, errors,
                    lhs, rhs,
                    *,
                    Integer, Integer => Integer
                )
            }
            Operation::IntegerDivide(lhs, rhs) => {
                const_eval_binary_operation!(
                    const_vars, errors,
                    lhs, rhs,
                    /,
                    Integer, Integer => Integer
                )
            }
            Operation::IntegerGreaterThanEquals(lhs, rhs) => {
                const_eval_binary_operation!(
                    const_vars, errors,
                    lhs, rhs,
                    >=,
                    Integer, Integer => Boolean
                )
            }
            Operation::IntegerLessThanEquals(lhs, rhs) => {
                const_eval_binary_operation!(
                    const_vars, errors,
                    lhs, rhs,
                    <=,
                    Integer, Integer => Boolean
                )
            }
            Operation::IntegerGreaterThan(lhs, rhs) => {
                const_eval_binary_operation!(
                    const_vars, errors,
                    lhs, rhs,
                    >,
                    Integer, Integer => Boolean
                )
            }
            Operation::IntegerLessThan(lhs, rhs) => {
                const_eval_binary_operation!(
                    const_vars, errors,
                    lhs, rhs,
                    <,
                    Integer, Integer => Boolean
                )
            }
            Operation::FloatEquals(lhs, rhs) => {
                const_eval_binary_operation!(
                    const_vars, errors,
                    lhs, rhs,
                    ==,
                    Float, Float => Boolean
                )
            }
            Operation::FloatNotEquals(lhs, rhs) => {
                const_eval_binary_operation!(
                    const_vars, errors,
                    lhs, rhs,
                    !=,
                    Float, Float => Boolean
                )
            }
            Operation::FloatPlus(lhs, rhs) => {
                const_eval_binary_operation!(
                    const_vars, errors,
                    lhs, rhs,
                    +,
                    Float, Float => Float
                )
            }
            Operation::FloatMinus(lhs, rhs) => {
                const_eval_binary_operation!(
                    const_vars, errors,
                    lhs, rhs,
                    -,
                    Float, Float => Float
                )
            }
            Operation::FloatMultiply(lhs, rhs) => {
                const_eval_binary_operation!(
                    const_vars, errors,
                    lhs, rhs,
                    *,
                    Float, Float => Float
                )
            }
            Operation::FloatDivide(lhs, rhs) => {
                const_eval_binary_operation!(
                    const_vars, errors,
                    lhs, rhs,
                    /,
                    Float, Float => Float
                )
            }
            Operation::FloatGreaterThanEquals(lhs, rhs) => {
                const_eval_binary_operation!(
                    const_vars, errors,
                    lhs, rhs,
                    >=,
                    Float, Float => Boolean
                )
            }
            Operation::FloatLessThanEquals(lhs, rhs) => {
                const_eval_binary_operation!(
                    const_vars, errors,
                    lhs, rhs,
                    <=,
                    Float, Float => Boolean
                )
            }
            Operation::FloatGreaterThan(lhs, rhs) => {
                const_eval_binary_operation!(
                    const_vars, errors,
                    lhs, rhs,
                    >,
                    Float, Float => Boolean
                )
            }
            Operation::FloatLessThan(lhs, rhs) => {
                const_eval_binary_operation!(
                    const_vars, errors,
                    lhs, rhs,
                    <,
                    Float, Float => Boolean
                )
            }
            Operation::BooleanEquals(lhs, rhs) => {
                const_eval_binary_operation!(
                    const_vars, errors,
                    lhs, rhs,
                    ==,
                    Boolean, Boolean => Boolean
                )
            }
            Operation::BooleanNotEquals(lhs, rhs) => {
                const_eval_binary_operation!(
                    const_vars, errors,
                    lhs, rhs,
                    !=,
                    Boolean, Boolean => Boolean
                )
            }
            Operation::IntegerNegate(rhs) => {
                const_eval_unary_operation!(const_vars, errors, rhs, -, Integer => Integer)
            }
            Operation::FloatNegate(rhs) => {
                const_eval_unary_operation!(const_vars, errors, rhs, -, Float => Float)
            }
            Operation::BooleanNot(rhs) => {
                const_eval_unary_operation!(const_vars, errors, rhs, !, Boolean => Boolean)
            }
        }),
    }
    .map(|value| Spanned(value, expr.1))
}

fn propagate_const<'src, 'file>(
    const_vars: &mut Scopes<VarId, Spanned<ConstValue<'file>>>,
    expr: Spanned<'file, TypedExpression<'file>>,
) -> Spanned<'file, TypedExpression<'file>> {
    Spanned(
        TypedExpression {
            expr: match expr.0.expr {
                Expression::Variable(name) => const_vars
                    .get(&name)
                    .map_or(Expression::Variable(name), |value| {
                        Expression::from(value.0.clone())
                    }),
                Expression::Boolean(value) => Expression::Boolean(value),
                Expression::Integer(value) => Expression::Integer(value),
                Expression::Float(value) => Expression::Float(value),
                Expression::Colour { r, g, b } => Expression::Colour { r, g, b },
                Expression::Vector { x, y } => Expression::Vector {
                    x: Box::new(propagate_const(const_vars, *x)),
                    y: Box::new(propagate_const(const_vars, *y)),
                },
                Expression::Operation(operation) => match *operation {
                    Operation::IntegerEquals(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs, rhs,
                            ==,
                            Integer, Integer => Boolean,
                            IntegerEquals
                        )
                    }
                    Operation::IntegerNotEquals(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs, rhs,
                            !=,
                            Integer, Integer => Boolean,
                            IntegerNotEquals
                        )
                    }
                    Operation::IntegerPlus(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs, rhs,
                            +,
                            Integer, Integer => Integer,
                            IntegerPlus
                        )
                    }
                    Operation::IntegerMinus(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs, rhs,
                            -,
                            Integer, Integer => Integer,
                            IntegerMinus
                        )
                    }
                    Operation::IntegerMultiply(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs, rhs,
                            *,
                            Integer, Integer => Integer,
                            IntegerMultiply
                        )
                    }
                    Operation::IntegerDivide(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs, rhs,
                            /,
                            Integer, Integer => Integer,
                            IntegerDivide
                        )
                    }
                    Operation::IntegerGreaterThanEquals(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs, rhs,
                            >=,
                            Integer, Integer => Boolean,
                            IntegerGreaterThanEquals
                        )
                    }
                    Operation::IntegerLessThanEquals(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs, rhs,
                            <=,
                            Integer, Integer => Boolean,
                            IntegerLessThanEquals
                        )
                    }
                    Operation::IntegerGreaterThan(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs, rhs,
                            >,
                            Integer, Integer => Boolean,
                            IntegerGreaterThan
                        )
                    }
                    Operation::IntegerLessThan(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs, rhs,
                            <,
                            Integer, Integer => Boolean,
                            IntegerLessThan
                        )
                    }
                    Operation::FloatEquals(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs, rhs,
                            ==,
                            Float, Float => Boolean,
                            FloatEquals
                        )
                    }
                    Operation::FloatNotEquals(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs, rhs,
                            !=,
                            Float, Float => Boolean,
                            FloatNotEquals
                        )
                    }
                    Operation::FloatPlus(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs, rhs,
                            +,
                            Float, Float => Float,
                            FloatPlus
                        )
                    }
                    Operation::FloatMinus(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs, rhs,
                            -,
                            Float, Float => Float,
                            FloatMinus
                        )
                    }
                    Operation::FloatMultiply(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs, rhs,
                            *,
                            Float, Float => Float,
                            FloatMultiply
                        )
                    }
                    Operation::FloatDivide(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs, rhs,
                            /,
                            Float, Float => Float,
                            FloatDivide
                        )
                    }
                    Operation::FloatGreaterThanEquals(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs, rhs,
                            >=,
                            Float, Float => Boolean,
                            FloatGreaterThanEquals
                        )
                    }
                    Operation::FloatLessThanEquals(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs, rhs,
                            <=,
                            Float, Float => Boolean,
                            FloatLessThanEquals
                        )
                    }
                    Operation::FloatGreaterThan(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs, rhs,
                            >,
                            Float, Float => Boolean,
                            FloatGreaterThan
                        )
                    }
                    Operation::FloatLessThan(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs, rhs,
                            <,
                            Float, Float => Boolean,
                            FloatLessThan
                        )
                    }
                    Operation::BooleanEquals(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs, rhs,
                            ==,
                            Boolean, Boolean => Boolean,
                            BooleanEquals
                        )
                    }
                    Operation::BooleanNotEquals(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs, rhs,
                            !=,
                            Boolean, Boolean => Boolean,
                            BooleanNotEquals
                        )
                    }
                    Operation::IntegerNegate(rhs) => {
                        const_propagate_unary_operation!(
                            const_vars,
                            rhs,
                            -,
                            Integer => Integer,
                            IntegerNegate
                        )
                    }
                    Operation::FloatNegate(rhs) => {
                        const_propagate_unary_operation!(
                            const_vars,
                            rhs,
                            -,
                            Float => Float,
                            FloatNegate
                        )
                    }
                    Operation::BooleanNot(rhs) => {
                        const_propagate_unary_operation!(
                            const_vars,
                            rhs,
                            !,
                            Boolean => Boolean,
                            BooleanNot
                        )
                    }
                },
            },
            ty: expr.0.ty,
        },
        expr.1,
    )
}

#[derive(Clone, PartialEq)]
enum ConstValue<'file> {
    Boolean(bool),
    Integer(i32),
    Float(f32),
    Colour {
        r: u8,
        g: u8,
        b: u8,
    },
    Vector {
        x: Box<Spanned<'file, ConstValue<'file>>>,
        y: Box<Spanned<'file, ConstValue<'file>>>,
    },
}

impl<'src, 'file> From<ConstValue<'file>> for Expression<'file> {
    fn from(expr: ConstValue<'file>) -> Self {
        match expr {
            ConstValue::Boolean(value) => Expression::Boolean(value),
            ConstValue::Integer(value) => Expression::Integer(value),
            ConstValue::Float(value) => Expression::Float(value),
            ConstValue::Colour { r, g, b } => Expression::Colour { r, g, b },
            ConstValue::Vector { x, y } => Expression::Vector {
                x: Box::new(Spanned(
                    TypedExpression {
                        expr: Expression::from(x.0),
                        ty: Type::Integer,
                    },
                    x.1,
                )),
                y: Box::new(Spanned(
                    TypedExpression {
                        expr: Expression::from(y.0),
                        ty: Type::Integer,
                    },
                    y.1,
                )),
            },
        }
    }
}

impl<'src, 'file> From<ConstValue<'file>> for TypedExpression<'file> {
    fn from(const_value: ConstValue<'file>) -> Self {
        let expr = const_value.clone().into();

        let ty = match const_value {
            ConstValue::Boolean(_) => Type::Boolean,
            ConstValue::Integer(_) => Type::Integer,
            ConstValue::Float(_) => Type::Float,
            ConstValue::Colour { .. } => Type::Colour,
            ConstValue::Vector { .. } => Type::Vector,
        };

        TypedExpression { expr, ty }
    }
}

macro_rules! const_eval_binary_operation {
    ($const_vars:expr, $errors:expr, $lhs:expr, $rhs:expr, $op:tt, $lhs_ty:ident, $rhs_ty:ident => $result_ty:ident) => {{
        let lhs = const_eval_expr($const_vars, $errors, $lhs)?;
        let rhs = const_eval_expr($const_vars, $errors, $rhs)?;

        match (lhs.0, rhs.0) {
            (ConstValue::$lhs_ty(lhs), ConstValue::$rhs_ty(rhs)) => ConstValue::$result_ty(lhs $op rhs),
            _ => unreachable!(),
        }
    }};
}
use const_eval_binary_operation;

macro_rules! const_eval_unary_operation {
    ($const_vars:expr, $errors:expr, $rhs:expr, $op:tt, $rhs_ty:ident => $result_ty:ident) => {{
        let rhs = const_eval_expr($const_vars, $errors, $rhs)?;

        match rhs.0 {
            ConstValue::$rhs_ty(rhs) => ConstValue::$result_ty($op rhs),
            _ => unreachable!(),
        }
    }};
}
use const_eval_unary_operation;

macro_rules! const_propagate_binary_operation {
    ($const_vars:ident, $lhs:expr, $rhs:expr, $op:tt, $lhs_ty:ident, $rhs_ty:ident => $result_ty:ident, $og_ty:ident) => {{
        let lhs = propagate_const($const_vars, $lhs);
        let rhs = propagate_const($const_vars, $rhs);

        match (&lhs.0.expr, &rhs.0.expr) {
            (Expression::$lhs_ty(lhs), Expression::$rhs_ty(rhs)) => Expression::$result_ty(lhs $op rhs),
            _ => Expression::Operation(Box::new(Operation::$og_ty(lhs, rhs))),
        }
    }};
}
use const_propagate_binary_operation;

macro_rules! const_propagate_unary_operation {
    ($const_vars:ident, $rhs:expr, $op:tt, $rhs_ty:ident => $result_ty:ident, $og_ty:ident) => {{
        let rhs = propagate_const($const_vars, $rhs);

        match &rhs.0.expr {
            Expression::$rhs_ty(rhs) => Expression::$result_ty($op rhs),
            _ => Expression::Operation(Box::new(Operation::$og_ty(rhs))),
        }
    }};
}
use const_propagate_unary_operation;
