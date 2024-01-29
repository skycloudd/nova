use crate::{
    error::Error,
    mir::{self, Expression, Operation, Type, TypedExpression, TypedStatement, VarId},
    scopes::Scopes,
    span::Spanned,
    FloatTy, IntTy,
};

pub fn const_eval<'file>(
    ast: Vec<Spanned<'file, TypedStatement<'file>>>,
) -> (
    Result<Vec<Spanned<'file, TypedStatement<'file>>>, ()>,
    Vec<Box<Error<'file>>>,
) {
    let mut errors = vec![];

    let mut const_vars = Scopes::new();

    let mut statements = vec![];

    for statement in ast {
        if let Ok(statement) = const_eval_statement(&mut const_vars, &mut errors, statement) {
            statements.push(statement);
        }
    }

    if errors.is_empty() {
        (Ok(statements), errors)
    } else {
        (Err(()), errors)
    }
}

fn const_eval_statement<'file>(
    const_vars: &mut Scopes<VarId, Spanned<'file, ConstValue<'file>>>,
    errors: &mut Vec<Box<Error<'file>>>,
    statement: Spanned<'file, TypedStatement<'file>>,
) -> Result<Spanned<'file, TypedStatement<'file>>, ()> {
    match statement.0 {
        TypedStatement::Expr(expr) => Ok(TypedStatement::Expr(propagate_const(const_vars, expr))),
        TypedStatement::Print(expr) => Ok(TypedStatement::Print(propagate_const(const_vars, expr))),
        TypedStatement::Block(statements) => {
            let mut stmts = vec![];

            for statement in statements.0 {
                if let Ok(statement) = const_eval_statement(const_vars, errors, statement) {
                    stmts.push(statement);
                }
            }

            Ok(TypedStatement::Block(Spanned(stmts, statements.1)))
        }
        TypedStatement::Loop(statements) => {
            let mut stmts = vec![];

            for statement in statements.0 {
                if let Ok(statement) = const_eval_statement(const_vars, errors, statement) {
                    stmts.push(statement);
                }
            }

            Ok(TypedStatement::Loop(Spanned(stmts, statements.1)))
        }
        TypedStatement::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let condition = propagate_const(const_vars, condition);

            let mut then = vec![];

            for statement in then_branch.0 {
                if let Ok(statement) = const_eval_statement(const_vars, errors, statement) {
                    then.push(statement);
                }
            }

            let else_ = else_branch.map(|else_branch| {
                let mut else_ = vec![];

                for statement in else_branch.0 {
                    if let Ok(statement) = const_eval_statement(const_vars, errors, statement) {
                        else_.push(statement);
                    }
                }

                Spanned(else_, else_branch.1)
            });

            Ok(TypedStatement::If {
                condition,
                then_branch: Spanned(then, then_branch.1),
                else_branch: else_,
            })
        }
        TypedStatement::For {
            name,
            start,
            end,
            inclusive,
            body,
        } => {
            let start = propagate_const(const_vars, start);
            let end = propagate_const(const_vars, end);

            let mut body_stmts = vec![];

            for statement in body.0 {
                if let Ok(statement) = const_eval_statement(const_vars, errors, statement) {
                    body_stmts.push(statement);
                }
            }

            Ok(TypedStatement::For {
                name,
                start,
                end,
                inclusive,
                body: Spanned(body_stmts, body.1),
            })
        }
        TypedStatement::Let { name, value } => {
            let value = propagate_const(const_vars, value);

            Ok(TypedStatement::Let { name, value })
        }
        TypedStatement::Const { name, value: expr } => const_eval_expr(const_vars, errors, expr)
            .map(|value| {
                const_vars.insert(*name, value.clone());

                TypedStatement::Const {
                    name,
                    value: value.map(Into::into),
                }
            }),
        TypedStatement::Assign { name, value } => {
            let value = propagate_const(const_vars, value);

            Ok(TypedStatement::Assign { name, value })
        }
        TypedStatement::Break => Ok(TypedStatement::Break),
        TypedStatement::Continue => Ok(TypedStatement::Continue),
        TypedStatement::Action { name, args } => {
            let args = Spanned(
                args.0
                    .into_iter()
                    .map(|arg| propagate_const(const_vars, arg))
                    .collect(),
                args.1,
            );

            Ok(TypedStatement::Action { name, args })
        }
    }
    .map(|stmt| Spanned(stmt, statement.1))
}

fn const_eval_expr<'file>(
    const_vars: &mut Scopes<VarId, Spanned<ConstValue<'file>>>,
    errors: &mut Vec<Box<Error<'file>>>,
    expr: Spanned<'file, TypedExpression<'file>>,
) -> Result<Spanned<'file, ConstValue<'file>>, ()> {
    match expr.0.expr {
        Expression::Variable(name) => {
            if let Some(value) = const_vars.get(&name) {
                Ok(value.0.clone())
            } else {
                errors.push(Box::new(Error::UnknownConstVariable {
                    name: name.to_string(),
                    span: expr.1,
                }));

                Err(())
            }
        }
        Expression::Boolean(value) => Ok(ConstValue::Boolean(value)),
        Expression::Integer(value) => Ok(ConstValue::Integer(value)),
        Expression::Float(value) => Ok(ConstValue::Float(value)),
        Expression::Colour { r, g, b, a } => Ok(ConstValue::Colour { r, g, b, a }),
        Expression::Vector { x, y } => {
            let x = const_eval_expr(const_vars, errors, x.map(|x| *x));
            let y = const_eval_expr(const_vars, errors, y.map(|y| *y));

            match (x, y) {
                (Ok(x), Ok(y)) => Ok(ConstValue::Vector {
                    x: x.map(Box::new),
                    y: y.map(Box::new),
                }),
                _ => Err(()),
            }
        }
        Expression::Object(object) => Ok(ConstValue::Object(match object {
            mir::Object::Player => Object::Player,
        })),
        Expression::Operation(operation) => Ok(match *operation {
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

fn propagate_const<'file>(
    const_vars: &mut Scopes<VarId, Spanned<ConstValue<'file>>>,
    expr: Spanned<'file, TypedExpression<'file>>,
) -> Spanned<'file, TypedExpression<'file>> {
    expr.map(|expr| TypedExpression {
        expr: match expr.expr {
            Expression::Variable(name) => const_vars
                .get(&name)
                .map_or(Expression::Variable(name), |value| {
                    Expression::from(value.0.clone())
                }),
            Expression::Boolean(value) => Expression::Boolean(value),
            Expression::Integer(value) => Expression::Integer(value),
            Expression::Float(value) => Expression::Float(value),
            Expression::Colour { r, g, b, a } => Expression::Colour { r, g, b, a },
            Expression::Vector { x, y } => Expression::Vector {
                x: propagate_const(const_vars, x.map(|x| *x)).map(Box::new),
                y: propagate_const(const_vars, y.map(|y| *y)).map(Box::new),
            },
            Expression::Object(object) => Expression::Object(object),
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
        ty: expr.ty,
    })
}

#[derive(Clone, PartialEq)]
enum ConstValue<'file> {
    Boolean(bool),
    Integer(IntTy),
    Float(FloatTy),
    Colour {
        r: u8,
        g: u8,
        b: u8,
        a: u8,
    },
    Vector {
        x: Spanned<'file, Box<ConstValue<'file>>>,
        y: Spanned<'file, Box<ConstValue<'file>>>,
    },
    Object(Object),
}

#[derive(Clone, PartialEq)]
enum Object {
    Player,
}

impl<'file> From<ConstValue<'file>> for Expression<'file> {
    fn from(expr: ConstValue<'file>) -> Self {
        match expr {
            ConstValue::Boolean(value) => Expression::Boolean(value),
            ConstValue::Integer(value) => Expression::Integer(value),
            ConstValue::Float(value) => Expression::Float(value),
            ConstValue::Colour { r, g, b, a } => Expression::Colour { r, g, b, a },
            ConstValue::Vector { x, y } => Expression::Vector {
                x: x.map(|x| TypedExpression {
                    expr: Expression::from(*x),
                    ty: Type::Integer,
                })
                .map(Box::new),
                y: y.map(|y| TypedExpression {
                    expr: Expression::from(*y),
                    ty: Type::Integer,
                })
                .map(Box::new),
            },
            ConstValue::Object(object) => Expression::Object(match object {
                Object::Player => mir::Object::Player,
            }),
        }
    }
}

impl<'file> From<ConstValue<'file>> for TypedExpression<'file> {
    fn from(const_value: ConstValue<'file>) -> Self {
        let expr = const_value.clone().into();

        let ty = match const_value {
            ConstValue::Boolean(_) => Type::Boolean,
            ConstValue::Integer(_) => Type::Integer,
            ConstValue::Float(_) => Type::Float,
            ConstValue::Colour { .. } => Type::Colour,
            ConstValue::Vector { .. } => Type::Vector,
            ConstValue::Object(_) => Type::Object,
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
