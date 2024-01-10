use crate::{
    error::Error,
    mir::{Expression, Operation, Type, TypedExpression, TypedStatement},
    scopes::Scopes,
    Spanned,
};

pub fn const_eval<'src, 'file>(
    ast: Vec<Spanned<'file, TypedStatement<'src, 'file>>>,
) -> (
    Option<Vec<Spanned<'file, TypedStatement<'src, 'file>>>>,
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
    const_vars: &mut Scopes<&'src str, Spanned<'file, ConstValue<'file>>>,
    errors: &mut Vec<Box<Error<'file>>>,
    statement: Spanned<'file, TypedStatement<'src, 'file>>,
) -> Option<Spanned<'file, TypedStatement<'src, 'file>>> {
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

            Some(TypedStatement::Loop((stmts, statements.1)))
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

                (else_, else_branch.1)
            });

            Some(TypedStatement::If {
                condition,
                then_branch: (then, then_branch.1),
                else_branch: else_,
            })
        }
        TypedStatement::Let { name, value } => {
            let value = propagate_const(const_vars, value);

            Some(TypedStatement::Let { name, value })
        }
        TypedStatement::Const { name, value: expr } => const_eval_expr(const_vars, errors, expr)
            .map(|value| {
                const_vars.insert(name.0, value.clone());

                let value = constvalue_to_typed_expression(value);

                Some(TypedStatement::Const { name, value })
            })
            .flatten(),
        TypedStatement::Assign { name, value } => {
            let value = propagate_const(const_vars, value);

            Some(TypedStatement::Assign { name, value })
        }
        TypedStatement::Break => Some(TypedStatement::Break),
        TypedStatement::Continue => Some(TypedStatement::Continue),
    }
    .map(|stmt| (stmt, statement.1))
}

fn const_eval_expr<'file>(
    const_vars: &mut Scopes<&str, Spanned<ConstValue<'file>>>,
    errors: &mut Vec<Box<Error<'file>>>,
    expr: Spanned<'file, TypedExpression<'_, 'file>>,
) -> Option<Spanned<'file, ConstValue<'file>>> {
    Some((
        match expr.0.expr {
            Expression::Variable(name) => match const_vars.get(&name) {
                Some(value) => value.0.clone(),
                None => {
                    errors.push(Box::new(Error::UnknownConstVariable {
                        name: name.to_string(),
                        span: expr.1,
                    }));

                    return None;
                }
            },
            Expression::Boolean(value) => ConstValue::Boolean(value),
            Expression::Integer(value) => ConstValue::Integer(value),
            Expression::Float(value) => ConstValue::Float(value),
            Expression::Colour { r, g, b } => ConstValue::Colour { r, g, b },
            Expression::Vector { x, y } => {
                let x = const_eval_expr(const_vars, errors, *x);
                let y = const_eval_expr(const_vars, errors, *y);

                match (x, y) {
                    (Some(x), Some(y)) => ConstValue::Vector {
                        x: Box::new(x),
                        y: Box::new(y),
                    },
                    _ => return None,
                }
            }
            Expression::Operation(operation) => match *operation {
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
            },
        },
        expr.1,
    ))
}

fn propagate_const<'src, 'file>(
    const_vars: &mut Scopes<&'src str, Spanned<ConstValue<'file>>>,
    expr: Spanned<'file, TypedExpression<'src, 'file>>,
) -> Spanned<'file, TypedExpression<'src, 'file>> {
    (
        TypedExpression {
            expr: match expr.0.expr {
                Expression::Variable(name) => match const_vars.get(&name) {
                    Some(value) => Expression::from(value.0.clone()),
                    None => Expression::Variable(name),
                },
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

impl<'src, 'file> From<ConstValue<'file>> for Expression<'src, 'file> {
    fn from(expr: ConstValue<'file>) -> Self {
        match expr {
            ConstValue::Boolean(value) => Expression::Boolean(value),
            ConstValue::Integer(value) => Expression::Integer(value),
            ConstValue::Float(value) => Expression::Float(value),
            ConstValue::Colour { r, g, b } => Expression::Colour { r, g, b },
            ConstValue::Vector { x, y } => Expression::Vector {
                x: Box::new((
                    TypedExpression {
                        expr: Expression::from(x.0),
                        ty: Type::Integer,
                    },
                    x.1,
                )),
                y: Box::new((
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

fn constvalue_to_typed_expression<'src, 'file>(
    const_value: Spanned<'file, ConstValue<'file>>,
) -> Spanned<'file, TypedExpression<'src, 'file>> {
    let expr = const_value.0.clone().into();

    let ty = match const_value.0 {
        ConstValue::Boolean(_) => Type::Boolean,
        ConstValue::Integer(_) => Type::Integer,
        ConstValue::Float(_) => Type::Float,
        ConstValue::Colour { .. } => Type::Colour,
        ConstValue::Vector { .. } => Type::Vector,
    };

    (TypedExpression { expr, ty }, const_value.1)
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
