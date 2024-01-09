use crate::{
    error::Error,
    mir::{Expression, Operation, Type, TypedExpression, TypedStatement},
    scopes::Scopes,
    Spanned,
};

pub fn const_eval<'file>(
    ast: &mut [Spanned<TypedStatement<'_, 'file>>],
) -> Result<(), Vec<Box<Error<'file>>>> {
    let mut errors = vec![];

    let mut const_vars = Scopes::new();

    for statement in ast {
        if let Err(errs) = const_eval_statement(&mut const_vars, statement) {
            errors.extend(errs);
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn const_eval_statement<'src, 'file>(
    const_vars: &mut Scopes<&'src str, Spanned<'file, ConstValue<'file>>>,
    statement: &mut Spanned<TypedStatement<'src, 'file>>,
) -> Result<(), Vec<Box<Error<'file>>>> {
    match statement.0 {
        TypedStatement::Expr(ref mut expr) => {
            *expr = propagate_const(const_vars, expr);

            Ok(())
        }
        TypedStatement::BuiltinPrint(ref mut expr) => {
            *expr = propagate_const(const_vars, expr);

            Ok(())
        }
        TypedStatement::Loop(ref mut statements) => {
            let mut errors = vec![];

            const_vars.push_scope();

            for statement in &mut statements.0 {
                if let Err(errs) = const_eval_statement(const_vars, statement) {
                    errors.extend(errs);
                }
            }

            const_vars.pop_scope();

            if errors.is_empty() {
                Ok(())
            } else {
                Err(errors)
            }
        }
        TypedStatement::If {
            ref mut condition,
            ref mut then_branch,
            ref mut else_branch,
        } => {
            *condition = propagate_const(const_vars, condition);

            let mut errors = vec![];

            const_vars.push_scope();

            for statement in &mut then_branch.0 {
                if let Err(errs) = const_eval_statement(const_vars, statement) {
                    errors.extend(errs);
                }
            }

            const_vars.pop_scope();

            if let Some(else_branch) = else_branch {
                const_vars.push_scope();

                for statement in &mut else_branch.0 {
                    if let Err(errs) = const_eval_statement(const_vars, statement) {
                        errors.extend(errs);
                    }
                }

                const_vars.pop_scope();
            }

            if errors.is_empty() {
                Ok(())
            } else {
                Err(errors)
            }
        }
        TypedStatement::Let {
            name: _,
            ref mut value,
        } => {
            *value = propagate_const(const_vars, value);

            Ok(())
        }
        TypedStatement::Const {
            name,
            ref mut value,
        } => {
            let const_value = const_eval_expr(const_vars, value).map_err(|err| vec![err])?;

            *value = (
                TypedExpression {
                    expr: const_value.0.clone().into(),
                    ty: value.0.ty,
                },
                value.1,
            );

            const_vars.insert(name.0, const_value);

            Ok(())
        }
        TypedStatement::Assign {
            name: _,
            ref mut value,
        } => {
            *value = propagate_const(const_vars, value);

            Ok(())
        }
        TypedStatement::Break => Ok(()),
        TypedStatement::Continue => Ok(()),
    }
}

fn const_eval_expr<'file>(
    const_vars: &mut Scopes<&str, Spanned<ConstValue<'file>>>,
    expr: &Spanned<'file, TypedExpression<'_, 'file>>,
) -> Result<Spanned<'file, ConstValue<'file>>, Box<Error<'file>>> {
    Ok((
        match &expr.0.expr {
            Expression::Variable(name) => match const_vars.get(name) {
                Some(value) => value.0.clone(),
                None => {
                    return Err(Box::new(Error::UnknownConstVariable {
                        name: name.to_string(),
                        span: expr.1,
                    }))
                }
            },
            Expression::Boolean(value) => ConstValue::Boolean(*value),
            Expression::Integer(value) => ConstValue::Integer(*value),
            Expression::Float(value) => ConstValue::Float(*value),
            Expression::Colour { r, g, b } => ConstValue::Colour {
                r: *r,
                g: *g,
                b: *b,
            },
            Expression::Vector { x, y } => {
                let x = const_eval_expr(const_vars, x)?;
                let y = const_eval_expr(const_vars, y)?;

                ConstValue::Vector {
                    x: Box::new(x),
                    y: Box::new(y),
                }
            }
            Expression::Operation(operation) => match operation.as_ref() {
                Operation::IntegerEquals(lhs, rhs) => {
                    const_eval_binary_operation!(
                        const_vars,
                        lhs,
                        rhs,
                        Integer,
                        Integer,
                        Boolean,
                        ==
                    )
                }
                Operation::IntegerNotEquals(lhs, rhs) => {
                    const_eval_binary_operation!(
                        const_vars,
                        lhs,
                        rhs,
                        Integer,
                        Integer,
                        Boolean,
                        !=
                    )
                }
                Operation::IntegerPlus(lhs, rhs) => {
                    const_eval_binary_operation!(
                        const_vars,
                        lhs,
                        rhs,
                        Integer,
                        Integer,
                        Integer,
                        +
                    )
                }
                Operation::IntegerMinus(lhs, rhs) => {
                    const_eval_binary_operation!(
                        const_vars,
                        lhs,
                        rhs,
                        Integer,
                        Integer,
                        Integer,
                        -
                    )
                }
                Operation::IntegerMultiply(lhs, rhs) => {
                    const_eval_binary_operation!(
                        const_vars,
                        lhs,
                        rhs,
                        Integer,
                        Integer,
                        Integer,
                        *
                    )
                }
                Operation::IntegerDivide(lhs, rhs) => {
                    const_eval_binary_operation!(
                        const_vars,
                        lhs,
                        rhs,
                        Integer,
                        Integer,
                        Integer,
                        /
                    )
                }
                Operation::IntegerGreaterThanEquals(lhs, rhs) => {
                    const_eval_binary_operation!(
                        const_vars,
                        lhs,
                        rhs,
                        Integer,
                        Integer,
                        Boolean,
                        >=
                    )
                }
                Operation::IntegerLessThanEquals(lhs, rhs) => {
                    const_eval_binary_operation!(
                        const_vars,
                        lhs,
                        rhs,
                        Integer,
                        Integer,
                        Boolean,
                        <=
                    )
                }
                Operation::IntegerGreaterThan(lhs, rhs) => {
                    const_eval_binary_operation!(
                        const_vars,
                        lhs,
                        rhs,
                        Integer,
                        Integer,
                        Boolean,
                        >
                    )
                }
                Operation::IntegerLessThan(lhs, rhs) => {
                    const_eval_binary_operation!(
                        const_vars,
                        lhs,
                        rhs,
                        Integer,
                        Integer,
                        Boolean,
                        <
                    )
                }
                Operation::FloatEquals(lhs, rhs) => {
                    const_eval_binary_operation!(
                        const_vars,
                        lhs,
                        rhs,
                        Float,
                        Float,
                        Boolean,
                        ==
                    )
                }
                Operation::FloatNotEquals(lhs, rhs) => {
                    const_eval_binary_operation!(
                        const_vars,
                        lhs,
                        rhs,
                        Float,
                        Float,
                        Boolean,
                        !=
                    )
                }
                Operation::FloatPlus(lhs, rhs) => {
                    const_eval_binary_operation!(
                        const_vars,
                        lhs,
                        rhs,
                        Float,
                        Float,
                        Float,
                        +
                    )
                }
                Operation::FloatMinus(lhs, rhs) => {
                    const_eval_binary_operation!(
                        const_vars,
                        lhs,
                        rhs,
                        Float,
                        Float,
                        Float,
                        -
                    )
                }
                Operation::FloatMultiply(lhs, rhs) => {
                    const_eval_binary_operation!(
                        const_vars,
                        lhs,
                        rhs,
                        Float,
                        Float,
                        Float,
                        *
                    )
                }
                Operation::FloatDivide(lhs, rhs) => {
                    const_eval_binary_operation!(
                        const_vars,
                        lhs,
                        rhs,
                        Float,
                        Float,
                        Float,
                        /
                    )
                }
                Operation::FloatGreaterThanEquals(lhs, rhs) => {
                    const_eval_binary_operation!(
                        const_vars,
                        lhs,
                        rhs,
                        Float,
                        Float,
                        Boolean,
                        >=
                    )
                }
                Operation::FloatLessThanEquals(lhs, rhs) => {
                    const_eval_binary_operation!(
                        const_vars,
                        lhs,
                        rhs,
                        Float,
                        Float,
                        Boolean,
                        <=
                    )
                }
                Operation::FloatGreaterThan(lhs, rhs) => {
                    const_eval_binary_operation!(
                        const_vars,
                        lhs,
                        rhs,
                        Float,
                        Float,
                        Boolean,
                        >
                    )
                }
                Operation::FloatLessThan(lhs, rhs) => {
                    const_eval_binary_operation!(
                        const_vars,
                        lhs,
                        rhs,
                        Float,
                        Float,
                        Boolean,
                        <
                    )
                }
                Operation::BooleanEquals(lhs, rhs) => {
                    const_eval_binary_operation!(
                        const_vars,
                        lhs,
                        rhs,
                        Boolean,
                        Boolean,
                        Boolean,
                        ==
                    )
                }
                Operation::BooleanNotEquals(lhs, rhs) => {
                    const_eval_binary_operation!(
                        const_vars,
                        lhs,
                        rhs,
                        Boolean,
                        Boolean,
                        Boolean,
                        !=
                    )
                }
                Operation::IntegerNegate(rhs) => {
                    const_eval_unary_operation!(const_vars, rhs, Integer, Integer, -)
                }
                Operation::FloatNegate(rhs) => {
                    const_eval_unary_operation!(const_vars, rhs, Float, Float, -)
                }
                Operation::BooleanNot(rhs) => {
                    const_eval_unary_operation!(const_vars, rhs, Boolean, Boolean, !)
                }
            },
        },
        expr.1,
    ))
}

fn propagate_const<'src, 'file>(
    const_vars: &mut Scopes<&'src str, Spanned<ConstValue<'file>>>,
    expr: &Spanned<'file, TypedExpression<'src, 'file>>,
) -> Spanned<'file, TypedExpression<'src, 'file>> {
    (
        TypedExpression {
            expr: match &expr.0.expr {
                Expression::Variable(name) => match const_vars.get(name) {
                    Some(value) => Expression::from(value.0.clone()),
                    None => Expression::Variable(name),
                },
                Expression::Boolean(value) => Expression::Boolean(*value),
                Expression::Integer(value) => Expression::Integer(*value),
                Expression::Float(value) => Expression::Float(*value),
                Expression::Colour { r, g, b } => Expression::Colour {
                    r: *r,
                    g: *g,
                    b: *b,
                },
                Expression::Vector { x, y } => Expression::Vector {
                    x: Box::new(propagate_const(const_vars, x)),
                    y: Box::new(propagate_const(const_vars, y)),
                },
                Expression::Operation(operation) => match operation.as_ref() {
                    Operation::IntegerEquals(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs,
                            rhs,
                            ==,
                            Integer,
                            Integer,
                            Boolean,
                            IntegerEquals
                        )
                    }
                    Operation::IntegerNotEquals(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs,
                            rhs,
                            !=,
                            Integer,
                            Integer,
                            Boolean,
                            IntegerNotEquals
                        )
                    }
                    Operation::IntegerPlus(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs,
                            rhs,
                            +,
                            Integer,
                            Integer,
                            Integer,
                            IntegerPlus
                        )
                    }
                    Operation::IntegerMinus(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs,
                            rhs,
                            -,
                            Integer,
                            Integer,
                            Integer,
                            IntegerMinus
                        )
                    }
                    Operation::IntegerMultiply(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs,
                            rhs,
                            *,
                            Integer,
                            Integer,
                            Integer,
                            IntegerMultiply
                        )
                    }
                    Operation::IntegerDivide(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs,
                            rhs,
                            /,
                            Integer,
                            Integer,
                            Integer,
                            IntegerDivide
                        )
                    }
                    Operation::IntegerGreaterThanEquals(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs,
                            rhs,
                            >=,
                            Integer,
                            Integer,
                            Boolean,
                            IntegerGreaterThanEquals
                        )
                    }
                    Operation::IntegerLessThanEquals(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs,
                            rhs,
                            <=,
                            Integer,
                            Integer,
                            Boolean,
                            IntegerLessThanEquals
                        )
                    }
                    Operation::IntegerGreaterThan(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs,
                            rhs,
                            >,
                            Integer,
                            Integer,
                            Boolean,
                            IntegerGreaterThan
                        )
                    }
                    Operation::IntegerLessThan(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs,
                            rhs,
                            <,
                            Integer,
                            Integer,
                            Boolean,
                            IntegerLessThan
                        )
                    }
                    Operation::FloatEquals(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs,
                            rhs,
                            ==,
                            Float,
                            Float,
                            Boolean,
                            FloatEquals
                        )
                    }
                    Operation::FloatNotEquals(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs,
                            rhs,
                            !=,
                            Float,
                            Float,
                            Boolean,
                            FloatNotEquals
                        )
                    }
                    Operation::FloatPlus(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs,
                            rhs,
                            +,
                            Float,
                            Float,
                            Float,
                            FloatPlus
                        )
                    }
                    Operation::FloatMinus(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs,
                            rhs,
                            -,
                            Float,
                            Float,
                            Float,
                            FloatMinus
                        )
                    }
                    Operation::FloatMultiply(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs,
                            rhs,
                            *,
                            Float,
                            Float,
                            Float,
                            FloatMultiply
                        )
                    }
                    Operation::FloatDivide(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs,
                            rhs,
                            /,
                            Float,
                            Float,
                            Float,
                            FloatDivide
                        )
                    }
                    Operation::FloatGreaterThanEquals(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs,
                            rhs,
                            >=,
                            Float,
                            Float,
                            Boolean,
                            FloatGreaterThanEquals
                        )
                    }
                    Operation::FloatLessThanEquals(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs,
                            rhs,
                            <=,
                            Float,
                            Float,
                            Boolean,
                            FloatLessThanEquals
                        )
                    }
                    Operation::FloatGreaterThan(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs,
                            rhs,
                            >,
                            Float,
                            Float,
                            Boolean,
                            FloatGreaterThan
                        )
                    }
                    Operation::FloatLessThan(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs,
                            rhs,
                            <,
                            Float,
                            Float,
                            Boolean,
                            FloatLessThan
                        )
                    }
                    Operation::BooleanEquals(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs,
                            rhs,
                            ==,
                            Boolean,
                            Boolean,
                            Boolean,
                            BooleanEquals
                        )
                    }
                    Operation::BooleanNotEquals(lhs, rhs) => {
                        const_propagate_binary_operation!(
                            const_vars,
                            lhs,
                            rhs,
                            !=,
                            Boolean,
                            Boolean,
                            Boolean,
                            BooleanNotEquals
                        )
                    }
                    Operation::IntegerNegate(rhs) => {
                        const_propagate_unary_operation!(
                            const_vars,
                            -rhs,
                            Integer,
                            Integer,
                            IntegerNegate
                        )
                    }
                    Operation::FloatNegate(rhs) => {
                        const_propagate_unary_operation!(
                            const_vars,
                            -rhs,
                            Float,
                            Float,
                            FloatNegate
                        )
                    }
                    Operation::BooleanNot(rhs) => {
                        const_propagate_unary_operation!(
                            const_vars, !rhs, Boolean, Boolean, BooleanNot
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

macro_rules! const_eval_binary_operation {
    ($const_vars:ident, $lhs:expr, $rhs:expr, $lhs_ty:ident, $rhs_ty:ident, $result_ty:ident, $op:tt) => {{
        let lhs = const_eval_expr($const_vars, $lhs)?;
        let rhs = const_eval_expr($const_vars, $rhs)?;

        match (lhs.0, rhs.0) {
            (ConstValue::$lhs_ty(lhs), ConstValue::$rhs_ty(rhs)) => ConstValue::$result_ty(lhs $op rhs),
            _ => unreachable!(),
        }
    }};
}
use const_eval_binary_operation;

macro_rules! const_eval_unary_operation {
    ($const_vars:ident, $rhs:expr, $rhs_ty:ident, $result_ty:ident, $op:tt) => {{
        let rhs = const_eval_expr($const_vars, $rhs)?;

        match rhs.0 {
            ConstValue::$rhs_ty(rhs) => ConstValue::$result_ty($op rhs),
            _ => unreachable!(),
        }
    }};
}
use const_eval_unary_operation;

macro_rules! const_propagate_binary_operation {
    ($const_vars:ident, $lhs:expr, $rhs:expr, $op:tt, $lhs_ty:ident, $rhs_ty:ident, $result_ty:ident, $og_ty:ident) => {{
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
    ($const_vars:ident, $op:tt $rhs:expr, $rhs_ty:ident, $result_ty:ident, $og_ty:ident) => {{
        let rhs = propagate_const($const_vars, $rhs);

        match &rhs.0.expr {
            Expression::$rhs_ty(rhs) => Expression::$result_ty($op rhs),
            _ => Expression::Operation(Box::new(Operation::$og_ty(rhs))),
        }
    }};
}
use const_propagate_unary_operation;
