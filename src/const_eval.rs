use crate::{
    error::Error,
    mir::{Expression, Operation, TypedExpression, TypedStatement},
    scopes::Scopes,
    Spanned,
};

pub fn const_eval(ast: &mut [Spanned<TypedStatement>]) -> Result<(), Vec<Error>> {
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

fn const_eval_statement<'src>(
    const_vars: &mut Scopes<&'src str, Spanned<ConstValue>>,
    statement: &mut Spanned<TypedStatement<'src>>,
) -> Result<(), Vec<Error>> {
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
                    expr: const_value.0.into(),
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

fn const_eval_expr<'src>(
    const_vars: &mut Scopes<&'src str, Spanned<ConstValue>>,
    expr: &Spanned<TypedExpression<'src>>,
) -> Result<Spanned<ConstValue>, Error> {
    Ok((
        match &expr.0.expr {
            Expression::Variable(name) => match const_vars.get(name) {
                Some(value) => value.0,
                None => {
                    return Err(Error::UnknownConstVariable {
                        name: name.to_string(),
                        span: expr.1,
                    })
                }
            },
            Expression::Boolean(value) => ConstValue::Boolean(*value),
            Expression::Integer(value) => ConstValue::Integer(*value),
            Expression::Null => ConstValue::Null,
            Expression::Colour { r, g, b } => ConstValue::Colour {
                r: *r,
                g: *g,
                b: *b,
            },
            Expression::Operation(operation) => match operation.as_ref() {
                Operation::IntegerEquals(lhs, rhs) => {
                    let lhs = const_eval_expr(const_vars, lhs)?;
                    let rhs = const_eval_expr(const_vars, rhs)?;

                    const_eval_operation!(
                        IntegerEquals,
                        lhs,
                        rhs,
                        Integer,
                        Integer,
                        Boolean,
                        lhs == rhs
                    )
                }
                Operation::IntegerNotEquals(lhs, rhs) => {
                    let lhs = const_eval_expr(const_vars, lhs)?;
                    let rhs = const_eval_expr(const_vars, rhs)?;

                    const_eval_operation!(
                        IntegerNotEquals,
                        lhs,
                        rhs,
                        Integer,
                        Integer,
                        Boolean,
                        lhs != rhs
                    )
                }
                Operation::IntegerPlus(lhs, rhs) => {
                    let lhs = const_eval_expr(const_vars, lhs)?;
                    let rhs = const_eval_expr(const_vars, rhs)?;

                    const_eval_operation!(
                        IntegerPlus,
                        lhs,
                        rhs,
                        Integer,
                        Integer,
                        Integer,
                        lhs + rhs
                    )
                }
                Operation::IntegerMinus(lhs, rhs) => {
                    let lhs = const_eval_expr(const_vars, lhs)?;
                    let rhs = const_eval_expr(const_vars, rhs)?;

                    const_eval_operation!(
                        IntegerMinus,
                        lhs,
                        rhs,
                        Integer,
                        Integer,
                        Integer,
                        lhs - rhs
                    )
                }
                Operation::IntegerMultiply(lhs, rhs) => {
                    let lhs = const_eval_expr(const_vars, lhs)?;
                    let rhs = const_eval_expr(const_vars, rhs)?;

                    const_eval_operation!(
                        IntegerMultiply,
                        lhs,
                        rhs,
                        Integer,
                        Integer,
                        Integer,
                        lhs * rhs
                    )
                }
                Operation::IntegerDivide(lhs, rhs) => {
                    let lhs = const_eval_expr(const_vars, lhs)?;
                    let rhs = const_eval_expr(const_vars, rhs)?;

                    const_eval_operation!(
                        IntegerDivide,
                        lhs,
                        rhs,
                        Integer,
                        Integer,
                        Integer,
                        lhs / rhs
                    )
                }
                Operation::IntegerGreaterThanEquals(lhs, rhs) => {
                    let lhs = const_eval_expr(const_vars, lhs)?;
                    let rhs = const_eval_expr(const_vars, rhs)?;

                    const_eval_operation!(
                        IntegerGreaterThanEquals,
                        lhs,
                        rhs,
                        Integer,
                        Integer,
                        Boolean,
                        lhs >= rhs
                    )
                }
                Operation::IntegerLessThanEquals(lhs, rhs) => {
                    let lhs = const_eval_expr(const_vars, lhs)?;
                    let rhs = const_eval_expr(const_vars, rhs)?;

                    const_eval_operation!(
                        IntegerLessThanEquals,
                        lhs,
                        rhs,
                        Integer,
                        Integer,
                        Boolean,
                        lhs <= rhs
                    )
                }
                Operation::IntegerGreaterThan(lhs, rhs) => {
                    let lhs = const_eval_expr(const_vars, lhs)?;
                    let rhs = const_eval_expr(const_vars, rhs)?;

                    const_eval_operation!(
                        IntegerGreaterThan,
                        lhs,
                        rhs,
                        Integer,
                        Integer,
                        Boolean,
                        lhs > rhs
                    )
                }
                Operation::IntegerLessThan(lhs, rhs) => {
                    let lhs = const_eval_expr(const_vars, lhs)?;
                    let rhs = const_eval_expr(const_vars, rhs)?;

                    const_eval_operation!(
                        IntegerLessThan,
                        lhs,
                        rhs,
                        Integer,
                        Integer,
                        Boolean,
                        lhs < rhs
                    )
                }
                Operation::BooleanEquals(lhs, rhs) => {
                    let lhs = const_eval_expr(const_vars, lhs)?;
                    let rhs = const_eval_expr(const_vars, rhs)?;

                    const_eval_operation!(
                        BooleanEquals,
                        lhs,
                        rhs,
                        Boolean,
                        Boolean,
                        Boolean,
                        lhs == rhs
                    )
                }
                Operation::BooleanNotEquals(lhs, rhs) => {
                    let lhs = const_eval_expr(const_vars, lhs)?;
                    let rhs = const_eval_expr(const_vars, rhs)?;

                    match (lhs.0, rhs.0) {
                        (ConstValue::Boolean(lhs), ConstValue::Boolean(rhs)) => {
                            ConstValue::Boolean(lhs != rhs)
                        }
                        _ => unreachable!(),
                    }
                }
                Operation::NullEquals(lhs, rhs) => {
                    let lhs = const_eval_expr(const_vars, lhs)?;
                    let rhs = const_eval_expr(const_vars, rhs)?;

                    match (lhs.0, rhs.0) {
                        (ConstValue::Null, ConstValue::Null) => ConstValue::Boolean(true),
                        _ => unreachable!(),
                    }
                }
                Operation::NullNotEquals(lhs, rhs) => {
                    let lhs = const_eval_expr(const_vars, lhs)?;
                    let rhs = const_eval_expr(const_vars, rhs)?;

                    match (lhs.0, rhs.0) {
                        (ConstValue::Null, ConstValue::Null) => ConstValue::Boolean(false),
                        _ => unreachable!(),
                    }
                }
                Operation::IntegerNegate(rhs) => {
                    let rhs = const_eval_expr(const_vars, rhs)?;

                    match rhs.0 {
                        ConstValue::Integer(rhs) => ConstValue::Integer(-rhs),
                        _ => unreachable!(),
                    }
                }
                Operation::BooleanNot(rhs) => {
                    let rhs = const_eval_expr(const_vars, rhs)?;

                    match rhs.0 {
                        ConstValue::Boolean(rhs) => ConstValue::Boolean(!rhs),
                        _ => unreachable!(),
                    }
                }
            },
        },
        expr.1,
    ))
}

fn propagate_const<'src>(
    const_vars: &mut Scopes<&'src str, Spanned<ConstValue>>,
    expr: &Spanned<TypedExpression<'src>>,
) -> Spanned<TypedExpression<'src>> {
    (
        TypedExpression {
            expr: match &expr.0.expr {
                Expression::Variable(name) => match const_vars.get(name) {
                    Some(value) => Expression::from(value.0),
                    None => Expression::Variable(name),
                },
                Expression::Boolean(value) => Expression::Boolean(*value),
                Expression::Integer(value) => Expression::Integer(*value),
                Expression::Null => Expression::Null,
                Expression::Colour { r, g, b } => Expression::Colour {
                    r: *r,
                    g: *g,
                    b: *b,
                },
                Expression::Operation(operation) => match operation.as_ref() {
                    Operation::IntegerEquals(lhs, rhs) => {
                        let lhs = propagate_const(const_vars, lhs);
                        let rhs = propagate_const(const_vars, rhs);

                        match (&lhs.0.expr, &rhs.0.expr) {
                            (Expression::Integer(lhs), Expression::Integer(rhs)) => {
                                Expression::Boolean(lhs == rhs)
                            }
                            _ => {
                                Expression::Operation(Box::new(Operation::IntegerEquals(lhs, rhs)))
                            }
                        }
                    }
                    Operation::IntegerNotEquals(lhs, rhs) => {
                        let lhs = propagate_const(const_vars, lhs);
                        let rhs = propagate_const(const_vars, rhs);

                        match (&lhs.0.expr, &rhs.0.expr) {
                            (Expression::Integer(lhs), Expression::Integer(rhs)) => {
                                Expression::Boolean(lhs != rhs)
                            }
                            _ => Expression::Operation(Box::new(Operation::IntegerNotEquals(
                                lhs, rhs,
                            ))),
                        }
                    }
                    Operation::IntegerPlus(lhs, rhs) => {
                        let lhs = propagate_const(const_vars, lhs);
                        let rhs = propagate_const(const_vars, rhs);

                        match (&lhs.0.expr, &rhs.0.expr) {
                            (Expression::Integer(lhs), Expression::Integer(rhs)) => {
                                Expression::Integer(lhs + rhs)
                            }
                            _ => Expression::Operation(Box::new(Operation::IntegerPlus(lhs, rhs))),
                        }
                    }
                    Operation::IntegerMinus(lhs, rhs) => {
                        let lhs = propagate_const(const_vars, lhs);
                        let rhs = propagate_const(const_vars, rhs);

                        match (&lhs.0.expr, &rhs.0.expr) {
                            (Expression::Integer(lhs), Expression::Integer(rhs)) => {
                                Expression::Integer(lhs - rhs)
                            }
                            _ => Expression::Operation(Box::new(Operation::IntegerMinus(lhs, rhs))),
                        }
                    }
                    Operation::IntegerMultiply(lhs, rhs) => {
                        let lhs = propagate_const(const_vars, lhs);
                        let rhs = propagate_const(const_vars, rhs);

                        match (&lhs.0.expr, &rhs.0.expr) {
                            (Expression::Integer(lhs), Expression::Integer(rhs)) => {
                                Expression::Integer(lhs * rhs)
                            }
                            _ => Expression::Operation(Box::new(Operation::IntegerMultiply(
                                lhs, rhs,
                            ))),
                        }
                    }
                    Operation::IntegerDivide(lhs, rhs) => {
                        let lhs = propagate_const(const_vars, lhs);
                        let rhs = propagate_const(const_vars, rhs);

                        match (&lhs.0.expr, &rhs.0.expr) {
                            (Expression::Integer(lhs), Expression::Integer(rhs)) => {
                                Expression::Integer(lhs / rhs)
                            }
                            _ => {
                                Expression::Operation(Box::new(Operation::IntegerDivide(lhs, rhs)))
                            }
                        }
                    }
                    Operation::IntegerGreaterThanEquals(lhs, rhs) => {
                        let lhs = propagate_const(const_vars, lhs);
                        let rhs = propagate_const(const_vars, rhs);

                        match (&lhs.0.expr, &rhs.0.expr) {
                            (Expression::Integer(lhs), Expression::Integer(rhs)) => {
                                Expression::Boolean(lhs >= rhs)
                            }
                            _ => Expression::Operation(Box::new(
                                Operation::IntegerGreaterThanEquals(lhs, rhs),
                            )),
                        }
                    }
                    Operation::IntegerLessThanEquals(lhs, rhs) => {
                        let lhs = propagate_const(const_vars, lhs);
                        let rhs = propagate_const(const_vars, rhs);

                        match (&lhs.0.expr, &rhs.0.expr) {
                            (Expression::Integer(lhs), Expression::Integer(rhs)) => {
                                Expression::Boolean(lhs <= rhs)
                            }
                            _ => Expression::Operation(Box::new(Operation::IntegerLessThanEquals(
                                lhs, rhs,
                            ))),
                        }
                    }
                    Operation::IntegerGreaterThan(lhs, rhs) => {
                        let lhs = propagate_const(const_vars, lhs);
                        let rhs = propagate_const(const_vars, rhs);

                        match (&lhs.0.expr, &rhs.0.expr) {
                            (Expression::Integer(lhs), Expression::Integer(rhs)) => {
                                Expression::Boolean(lhs > rhs)
                            }
                            _ => Expression::Operation(Box::new(Operation::IntegerGreaterThan(
                                lhs, rhs,
                            ))),
                        }
                    }
                    Operation::IntegerLessThan(lhs, rhs) => {
                        let lhs = propagate_const(const_vars, lhs);
                        let rhs = propagate_const(const_vars, rhs);

                        match (&lhs.0.expr, &rhs.0.expr) {
                            (Expression::Integer(lhs), Expression::Integer(rhs)) => {
                                Expression::Boolean(lhs < rhs)
                            }
                            _ => Expression::Operation(Box::new(Operation::IntegerLessThan(
                                lhs, rhs,
                            ))),
                        }
                    }
                    Operation::BooleanEquals(lhs, rhs) => {
                        let lhs = propagate_const(const_vars, lhs);
                        let rhs = propagate_const(const_vars, rhs);

                        match (&lhs.0.expr, &rhs.0.expr) {
                            (Expression::Boolean(lhs), Expression::Boolean(rhs)) => {
                                Expression::Boolean(lhs == rhs)
                            }
                            _ => {
                                Expression::Operation(Box::new(Operation::BooleanEquals(lhs, rhs)))
                            }
                        }
                    }
                    Operation::BooleanNotEquals(lhs, rhs) => {
                        let lhs = propagate_const(const_vars, lhs);
                        let rhs = propagate_const(const_vars, rhs);

                        match (&lhs.0.expr, &rhs.0.expr) {
                            (Expression::Boolean(lhs), Expression::Boolean(rhs)) => {
                                Expression::Boolean(lhs != rhs)
                            }
                            _ => Expression::Operation(Box::new(Operation::BooleanNotEquals(
                                lhs, rhs,
                            ))),
                        }
                    }
                    Operation::NullEquals(lhs, rhs) => {
                        let lhs = propagate_const(const_vars, lhs);
                        let rhs = propagate_const(const_vars, rhs);

                        match (&lhs.0.expr, &rhs.0.expr) {
                            (Expression::Null, Expression::Null) => Expression::Boolean(true),
                            _ => Expression::Operation(Box::new(Operation::NullEquals(lhs, rhs))),
                        }
                    }
                    Operation::NullNotEquals(lhs, rhs) => {
                        let lhs = propagate_const(const_vars, lhs);
                        let rhs = propagate_const(const_vars, rhs);

                        match (&lhs.0.expr, &rhs.0.expr) {
                            (Expression::Null, Expression::Null) => Expression::Boolean(false),
                            _ => {
                                Expression::Operation(Box::new(Operation::NullNotEquals(lhs, rhs)))
                            }
                        }
                    }
                    Operation::IntegerNegate(rhs) => {
                        let rhs = propagate_const(const_vars, rhs);

                        match &rhs.0.expr {
                            Expression::Integer(rhs) => Expression::Integer(-rhs),
                            _ => Expression::Operation(Box::new(Operation::IntegerNegate(rhs))),
                        }
                    }
                    Operation::BooleanNot(rhs) => {
                        let rhs = propagate_const(const_vars, rhs);

                        match &rhs.0.expr {
                            Expression::Boolean(rhs) => Expression::Boolean(!rhs),
                            _ => Expression::Operation(Box::new(Operation::BooleanNot(rhs))),
                        }
                    }
                },
            },
            ty: expr.0.ty,
        },
        expr.1,
    )
}

#[derive(Clone, Copy, PartialEq)]
enum ConstValue {
    Boolean(bool),
    Integer(i32),
    Null,
    Colour { r: u8, g: u8, b: u8 },
}

impl From<ConstValue> for Expression<'_> {
    fn from(expr: ConstValue) -> Self {
        match expr {
            ConstValue::Boolean(value) => Expression::Boolean(value),
            ConstValue::Integer(value) => Expression::Integer(value),
            ConstValue::Null => Expression::Null,
            ConstValue::Colour { r, g, b } => Expression::Colour { r, g, b },
        }
    }
}

macro_rules! const_eval_operation {
    ($operation:ident, $lhs:ident, $rhs:ident, $lhs_ty:ident, $rhs_ty:ident, $result_ty:ident, $result:expr) => {
        match ($lhs.0, $rhs.0) {
            (ConstValue::$lhs_ty($lhs), ConstValue::$rhs_ty($rhs)) => {
                ConstValue::$result_ty($result)
            }
            _ => unreachable!(),
        }
    };
}
use const_eval_operation;
