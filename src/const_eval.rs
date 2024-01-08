use crate::{
    error::Error,
    mir::{Expression, Operation, Type, TypedExpression, TypedStatement},
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

fn const_eval_expr<'src>(
    const_vars: &mut Scopes<&'src str, Spanned<ConstValue>>,
    expr: &Spanned<TypedExpression<'src>>,
) -> Result<Spanned<ConstValue>, Error> {
    Ok((
        match &expr.0.expr {
            Expression::Variable(name) => match const_vars.get(name) {
                Some(value) => value.0.clone(),
                None => {
                    return Err(Error::UnknownConstVariable {
                        name: name.to_string(),
                        span: expr.1,
                    })
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

fn propagate_const<'src>(
    const_vars: &mut Scopes<&'src str, Spanned<ConstValue>>,
    expr: &Spanned<TypedExpression<'src>>,
) -> Spanned<TypedExpression<'src>> {
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
                    Operation::FloatEquals(lhs, rhs) => {
                        let lhs = propagate_const(const_vars, lhs);
                        let rhs = propagate_const(const_vars, rhs);

                        match (&lhs.0.ty, &rhs.0.ty) {
                            (Type::Float, Type::Float) => match (&lhs.0.expr, &rhs.0.expr) {
                                (Expression::Float(lhs), Expression::Float(rhs)) => {
                                    Expression::Boolean(lhs == rhs)
                                }
                                _ => Expression::Operation(Box::new(Operation::FloatEquals(
                                    lhs, rhs,
                                ))),
                            },
                            _ => unreachable!(),
                        }
                    }
                    Operation::FloatNotEquals(lhs, rhs) => {
                        let lhs = propagate_const(const_vars, lhs);
                        let rhs = propagate_const(const_vars, rhs);

                        match (&lhs.0.ty, &rhs.0.ty) {
                            (Type::Float, Type::Float) => match (&lhs.0.expr, &rhs.0.expr) {
                                (Expression::Float(lhs), Expression::Float(rhs)) => {
                                    Expression::Boolean(lhs != rhs)
                                }
                                _ => Expression::Operation(Box::new(Operation::FloatNotEquals(
                                    lhs, rhs,
                                ))),
                            },
                            _ => unreachable!(),
                        }
                    }
                    Operation::FloatPlus(lhs, rhs) => {
                        let lhs = propagate_const(const_vars, lhs);
                        let rhs = propagate_const(const_vars, rhs);

                        match (&lhs.0.ty, &rhs.0.ty) {
                            (Type::Float, Type::Float) => match (&lhs.0.expr, &rhs.0.expr) {
                                (Expression::Float(lhs), Expression::Float(rhs)) => {
                                    Expression::Float(lhs + rhs)
                                }
                                _ => {
                                    Expression::Operation(Box::new(Operation::FloatPlus(lhs, rhs)))
                                }
                            },
                            _ => unreachable!(),
                        }
                    }
                    Operation::FloatMinus(lhs, rhs) => {
                        let lhs = propagate_const(const_vars, lhs);
                        let rhs = propagate_const(const_vars, rhs);

                        match (&lhs.0.ty, &rhs.0.ty) {
                            (Type::Float, Type::Float) => match (&lhs.0.expr, &rhs.0.expr) {
                                (Expression::Float(lhs), Expression::Float(rhs)) => {
                                    Expression::Float(lhs - rhs)
                                }
                                _ => {
                                    Expression::Operation(Box::new(Operation::FloatMinus(lhs, rhs)))
                                }
                            },
                            _ => unreachable!(),
                        }
                    }
                    Operation::FloatMultiply(lhs, rhs) => {
                        let lhs = propagate_const(const_vars, lhs);
                        let rhs = propagate_const(const_vars, rhs);

                        match (&lhs.0.ty, &rhs.0.ty) {
                            (Type::Float, Type::Float) => match (&lhs.0.expr, &rhs.0.expr) {
                                (Expression::Float(lhs), Expression::Float(rhs)) => {
                                    Expression::Float(lhs * rhs)
                                }
                                _ => Expression::Operation(Box::new(Operation::FloatMultiply(
                                    lhs, rhs,
                                ))),
                            },
                            _ => unreachable!(),
                        }
                    }
                    Operation::FloatDivide(lhs, rhs) => {
                        let lhs = propagate_const(const_vars, lhs);
                        let rhs = propagate_const(const_vars, rhs);

                        match (&lhs.0.ty, &rhs.0.ty) {
                            (Type::Float, Type::Float) => match (&lhs.0.expr, &rhs.0.expr) {
                                (Expression::Float(lhs), Expression::Float(rhs)) => {
                                    Expression::Float(lhs / rhs)
                                }
                                _ => Expression::Operation(Box::new(Operation::FloatDivide(
                                    lhs, rhs,
                                ))),
                            },
                            _ => unreachable!(),
                        }
                    }
                    Operation::FloatGreaterThanEquals(lhs, rhs) => {
                        let lhs = propagate_const(const_vars, lhs);
                        let rhs = propagate_const(const_vars, rhs);

                        match (&lhs.0.ty, &rhs.0.ty) {
                            (Type::Float, Type::Float) => match (&lhs.0.expr, &rhs.0.expr) {
                                (Expression::Float(lhs), Expression::Float(rhs)) => {
                                    Expression::Boolean(lhs >= rhs)
                                }
                                _ => Expression::Operation(Box::new(
                                    Operation::FloatGreaterThanEquals(lhs, rhs),
                                )),
                            },
                            _ => unreachable!(),
                        }
                    }
                    Operation::FloatLessThanEquals(lhs, rhs) => {
                        let lhs = propagate_const(const_vars, lhs);
                        let rhs = propagate_const(const_vars, rhs);

                        match (&lhs.0.ty, &rhs.0.ty) {
                            (Type::Float, Type::Float) => match (&lhs.0.expr, &rhs.0.expr) {
                                (Expression::Float(lhs), Expression::Float(rhs)) => {
                                    Expression::Boolean(lhs <= rhs)
                                }
                                _ => Expression::Operation(Box::new(
                                    Operation::FloatLessThanEquals(lhs, rhs),
                                )),
                            },
                            _ => unreachable!(),
                        }
                    }
                    Operation::FloatGreaterThan(lhs, rhs) => {
                        let lhs = propagate_const(const_vars, lhs);
                        let rhs = propagate_const(const_vars, rhs);

                        match (&lhs.0.ty, &rhs.0.ty) {
                            (Type::Float, Type::Float) => match (&lhs.0.expr, &rhs.0.expr) {
                                (Expression::Float(lhs), Expression::Float(rhs)) => {
                                    Expression::Boolean(lhs > rhs)
                                }
                                _ => Expression::Operation(Box::new(Operation::FloatGreaterThan(
                                    lhs, rhs,
                                ))),
                            },
                            _ => unreachable!(),
                        }
                    }
                    Operation::FloatLessThan(lhs, rhs) => {
                        let lhs = propagate_const(const_vars, lhs);
                        let rhs = propagate_const(const_vars, rhs);

                        match (&lhs.0.ty, &rhs.0.ty) {
                            (Type::Float, Type::Float) => match (&lhs.0.expr, &rhs.0.expr) {
                                (Expression::Float(lhs), Expression::Float(rhs)) => {
                                    Expression::Boolean(lhs < rhs)
                                }
                                _ => Expression::Operation(Box::new(Operation::FloatLessThan(
                                    lhs, rhs,
                                ))),
                            },
                            _ => unreachable!(),
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
                    Operation::IntegerNegate(rhs) => {
                        let rhs = propagate_const(const_vars, rhs);

                        match &rhs.0.expr {
                            Expression::Integer(rhs) => Expression::Integer(-rhs),
                            _ => Expression::Operation(Box::new(Operation::IntegerNegate(rhs))),
                        }
                    }
                    Operation::FloatNegate(rhs) => {
                        let rhs = propagate_const(const_vars, rhs);

                        match &rhs.0.expr {
                            Expression::Float(rhs) => Expression::Float(-rhs),
                            _ => Expression::Operation(Box::new(Operation::FloatNegate(rhs))),
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

#[derive(Clone, PartialEq)]
enum ConstValue {
    Boolean(bool),
    Integer(i32),
    Float(f32),
    Colour {
        r: u8,
        g: u8,
        b: u8,
    },
    Vector {
        x: Box<Spanned<ConstValue>>,
        y: Box<Spanned<ConstValue>>,
    },
}

impl From<ConstValue> for Expression<'_> {
    fn from(expr: ConstValue) -> Self {
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
