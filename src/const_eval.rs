use crate::{
    ast::{
        typed::{Expr, TypedExpr, TypedStatement},
        BinaryOp, UnaryOp,
    },
    error::Error,
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
            *expr = propagate_const(const_vars, &expr);

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
            *condition = propagate_const(const_vars, &condition);

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
            name,
            ref mut value,
        } => {
            match const_eval_expr(const_vars, value) {
                Ok(const_value) => {
                    *value = (
                        TypedExpr {
                            expr: const_value.0.into(),
                            ty: value.0.ty,
                        },
                        const_value.1,
                    );

                    const_vars.insert(name.0, const_value);
                }
                Err(_) => {
                    *value = propagate_const(const_vars, &value);
                }
            }

            Ok(())
        }
        TypedStatement::Const {
            name,
            ref mut value,
        } => {
            let const_value = const_eval_expr(const_vars, value).map_err(|err| vec![err])?;

            *value = (
                TypedExpr {
                    expr: const_value.0.into(),
                    ty: value.0.ty,
                },
                value.1,
            );

            const_vars.insert(name.0, const_value);

            Ok(())
        }
    }
}

fn const_eval_expr<'src>(
    const_vars: &mut Scopes<&'src str, Spanned<ConstValue>>,
    expr: &Spanned<TypedExpr<'src>>,
) -> Result<Spanned<ConstValue>, Error> {
    Ok((
        match &expr.0.expr {
            Expr::Variable(name) => match const_vars.get(name) {
                Some(value) => value.0,
                None => {
                    return Err(Error::UnknownConstVariable {
                        name: name.to_string(),
                        span: expr.1,
                    })
                }
            },
            Expr::Boolean(value) => ConstValue::Boolean(*value),
            Expr::Integer(value) => ConstValue::Integer(*value),
            Expr::Null => ConstValue::Null,
            Expr::Binary(lhs, op, rhs) => {
                let lhs = const_eval_expr(const_vars, lhs)?;
                let rhs = const_eval_expr(const_vars, rhs)?;

                use BinaryOp::*;
                use ConstValue::{Boolean, Integer, Null};
                match (&lhs.0, &rhs.0) {
                    (Integer(lhs), Integer(rhs)) => match op.0 {
                        Equals => Boolean(lhs == rhs),
                        NotEquals => Boolean(lhs != rhs),
                        Plus => Integer(lhs + rhs),
                        Minus => Integer(lhs - rhs),
                        Multiply => Integer(lhs * rhs),
                        Divide => Integer(lhs / rhs),
                        GreaterThanEquals => Boolean(lhs >= rhs),
                        LessThanEquals => Boolean(lhs <= rhs),
                        GreaterThan => Boolean(lhs > rhs),
                        LessThan => Boolean(lhs < rhs),
                    },

                    (Boolean(lhs), Boolean(rhs)) => match op.0 {
                        Equals => Boolean(lhs == rhs),
                        NotEquals => Boolean(lhs != rhs),
                        _ => unreachable!(),
                    },

                    (Null, Null) => match op.0 {
                        Equals => Boolean(true),
                        NotEquals => Boolean(false),
                        _ => unreachable!(),
                    },

                    _ => unreachable!(),
                }
            }
            Expr::Unary(op, rhs) => {
                let rhs = const_eval_expr(const_vars, rhs)?;

                use ConstValue::{Boolean, Integer};
                use UnaryOp::*;
                match (op.0, rhs.0) {
                    (Negate, Integer(value)) => Integer(-value),
                    (Not, Boolean(value)) => Boolean(!value),

                    _ => unreachable!(),
                }
            }
        },
        expr.1,
    ))
}

fn propagate_const<'src>(
    const_vars: &mut Scopes<&'src str, Spanned<ConstValue>>,
    expr: &Spanned<TypedExpr<'src>>,
) -> Spanned<TypedExpr<'src>> {
    (
        TypedExpr {
            expr: match &expr.0.expr {
                Expr::Variable(name) => match const_vars.get(name) {
                    Some(value) => Expr::from(value.0),
                    None => Expr::Variable(name),
                },
                Expr::Boolean(value) => Expr::Boolean(*value),
                Expr::Integer(value) => Expr::Integer(*value),
                Expr::Null => Expr::Null,
                Expr::Binary(lhs, op, rhs) => {
                    let lhs = propagate_const(const_vars, lhs);
                    let rhs = propagate_const(const_vars, rhs);

                    match (&lhs.0.expr, op.0, &rhs.0.expr) {
                        (Expr::Integer(lhs), BinaryOp::Equals, Expr::Integer(rhs)) => {
                            Expr::Boolean(lhs == rhs)
                        }
                        (Expr::Integer(lhs), BinaryOp::NotEquals, Expr::Integer(rhs)) => {
                            Expr::Boolean(lhs != rhs)
                        }
                        (Expr::Integer(lhs), BinaryOp::Plus, Expr::Integer(rhs)) => {
                            Expr::Integer(lhs + rhs)
                        }
                        (Expr::Integer(lhs), BinaryOp::Minus, Expr::Integer(rhs)) => {
                            Expr::Integer(lhs - rhs)
                        }
                        (Expr::Integer(lhs), BinaryOp::Multiply, Expr::Integer(rhs)) => {
                            Expr::Integer(lhs * rhs)
                        }
                        (Expr::Integer(lhs), BinaryOp::Divide, Expr::Integer(rhs)) => {
                            Expr::Integer(lhs / rhs)
                        }
                        (Expr::Integer(lhs), BinaryOp::GreaterThanEquals, Expr::Integer(rhs)) => {
                            Expr::Boolean(lhs >= rhs)
                        }
                        (Expr::Integer(lhs), BinaryOp::LessThanEquals, Expr::Integer(rhs)) => {
                            Expr::Boolean(lhs <= rhs)
                        }
                        (Expr::Integer(lhs), BinaryOp::GreaterThan, Expr::Integer(rhs)) => {
                            Expr::Boolean(lhs > rhs)
                        }
                        (Expr::Integer(lhs), BinaryOp::LessThan, Expr::Integer(rhs)) => {
                            Expr::Boolean(lhs < rhs)
                        }

                        (Expr::Boolean(lhs), BinaryOp::Equals, Expr::Boolean(rhs)) => {
                            Expr::Boolean(lhs == rhs)
                        }
                        (Expr::Boolean(lhs), BinaryOp::NotEquals, Expr::Boolean(rhs)) => {
                            Expr::Boolean(lhs != rhs)
                        }

                        (Expr::Null, BinaryOp::Equals, Expr::Null) => Expr::Boolean(true),
                        (Expr::Null, BinaryOp::NotEquals, Expr::Null) => Expr::Boolean(false),

                        _ => Expr::Binary(Box::new(lhs), *op, Box::new(rhs)),
                    }
                }
                Expr::Unary(op, rhs) => {
                    let rhs = propagate_const(const_vars, rhs);

                    match (op.0, &rhs.0.expr) {
                        (UnaryOp::Negate, Expr::Integer(value)) => Expr::Integer(-value),
                        (UnaryOp::Not, Expr::Boolean(value)) => Expr::Boolean(!value),

                        _ => Expr::Unary(*op, Box::new(rhs)),
                    }
                }
            },
            ty: expr.0.ty,
        },
        expr.1,
    )
}

#[derive(Clone, Copy, Debug)]
enum ConstValue {
    Boolean(bool),
    Integer(i32),
    Null,
}

impl From<ConstValue> for Expr<'_> {
    fn from(expr: ConstValue) -> Self {
        match expr {
            ConstValue::Boolean(value) => Expr::Boolean(value),
            ConstValue::Integer(value) => Expr::Integer(value),
            ConstValue::Null => Expr::Null,
        }
    }
}
