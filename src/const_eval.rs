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
        TypedStatement::Expr(_) => Ok(()),
        TypedStatement::BuiltinPrint(_) => Ok(()),
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
            condition: _,
            ref mut then_branch,
            ref mut else_branch,
        } => {
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
            if let Ok(const_value) = const_eval_expr(const_vars, value) {
                *value = (
                    TypedExpr {
                        expr: match const_value.0 {
                            ConstValue::Boolean(value) => Expr::Boolean(value),
                            ConstValue::Integer(value) => Expr::Integer(value),
                            ConstValue::Null => Expr::Null,
                        },
                        ty: value.0.ty,
                    },
                    const_value.1,
                );

                const_vars.insert(name.0, const_value);
            };

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
                None => todo!("error"),
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

#[derive(Debug, Clone, Copy)]
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
