use crate::{
    ast::{
        typed::{Expr, TypedExpr, TypedStatement},
        BinaryOp, UnaryOp,
    },
    error::Error,
    scopes::Scopes,
    Spanned,
};

pub fn const_eval<'src>(ast: &mut [Spanned<TypedStatement<'src>>]) -> Result<(), Vec<Error>> {
    let mut errors = vec![];

    let mut const_vars = Scopes::new();

    for statement in ast {
        if let Err(error) = const_eval_statement(&mut const_vars, statement) {
            errors.push(error);
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

fn const_eval_statement<'ast, 'src>(
    const_vars: &mut Scopes<&'src str, &'ast Spanned<TypedExpr<'src>>>,
    statement: &'ast mut Spanned<TypedStatement<'src>>,
) -> Result<(), Error> {
    match statement.0 {
        TypedStatement::Const {
            name,
            ref mut value,
        } => {
            *value = const_eval_expr(const_vars, value)?;

            const_vars.insert(name.0, value);

            Ok(())
        }
        _ => Ok(()),
    }
}

fn const_eval_expr<'ast, 'src>(
    const_vars: &mut Scopes<&'src str, &'ast Spanned<TypedExpr<'src>>>,
    expr: &Spanned<TypedExpr<'src>>,
) -> Result<Spanned<TypedExpr<'src>>, Error> {
    Ok((
        TypedExpr {
            expr: match &expr.0.expr {
                Expr::Variable(name) => match const_vars.get(&name.0) {
                    Some(value) => value.0.expr.clone(),
                    None => todo!("error"),
                },
                Expr::Boolean(value) => Expr::Boolean(*value),
                Expr::Integer(value) => Expr::Integer(*value),
                Expr::Null => Expr::Null,
                Expr::Binary(lhs, op, rhs) => {
                    let lhs = const_eval_expr(const_vars, lhs)?;
                    let rhs = const_eval_expr(const_vars, rhs)?;

                    let span = (lhs.1.start..rhs.1.end).into();

                    use BinaryOp::*;
                    use Expr::{Boolean, Integer, Null};
                    match (&lhs.0.expr, &rhs.0.expr) {
                        (Integer(lhs), Integer(rhs)) => match op.0 {
                            Equals => Boolean((lhs.0 == rhs.0, span)),
                            NotEquals => Boolean((lhs.0 != rhs.0, span)),
                            Plus => Integer((lhs.0 + rhs.0, span)),
                            Minus => Integer((lhs.0 - rhs.0, span)),
                            Multiply => Integer((lhs.0 * rhs.0, span)),
                            Divide => Integer((lhs.0 / rhs.0, span)),
                            GreaterThanEquals => Boolean((lhs.0 >= rhs.0, span)),
                            LessThanEquals => Boolean((lhs.0 <= rhs.0, span)),
                            GreaterThan => Boolean((lhs.0 > rhs.0, span)),
                            LessThan => Boolean((lhs.0 < rhs.0, span)),
                        },

                        (Boolean(lhs), Boolean(rhs)) => match op.0 {
                            Equals => Boolean((lhs.0 == rhs.0, span)),
                            NotEquals => Boolean((lhs.0 != rhs.0, span)),
                            _ => unreachable!(),
                        },

                        (Null, Null) => match op.0 {
                            Equals => Boolean((true, span)),
                            NotEquals => Boolean((false, span)),
                            _ => unreachable!(),
                        },

                        _ => unreachable!(),
                    }
                }
                Expr::Unary(op, rhs) => {
                    let rhs = const_eval_expr(const_vars, rhs)?;

                    use Expr::{Boolean, Integer};
                    use UnaryOp::*;
                    match (op.0, &rhs.0.expr) {
                        (Negate, Integer(value)) => Integer((-value.0, value.1)),
                        (Not, Boolean(value)) => Boolean((!value.0, value.1)),
                        _ => unreachable!(),
                    }
                }
            },
            ty: expr.0.ty,
        },
        expr.1,
    ))
}
