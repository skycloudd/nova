use crate::{
    error::Error,
    parser::{BinaryOp, Expr, Statement, UnaryOp},
    Span, Spanned,
};
use std::{collections::HashMap, hash::Hash};

#[derive(Clone, Debug)]
enum Value<'ast, 'src> {
    Null,
    Boolean(bool),
    Integer(i32),
    Function {
        name: &'src str,
        parameters: Vec<&'src str>,
        body: &'ast [Spanned<Statement<'src>>],
    },
}

impl<'ast, 'src> Value<'ast, 'src> {
    fn ty(&self) -> String {
        match self {
            Value::Null => "null",
            Value::Boolean(_) => "boolean",
            Value::Integer(_) => "integer",
            Value::Function { .. } => "function",
        }
        .to_owned()
    }

    fn add(self, span: Span, rhs: Spanned<Self>) -> Result<Self, Error> {
        match (self, rhs) {
            (Value::Integer(lhs), (Value::Integer(rhs), _)) => Ok(Value::Integer(lhs + rhs)),

            (lhs, rhs) => Err(Error::BinaryExpressionTypeMismatch {
                op: BinaryOp::Plus.to_string(),
                left: (lhs.ty(), span),
                right: (rhs.0.ty(), rhs.1),
            }),
        }
    }

    fn sub(self, span: Span, rhs: Spanned<Self>) -> Result<Self, Error> {
        match (self, rhs) {
            (Value::Integer(lhs), (Value::Integer(rhs), _)) => Ok(Value::Integer(lhs - rhs)),

            (lhs, rhs) => Err(Error::BinaryExpressionTypeMismatch {
                op: BinaryOp::Minus.to_string(),
                left: (lhs.ty(), span),
                right: (rhs.0.ty(), rhs.1),
            }),
        }
    }

    fn mul(self, span: Span, rhs: Spanned<Self>) -> Result<Self, Error> {
        match (self, rhs) {
            (Value::Integer(lhs), (Value::Integer(rhs), _)) => Ok(Value::Integer(lhs * rhs)),

            (lhs, rhs) => Err(Error::BinaryExpressionTypeMismatch {
                op: BinaryOp::Multiply.to_string(),
                left: (lhs.ty(), span),
                right: (rhs.0.ty(), rhs.1),
            }),
        }
    }

    fn div(self, span: Span, rhs: Spanned<Self>) -> Result<Option<Self>, Error> {
        match (self, rhs) {
            (Value::Integer(lhs), (Value::Integer(rhs), _)) => Ok(Some(Value::Integer(lhs / rhs))),

            (lhs, rhs) => Err(Error::BinaryExpressionTypeMismatch {
                op: BinaryOp::Divide.to_string(),
                left: (lhs.ty(), span),
                right: (rhs.0.ty(), rhs.1),
            }),
        }
    }

    fn neg(self, span: Span) -> Result<Self, Error> {
        match self {
            Value::Integer(n) => Ok(Value::Integer(-n)),

            value => Err(Error::UnaryExpressionTypeMismatch {
                op: UnaryOp::Negate.to_string(),
                operand: (value.ty(), span),
            }),
        }
    }

    fn eq(self, span: Span, rhs: Spanned<Self>) -> Result<Self, Error> {
        match (self, rhs.0) {
            (Value::Null, Value::Null) => Ok(Value::Boolean(true)),

            (Value::Boolean(lhs), Value::Boolean(rhs)) => Ok(Value::Boolean(lhs == rhs)),

            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(Value::Boolean(lhs == rhs)),

            (Value::Null, Value::Boolean(_)) => Ok(Value::Boolean(false)),
            (Value::Null, Value::Integer(_)) => Ok(Value::Boolean(false)),

            (Value::Boolean(_), Value::Null) => Ok(Value::Boolean(false)),
            (Value::Integer(_), Value::Null) => Ok(Value::Boolean(false)),

            (lhs, rhs_) => Err(Error::BinaryExpressionTypeMismatch {
                op: BinaryOp::Equals.to_string(),
                left: (lhs.ty(), span),
                right: (rhs_.ty(), rhs.1),
            }),
        }
    }

    fn neq(self, span: Span, rhs: Spanned<Self>) -> Result<Self, Error> {
        Ok(self.eq(span, rhs)?.not(span)?)
    }

    fn not(self, span: Span) -> Result<Self, Error> {
        match self {
            Value::Boolean(b) => Ok(Value::Boolean(!b)),

            value => Err(Error::UnaryExpressionTypeMismatch {
                op: UnaryOp::Negate.to_string(),
                operand: (value.ty(), span),
            }),
        }
    }
}

impl std::fmt::Display for Value<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Integer(n) => write!(f, "{}", n),
            Value::Function {
                name,
                parameters,
                body: _,
            } => {
                write!(f, "func {} |{}|", name, parameters.join(", "))
            }
        }
    }
}

pub fn interpret(ast: &[Spanned<Statement>]) -> Result<(), Error> {
    let mut variables = Scopes::new();

    for statement in ast {
        match eval_statement(&mut variables, statement)? {
            ControlFlow::Normal => {}
            ControlFlow::Return(_) => {
                return Err(Error::Custom {
                    message: "Return outside of function".into(),
                    span: statement.1,
                })
            }
            ControlFlow::Break => {
                return Err(Error::Custom {
                    message: "Break outside of loop".into(),
                    span: statement.1,
                })
            }
            ControlFlow::Continue => {
                return Err(Error::Custom {
                    message: "Continue outside of loop".into(),
                    span: statement.1,
                })
            }
        }
    }

    Ok(())
}

fn eval_statement<'ast, 'src>(
    variables: &mut Scopes<&'src str, Value<'ast, 'src>>,
    statement: &'ast Spanned<Statement<'src>>,
) -> Result<ControlFlow<'ast, 'src>, Error> {
    Ok(match &statement.0 {
        Statement::Expr(expr) => {
            eval_expr(variables, expr)?;

            ControlFlow::Normal
        }
        Statement::Function {
            name,
            parameters,
            body,
        } => {
            variables.insert(
                name.0,
                Value::Function {
                    name: name.0,
                    parameters: parameters.0.iter().map(|p| p.0).collect(),
                    body: &body.0,
                },
            );

            ControlFlow::Normal
        }
        Statement::Return(expr) => {
            let result = eval_expr(variables, expr)?;

            return Ok(ControlFlow::Return(result));
        }
        Statement::Break => ControlFlow::Break,
        Statement::Continue => ControlFlow::Continue,
        Statement::Print(expr) => {
            let result = eval_expr(variables, expr)?;

            println!("{}", result);

            ControlFlow::Normal
        }
        Statement::Loop(body) => 'outer: loop {
            variables.push_scope();

            for statement in &body.0 {
                match eval_statement(variables, statement)? {
                    ControlFlow::Normal => {}
                    ControlFlow::Return(value) => {
                        variables.pop_scope();

                        return Ok(ControlFlow::Return(value));
                    }
                    ControlFlow::Break => {
                        break 'outer ControlFlow::Normal;
                    }
                    ControlFlow::Continue => {
                        continue 'outer;
                    }
                }
            }

            variables.pop_scope();
        },
    })
}

fn eval_expr<'ast, 'src>(
    variables: &mut Scopes<&'src str, Value<'ast, 'src>>,
    expr: &Spanned<Expr<'src>>,
) -> Result<Value<'ast, 'src>, Error> {
    Ok(match &expr.0 {
        Expr::Variable(v) => variables.get(&v.0).cloned().ok_or(Error::Custom {
            message: "Undefined variable".into(),
            span: expr.1,
        })?,
        Expr::Boolean(b) => Value::Boolean(b.0),
        Expr::Integer(n) => Value::Integer(n.0),
        Expr::Null => Value::Null,
        Expr::Binary(lhs, op, rhs) => {
            let lhs = (eval_expr(variables, lhs)?, lhs.1);
            let rhs = (eval_expr(variables, rhs)?, rhs.1);

            match op.0 {
                BinaryOp::Multiply => lhs.0.mul(lhs.1, rhs)?,
                BinaryOp::Divide => lhs.0.div(lhs.1, rhs)?.ok_or(Error::Custom {
                    message: "Division by zero".into(),
                    span: expr.1,
                })?,
                BinaryOp::Equals => lhs.0.eq(lhs.1, rhs)?,
                BinaryOp::NotEquals => lhs.0.neq(lhs.1, rhs)?,
                BinaryOp::Plus => lhs.0.add(lhs.1, rhs)?,
                BinaryOp::Minus => lhs.0.sub(lhs.1, rhs)?,
            }
        }
        Expr::Unary(op, expr) => {
            let rhs = eval_expr(variables, expr)?;

            match op.0 {
                UnaryOp::Negate => rhs.neg(expr.1)?,
            }
        }
        Expr::Call(func_expr, args) => {
            let func = eval_expr(variables, func_expr)?;

            match func {
                Value::Function {
                    name: _,
                    parameters,
                    body,
                } => {
                    if parameters.len() != args.0.len() {
                        return Err(Error::Custom {
                            message: "Incorrect number of arguments".into(),
                            span: args.1,
                        });
                    }

                    variables.push_scope();

                    let mut values = vec![];

                    for arg in &args.0 {
                        values.push(eval_expr(variables, arg)?);
                    }

                    for (parameter, value) in parameters.iter().zip(values) {
                        variables.insert(parameter, value);
                    }

                    for statement in body {
                        match eval_statement(variables, statement)? {
                            ControlFlow::Normal => {}
                            ControlFlow::Return(value) => {
                                variables.pop_scope();

                                return Ok(value);
                            }
                            ControlFlow::Break => {
                                variables.pop_scope();

                                return Err(Error::Custom {
                                    message: "Break outside of loop".into(),
                                    span: statement.1,
                                });
                            }
                            ControlFlow::Continue => {
                                variables.pop_scope();

                                return Err(Error::Custom {
                                    message: "Continue outside of loop".into(),
                                    span: statement.1,
                                });
                            }
                        }
                    }

                    variables.pop_scope();

                    Value::Null
                }
                _ => {
                    return Err(Error::Custom {
                        message: "Expected function".into(),
                        span: func_expr.1,
                    })
                }
            }
        }
    })
}

struct Scopes<K, V> {
    base: HashMap<K, V>,
    scopes: Vec<HashMap<K, V>>,
}

impl<K: Eq + Hash, V> Scopes<K, V> {
    fn new() -> Scopes<K, V> {
        Scopes {
            base: HashMap::new(),
            scopes: vec![],
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn insert(&mut self, k: K, v: V) {
        self.scopes
            .last_mut()
            .unwrap_or(&mut self.base)
            .insert(k, v);
    }

    fn get(&self, k: &K) -> Option<&V> {
        for scope in self.scopes.iter().rev() {
            if let Some(v) = scope.get(k) {
                return Some(v);
            }
        }

        self.base.get(k)
    }
}

enum ControlFlow<'ast, 'src> {
    Normal,
    Return(Value<'ast, 'src>),
    Break,
    Continue,
}
