use crate::{
    error::Error,
    parser::{BinaryOp, Expr, Statement, UnaryOp},
    Spanned,
};
use std::{collections::HashMap, hash::Hash};

#[derive(Clone, Debug)]
enum Value<'ast, 'src> {
    Null,
    Number(i32),
    Function {
        parameters: Vec<&'src str>,
        body: &'ast [Spanned<Statement<'src>>],
    },
}

impl<'ast, 'src> Value<'ast, 'src> {
    fn add(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Value::Number(lhs + rhs),
            _ => panic!(),
        }
    }

    fn sub(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Value::Number(lhs - rhs),
            _ => panic!(),
        }
    }

    fn mul(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Value::Number(lhs * rhs),
            _ => panic!(),
        }
    }

    fn div(self, rhs: Self) -> Option<Self> {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) if rhs != 0 => Some(Value::Number(lhs / rhs)),
            (Value::Number(_), Value::Number(_)) => None,
            _ => panic!(),
        }
    }

    fn neg(self) -> Self {
        match self {
            Value::Number(n) => Value::Number(-n),
            _ => panic!(),
        }
    }

    fn eq(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Value::Number((lhs == rhs) as i32),
            _ => panic!(),
        }
    }

    fn neq(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Value::Number(lhs), Value::Number(rhs)) => Value::Number((lhs != rhs) as i32),
            _ => panic!(),
        }
    }
}

impl std::fmt::Display for Value<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Number(n) => write!(f, "{}", n),
            Value::Function { .. } => write!(f, "<function>"),
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
        Expr::Number(n) => Value::Number(n.0),
        Expr::Binary(lhs, op, rhs) => {
            let lhs = eval_expr(variables, lhs)?;
            let rhs = eval_expr(variables, rhs)?;

            match op.0 {
                BinaryOp::Multiply => lhs.mul(rhs),
                BinaryOp::Divide => lhs.div(rhs).ok_or(Error::Custom {
                    message: "Division by zero".into(),
                    span: expr.1,
                })?,
                BinaryOp::Equals => lhs.eq(rhs),
                BinaryOp::NotEquals => lhs.neq(rhs),
                BinaryOp::Plus => lhs.add(rhs),
                BinaryOp::Minus => lhs.sub(rhs),
            }
        }
        Expr::Unary(op, expr) => {
            let rhs = eval_expr(variables, expr)?;

            match op.0 {
                UnaryOp::Negate => rhs.neg(),
            }
        }
        Expr::Call(func_expr, args) => {
            let func = eval_expr(variables, func_expr)?;

            match func {
                Value::Function { parameters, body } => {
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
