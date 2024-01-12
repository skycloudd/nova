use crate::{
    ast::{
        typed::{self, Type, TypedExpr, TypedStatement},
        BinaryOp, Expr, Statement, UnaryOp,
    },
    error::Error,
    scopes::Scopes,
    span::Spanned,
};
use rustc_hash::FxHashMap;

pub fn typecheck<'src, 'file>(
    ast: Vec<Spanned<'file, Statement<'src, 'file>>>,
) -> (
    Result<Vec<Spanned<'file, TypedStatement<'src, 'file>>>, ()>,
    Vec<Box<Error<'file>>>,
) {
    typecheck_ast(ast)
}

fn typecheck_ast<'src, 'file>(
    ast: Vec<Spanned<'file, Statement<'src, 'file>>>,
) -> (
    Result<Vec<Spanned<'file, TypedStatement<'src, 'file>>>, ()>,
    Vec<Box<Error<'file>>>,
) {
    let mut engine = Engine::new();
    let mut variables = Scopes::new();
    let mut const_variables = Scopes::new();

    let mut errors = Vec::new();

    let mut typed_ast = Vec::new();

    for statement in ast {
        match typecheck_statement(&mut engine, &mut variables, &mut const_variables, statement) {
            Ok(statement) => typed_ast.push(statement),
            Err(error) => errors.push(error),
        }
    }

    if errors.is_empty() {
        (Ok(typed_ast), errors)
    } else {
        (Err(()), errors)
    }
}

fn typecheck_statement<'src, 'file>(
    engine: &mut Engine<'file>,
    variables: &mut Scopes<&'src str, TypeId>,
    const_variables: &mut Scopes<&'src str, TypeId>,
    statement: Spanned<'file, Statement<'src, 'file>>,
) -> Result<Spanned<'file, TypedStatement<'src, 'file>>, Box<Error<'file>>> {
    Ok(Spanned(
        match statement.0 {
            Statement::Expr(expr) => {
                let expr = typecheck_expression(engine, variables, const_variables, expr)?;

                TypedStatement::Expr(expr)
            }
            Statement::BuiltinPrint(expr) => {
                let expr = typecheck_expression(engine, variables, const_variables, expr)?;

                TypedStatement::BuiltinPrint(expr)
            }
            Statement::Loop(statements) => {
                push_scope(variables, const_variables);

                let statements = Spanned(
                    statements
                        .0
                        .into_iter()
                        .map(|statement| {
                            typecheck_statement(engine, variables, const_variables, statement)
                        })
                        .collect::<Result<Vec<_>, _>>()?,
                    statements.1,
                );

                pop_scope(variables, const_variables);

                TypedStatement::Loop(statements)
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition =
                    typecheck_expression(engine, variables, const_variables, condition)?;
                let condition_ty =
                    engine.insert(type_to_typeinfo(Spanned(&condition.0.ty, condition.1)));

                let bool = engine.insert(Spanned(TypeInfo::Boolean, condition.1));

                engine.unify(condition_ty, bool)?;

                push_scope(variables, const_variables);

                let then_branch = Spanned(
                    then_branch
                        .0
                        .into_iter()
                        .map(|statement| {
                            typecheck_statement(engine, variables, const_variables, statement)
                        })
                        .collect::<Result<Vec<_>, _>>()?,
                    then_branch.1,
                );

                pop_scope(variables, const_variables);

                let else_branch = match else_branch {
                    Some(else_branch) => {
                        push_scope(variables, const_variables);

                        let else_branch = Spanned(
                            else_branch
                                .0
                                .into_iter()
                                .map(|statement| {
                                    typecheck_statement(
                                        engine,
                                        variables,
                                        const_variables,
                                        statement,
                                    )
                                })
                                .collect::<Result<Vec<_>, _>>()?,
                            else_branch.1,
                        );

                        pop_scope(variables, const_variables);

                        Some(else_branch)
                    }
                    None => None,
                };

                TypedStatement::If {
                    condition,
                    then_branch,
                    else_branch,
                }
            }
            Statement::Let { name, value } => {
                let value = typecheck_expression(engine, variables, const_variables, value)?;
                let value_ty = engine.insert(type_to_typeinfo(Spanned(&value.0.ty, value.1)));

                variables.insert(name.0, value_ty);

                TypedStatement::Let { name, value }
            }
            Statement::Const { name, value } => {
                let value = typecheck_expression(engine, variables, const_variables, value)?;
                let value_ty = engine.insert(type_to_typeinfo(Spanned(&value.0.ty, value.1)));

                if variables.contains_key(&name.0) || const_variables.contains_key(&name.0) {
                    return Err(Box::new(Error::ConstAlreadyDefined {
                        name: name.0.to_string(),
                        span: name.1,
                    }));
                }

                const_variables.insert(name.0, value_ty);

                TypedStatement::Const { name, value }
            }
            Statement::Assign { name, value } => {
                let value = typecheck_expression(engine, variables, const_variables, value)?;
                let value_ty = engine.insert(type_to_typeinfo(Spanned(&value.0.ty, value.1)));

                let Some(name_ty) = variables.get(&name.0) else {
                    return Err(Box::new(Error::UndefinedVariable {
                        name: name.0.to_string(),
                        span: name.1,
                    }));
                };

                engine.unify(*name_ty, value_ty)?;

                TypedStatement::Assign { name, value }
            }
            Statement::Break => TypedStatement::Break,
            Statement::Continue => TypedStatement::Continue,
        },
        statement.1,
    ))
}

fn typecheck_expression<'src, 'file>(
    engine: &mut Engine<'file>,
    variables: &mut Scopes<&str, TypeId>,
    const_variables: &Scopes<&str, TypeId>,
    expr: Spanned<'file, Expr<'src, 'file>>,
) -> Result<Spanned<'file, TypedExpr<'src, 'file>>, Box<Error<'file>>> {
    Ok(Spanned(
        match expr.0 {
            Expr::Variable(var) => {
                let var_result = variables.get(&var);
                let const_var_result = const_variables.get(&var);

                match (var_result, const_var_result) {
                    (Some(ty), _) => TypedExpr {
                        expr: typed::Expr::Variable(var),
                        ty: engine.reconstruct(*ty)?.0,
                    },

                    (None, Some(ty)) => TypedExpr {
                        expr: typed::Expr::Variable(var),
                        ty: engine.reconstruct(*ty)?.0,
                    },

                    (None, None) => {
                        return Err(Box::new(Error::UndefinedVariable {
                            name: var.to_string(),
                            span: expr.1,
                        }))
                    }
                }
            }
            Expr::Boolean(boolean) => TypedExpr {
                expr: typed::Expr::Boolean(boolean),
                ty: Type::Boolean,
            },
            Expr::Integer(integer) => TypedExpr {
                expr: typed::Expr::Integer(integer),
                ty: Type::Integer,
            },
            Expr::Float(float) => TypedExpr {
                expr: typed::Expr::Float(float),
                ty: Type::Float,
            },
            Expr::Colour { r, g, b } => TypedExpr {
                expr: typed::Expr::Colour { r, g, b },
                ty: Type::Colour,
            },
            Expr::Vector { x, y } => {
                let x = typecheck_expression(engine, variables, const_variables, x.map(|x| *x))?;
                let y = typecheck_expression(engine, variables, const_variables, y.map(|y| *y))?;

                let x_ty = engine.insert(type_to_typeinfo(Spanned(&x.0.ty, x.1)));
                let y_ty = engine.insert(type_to_typeinfo(Spanned(&y.0.ty, y.1)));

                engine.unify(x_ty, y_ty)?;

                let float_ty = engine.insert(Spanned(TypeInfo::Float, x.1));

                engine.unify(x_ty, float_ty)?;
                engine.unify(y_ty, float_ty)?;

                TypedExpr {
                    expr: typed::Expr::Vector {
                        x: x.map(Box::new),
                        y: y.map(Box::new),
                    },
                    ty: Type::Vector,
                }
            }
            Expr::Binary(lhs, op, rhs) => {
                let lhs =
                    typecheck_expression(engine, variables, const_variables, lhs.map(|l| *l))?;
                let rhs =
                    typecheck_expression(engine, variables, const_variables, rhs.map(|r| *r))?;

                let lhs_ty = engine.insert(type_to_typeinfo(Spanned(&lhs.0.ty, lhs.1)));
                let rhs_ty = engine.insert(type_to_typeinfo(Spanned(&rhs.0.ty, rhs.1)));

                engine.unify(lhs_ty, rhs_ty)?;

                let ty = bin_op!(
                    lhs.0.ty,
                    rhs.0.ty,
                    op.0,
                    (Boolean, Boolean, Equals, Boolean),
                    (Boolean, Boolean, NotEquals, Boolean),
                    (Integer, Integer, Equals, Boolean),
                    (Integer, Integer, NotEquals, Boolean),
                    (Integer, Integer, Plus, Integer),
                    (Integer, Integer, Minus, Integer),
                    (Integer, Integer, Multiply, Integer),
                    (Integer, Integer, Divide, Integer),
                    (Integer, Integer, GreaterThanEquals, Boolean),
                    (Integer, Integer, LessThanEquals, Boolean),
                    (Integer, Integer, GreaterThan, Boolean),
                    (Integer, Integer, LessThan, Boolean),
                    (Float, Float, Equals, Boolean),
                    (Float, Float, NotEquals, Boolean),
                    (Float, Float, Plus, Float),
                    (Float, Float, Minus, Float),
                    (Float, Float, Multiply, Float),
                    (Float, Float, Divide, Float),
                    (Float, Float, GreaterThanEquals, Boolean),
                    (Float, Float, LessThanEquals, Boolean),
                    (Float, Float, GreaterThan, Boolean),
                    (Float, Float, LessThan, Boolean)
                )
                .map_err(|()| Error::BinaryOp {
                    lhs: lhs.0.ty.to_string(),
                    lhs_span: lhs.1,
                    rhs: rhs.0.ty.to_string(),
                    rhs_span: rhs.1,
                    op: op.0.to_string(),
                    op_span: op.1,
                })?;

                TypedExpr {
                    expr: typed::Expr::Binary(lhs.map(Box::new), op, rhs.map(Box::new)),
                    ty,
                }
            }
            Expr::Unary(op, expr) => {
                let expr =
                    typecheck_expression(engine, variables, const_variables, expr.map(|e| *e))?;

                let ty = unary_op!(
                    expr.0.ty,
                    op.0,
                    (Integer, Negate, Integer),
                    (Float, Negate, Float),
                    (Boolean, Not, Boolean)
                )
                .map_err(|()| Error::UnaryOp {
                    ty: expr.0.ty.to_string(),
                    ty_span: expr.1,
                    op: op.0.to_string(),
                    op_span: op.1,
                })?;

                TypedExpr {
                    expr: typed::Expr::Unary(op, expr.map(Box::new)),
                    ty,
                }
            }
        },
        expr.1,
    ))
}

fn push_scope(variables: &mut Scopes<&str, TypeId>, const_variables: &mut Scopes<&str, TypeId>) {
    variables.push_scope();
    const_variables.push_scope();
}

fn pop_scope(variables: &mut Scopes<&str, TypeId>, const_variables: &mut Scopes<&str, TypeId>) {
    variables.pop_scope();
    const_variables.pop_scope();
}

struct Engine<'file> {
    id_counter: usize,
    vars: FxHashMap<TypeId, Spanned<'file, TypeInfo>>,
}

impl<'file> Engine<'file> {
    fn new() -> Self {
        Self {
            id_counter: 0,
            vars: FxHashMap::default(),
        }
    }

    fn insert(&mut self, info: Spanned<'file, TypeInfo>) -> TypeId {
        self.id_counter += 1;
        let id = self.id_counter;
        self.vars.insert(id, info);
        id
    }

    fn unify(&mut self, a: TypeId, b: TypeId) -> Result<(), Box<Error<'file>>> {
        let var_a = &self.vars[&a];
        let var_b = &self.vars[&b];

        match (&var_a.0, &var_b.0) {
            (TypeInfo::Ref(a), _) => self.unify(*a, b),
            (_, TypeInfo::Ref(b)) => self.unify(a, *b),

            (TypeInfo::Unknown, _) => {
                self.vars.insert(a, Spanned(TypeInfo::Ref(b), var_b.1));
                Ok(())
            }
            (_, TypeInfo::Unknown) => {
                self.vars.insert(b, Spanned(TypeInfo::Ref(a), var_a.1));
                Ok(())
            }

            (TypeInfo::Boolean, TypeInfo::Boolean)
            | (TypeInfo::Integer, TypeInfo::Integer)
            | (TypeInfo::Float, TypeInfo::Float)
            | (TypeInfo::Colour, TypeInfo::Colour) => Ok(()),

            (a, b) => Err(Box::new(Error::IncompatibleTypes {
                a: a.to_string(),
                a_span: var_a.1,
                b: b.to_string(),
                b_span: var_b.1,
            })),
        }
    }

    fn reconstruct(&self, id: TypeId) -> Result<Spanned<Type>, Box<Error<'file>>> {
        let var = &self.vars[&id];

        match var.0 {
            TypeInfo::Unknown => Err(Box::new(Error::UnknownType { span: var.1 })),
            TypeInfo::Ref(id) => Ok(self.reconstruct(id)?.0),
            TypeInfo::Boolean => Ok(Type::Boolean),
            TypeInfo::Integer => Ok(Type::Integer),
            TypeInfo::Float => Ok(Type::Float),
            TypeInfo::Colour => Ok(Type::Colour),
            TypeInfo::Vector => Ok(Type::Vector),
        }
        .map(|ty| Spanned(ty, var.1))
    }
}

type TypeId = usize;

enum TypeInfo {
    #[allow(dead_code)]
    Unknown,
    Ref(TypeId),
    Boolean,
    Integer,
    Float,
    Colour,
    Vector,
}

impl std::fmt::Display for TypeInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unknown => write!(f, "unknown"),
            Self::Ref(id) => write!(f, "ref {id}"),
            Self::Boolean => write!(f, "boolean"),
            Self::Integer => write!(f, "integer"),
            Self::Float => write!(f, "float"),
            Self::Colour => write!(f, "colour"),
            Self::Vector => write!(f, "vector"),
        }
    }
}

fn type_to_typeinfo<'file>(ty: Spanned<'file, &Type>) -> Spanned<'file, TypeInfo> {
    ty.map(|ty| match ty {
        Type::Boolean => TypeInfo::Boolean,
        Type::Integer => TypeInfo::Integer,
        Type::Float => TypeInfo::Float,
        Type::Colour => TypeInfo::Colour,
        Type::Vector => TypeInfo::Vector,
    })
}

macro_rules! bin_op {
    ($lhs_value:expr, $rhs_value:expr, $op_value:expr, $(($lhs:ident, $rhs:ident, $op:ident, $ret_ty:ident)),*) => {
        match ($lhs_value, $rhs_value, $op_value) {
            $(
                (Type::$lhs, Type::$rhs, BinaryOp::$op) => Ok(Type::$ret_ty),
            )*
            _ => Err(()),
        }
    };
}
use bin_op;

macro_rules! unary_op {
    ($value:expr, $op_value:expr, $(($ty:ident, $op:ident, $ret_ty:ident)),*) => {
        match ($value, $op_value) {
            $(
                (Type::$ty, UnaryOp::$op) => Ok(Type::$ret_ty),
            )*
            _ => Err(()),
        }
    };
}
use unary_op;
