use crate::{
    ast::{
        typed::{self, TypedExpr, TypedProcedure, TypedStatement, TypedTopLevel},
        BinaryOp, Expr, Procedure, Statement, TopLevel, Type, UnaryOp,
    },
    error::Error,
    scopes::Scopes,
    span::{Span, Spanned},
};
use rustc_hash::FxHashMap;

pub fn typecheck<'src, 'file>(
    ast: Vec<Spanned<'file, TopLevel<'src, 'file>>>,
) -> Result<Vec<Spanned<'file, TypedTopLevel<'src, 'file>>>, Vec<Box<Error<'file>>>> {
    let mut engine = Engine::new();
    let mut procedures = FxHashMap::default();
    let mut variables = Scopes::new();

    let mut errors = Vec::new();

    for top_level in &ast {
        let (name, args) = match &top_level.0 {
            TopLevel::Procedure(procedure) => (&procedure.name, &procedure.args),
            _ => continue,
        };

        match procedures.get(&name.0) {
            None => {
                let args = args
                    .0
                    .iter()
                    .map(|(_, ty)| engine.insert(type_to_typeinfo(Spanned(ty, ty.1))))
                    .collect();

                procedures.insert(name.0, (name.1, args));
            }
            Some((span, _)) => {
                errors.push(Box::new(Error::ProcedureRedefinition {
                    name: name.0.to_string(),
                    old_span: *span,
                    new_span: name.1,
                }));
            }
        }
    }

    let typed_ast = ast
        .into_iter()
        .filter_map(|top_level| match top_level.0 {
            TopLevel::Procedure(procedure) => {
                match typecheck_procedure(&mut engine, &mut procedures, &mut variables, procedure) {
                    Ok(procedure) => {
                        Some(Spanned(TypedTopLevel::Procedure(procedure), top_level.1))
                    }
                    Err(err) => {
                        errors.push(err);

                        None
                    }
                }
            }
            TopLevel::Run(name) => match typecheck_run(&procedures, name) {
                Ok(run) => Some(Spanned(run, top_level.1)),
                Err(err) => {
                    errors.push(err);

                    None
                }
            },
        })
        .collect();

    if errors.is_empty() {
        Ok(typed_ast)
    } else {
        Err(errors)
    }
}

fn typecheck_run<'src, 'file>(
    procedures: &FxHashMap<&'src str, (Span<'file>, Vec<TypeId>)>,
    name: Spanned<'file, &'src str>,
) -> Result<TypedTopLevel<'src, 'file>, Box<Error<'file>>> {
    if !procedures.contains_key(name.0) {
        return Err(Box::new(Error::UndefinedProcedure {
            name: name.0.to_string(),
            span: name.1,
        }));
    }

    let span = procedures[name.0].0;
    let number_args = procedures[name.0].1.len();

    if number_args != 0 {
        return Err(Box::new(Error::RunFunctionHasArgs {
            got: number_args,
            span,
            run_span: name.1,
        }));
    }

    Ok(TypedTopLevel::Run(name))
}

fn typecheck_procedure<'src, 'file>(
    engine: &mut Engine<'file>,
    procedures: &mut FxHashMap<&'src str, (Span, Vec<TypeId>)>,
    variables: &mut Scopes<&'src str, TypeId>,
    procedure: Procedure<'src, 'file>,
) -> Result<TypedProcedure<'src, 'file>, Box<Error<'file>>> {
    push_scope(variables);

    procedure
        .args
        .0
        .iter()
        .map(|(name, _)| name.0)
        .zip(&procedures.get(&procedure.name.0).unwrap().1)
        .for_each(|(name, ty)| variables.insert(name, *ty));

    let body = Spanned(
        procedure
            .body
            .0
            .into_iter()
            .map(|statement| typecheck_statement(engine, procedures, variables, statement))
            .collect::<Result<_, _>>()?,
        procedure.body.1,
    );

    pop_scope(variables);

    Ok(TypedProcedure {
        name: procedure.name,
        args: procedure.args,
        body,
    })
}

fn typecheck_statement<'src, 'file>(
    engine: &mut Engine<'file>,
    procedures: &mut FxHashMap<&'src str, (Span, Vec<TypeId>)>,
    variables: &mut Scopes<&'src str, TypeId>,
    statement: Spanned<'file, Statement<'src, 'file>>,
) -> Result<Spanned<'file, TypedStatement<'src, 'file>>, Box<Error<'file>>> {
    Ok(Spanned(
        match statement.0 {
            Statement::Expr(expr) => {
                let expr = typecheck_expression(engine, variables, expr)?;

                TypedStatement::Expr(expr)
            }
            Statement::Block(statements) => {
                push_scope(variables);

                let statements = Spanned(
                    statements
                        .0
                        .into_iter()
                        .map(|statement| {
                            typecheck_statement(engine, procedures, variables, statement)
                        })
                        .collect::<Result<_, _>>()?,
                    statements.1,
                );

                pop_scope(variables);

                TypedStatement::Block(statements)
            }
            Statement::Loop(statements) => {
                push_scope(variables);

                let statements = Spanned(
                    statements
                        .0
                        .into_iter()
                        .map(|statement| {
                            typecheck_statement(engine, procedures, variables, statement)
                        })
                        .collect::<Result<_, _>>()?,
                    statements.1,
                );

                pop_scope(variables);

                TypedStatement::Loop(statements)
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition = typecheck_expression(engine, variables, condition)?;
                let condition_ty =
                    engine.insert(type_to_typeinfo(Spanned(&condition.0.ty, condition.1)));

                let bool = engine.insert(Spanned(TypeInfo::Boolean, condition.1));

                engine.unify(condition_ty, bool)?;

                push_scope(variables);

                let then_branch = Spanned(
                    then_branch
                        .0
                        .into_iter()
                        .map(|statement| {
                            typecheck_statement(engine, procedures, variables, statement)
                        })
                        .collect::<Result<_, _>>()?,
                    then_branch.1,
                );

                pop_scope(variables);

                let else_branch = match else_branch {
                    Some(else_branch) => {
                        push_scope(variables);

                        let else_branch = Spanned(
                            else_branch
                                .0
                                .into_iter()
                                .map(|statement| {
                                    typecheck_statement(engine, procedures, variables, statement)
                                })
                                .collect::<Result<_, _>>()?,
                            else_branch.1,
                        );

                        pop_scope(variables);

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
            Statement::For {
                name,
                start,
                end,
                inclusive,
                body,
            } => {
                let start = typecheck_expression(engine, variables, start)?;
                let end = typecheck_expression(engine, variables, end)?;

                let start_ty = engine.insert(type_to_typeinfo(Spanned(&start.0.ty, start.1)));
                let end_ty = engine.insert(type_to_typeinfo(Spanned(&end.0.ty, end.1)));

                engine.unify(start_ty, end_ty)?;

                let integer = engine.insert(Spanned(TypeInfo::Integer, start.1));

                engine.unify(start_ty, integer)?;
                engine.unify(end_ty, integer)?;

                push_scope(variables);

                variables.insert(name.0, integer);

                let body = Spanned(
                    body.0
                        .into_iter()
                        .map(|statement| {
                            typecheck_statement(engine, procedures, variables, statement)
                        })
                        .collect::<Result<_, _>>()?,
                    body.1,
                );

                pop_scope(variables);

                TypedStatement::For {
                    name,
                    start,
                    end,
                    inclusive,
                    body,
                }
            }
            Statement::Let { name, value } => {
                let value = typecheck_expression(engine, variables, value)?;
                let value_ty = engine.insert(type_to_typeinfo(Spanned(&value.0.ty, value.1)));

                variables.insert(name.0, value_ty);

                TypedStatement::Let { name, value }
            }
            Statement::Assign { name, value } => {
                let value = typecheck_expression(engine, variables, value)?;
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
            Statement::Return => TypedStatement::Return,
            Statement::Action { name, args } => {
                let args = Spanned(
                    args.0
                        .into_iter()
                        .map(|expr| typecheck_expression(engine, variables, expr))
                        .collect::<Result<Vec<_>, _>>()?,
                    args.1,
                );

                let expected_types = match name.0 {
                    "wait" => {
                        action_args_count!(1, args.len(), name.1);

                        vec![vec![TypeInfo::Float]]
                    }
                    "waitframes" => {
                        action_args_count!(1, args.len(), name.1);

                        vec![vec![TypeInfo::Integer]]
                    }
                    "print" => {
                        action_args_count!(1, args.len(), name.1);

                        vec![vec![
                            TypeInfo::Boolean,
                            TypeInfo::Integer,
                            TypeInfo::Float,
                            TypeInfo::String,
                        ]]
                    }
                    _ => {
                        return Err(Box::new(Error::UnknownAction {
                            name: name.0.to_string(),
                            span: name.1,
                        }))
                    }
                };

                let expected_types = expected_types
                    .into_iter()
                    .zip(args.0.iter().map(|arg| arg.1))
                    .map(|(tys, span)| {
                        tys.into_iter()
                            .map(|ty| Spanned(ty, span))
                            .collect::<Vec<_>>()
                    })
                    .map(|tys| {
                        tys.into_iter()
                            .map(|ty| engine.insert(ty))
                            .collect::<Vec<_>>()
                    })
                    .collect::<Vec<_>>();

                for (expected, got) in expected_types
                    .into_iter()
                    .zip(args.0.iter().map(|arg| arg.0.ty))
                {
                    let got = engine.insert(type_to_typeinfo(Spanned(&got, args.1)));

                    let mut errors = vec![];

                    for expected in expected {
                        match engine.unify(expected, got) {
                            Ok(()) => {
                                errors.clear();
                                break;
                            }
                            Err(err) => errors.push(err),
                        }
                    }

                    if let Some(error) = errors.into_iter().next() {
                        return Err(error);
                    }
                }

                TypedStatement::Action { name, args }
            }
            Statement::Call { proc, args } => {
                let args = Spanned(
                    args.0
                        .into_iter()
                        .map(|expr| typecheck_expression(engine, variables, expr))
                        .collect::<Result<Vec<_>, _>>()?,
                    args.1,
                );

                let Some((_, arg_types)) = procedures.get(&proc.0) else {
                    return Err(Box::new(Error::UndefinedProcedure {
                        name: proc.to_string(),
                        span: proc.1,
                    }));
                };

                if arg_types.len() != args.0.len() {
                    return Err(Box::new(Error::WrongNumberOfProcedureArguments {
                        expected: arg_types.len(),
                        got: args.0.len(),
                        span: proc.1,
                    }));
                }

                for (ty, arg) in arg_types.iter().zip(args.0.iter().map(|arg| arg.0.ty)) {
                    let arg_ty = engine.insert(type_to_typeinfo(Spanned(&arg, args.1)));

                    engine.unify(*ty, arg_ty)?;
                }

                TypedStatement::Call { proc, args }
            }
        },
        statement.1,
    ))
}

macro_rules! action_args_count {
    ($count:expr, $len:expr, $span:expr) => {
        if $len != $count {
            return Err(Box::new(Error::WrongNumberOfActionArguments {
                expected: $count,
                got: $len,
                span: $span,
            }));
        }
    };
}
use action_args_count;

fn typecheck_expression<'src, 'file>(
    engine: &mut Engine<'file>,
    variables: &mut Scopes<&str, TypeId>,
    expr: Spanned<'file, Expr<'src, 'file>>,
) -> Result<Spanned<'file, TypedExpr<'src, 'file>>, Box<Error<'file>>> {
    Ok(Spanned(
        match expr.0 {
            Expr::Variable(var) => {
                let var_result = variables.get(&var);

                match var_result {
                    Some(ty) => TypedExpr {
                        expr: typed::Expr::Variable(var),
                        ty: engine.reconstruct(*ty)?.0,
                    },
                    None => {
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
            Expr::String(string) => TypedExpr {
                expr: typed::Expr::String(string),
                ty: Type::String,
            },
            Expr::Colour { r, g, b, a } => TypedExpr {
                expr: typed::Expr::Colour { r, g, b, a },
                ty: Type::Colour,
            },
            Expr::Vector { x, y } => {
                let x = typecheck_expression(engine, variables, x.map(|x| *x))?;
                let y = typecheck_expression(engine, variables, y.map(|y| *y))?;

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
                let lhs = typecheck_expression(engine, variables, lhs.map(|l| *l))?;
                let rhs = typecheck_expression(engine, variables, rhs.map(|r| *r))?;

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
                    (Float, Float, LessThan, Boolean),
                    (String, String, Equals, Boolean),
                    (String, String, NotEquals, Boolean),
                    (String, String, Plus, String)
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
                let expr = typecheck_expression(engine, variables, expr.map(|e| *e))?;

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

fn push_scope(variables: &mut Scopes<&str, TypeId>) {
    variables.push_scope();
}

fn pop_scope(variables: &mut Scopes<&str, TypeId>) {
    variables.pop_scope();
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
        let id = TypeId(self.id_counter);
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

            (a, b) if a == b => Ok(()),

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
            TypeInfo::String => Ok(Type::String),
            TypeInfo::Colour => Ok(Type::Colour),
            TypeInfo::Vector => Ok(Type::Vector),
        }
        .map(|ty| Spanned(ty, var.1))
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
struct TypeId(usize);

#[derive(Debug, PartialEq, Eq)]
enum TypeInfo {
    #[allow(dead_code)]
    Unknown,
    Ref(TypeId),
    Boolean,
    Integer,
    Float,
    String,
    Colour,
    Vector,
}

impl std::fmt::Display for TypeInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unknown => write!(f, "unknown"),
            Self::Ref(id) => write!(f, "ref {}", id.0),
            Self::Boolean => write!(f, "boolean"),
            Self::Integer => write!(f, "integer"),
            Self::Float => write!(f, "float"),
            Self::String => write!(f, "string"),
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
        Type::String => TypeInfo::String,
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
