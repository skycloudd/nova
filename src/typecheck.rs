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
use snake_case::is_snake_case;

pub fn typecheck<'src: 'file, 'file>(
    ast: Vec<Spanned<'file, TopLevel<'src, 'file>>>,
) -> (
    Vec<Spanned<'file, TypedTopLevel<'src, 'file>>>,
    Vec<Error<'file>>,
    Vec<Error<'file>>,
) {
    Typechecker::new().typecheck_ast(ast)
}

struct Typechecker<'src, 'file> {
    engine: Engine<'file>,
    procedures: FxHashMap<&'src str, (Span<'file>, Vec<TypeId>)>,
    variables: Scopes<&'src str, TypeId>,
}

impl<'src: 'file, 'file> Typechecker<'src, 'file> {
    fn new() -> Self {
        Self {
            engine: Engine::new(),
            procedures: FxHashMap::default(),
            variables: Scopes::new(),
        }
    }

    fn typecheck_ast(
        &mut self,
        ast: Vec<Spanned<'file, TopLevel<'src, 'file>>>,
    ) -> (
        Vec<Spanned<'file, TypedTopLevel<'src, 'file>>>,
        Vec<Error<'file>>,
        Vec<Error<'file>>,
    ) {
        let mut warnings = Vec::new();
        let mut errors = Vec::new();

        for top_level in &ast {
            let (name, args) = match &top_level.0 {
                TopLevel::Procedure(procedure) => (&procedure.name, &procedure.args),
                TopLevel::Run(_) | TopLevel::Error => continue,
            };

            match self.procedures.get(&name.0) {
                None => {
                    let args = args
                        .0
                        .iter()
                        .map(|(_, ty)| self.engine.insert(type_to_typeinfo(Spanned(ty, ty.1))))
                        .collect();

                    self.procedures.insert(name.0, (name.1, args));
                }
                Some((span, _)) => {
                    errors.push(Error::ProcedureRedefinition {
                        name: name.0.to_string(),
                        old_span: *span,
                        new_span: name.1,
                    });
                }
            }
        }

        let typed_ast = ast
            .into_iter()
            .map(|top_level| {
                top_level.map(|top_level| match top_level {
                    TopLevel::Procedure(procedure) => {
                        let (procedure, wrnings, errs) = self.typecheck_procedure(procedure);

                        warnings.extend(wrnings);
                        errors.extend(errs);

                        procedure
                    }
                    TopLevel::Run(name) => {
                        let (run, wrnings, errs) = self.typecheck_run(name);

                        warnings.extend(wrnings);
                        errors.extend(errs);

                        run
                    }
                    TopLevel::Error => TypedTopLevel::Error,
                })
            })
            .collect();

        (typed_ast, warnings, errors)
    }

    fn typecheck_run(
        &mut self,
        name: Spanned<'file, &'src str>,
    ) -> (
        TypedTopLevel<'src, 'file>,
        Vec<Error<'file>>,
        Vec<Error<'file>>,
    ) {
        let warnings = Vec::new();
        let mut errors = Vec::new();

        if !self.procedures.contains_key(name.0) {
            errors.push(Error::UndefinedProcedure {
                name: name.0.to_string(),
                span: name.1,
            });
        }

        let span = self.procedures[name.0].0;
        let number_args = self.procedures[name.0].1.len();

        if number_args != 0 {
            errors.push(Error::RunFunctionHasArgs {
                got: number_args,
                span,
                run_span: name.1,
            });
        }

        (TypedTopLevel::Run(name), warnings, errors)
    }

    fn typecheck_procedure(
        &mut self,
        procedure: Procedure<'src, 'file>,
    ) -> (
        TypedTopLevel<'src, 'file>,
        Vec<Error<'file>>,
        Vec<Error<'file>>,
    ) {
        let mut warnings = Vec::new();
        let mut errors = Vec::new();

        if bad_name(procedure.name.0) {
            warnings.push(Error::NameWarning {
                name: procedure.name.0.to_string(),
                span: procedure.name.1,
            });
        }

        self.push_scope();

        procedure
            .args
            .0
            .iter()
            .map(|(name, _)| name)
            .zip(&self.procedures.get(&procedure.name.0).unwrap().1)
            .for_each(|(name, ty)| {
                if bad_name(name.0) {
                    warnings.push(Error::NameWarning {
                        name: name.0.to_string(),
                        span: name.1,
                    });
                }

                self.variables.insert(name, *ty)
            });

        let body = procedure.body.map(|body| {
            body.into_iter()
                .map(|stmt| {
                    let (stmt, wrnings, errs) = self.typecheck_statement(stmt);

                    warnings.extend(wrnings);
                    errors.extend(errs);

                    stmt
                })
                .collect()
        });

        self.pop_scope();

        (
            TypedTopLevel::Procedure(TypedProcedure {
                name: procedure.name,
                args: procedure.args,
                body,
            }),
            warnings,
            errors,
        )
    }

    fn typecheck_statements(
        &mut self,
        statements: Spanned<'file, Vec<Spanned<'file, Statement<'src, 'file>>>>,
    ) -> (
        Spanned<'file, Vec<Spanned<'file, TypedStatement<'src, 'file>>>>,
        Vec<Error<'file>>,
        Vec<Error<'file>>,
    ) {
        let mut warnings = Vec::new();
        let mut errors = Vec::new();

        let statements = statements.map(|statements| {
            statements
                .into_iter()
                .map(|stmt| {
                    let (stmt, wrnings, errs) = self.typecheck_statement(stmt);

                    warnings.extend(wrnings);
                    errors.extend(errs);

                    stmt
                })
                .collect()
        });

        (statements, warnings, errors)
    }

    fn typecheck_statement(
        &mut self,
        statement: Spanned<'file, Statement<'src, 'file>>,
    ) -> (
        Spanned<'file, TypedStatement<'src, 'file>>,
        Vec<Error<'file>>,
        Vec<Error<'file>>,
    ) {
        let mut warnings = Vec::new();
        let mut errors = Vec::new();

        let stmt = statement.map(|statement| match statement {
            Statement::Error => TypedStatement::Error,
            Statement::Expr(expr) => {
                let (expr, wrnings, errs) = self.typecheck_expression(expr);

                warnings.extend(wrnings);
                errors.extend(errs);

                TypedStatement::Expr(expr)
            }
            Statement::Block(statements) => {
                self.push_scope();

                let (statements, wrnings, errs) = self.typecheck_statements(statements);

                warnings.extend(wrnings);
                errors.extend(errs);

                self.pop_scope();

                TypedStatement::Block(statements)
            }
            Statement::Loop(statements) => {
                self.push_scope();

                let (statements, wrnings, errs) = self.typecheck_statements(statements);

                warnings.extend(wrnings);
                errors.extend(errs);

                self.pop_scope();

                TypedStatement::Loop(statements)
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let (condition, wrnings, errs) = self.typecheck_expression(condition);

                warnings.extend(wrnings);
                errors.extend(errs);

                let condition_ty = self
                    .engine
                    .insert(type_to_typeinfo(Spanned(&condition.0.ty, condition.1)));

                self.engine
                    .expect(condition_ty, TypeInfo::Boolean)
                    .unwrap_or_else(|err| {
                        errors.push(*err);
                    });

                self.push_scope();

                let (then_branch, wrnings, errs) = self.typecheck_statements(then_branch);

                warnings.extend(wrnings);
                errors.extend(errs);

                self.pop_scope();

                let else_branch = match else_branch {
                    Some(else_branch) => {
                        self.push_scope();

                        let (else_branch, wrnings, errs) = self.typecheck_statements(else_branch);

                        warnings.extend(wrnings);
                        errors.extend(errs);

                        self.pop_scope();

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
                let (start, wrnings, errs) = self.typecheck_expression(start);

                warnings.extend(wrnings);
                errors.extend(errs);

                let (end, wrnings, errs) = self.typecheck_expression(end);

                warnings.extend(wrnings);
                errors.extend(errs);

                let start_ty = self
                    .engine
                    .insert(type_to_typeinfo(Spanned(&start.0.ty, start.1)));
                let end_ty = self
                    .engine
                    .insert(type_to_typeinfo(Spanned(&end.0.ty, end.1)));

                self.engine
                    .expect(start_ty, TypeInfo::Integer)
                    .unwrap_or_else(|err| {
                        errors.push(*err);
                    });

                self.engine
                    .expect(end_ty, TypeInfo::Integer)
                    .unwrap_or_else(|err| {
                        errors.push(*err);
                    });

                self.push_scope();

                if bad_name(name.0) {
                    warnings.push(Error::NameWarning {
                        name: name.0.to_string(),
                        span: name.1,
                    });
                }

                let name_ty = self
                    .engine
                    .insert(type_to_typeinfo(Spanned(&Type::Integer, name.1)));
                self.variables.insert(name.0, name_ty);

                let (body, wrnings, errs) = self.typecheck_statements(body);

                warnings.extend(wrnings);
                errors.extend(errs);

                self.pop_scope();

                TypedStatement::For {
                    name,
                    start,
                    end,
                    inclusive,
                    body,
                }
            }
            Statement::Let { name, value } => {
                let (value, wrnings, errs) = self.typecheck_expression(value);

                warnings.extend(wrnings);
                errors.extend(errs);

                let value_ty = self
                    .engine
                    .insert(type_to_typeinfo(Spanned(&value.0.ty, value.1)));

                if bad_name(name.0) {
                    warnings.push(Error::NameWarning {
                        name: name.0.to_string(),
                        span: name.1,
                    });
                }

                self.variables.insert(name.0, value_ty);

                TypedStatement::Let { name, value }
            }
            Statement::Assign { name, value } => {
                let (value, wrnings, errs) = self.typecheck_expression(value);

                warnings.extend(wrnings);
                errors.extend(errs);

                let value_ty = self
                    .engine
                    .insert(type_to_typeinfo(Spanned(&value.0.ty, value.1)));

                let Some(name_ty) = self.variables.get(&name.0) else {
                    errors.push(Error::UndefinedVariable {
                        name: name.0.to_string(),
                        span: name.1,
                    });

                    return TypedStatement::Error;
                };

                self.engine.unify(*name_ty, value_ty).unwrap_or_else(|err| {
                    errors.push(*err);
                });

                TypedStatement::Assign { name, value }
            }
            Statement::Break => TypedStatement::Break,
            Statement::Continue => TypedStatement::Continue,
            Statement::Return => TypedStatement::Return,
            Statement::Action { name, args } => {
                let args = args.map(|args| {
                    args.into_iter()
                        .map(|arg| {
                            let (arg, wrnings, errs) = self.typecheck_expression(arg);

                            warnings.extend(wrnings);
                            errors.extend(errs);

                            arg
                        })
                        .collect::<Vec<_>>()
                });

                let expected_types = match name.0 {
                    "wait" => {
                        if (args.len()) != 1 {
                            errors.push(Error::WrongNumberOfActionArguments {
                                expected: 1,
                                got: (args.len()),
                                span: (name.1),
                            });

                            return TypedStatement::Error;
                        };

                        vec![vec![TypeInfo::Float]]
                    }
                    "waitframes" => {
                        if (args.len()) != 1 {
                            errors.push(Error::WrongNumberOfActionArguments {
                                expected: 1,
                                got: (args.len()),
                                span: (name.1),
                            });

                            return TypedStatement::Error;
                        };

                        vec![vec![TypeInfo::Integer]]
                    }
                    "print" => {
                        if (args.len()) != 1 {
                            errors.push(Error::WrongNumberOfActionArguments {
                                expected: 1,
                                got: (args.len()),
                                span: (name.1),
                            });

                            return TypedStatement::Error;
                        };

                        vec![vec![
                            TypeInfo::Boolean,
                            TypeInfo::Integer,
                            TypeInfo::Float,
                            TypeInfo::String,
                        ]]
                    }
                    _ => {
                        errors.push(Error::UnknownAction {
                            name: name.0.to_string(),
                            span: name.1,
                        });

                        return TypedStatement::Error;
                    }
                };

                for (expected, got) in expected_types
                    .into_iter()
                    .zip(args.0.iter().map(|arg| arg.0.ty))
                {
                    let got = self.engine.insert(type_to_typeinfo(Spanned(&got, args.1)));

                    let mut my_errs = vec![];

                    for expected in expected {
                        match self.engine.expect(got, expected) {
                            Ok(()) => {
                                my_errs.clear();
                                break;
                            }
                            Err(err) => my_errs.push(err),
                        }
                    }

                    if let Some(error) = my_errs.into_iter().next() {
                        errors.push(*error);
                    }
                }

                TypedStatement::Action { name, args }
            }
            Statement::Call { proc, args } => {
                let args = args.map(|args| {
                    args.into_iter()
                        .map(|arg| {
                            let (arg, wrnings, errs) = self.typecheck_expression(arg);

                            warnings.extend(wrnings);
                            errors.extend(errs);

                            arg
                        })
                        .collect::<Vec<_>>()
                });

                let Some((_, arg_types)) = self.procedures.get(&proc.0) else {
                    errors.push(Error::UndefinedProcedure {
                        name: proc.to_string(),
                        span: proc.1,
                    });

                    return TypedStatement::Error;
                };

                if arg_types.len() != args.0.len() {
                    errors.push(Error::WrongNumberOfProcedureArguments {
                        expected: arg_types.len(),
                        got: args.0.len(),
                        span: proc.1,
                    });

                    return TypedStatement::Error;
                }

                for (ty, arg) in arg_types.iter().zip(args.0.iter().map(|arg| arg.0.ty)) {
                    let arg_ty = self.engine.insert(type_to_typeinfo(Spanned(&arg, args.1)));

                    self.engine.unify(*ty, arg_ty).unwrap_or_else(|err| {
                        errors.push(*err);
                    });
                }

                TypedStatement::Call { proc, args }
            }
        });

        (stmt, warnings, errors)
    }

    fn typecheck_expression(
        &mut self,
        expr: Spanned<'file, Expr<'src, 'file>>,
    ) -> (
        Spanned<'file, TypedExpr<'src, 'file>>,
        Vec<Error<'file>>,
        Vec<Error<'file>>,
    ) {
        let mut warnings = Vec::new();
        let mut errors = Vec::new();

        let expr = expr.map(|expr| match expr {
            Expr::Error => TypedExpr {
                expr: typed::Expr::Error,
                ty: Type::Error,
            },
            Expr::Variable(var) => {
                let var_result = self.variables.get(&var);

                match var_result {
                    Some(ty) => TypedExpr {
                        expr: typed::Expr::Variable(var),
                        ty: match self.engine.reconstruct(*ty) {
                            Ok(ty) => ty.0,
                            Err(err) => {
                                errors.push(*err);
                                Type::Error
                            }
                        },
                    },
                    None => {
                        errors.push(Error::UndefinedVariable {
                            name: var.0.to_string(),
                            span: var.1,
                        });

                        TypedExpr {
                            expr: typed::Expr::Error,
                            ty: Type::Error,
                        }
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
                let (x, wrnings, errs) = self.typecheck_expression(x.map(|x| *x));

                warnings.extend(wrnings);
                errors.extend(errs);

                let (y, wrnings, errs) = self.typecheck_expression(y.map(|y| *y));

                warnings.extend(wrnings);
                errors.extend(errs);

                let x_ty = self.engine.insert(type_to_typeinfo(Spanned(&x.0.ty, x.1)));
                let y_ty = self.engine.insert(type_to_typeinfo(Spanned(&y.0.ty, y.1)));

                self.engine.unify(x_ty, y_ty).unwrap_or_else(|err| {
                    errors.push(*err);
                });

                let float_ty = self.engine.insert(Spanned(TypeInfo::Float, x.1));

                self.engine.unify(x_ty, float_ty).unwrap_or_else(|err| {
                    errors.push(*err);
                });
                self.engine.unify(y_ty, float_ty).unwrap_or_else(|err| {
                    errors.push(*err);
                });

                TypedExpr {
                    expr: typed::Expr::Vector {
                        x: x.map(Box::new),
                        y: y.map(Box::new),
                    },
                    ty: Type::Vector,
                }
            }
            Expr::Binary(lhs, op, rhs) => {
                let (lhs, wrnings, errs) = self.typecheck_expression(lhs.map(|l| *l));

                warnings.extend(wrnings);
                errors.extend(errs);

                let (rhs, wrnings, errs) = self.typecheck_expression(rhs.map(|r| *r));

                warnings.extend(wrnings);
                errors.extend(errs);

                let lhs_ty = self
                    .engine
                    .insert(type_to_typeinfo(Spanned(&lhs.0.ty, lhs.1)));
                let rhs_ty = self
                    .engine
                    .insert(type_to_typeinfo(Spanned(&rhs.0.ty, rhs.1)));

                self.engine.unify(lhs_ty, rhs_ty).unwrap_or_else(|err| {
                    errors.push(*err);
                });

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
                });

                let ty = match ty {
                    Ok(ty) => ty,
                    Err(err) => {
                        errors.push(err);
                        Type::Error
                    }
                };

                TypedExpr {
                    expr: typed::Expr::Binary(lhs.map(Box::new), op, rhs.map(Box::new)),
                    ty,
                }
            }
            Expr::Unary(op, expr) => {
                let (expr, wrnings, errs) = self.typecheck_expression(expr.map(|e| *e));

                warnings.extend(wrnings);
                errors.extend(errs);

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
                });

                let ty = match ty {
                    Ok(ty) => ty,
                    Err(err) => {
                        errors.push(err);
                        Type::Error
                    }
                };

                TypedExpr {
                    expr: typed::Expr::Unary(op, expr.map(Box::new)),
                    ty,
                }
            }
        });

        (expr, warnings, errors)
    }

    fn push_scope(&mut self) {
        self.variables.push_scope();
    }

    fn pop_scope(&mut self) {
        self.variables.pop_scope();
    }
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

    fn expect(&self, id: TypeId, ty: TypeInfo) -> Result<(), Box<Error<'file>>> {
        let var = &self.vars[&id];

        if var.0 == ty {
            Ok(())
        } else {
            Err(Box::new(Error::IncompatibleTypes {
                a: var.0.to_string(),
                a_span: var.1,
                b: ty.to_string(),
                b_span: var.1,
            }))
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
        Type::Error => TypeInfo::Unknown,
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

fn bad_name(name: &str) -> bool {
    !is_snake_case(name)
}
