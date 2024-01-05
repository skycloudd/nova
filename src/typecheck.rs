use crate::{
    ast::{
        typed::{self, Type, TypedExpr, TypedStatement},
        BinaryOp, Expr, Statement, UnaryOp,
    },
    error::Error,
    scopes::Scopes,
    Spanned,
};
use rustc_hash::FxHashMap;

pub fn typecheck<'src>(
    ast: &[Spanned<Statement<'src>>],
) -> (Option<Vec<Spanned<TypedStatement<'src>>>>, Vec<Error>) {
    Typechecker::new().typecheck_ast(ast)
}

struct Typechecker<'src> {
    engine: Engine,
    variables: Scopes<&'src str, TypeId>,
    const_variables: Scopes<&'src str, TypeId>,
}

impl<'src> Typechecker<'src> {
    fn new() -> Self {
        Self {
            engine: Engine::new(),
            variables: Scopes::new(),
            const_variables: Scopes::new(),
        }
    }

    fn push_scope(&mut self) {
        self.variables.push_scope();
        self.const_variables.push_scope();
    }

    fn pop_scope(&mut self) {
        self.variables.pop_scope();
        self.const_variables.pop_scope();
    }

    fn typecheck_ast(
        &mut self,
        ast: &[Spanned<Statement<'src>>],
    ) -> (Option<Vec<Spanned<TypedStatement<'src>>>>, Vec<Error>) {
        let mut statements = vec![];
        let mut errors = vec![];

        for statement in ast {
            match self.typecheck_statement(statement) {
                Ok(statement) => statements.push(statement),
                Err(error) => errors.push(error),
            }
        }

        (Some(statements), errors)
    }

    fn typecheck_statement(
        &mut self,
        statement: &Spanned<Statement<'src>>,
    ) -> Result<Spanned<TypedStatement<'src>>, Error> {
        Ok((
            match &statement.0 {
                Statement::Expr(expr) => {
                    let expr = self.typecheck_expr(expr)?;

                    TypedStatement::Expr(expr)
                }
                Statement::BuiltinPrint(expr) => {
                    let expr = self.typecheck_expr(expr)?;

                    TypedStatement::BuiltinPrint(expr)
                }
                Statement::Loop(statements) => {
                    self.push_scope();

                    let statements = (
                        statements
                            .0
                            .iter()
                            .map(|statement| self.typecheck_statement(statement))
                            .collect::<Result<Vec<_>, _>>()?,
                        statements.1,
                    );

                    self.pop_scope();

                    TypedStatement::Loop(statements)
                }
                Statement::If {
                    condition,
                    then_branch,
                    else_branch,
                } => {
                    let condition = self.typecheck_expr(condition)?;
                    let condition_ty = self
                        .engine
                        .insert(type_to_typeinfo((&condition.0.ty, condition.1)));

                    let bool = self.engine.insert((TypeInfo::Boolean, condition.1));

                    self.engine.unify(condition_ty, bool)?;

                    self.push_scope();

                    let then_branch = (
                        then_branch
                            .0
                            .iter()
                            .map(|statement| self.typecheck_statement(statement))
                            .collect::<Result<Vec<_>, _>>()?,
                        then_branch.1,
                    );

                    self.pop_scope();

                    let else_branch = match else_branch {
                        Some(else_branch) => {
                            self.push_scope();

                            let else_branch = (
                                else_branch
                                    .0
                                    .iter()
                                    .map(|statement| self.typecheck_statement(statement))
                                    .collect::<Result<Vec<_>, _>>()?,
                                else_branch.1,
                            );

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
                Statement::Let { name, value } => {
                    let value = self.typecheck_expr(value)?;
                    let value_ty = self.engine.insert(type_to_typeinfo((&value.0.ty, value.1)));

                    self.variables.insert(name.0, value_ty);

                    TypedStatement::Let { name: *name, value }
                }
                Statement::Const { name, value } => {
                    let value = self.typecheck_expr(value)?;
                    let value_ty = self.engine.insert(type_to_typeinfo((&value.0.ty, value.1)));

                    if self.variables.contains_key(&name.0)
                        || self.const_variables.contains_key(&name.0)
                    {
                        return Err(Error::ConstAlreadyDefined {
                            name: name.0.to_string(),
                            span: name.1,
                        });
                    }

                    self.const_variables.insert(name.0, value_ty);

                    TypedStatement::Const { name: *name, value }
                }
                Statement::Assign { name, value } => {
                    let value = self.typecheck_expr(value)?;
                    let value_ty = self.engine.insert(type_to_typeinfo((&value.0.ty, value.1)));

                    let name_ty = match self.variables.get(&name.0) {
                        Some(ty) => ty,
                        None => {
                            return Err(Error::UndefinedVariable {
                                name: name.0.to_string(),
                                span: name.1,
                            })
                        }
                    };

                    self.engine.unify(*name_ty, value_ty)?;

                    TypedStatement::Assign { name: *name, value }
                }
                Statement::Break => TypedStatement::Break,
                Statement::Continue => TypedStatement::Continue,
            },
            statement.1,
        ))
    }

    fn typecheck_expr(
        &mut self,
        expr: &Spanned<Expr<'src>>,
    ) -> Result<Spanned<TypedExpr<'src>>, Error> {
        Ok((
            match &expr.0 {
                Expr::Variable(var) => {
                    let var_result = self.variables.get(var);
                    let const_var_result = self.const_variables.get(var);

                    match (var_result, const_var_result) {
                        (Some(ty), _) => TypedExpr {
                            expr: typed::Expr::Variable(var),
                            ty: self.engine.reconstruct(*ty)?.0,
                        },

                        (None, Some(ty)) => TypedExpr {
                            expr: typed::Expr::Variable(var),
                            ty: self.engine.reconstruct(*ty)?.0,
                        },

                        (None, None) => {
                            return Err(Error::UndefinedVariable {
                                name: var.to_string(),
                                span: expr.1,
                            })
                        }
                    }
                }
                Expr::Boolean(boolean) => TypedExpr {
                    expr: typed::Expr::Boolean(*boolean),
                    ty: Type::Boolean,
                },
                Expr::Integer(integer) => TypedExpr {
                    expr: typed::Expr::Integer(*integer),
                    ty: Type::Integer,
                },
                Expr::Null => TypedExpr {
                    expr: typed::Expr::Null,
                    ty: Type::Null,
                },
                Expr::Colour { r, g, b } => TypedExpr {
                    expr: typed::Expr::Colour {
                        r: *r,
                        g: *g,
                        b: *b,
                    },
                    ty: Type::Colour,
                },
                Expr::Vector { x, y } => {
                    let x = self.typecheck_expr(x)?;
                    let y = self.typecheck_expr(y)?;

                    let x_ty = self.engine.insert(type_to_typeinfo((&x.0.ty, x.1)));
                    let y_ty = self.engine.insert(type_to_typeinfo((&y.0.ty, y.1)));

                    self.engine.unify(x_ty, y_ty)?;

                    let x_ty = self.engine.reconstruct(x_ty)?;

                    TypedExpr {
                        expr: typed::Expr::Vector {
                            x: Box::new(x),
                            y: Box::new(y),
                        },
                        ty: x_ty.0,
                    }
                }
                Expr::Binary(lhs, op, rhs) => {
                    let lhs = self.typecheck_expr(lhs)?;
                    let rhs = self.typecheck_expr(rhs)?;

                    let lhs_ty = self.engine.insert(type_to_typeinfo((&lhs.0.ty, lhs.1)));
                    let rhs_ty = self.engine.insert(type_to_typeinfo((&rhs.0.ty, rhs.1)));

                    self.engine.unify(lhs_ty, rhs_ty)?;

                    use BinaryOp::*;
                    use Type::*;
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
                        (Null, Null, Equals, Boolean),
                        (Null, Null, NotEquals, Boolean)
                    )
                    .map_err(|_| Error::BinaryOp {
                        lhs: lhs.0.ty.to_string(),
                        lhs_span: lhs.1,
                        rhs: rhs.0.ty.to_string(),
                        rhs_span: rhs.1,
                        op: op.0.to_string(),
                        op_span: op.1,
                    })?;

                    TypedExpr {
                        expr: typed::Expr::Binary(Box::new(lhs), *op, Box::new(rhs)),
                        ty,
                    }
                }
                Expr::Unary(op, expr) => {
                    let expr = self.typecheck_expr(expr)?;

                    use Type::*;
                    use UnaryOp::*;
                    let ty = unary_op!(
                        expr.0.ty,
                        op.0,
                        (Boolean, Not, Boolean),
                        (Integer, Negate, Integer)
                    )
                    .map_err(|_| Error::UnaryOp {
                        ty: expr.0.ty.to_string(),
                        ty_span: expr.1,
                        op: op.0.to_string(),
                        op_span: op.1,
                    })?;

                    TypedExpr {
                        expr: typed::Expr::Unary(*op, Box::new(expr)),
                        ty,
                    }
                }
            },
            expr.1,
        ))
    }
}

macro_rules! bin_op {
    ($lhs_value:expr, $rhs_value:expr, $op_value:expr, $(($lhs:pat, $rhs:pat, $op:pat, $ret_ty:expr)),*) => {
        match ($lhs_value, $rhs_value, $op_value) {
            $(
                ($lhs, $rhs, $op) => Ok($ret_ty),
            )*
            _ => Err(()),
        }
    };
}
use bin_op;

macro_rules! unary_op {
    ($value:expr, $op_value:expr, $(($ty:pat, $op:pat, $ret_ty:expr)),*) => {
        match ($value, $op_value) {
            $(
                ($ty, $op) => Ok($ret_ty),
            )*
            _ => Err(()),
        }
    };
}
use unary_op;

struct Engine {
    id_counter: usize,
    vars: FxHashMap<TypeId, Spanned<TypeInfo>>,
}

impl Engine {
    fn new() -> Self {
        Self {
            id_counter: 0,
            vars: FxHashMap::default(),
        }
    }

    fn insert(&mut self, info: Spanned<TypeInfo>) -> TypeId {
        self.id_counter += 1;
        let id = self.id_counter;
        self.vars.insert(id, info);
        id
    }

    fn unify(&mut self, a: TypeId, b: TypeId) -> Result<(), Error> {
        let var_a = self.vars[&a];
        let var_b = self.vars[&b];

        match (var_a.0, var_b.0) {
            (TypeInfo::Ref(a), _) => self.unify(a, b),
            (_, TypeInfo::Ref(b)) => self.unify(a, b),

            (TypeInfo::Unknown, _) => {
                self.vars.insert(a, (TypeInfo::Ref(b), var_b.1));
                Ok(())
            }
            (_, TypeInfo::Unknown) => {
                self.vars.insert(b, (TypeInfo::Ref(a), var_a.1));
                Ok(())
            }

            (TypeInfo::Boolean, TypeInfo::Boolean) => Ok(()),
            (TypeInfo::Integer, TypeInfo::Integer) => Ok(()),
            (TypeInfo::Null, TypeInfo::Null) => Ok(()),
            (TypeInfo::Colour, TypeInfo::Colour) => Ok(()),

            (a, b) => Err(Error::IncompatibleTypes {
                a: a.to_string(),
                a_span: var_a.1,
                b: b.to_string(),
                b_span: var_b.1,
            }),
        }
    }

    fn reconstruct(&self, id: TypeId) -> Result<Spanned<Type>, Error> {
        let var = self.vars[&id];

        Ok((
            match var.0 {
                TypeInfo::Unknown => return Err(Error::UnknownType { span: var.1 }),
                TypeInfo::Ref(id) => self.reconstruct(id)?.0,
                TypeInfo::Boolean => Type::Boolean,
                TypeInfo::Integer => Type::Integer,
                TypeInfo::Null => Type::Null,
                TypeInfo::Colour => Type::Colour,
            },
            var.1,
        ))
    }
}

type TypeId = usize;

#[derive(Clone, Copy)]
enum TypeInfo {
    #[allow(dead_code)]
    Unknown,
    Ref(TypeId),
    Boolean,
    Integer,
    Null,
    Colour,
}

impl std::fmt::Display for TypeInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeInfo::Unknown => write!(f, "unknown"),
            TypeInfo::Ref(id) => write!(f, "ref {}", id),
            TypeInfo::Boolean => write!(f, "boolean"),
            TypeInfo::Integer => write!(f, "integer"),
            TypeInfo::Null => write!(f, "null"),
            TypeInfo::Colour => write!(f, "colour"),
        }
    }
}

fn type_to_typeinfo(ty: Spanned<&Type>) -> Spanned<TypeInfo> {
    (
        match ty.0 {
            Type::Boolean => TypeInfo::Boolean,
            Type::Integer => TypeInfo::Integer,
            Type::Null => TypeInfo::Null,
            Type::Colour => TypeInfo::Colour,
        },
        ty.1,
    )
}
