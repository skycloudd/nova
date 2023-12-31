use crate::{
    ast::{
        typed::{self, Type, TypedExpr, TypedStatement},
        BinaryOp, Expr, Statement, UnaryOp,
    },
    error::Error,
    Spanned,
};
use rustc_hash::FxHashMap;
use std::hash::Hash;

pub fn typecheck<'src>(
    ast: &[Spanned<Statement<'src>>],
) -> (Option<Vec<Spanned<TypedStatement<'src>>>>, Vec<Error>) {
    Typechecker::new().typecheck_ast(ast)
}

struct Typechecker<'src> {
    engine: Engine,
    bindings: Scopes<&'src str, TypeId>,
}

impl<'src> Typechecker<'src> {
    fn new() -> Self {
        Self {
            engine: Engine::new(),
            bindings: Scopes::new(),
        }
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
                    self.bindings.push_scope();

                    let statements = (
                        statements
                            .0
                            .iter()
                            .map(|statement| self.typecheck_statement(statement))
                            .collect::<Result<Vec<_>, _>>()?,
                        statements.1,
                    );

                    self.bindings.pop_scope();

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

                    self.bindings.push_scope();

                    let then_branch = (
                        then_branch
                            .0
                            .iter()
                            .map(|statement| self.typecheck_statement(statement))
                            .collect::<Result<Vec<_>, _>>()?,
                        then_branch.1,
                    );

                    self.bindings.pop_scope();

                    let else_branch = match else_branch {
                        Some(else_branch) => {
                            self.bindings.push_scope();

                            let else_branch = (
                                else_branch
                                    .0
                                    .iter()
                                    .map(|statement| self.typecheck_statement(statement))
                                    .collect::<Result<Vec<_>, _>>()?,
                                else_branch.1,
                            );

                            self.bindings.pop_scope();

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

                    self.bindings.insert(name.0, value_ty);

                    TypedStatement::Let { name: *name, value }
                }
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
                Expr::Variable(var) => match self.bindings.get(&var.0) {
                    Some(ty) => TypedExpr {
                        expr: typed::Expr::Variable(*var),
                        ty: self.engine.reconstruct(*ty)?.0,
                    },
                    None => todo!("error"),
                },
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
                Expr::Binary(lhs, op, rhs) => {
                    let lhs = self.typecheck_expr(lhs)?;
                    let rhs = self.typecheck_expr(rhs)?;

                    let lhs_ty = self.engine.insert(type_to_typeinfo((&lhs.0.ty, lhs.1)));
                    let rhs_ty = self.engine.insert(type_to_typeinfo((&rhs.0.ty, rhs.1)));

                    bin_op!(
                        lhs.0.ty,
                        rhs.0.ty,
                        op.0,
                        (Type::Boolean, Type::Boolean, BinaryOp::Equals),
                        (Type::Boolean, Type::Boolean, BinaryOp::NotEquals),
                        (Type::Integer, Type::Integer, BinaryOp::Equals),
                        (Type::Integer, Type::Integer, BinaryOp::NotEquals),
                        (Type::Integer, Type::Integer, BinaryOp::Plus),
                        (Type::Integer, Type::Integer, BinaryOp::Minus),
                        (Type::Integer, Type::Integer, BinaryOp::Multiply),
                        (Type::Integer, Type::Integer, BinaryOp::Divide),
                        (Type::Integer, Type::Integer, BinaryOp::GreaterThanEquals),
                        (Type::Integer, Type::Integer, BinaryOp::LessThanEquals),
                        (Type::Integer, Type::Integer, BinaryOp::GreaterThan),
                        (Type::Integer, Type::Integer, BinaryOp::LessThan)
                    );

                    self.engine.unify(lhs_ty, rhs_ty)?;

                    let ty = self.engine.reconstruct(lhs_ty)?.0;

                    TypedExpr {
                        expr: typed::Expr::Binary(Box::new(lhs), *op, Box::new(rhs)),
                        ty,
                    }
                }
                Expr::Unary(op, expr) => {
                    let expr = self.typecheck_expr(expr)?;

                    let expr_ty = self.engine.insert(type_to_typeinfo((&expr.0.ty, expr.1)));

                    unary_op!(
                        expr.0.ty,
                        op.0,
                        (Type::Boolean, UnaryOp::Not),
                        (Type::Integer, UnaryOp::Negate)
                    );

                    let ty = self.engine.reconstruct(expr_ty)?.0;

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
    ($lhs_value:expr, $rhs_value:expr, $op_value:expr, $(($lhs:pat, $rhs:pat, $op:pat)),*) => {
        match ($lhs_value, $rhs_value, $op_value) {
            $(
                ($lhs, $rhs, $op) => $lhs_value,
            )*
            _ => todo!("error"),
        }
    };
}
use bin_op;

macro_rules! unary_op {
    ($value:expr, $op_value:expr, $(($ty:pat, $op:pat)),*) => {
        match ($value, $op_value) {
            $(
                ($ty, $op) => $value,
            )*
            _ => todo!("error"),
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

            (_, _) => todo!("error"),
        }
    }

    fn reconstruct(&self, id: TypeId) -> Result<Spanned<Type>, Error> {
        let var = self.vars[&id];

        Ok((
            match var.0 {
                TypeInfo::Unknown => {
                    todo!("error")
                }
                TypeInfo::Ref(id) => self.reconstruct(id)?.0,
                TypeInfo::Boolean => Type::Boolean,
                TypeInfo::Integer => Type::Integer,
                TypeInfo::Null => Type::Null,
            },
            var.1,
        ))
    }
}

type TypeId = usize;

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
enum TypeInfo {
    #[allow(dead_code)]
    Unknown,
    Ref(TypeId),
    Boolean,
    Integer,
    Null,
}

fn type_to_typeinfo(ty: Spanned<&Type>) -> Spanned<TypeInfo> {
    (
        match ty.0 {
            Type::Boolean => TypeInfo::Boolean,
            Type::Integer => TypeInfo::Integer,
            Type::Null => TypeInfo::Null,
        },
        ty.1,
    )
}

struct Scopes<K, V> {
    base: FxHashMap<K, V>,
    scopes: Vec<FxHashMap<K, V>>,
}

impl<K: Eq + Hash, V> Scopes<K, V> {
    fn new() -> Self {
        Self {
            base: FxHashMap::default(),
            scopes: vec![],
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(FxHashMap::default());
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
