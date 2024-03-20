use crate::{
    ast::{
        typed::{self, TypedExpr, TypedFunction, TypedStatement, TypedTopLevel},
        BinaryOp, Expr, Function, Statement, TopLevel, Type, UnaryOp,
    },
    error::{Error, Warning},
    scopes::Scopes,
    span::Spanned,
};
use rustc_hash::FxHashMap;
use snake_case::is_snake_case;

pub fn typecheck<'src, 'file>(
    ast: Vec<Spanned<'file, TopLevel<'src, 'file>>>,
) -> (
    Vec<Spanned<'file, TypedTopLevel<'src, 'file>>>,
    Vec<Warning<'file>>,
    Vec<Error<'file>>,
) {
    let mut warnings = Vec::new();
    let mut errors = Vec::new();

    let mut typechecker = Typechecker::new(&mut warnings, &mut errors);

    (typechecker.typecheck_ast(ast), warnings, errors)
}

#[derive(Debug)]
struct FunctionSignature<'file> {
    args: Spanned<'file, Vec<Spanned<'file, TypeId>>>,
    return_ty: Spanned<'file, TypeId>,
}

struct Typechecker<'src, 'file, 'w, 'e> {
    engine: Engine<'file>,
    functions: FxHashMap<&'src str, FunctionSignature<'file>>,
    variables: Scopes<&'src str, TypeId>,

    current_function: Option<&'src str>,

    warnings: &'w mut Vec<Warning<'file>>,
    errors: &'e mut Vec<Error<'file>>,
}

impl<'src, 'file, 'w, 'e> Typechecker<'src, 'file, 'w, 'e> {
    fn new(warnings: &'w mut Vec<Warning<'file>>, errors: &'e mut Vec<Error<'file>>) -> Self {
        Self {
            engine: Engine::new(),
            functions: FxHashMap::default(),
            variables: Scopes::new(),

            current_function: None,

            warnings,
            errors,
        }
    }

    fn typecheck_ast(
        &mut self,
        ast: Vec<Spanned<'file, TopLevel<'src, 'file>>>,
    ) -> Vec<Spanned<'file, TypedTopLevel<'src, 'file>>> {
        for top_level in &ast {
            let TopLevel::Function(function) = &top_level.0;

            match self.functions.get(&function.name.0) {
                None => {
                    let args = Spanned(
                        function
                            .args
                            .0
                            .iter()
                            .map(|(_, ty)| Spanned(self.engine.insert_type(ty), ty.1))
                            .collect(),
                        function.args.1,
                    );

                    let return_ty = Spanned(
                        self.engine.insert_type(&function.return_ty),
                        function.return_ty.1,
                    );

                    self.functions
                        .insert(function.name.0, FunctionSignature { args, return_ty });
                }
                Some(_) => panic!(),
            }
        }

        let mut top_levels = Vec::with_capacity(ast.len());

        for top_level in ast {
            let top_level = Spanned(
                match top_level.0 {
                    TopLevel::Function(function) => self.typecheck_function(function),
                },
                top_level.1,
            );

            top_levels.push(top_level);
        }

        top_levels
    }

    fn typecheck_function(
        &mut self,
        function: Function<'src, 'file>,
    ) -> TypedTopLevel<'src, 'file> {
        self.current_function = Some(&function.name.0);

        if !is_snake_case(function.name.0) {
            self.warnings.push(Warning::BadName {
                name: function.name.0.to_string(),
                span: function.name.1,
            });
        }

        self.push_scope();

        for (name, ty) in function
            .args
            .0
            .iter()
            .map(|(name, _)| name)
            .zip(&self.functions.get(&function.name.0).unwrap().args.0)
        {
            if !is_snake_case(name.0) {
                self.warnings.push(Warning::BadName {
                    name: name.0.to_string(),
                    span: name.1,
                });
            }

            self.variables.insert(name.0, ty.0);
        }

        let body = Spanned(
            function
                .body
                .0
                .into_iter()
                .map(|stmt| self.typecheck_statement(stmt))
                .collect(),
            function.body.1,
        );

        self.pop_scope();

        self.current_function = None;

        TypedTopLevel::Function(TypedFunction {
            name: function.name,
            args: function.args.clone(),
            return_ty: function.return_ty,
            body,
        })
    }

    fn typecheck_statements(
        &mut self,
        statements: Spanned<'file, Vec<Spanned<'file, Statement<'src, 'file>>>>,
    ) -> Spanned<'file, Vec<Spanned<'file, TypedStatement<'src, 'file>>>> {
        Spanned(
            statements
                .0
                .into_iter()
                .map(|stmt| self.typecheck_statement(stmt))
                .collect(),
            statements.1,
        )
    }

    #[allow(clippy::too_many_lines)]
    fn typecheck_statement(
        &mut self,
        statement: Spanned<'file, Statement<'src, 'file>>,
    ) -> Spanned<'file, TypedStatement<'src, 'file>> {
        Spanned(
            match statement.0 {
                Statement::Expr(expr) => {
                    let expr = self.typecheck_expression(expr.clone());

                    TypedStatement::Expr(expr)
                }
                Statement::Block(statements) => {
                    self.push_scope();

                    let statements = self.typecheck_statements(statements);

                    self.pop_scope();

                    TypedStatement::Block(statements)
                }
                Statement::Loop(statements) => {
                    self.push_scope();

                    let statements = self.typecheck_statements(statements);

                    self.pop_scope();

                    TypedStatement::Loop(statements)
                }
                Statement::If {
                    condition,
                    then_branch,
                    else_branch,
                } => {
                    let condition = self.typecheck_expression(condition.clone());

                    let condition_ty = self
                        .engine
                        .insert_type(&Spanned(condition.0.ty, condition.1));

                    self.engine
                        .expect(condition_ty, &TypeInfo::Boolean)
                        .unwrap_or_else(|err| {
                            self.errors.push(*err);
                        });

                    self.push_scope();

                    let then_branch = self.typecheck_statements(then_branch);

                    self.pop_scope();

                    let else_branch = else_branch.map_or_else(
                        || None,
                        |else_branch| {
                            self.push_scope();

                            let else_branch = self.typecheck_statements(else_branch);

                            self.pop_scope();

                            Some(else_branch)
                        },
                    );

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
                    let start = self.typecheck_expression(start.clone());

                    let start_ty = self.engine.insert_type(&Spanned(start.0.ty, start.1));

                    let end = self.typecheck_expression(end.clone());

                    let end_ty = self.engine.insert_type(&Spanned(end.0.ty, end.1));

                    self.engine
                        .expect(start_ty, &TypeInfo::Integer)
                        .unwrap_or_else(|err| {
                            self.errors.push(*err);
                        });

                    self.engine
                        .expect(end_ty, &TypeInfo::Integer)
                        .unwrap_or_else(|err| {
                            self.errors.push(*err);
                        });

                    self.push_scope();

                    if !is_snake_case(name.0) {
                        self.warnings.push(Warning::BadName {
                            name: name.0.to_string(),
                            span: name.1,
                        });
                    }

                    let name_ty = self.engine.insert_type(&Spanned(Type::Integer, name.1));
                    self.variables.insert(name.0, name_ty);

                    let body = self.typecheck_statements(body);

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
                    let value = self.typecheck_expression(value.clone());

                    let value_ty = self.engine.insert_type(&Spanned(value.0.ty, value.1));

                    if !is_snake_case(name.0) {
                        self.warnings.push(Warning::BadName {
                            name: name.0.to_string(),
                            span: name.1,
                        });
                    }

                    self.variables.insert(name.0, value_ty);

                    TypedStatement::Let { name, value }
                }
                Statement::Assign { name, value } => {
                    let value = self.typecheck_expression(value.clone());

                    let value_ty = self.engine.insert_type(&Spanned(value.0.ty, value.1));

                    let Some(name_ty) = self.variables.get(&name.0) else {
                        panic!()
                    };

                    self.engine.unify(*name_ty, value_ty).unwrap_or_else(|err| {
                        self.errors.push(*err);
                    });

                    TypedStatement::Assign { name, value }
                }
                Statement::Break => TypedStatement::Break,
                Statement::Continue => TypedStatement::Continue,
                Statement::Return(expr) => {
                    let expr = self.typecheck_expression(expr.clone());

                    let return_ty = self
                        .functions
                        .get(self.current_function.unwrap())
                        .unwrap()
                        .return_ty;

                    let expr_ty = self.engine.insert_type(&Spanned(expr.0.ty, expr.1));

                    self.engine
                        .unify(return_ty.0, expr_ty)
                        .unwrap_or_else(|err| {
                            self.errors.push(*err);
                        });

                    TypedStatement::Return(expr)
                }
            },
            statement.1,
        )
    }

    #[allow(clippy::too_many_lines)]
    fn typecheck_expression(
        &mut self,
        expr: Spanned<'file, Expr<'src, 'file>>,
    ) -> Spanned<'file, TypedExpr<'src, 'file>> {
        Spanned(
            match expr.0 {
                Expr::Error => TypedExpr {
                    expr: typed::Expr::Error,
                    ty: Type::Error,
                },
                Expr::Variable(var) => {
                    let var_result = self.variables.get(&var.0);

                    if let Some(ty) = var_result {
                        TypedExpr {
                            expr: typed::Expr::Variable(var),
                            ty: match self.engine.reconstruct(*ty) {
                                Ok(ty) => ty.0,
                                Err(err) => {
                                    self.errors.push(*err);
                                    Type::Error
                                }
                            },
                        }
                    } else {
                        panic!()
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
                Expr::Binary(lhs, op, rhs) => {
                    let lhs = self.typecheck_expression(Spanned(*lhs.0, lhs.1));

                    let lhs_ty = self.engine.insert_type(&Spanned(lhs.0.ty, lhs.1));

                    let rhs = self.typecheck_expression(Spanned(*rhs.0, rhs.1));

                    let rhs_ty = self.engine.insert_type(&Spanned(rhs.0.ty, rhs.1));

                    let ty = bin_op!(
                        &lhs.0.ty,
                        &rhs.0.ty,
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
                    .map_err(|()| panic!());

                    let ty = match ty {
                        Ok(ty) => ty,
                        Err(err) => {
                            self.errors.push(err);
                            Type::Error
                        }
                    };

                    self.engine.unify(lhs_ty, rhs_ty).ok();

                    TypedExpr {
                        expr: typed::Expr::Binary(
                            Spanned(Box::new(lhs.0), lhs.1),
                            op,
                            Spanned(Box::new(rhs.0), rhs.1),
                        ),
                        ty,
                    }
                }
                Expr::Unary(op, expr) => {
                    let expr = self.typecheck_expression(Spanned(*expr.0, expr.1));

                    let ty = match (&expr.0.ty, op.0) {
                        (Type::Integer, UnaryOp::Negate) => Ok(Type::Integer),
                        (Type::Float, UnaryOp::Negate) => Ok(Type::Float),
                        (Type::Boolean, UnaryOp::Not) => Ok(Type::Boolean),
                        _ => panic!(),
                    };

                    let ty = match ty {
                        Ok(ty) => ty,
                        Err(err) => {
                            self.errors.push(err);
                            Type::Error
                        }
                    };

                    TypedExpr {
                        expr: typed::Expr::Unary(op, Spanned(Box::new(expr.0), expr.1)),
                        ty,
                    }
                }
                Expr::Convert { ty, expr } => {
                    let expr = self.typecheck_expression(Spanned(*expr.0, expr.1));

                    #[allow(clippy::match_same_arms)]
                    match (&expr.0.ty, &ty.0) {
                        (from, to) if from == to => {}

                        (Type::Error, _) | (_, Type::Error) => {}

                        (Type::Integer, Type::Float | Type::String) => {}

                        (Type::Float, Type::Integer | Type::String) => {}

                        _ => panic!(),
                    }

                    TypedExpr {
                        ty: ty.0,
                        expr: typed::Expr::Convert {
                            ty,
                            expr: Spanned(Box::new(expr.0), expr.1),
                        },
                    }
                }
                Expr::Call { func, args } => {
                    let args = Spanned(
                        args.0
                            .into_iter()
                            .map(|arg| self.typecheck_expression(arg))
                            .collect::<Vec<_>>(),
                        args.1,
                    );

                    let Some(signature) = self.functions.get(&func.0) else {
                        self.errors.push(Error::UndefinedFunction {
                            name: func.0.to_string(),
                            span: func.1,
                        });

                        return Spanned(
                            TypedExpr {
                                expr: typed::Expr::Error,
                                ty: Type::Error,
                            },
                            expr.1,
                        );
                    };

                    if args.0.len() != signature.args.0.len() {
                        self.errors.push(Error::FunctionArgumentCountMismatch {
                            expected: signature.args.0.len(),
                            found: args.0.len(),
                            expected_span: signature.args.1,
                            found_span: args.1,
                        });
                    }

                    for (arg, ty) in args.0.iter().zip(&signature.args.0) {
                        let arg_ty = self.engine.insert_type(&Spanned(arg.0.ty, arg.1));

                        self.engine.unify(arg_ty, ty.0).unwrap_or_else(|err| {
                            self.errors.push(*err);
                        });
                    }

                    let return_ty = match self.engine.reconstruct(signature.return_ty.0) {
                        Ok(ty) => ty.0,
                        Err(err) => {
                            self.errors.push(*err);
                            Type::Error
                        }
                    };

                    TypedExpr {
                        expr: typed::Expr::Call { func, args },
                        ty: return_ty,
                    }
                }
            },
            expr.1,
        )
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

    fn insert_type(&mut self, ty: &Spanned<'file, Type>) -> TypeId {
        self.insert(Spanned(ty.0.into(), ty.1))
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

            (TypeInfo::Error, _) | (_, TypeInfo::Error) => Ok(()),

            (a, b) if a == b => Ok(()),

            (&a, &b) => Err(Box::new(Error::TypeMismatch {
                expected: a.into(),
                found: b.into(),
                span: var_b.1,
            })),
        }
    }

    fn expect(&self, id: TypeId, ty: &TypeInfo) -> Result<(), Box<Error<'file>>> {
        let var = &self.vars[&id];

        if &var.0 == ty {
            Ok(())
        } else {
            Err(Box::new(Error::TypeMismatch {
                expected: *ty,
                found: var.0,
                span: var.1,
            }))
        }
    }

    fn reconstruct(&self, id: TypeId) -> Result<Spanned<'file, Type>, Box<Error<'file>>> {
        let var = self.vars[&id];

        match var.0 {
            TypeInfo::Unknown => panic!(),
            TypeInfo::Ref(id) => Ok(self.reconstruct(id)?.0),
            TypeInfo::Error => Ok(Type::Error),
            TypeInfo::Boolean => Ok(Type::Boolean),
            TypeInfo::Integer => Ok(Type::Integer),
            TypeInfo::Float => Ok(Type::Float),
            TypeInfo::String => Ok(Type::String),
        }
        .map(|ty| Spanned(ty, var.1))
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct TypeId(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TypeInfo {
    // #[allow(dead_code)]
    Unknown,
    Ref(TypeId),
    Error,
    Boolean,
    Integer,
    Float,
    String,
}

impl std::fmt::Display for TypeInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unknown => write!(f, "unknown"),
            Self::Ref(id) => write!(f, "ref {}", id.0),
            Self::Error => write!(f, "error"),
            Self::Boolean => write!(f, "bool"),
            Self::Integer => write!(f, "int"),
            Self::Float => write!(f, "float"),
            Self::String => write!(f, "str"),
        }
    }
}

impl From<Type> for TypeInfo {
    fn from(ty: Type) -> Self {
        match ty {
            Type::Error => Self::Error,
            Type::Boolean => Self::Boolean,
            Type::Integer => Self::Integer,
            Type::Float => Self::Float,
            Type::String => Self::String,
        }
    }
}

macro_rules! bin_op {
    ($lhs_value:expr, $rhs_value:expr, $op_value:expr, $(($lhs:ident, $rhs:ident, $op:ident, $ret_ty:ident)),*) => {
        match ($lhs_value, $rhs_value, $op_value) {
            $(
                (Type::$lhs, Type::$rhs, BinaryOp::$op) => Ok(Type::$ret_ty),
            )*
            (Type::Error, _, _) => Ok(Type::Error),
            (_, Type::Error, _) => Ok(Type::Error),
            _ => Err(()),
        }
    };
}
use bin_op;
