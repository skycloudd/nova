use crate::{
    ast::{
        typed::{self, TypedExpr, TypedFunction, TypedStatement, TypedTopLevel},
        BinaryOp, Expr, Function, Statement, TopLevel, Type, UnaryOp,
    },
    error::{Error, Warning},
    scopes::{ClosestStrKey, Scopes},
    span::Spanned,
    FloatTy, IntTy,
};
use rustc_hash::FxHashMap;
use snake_case::is_snake_case;

pub fn typecheck(
    ast: Vec<Spanned<TopLevel>>,
) -> (Vec<Spanned<TypedTopLevel>>, Vec<Warning>, Vec<Error>) {
    let mut warnings = Vec::new();
    let mut errors = Vec::new();

    let mut typechecker = Typechecker::new(&mut warnings, &mut errors);

    (typechecker.typecheck_ast(ast), warnings, errors)
}

#[derive(Debug)]
struct FunctionSignature {
    params: Spanned<Vec<Spanned<TypeId>>>,
    return_ty: Spanned<TypeId>,
}

struct Typechecker<'warning, 'error> {
    engine: Engine,
    functions: FxHashMap<&'static str, Spanned<FunctionSignature>>,
    variables: Scopes<&'static str, TypeId>,

    current_function: Option<&'static str>,

    warnings: &'warning mut Vec<Warning>,
    errors: &'error mut Vec<Error>,
}

impl<'warning, 'error> Typechecker<'warning, 'error> {
    fn new(warnings: &'warning mut Vec<Warning>, errors: &'error mut Vec<Error>) -> Self {
        Self {
            engine: Engine::new(),
            functions: FxHashMap::default(),
            variables: Scopes::new(),

            current_function: None,

            warnings,
            errors,
        }
    }

    fn typecheck_ast(&mut self, ast: Vec<Spanned<TopLevel>>) -> Vec<Spanned<TypedTopLevel>> {
        for top_level in &ast {
            match &top_level.0 {
                TopLevel::Function(function) => match self.functions.get(&function.0.name.0) {
                    None => {
                        let params = Spanned(
                            function
                                .0
                                .params
                                .0
                                .iter()
                                .filter(|(_, ty)| ty.0 != Type::Error)
                                .map(|(_, ty)| Spanned(self.engine.insert_type(ty.clone()), ty.1))
                                .collect(),
                            function.0.params.1,
                        );

                        let return_ty = Spanned(
                            self.engine.insert_type(function.0.return_ty.clone()),
                            function.0.return_ty.1,
                        );

                        self.functions.insert(
                            function.0.name.0,
                            Spanned(FunctionSignature { params, return_ty }, function.1),
                        );
                    }
                    Some(already_defined) => {
                        self.errors.push(Error::FunctionAlreadyDefined {
                            name: function.0.name.0.to_string(),
                            span: function.1,
                            already_defined_span: already_defined.1,
                        });
                    }
                },
            }
        }

        let mut top_levels = Vec::with_capacity(ast.len());

        for top_level in ast {
            let top_level = Spanned(
                match top_level.0 {
                    TopLevel::Function(function) => {
                        TypedTopLevel::Function(self.typecheck_function(function))
                    }
                },
                top_level.1,
            );

            top_levels.push(top_level);
        }

        top_levels
    }

    fn typecheck_function(&mut self, function: Spanned<Function>) -> Spanned<TypedFunction> {
        self.current_function = Some(function.0.name.0);

        if !is_snake_case(function.0.name.0) {
            self.warnings.push(Warning::BadName {
                name: function.0.name.0.to_string(),
                span: function.0.name.1,
            });
        }

        self.push_scope();

        for (name, ty) in function
            .0
            .params
            .0
            .iter()
            .map(|(name, _)| name)
            .zip(&self.functions.get(&function.0.name.0).unwrap().0.params.0)
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
                .0
                .body
                .0
                .into_iter()
                .map(|stmt| self.typecheck_statement(stmt))
                .collect(),
            function.0.body.1,
        );

        self.pop_scope();

        self.current_function = None;

        Spanned(
            TypedFunction {
                name: function.0.name,
                params: function.0.params.clone(),
                return_ty: function.0.return_ty,
                body,
            },
            function.1,
        )
    }

    fn typecheck_statements(
        &mut self,
        statements: Spanned<Vec<Spanned<Statement>>>,
    ) -> Spanned<Vec<Spanned<TypedStatement>>> {
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
    fn typecheck_statement(&mut self, statement: Spanned<Statement>) -> Spanned<TypedStatement> {
        Spanned(
            match statement.0 {
                Statement::Error => TypedStatement::Error,
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
                        .insert_type(Spanned(condition.0.ty.clone(), condition.1));

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

                    let start_ty = self
                        .engine
                        .insert_type(Spanned(start.0.ty.clone(), start.1));

                    let end = self.typecheck_expression(end.clone());

                    let end_ty = self.engine.insert_type(Spanned(end.0.ty.clone(), end.1));

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

                    let name_ty = self.engine.insert_type(Spanned(Type::Integer, name.1));
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

                    let value_ty = self
                        .engine
                        .insert_type(Spanned(value.0.ty.clone(), value.1));

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

                    let value_ty = self
                        .engine
                        .insert_type(Spanned(value.0.ty.clone(), value.1));

                    let Some(name_ty) = self.variables.get(&name.0) else {
                        self.errors.push(Error::UndefinedVariable {
                            name: name.0.to_string(),
                            span: name.1,
                            closest: self
                                .variables
                                .closest_str_key(name.0)
                                .map(|(name, dist)| (name.to_string(), dist)),
                        });

                        return Spanned(TypedStatement::Error, statement.1);
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
                        .0
                        .return_ty;

                    let expr_ty = self.engine.insert_type(Spanned(expr.0.ty.clone(), expr.1));

                    self.engine
                        .unify(return_ty.0, expr_ty)
                        .unwrap_or_else(|err| {
                            if let Error::TypeMismatch {
                                expected,
                                found,
                                span,
                            } = *err
                            {
                                self.errors.push(Error::ReturnTypeMismatch {
                                    expected,
                                    found,
                                    expected_span: return_ty.1,
                                    found_span: span,
                                });
                            } else {
                                self.errors.push(*err);
                            }
                        });

                    TypedStatement::Return(expr)
                }
            },
            statement.1,
        )
    }

    #[allow(clippy::too_many_lines)]
    fn typecheck_expression(&mut self, expr: Spanned<Expr>) -> Spanned<TypedExpr> {
        let checked_expr = Spanned(
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
                        self.errors.push(Error::UndefinedVariable {
                            name: var.0.to_string(),
                            span: var.1,
                            closest: self
                                .variables
                                .closest_str_key(var.0)
                                .map(|(name, dist)| (name.to_string(), dist)),
                        });

                        TypedExpr {
                            expr: typed::Expr::Error,
                            ty: Type::Error,
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
                Expr::Binary(lhs, op, rhs) => {
                    let lhs = self.typecheck_expression(Spanned(*lhs.0, lhs.1));

                    let lhs_ty = self.engine.insert_type(Spanned(lhs.0.ty.clone(), lhs.1));

                    let rhs = self.typecheck_expression(Spanned(*rhs.0, rhs.1));

                    let rhs_ty = self.engine.insert_type(Spanned(rhs.0.ty.clone(), rhs.1));

                    let ty = {
                        use BinaryOp::{
                            Divide, Equals, GreaterThan, GreaterThanEquals, LessThan,
                            LessThanEquals, Minus, Multiply, NotEquals, Plus,
                        };
                        use Type::{Boolean, Float, Integer};

                        #[allow(clippy::match_same_arms)]
                        match ((&lhs.0.ty), (&rhs.0.ty), (op.0)) {
                            (Boolean, Boolean, Equals) | (Boolean, Boolean, NotEquals) => {
                                Ok(Boolean)
                            }

                            (Integer, Integer, Equals)
                            | (Integer, Integer, NotEquals)
                            | (Integer, Integer, GreaterThanEquals)
                            | (Integer, Integer, LessThanEquals)
                            | (Integer, Integer, GreaterThan)
                            | (Integer, Integer, LessThan) => Ok(Boolean),

                            (Integer, Integer, Plus)
                            | (Integer, Integer, Minus)
                            | (Integer, Integer, Multiply)
                            | (Integer, Integer, Divide) => Ok(Integer),

                            (Float, Float, Equals)
                            | (Float, Float, NotEquals)
                            | (Float, Float, GreaterThanEquals)
                            | (Float, Float, LessThanEquals)
                            | (Float, Float, GreaterThan)
                            | (Float, Float, LessThan) => Ok(Boolean),

                            (Float, Float, Plus)
                            | (Float, Float, Minus)
                            | (Float, Float, Multiply)
                            | (Float, Float, Divide) => Ok(Float),

                            (Type::Error, _, _) => Ok(Type::Error),
                            (_, Type::Error, _) => Ok(Type::Error),
                            _ => Err(()),
                        }
                        .map_err(|()| Error::InvalidBinaryOperation {
                            lhs: lhs.0.ty.clone(),
                            lhs_span: lhs.1,
                            rhs: rhs.0.ty.clone(),
                            rhs_span: rhs.1,
                            op: op.0,
                            op_span: op.1,
                            full_span: expr.1,
                        })
                    };

                    let ty = ty.unwrap_or_else(|err| {
                        self.errors.push(err);
                        Type::Error
                    });

                    if ty != Type::Error {
                        if let Err(err) = self.engine.unify(lhs_ty, rhs_ty) {
                            self.errors.push(*err);

                            return Spanned(
                                TypedExpr {
                                    expr: typed::Expr::Error,
                                    ty: Type::Error,
                                },
                                expr.1,
                            );
                        }
                    }

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
                        (Type::Pointer(inner), UnaryOp::Deref) => Ok((*inner.0).clone()),
                        (Type::Error, _) => Ok(Type::Error),
                        (any, UnaryOp::Ref) => {
                            Ok(Type::Pointer(Spanned(Box::new(any.clone()), expr.1)))
                        }
                        _ => Err(Error::InvalidUnaryOperation {
                            ty: expr.0.ty.clone(),
                            ty_span: expr.1,
                            op: op.0,
                            op_span: op.1,
                            full_span: expr.1,
                        }),
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
                        (from_ty, into_ty) => {
                            self.errors.push(Error::InvalidConversion {
                                from: from_ty.clone(),
                                from_span: expr.1,
                                into: into_ty.clone(),
                                into_span: ty.1,
                                full_span: expr.1,
                            });

                            return Spanned(
                                TypedExpr {
                                    expr: typed::Expr::Error,
                                    ty: Type::Error,
                                },
                                expr.1,
                            );
                        }
                    }

                    TypedExpr {
                        ty: ty.0.clone(),
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
                            closest: self
                                .functions
                                .closest_str_key(func.0)
                                .map(|(name, dist)| (name.to_string(), dist)),
                        });

                        return Spanned(
                            TypedExpr {
                                expr: typed::Expr::Error,
                                ty: Type::Error,
                            },
                            expr.1,
                        );
                    };

                    if args.0.len() != signature.0.params.0.len() {
                        self.errors.push(Error::FunctionArgumentCountMismatch {
                            expected: signature.0.params.0.len(),
                            found: args.0.len(),
                            expected_span: signature.0.params.1,
                            found_span: args.1,
                        });
                    }

                    for (arg, ty) in args.0.iter().zip(&signature.0.params.0) {
                        let arg_ty = self.engine.insert_type(Spanned(arg.0.ty.clone(), arg.1));

                        self.engine.unify(ty.0, arg_ty).unwrap_or_else(|err| {
                            self.errors.push(*err);
                        });
                    }

                    let return_ty = match self.engine.reconstruct(signature.0.return_ty.0) {
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
        );

        let evaluated_expr = self
            .evaluate_expression(checked_expr.clone())
            .map(Value::recreate);

        Spanned(evaluated_expr.unwrap_or(checked_expr.0), expr.1)
    }

    #[allow(clippy::too_many_lines)]
    fn evaluate_expression(&mut self, expr: Spanned<typed::TypedExpr>) -> Option<Value> {
        if expr.0.ty == Type::Error {
            return None;
        }

        #[allow(clippy::match_same_arms)]
        match expr.0.expr {
            typed::Expr::Error | typed::Expr::Variable(_) => None,
            typed::Expr::Boolean(bool) => Some(Value::Boolean(bool)),
            typed::Expr::Integer(int) => Some(Value::Integer(int)),
            typed::Expr::Float(float) => Some(Value::Float(float)),
            typed::Expr::String(string) => Some(Value::String(string)),
            typed::Expr::Unary(op, rhs) => {
                let rhs = self.evaluate_expression(Spanned(*rhs.0, rhs.1))?;

                match op.0 {
                    UnaryOp::Negate => match rhs {
                        Value::Integer(int) => {
                            checked_arithmetic!(
                                self,
                                int.overflowing_neg(),
                                "negation",
                                "an overflow or underflow".into(),
                                expr.1
                            )
                        }
                        Value::Float(_float) => None,
                        _ => unreachable!(),
                    },
                    UnaryOp::Not => match rhs {
                        Value::Boolean(bool) => Some(Value::Boolean(!bool)),
                        _ => unreachable!(),
                    },
                    UnaryOp::Ref => None,
                    UnaryOp::Deref => None,
                }
            }
            typed::Expr::Binary(lhs, op, rhs) => {
                assert_eq!(lhs.0.ty, rhs.0.ty);

                let lhs_value = self.evaluate_expression(Spanned(*lhs.0, lhs.1))?;
                let rhs_value = self.evaluate_expression(Spanned(*rhs.0, rhs.1))?;

                match (lhs_value, rhs_value) {
                    (Value::Boolean(lhs), Value::Boolean(rhs)) => match op.0 {
                        BinaryOp::Equals => Some(Value::Boolean(lhs == rhs)),
                        BinaryOp::NotEquals => Some(Value::Boolean(lhs != rhs)),
                        _ => unreachable!(),
                    },
                    (Value::Integer(lhs), Value::Integer(rhs)) => match op.0 {
                        BinaryOp::Equals => Some(Value::Boolean(lhs == rhs)),
                        BinaryOp::NotEquals => Some(Value::Boolean(lhs != rhs)),
                        BinaryOp::Plus => {
                            checked_arithmetic!(
                                self,
                                lhs.overflowing_add(rhs),
                                "addition",
                                "an overflow or underflow".into(),
                                expr.1
                            )
                        }
                        BinaryOp::Minus => {
                            checked_arithmetic!(
                                self,
                                lhs.overflowing_sub(rhs),
                                "subtraction",
                                "an overflow or underflow".into(),
                                expr.1
                            )
                        }
                        BinaryOp::Multiply => checked_arithmetic!(
                            self,
                            lhs.overflowing_mul(rhs),
                            "multiplication",
                            "an overflow or underflow".into(),
                            expr.1
                        ),
                        BinaryOp::Divide => {
                            if rhs == 0 {
                                self.errors.push(Error::IntegerArithmetic {
                                    name: "division".into(),
                                    effect: "a division by zero".into(),
                                    result: None,
                                    span: expr.1,
                                });
                            }

                            checked_arithmetic!(
                                self,
                                lhs.overflowing_div(rhs),
                                "division",
                                "an overflow or underflow".into(),
                                expr.1
                            )
                        }
                        BinaryOp::GreaterThanEquals => Some(Value::Boolean(lhs >= rhs)),
                        BinaryOp::LessThanEquals => Some(Value::Boolean(lhs <= rhs)),
                        BinaryOp::GreaterThan => Some(Value::Boolean(lhs > rhs)),
                        BinaryOp::LessThan => Some(Value::Boolean(lhs < rhs)),
                    },
                    (Value::Float(_lhs), Value::Float(_rhs)) => None,
                    (Value::String(_lhs), Value::String(_rhs)) => unreachable!(),
                    _ => unreachable!(),
                }
            }
            typed::Expr::Convert { ty, expr } => {
                let expr_ty = expr.0.ty.clone();
                let expr = self.evaluate_expression(Spanned(*expr.0, expr.1))?;

                match (ty.0, expr_ty) {
                    (from, to) if from == to => Some(expr),
                    (Type::Error, _) => None,
                    _ => unreachable!(),
                }
            }
            typed::Expr::Call { func: _, args: _ } => None,
        }
    }

    fn push_scope(&mut self) {
        self.variables.push_scope();
    }

    fn pop_scope(&mut self) {
        self.variables.pop_scope();
    }
}

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
        self.id_counter = self.id_counter.checked_add(1).unwrap();
        let id = TypeId(self.id_counter);
        self.vars.insert(id, info);
        id
    }

    fn insert_type(&mut self, ty: Spanned<Type>) -> TypeId {
        let typeinfo = self.type_to_typeinfo(ty.0);

        self.insert(Spanned(typeinfo, ty.1))
    }

    fn unify(&mut self, a: TypeId, b: TypeId) -> Result<(), Box<Error>> {
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

            (TypeInfo::Pointer(a), TypeInfo::Pointer(b)) => self.unify(*a, *b),
            (a, b) if a == b => Ok(()),

            (a, b) => Err(Box::new(Error::TypeMismatch {
                expected: self.typeinfo_to_type(a),
                found: self.typeinfo_to_type(b),
                span: var_b.1,
            })),
        }
    }

    /// Found `id`, expected `ty`
    fn expect(&self, id: TypeId, ty: &TypeInfo) -> Result<(), Box<Error>> {
        let var = &self.vars[&id];

        if &var.0 == ty || var.0 == TypeInfo::Error {
            Ok(())
        } else {
            Err(Box::new(Error::TypeMismatch {
                expected: self.typeinfo_to_type(ty),
                found: self.typeinfo_to_type(&var.0),
                span: var.1,
            }))
        }
    }

    fn reconstruct(&self, id: TypeId) -> Result<Spanned<Type>, Box<Error>> {
        let var = &self.vars[&id];

        match &var.0 {
            TypeInfo::Unknown => Err(Box::new(Error::CantInferType { span: var.1 })),
            TypeInfo::Ref(id) => Ok(self.reconstruct(*id)?.0),
            TypeInfo::Error => Ok(Type::Error),
            TypeInfo::Boolean => Ok(Type::Boolean),
            TypeInfo::Integer => Ok(Type::Integer),
            TypeInfo::Float => Ok(Type::Float),
            TypeInfo::String => Ok(Type::String),
            TypeInfo::Pointer(inner) => {
                let inner = self.reconstruct(*inner)?;

                Ok(Type::Pointer(Spanned(Box::new(inner.0), inner.1)))
            }
        }
        .map(|ty| Spanned(ty, var.1))
    }

    fn type_to_typeinfo(&mut self, ty: Type) -> TypeInfo {
        match ty {
            Type::Error => TypeInfo::Error,
            Type::Boolean => TypeInfo::Boolean,
            Type::Integer => TypeInfo::Integer,
            Type::Float => TypeInfo::Float,
            Type::String => TypeInfo::String,
            Type::Pointer(inner) => TypeInfo::Pointer(self.insert_type(Spanned(*inner.0, inner.1))),
        }
    }

    fn typeinfo_to_type(&self, info: &TypeInfo) -> Type {
        match info {
            TypeInfo::Unknown | TypeInfo::Error => Type::Error,
            TypeInfo::Ref(id) => self.reconstruct(*id).unwrap().0,
            TypeInfo::Boolean => Type::Boolean,
            TypeInfo::Integer => Type::Integer,
            TypeInfo::Float => Type::Float,
            TypeInfo::String => Type::String,
            TypeInfo::Pointer(inner) => {
                let inner = self.reconstruct(*inner).unwrap();

                Type::Pointer(Spanned(Box::new(inner.0), inner.1))
            }
        }
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct TypeId(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TypeInfo {
    #[allow(dead_code)]
    Unknown,
    Ref(TypeId),
    Error,
    Boolean,
    Integer,
    Float,
    String,
    Pointer(TypeId),
}

#[derive(Debug, PartialEq)]
pub enum Value {
    Boolean(bool),
    Integer(IntTy),
    Float(FloatTy),
    String(String),
}

impl Value {
    fn recreate(self) -> TypedExpr {
        match self {
            Self::Boolean(bool) => TypedExpr {
                expr: typed::Expr::Boolean(bool),
                ty: Type::Boolean,
            },
            Self::Integer(int) => TypedExpr {
                expr: typed::Expr::Integer(int),
                ty: Type::Integer,
            },
            Self::Float(float) => TypedExpr {
                expr: typed::Expr::Float(float),
                ty: Type::Float,
            },
            Self::String(string) => TypedExpr {
                expr: typed::Expr::String(string),
                ty: Type::String,
            },
        }
    }
}

macro_rules! checked_arithmetic {
    ($self:expr, $op:expr, $name:expr, $effect:expr, $span:expr) => {
        Some(Value::Integer(match $op {
            (result, false) => result,
            (result, true) => {
                $self.errors.push(Error::IntegerArithmetic {
                    name: $name.into(),
                    effect: $effect,
                    result: Some(result),
                    span: $span,
                });
                0
            }
        }))
    };
}
use checked_arithmetic;
