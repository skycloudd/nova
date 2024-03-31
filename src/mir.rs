use crate::{
    ast::{
        self,
        typed::{Expr, TypedAst, TypedExpr, TypedFunction, TypedStatement, TypedTopLevel},
        BinaryOp, Primitive, UnaryOp,
    },
    scopes::Scopes,
    span::Spanned,
    FloatTy, IdGen, IntTy,
};

#[derive(Debug)]
pub struct Mir {
    pub top_levels: Vec<Spanned<TopLevel>>,
}

#[derive(Debug)]
pub enum TopLevel {
    Function(Spanned<Function>),
}

#[derive(Debug)]
pub struct Function {
    pub id: Spanned<FuncId>,
    pub name: Spanned<&'static str>,
    pub params: Spanned<Vec<(Spanned<VarId>, Spanned<Type>)>>,
    pub return_ty: Spanned<Type>,
    pub body: Spanned<Vec<Spanned<Statement>>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Error,
    Primitive(Primitive),
    Pointer(Spanned<Box<Type>>),
}

impl From<Primitive> for Type {
    fn from(p: Primitive) -> Self {
        Self::Primitive(p)
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct FuncId(pub usize);

#[derive(Debug)]
pub enum Statement {
    Error,
    Expr(Spanned<TypedExpression>),
    Block(Spanned<Vec<Spanned<Self>>>),
    Loop(Spanned<Vec<Spanned<Self>>>),
    If {
        condition: Spanned<TypedExpression>,
        then_branch: Spanned<Vec<Spanned<Self>>>,
        else_branch: Option<Spanned<Vec<Spanned<Self>>>>,
    },
    For {
        name: Spanned<VarId>,
        start: Spanned<TypedExpression>,
        end: Spanned<TypedExpression>,
        inclusive: bool,
        body: Spanned<Vec<Spanned<Self>>>,
    },
    Let {
        name: Spanned<VarId>,
        value: Spanned<TypedExpression>,
    },
    Assign {
        name: Spanned<VarId>,
        value: Spanned<TypedExpression>,
    },
    Break,
    Continue,
    Return(Spanned<TypedExpression>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedExpression {
    pub expr: Expression,
    pub ty: Type,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Error,
    Variable(Spanned<VarId>),
    Boolean(bool),
    Integer(IntTy),
    Float(FloatTy),
    Unary {
        op: Spanned<UnaryOp>,
        rhs: Spanned<Box<TypedExpression>>,
    },
    Binary {
        lhs: Spanned<Box<TypedExpression>>,
        op: Spanned<BinaryOp>,
        rhs: Spanned<Box<TypedExpression>>,
    },
    Convert {
        ty: Spanned<Type>,
        expr: Spanned<Box<TypedExpression>>,
    },
    Call {
        func: Spanned<FuncId>,
        args: Spanned<Vec<Spanned<TypedExpression>>>,
    },
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct VarId(pub usize);

trait IdMap {
    type Id;

    fn get(&self, name: &'static str) -> Option<&Self::Id>;
    fn insert(&mut self, name: Spanned<&'static str>) -> Self::Id;
}

struct VarIdMap {
    map: Scopes<&'static str, Spanned<VarId>>,
    id_gen: IdGen,
}

impl VarIdMap {
    fn new() -> Self {
        Self {
            map: Scopes::new(),
            id_gen: IdGen::new(),
        }
    }
}

impl IdMap for VarIdMap {
    type Id = Spanned<VarId>;

    fn get(&self, name: &'static str) -> Option<&Spanned<VarId>> {
        self.map.get(&name)
    }

    fn insert(&mut self, name: Spanned<&'static str>) -> Spanned<VarId> {
        let id = Spanned(VarId(self.id_gen.next().unwrap()), name.1);
        self.map.insert(name.0, id);
        id
    }
}

struct FuncIdMap {
    map: Scopes<&'static str, Spanned<FuncId>>,
    id_gen: IdGen,
}

impl FuncIdMap {
    fn new() -> Self {
        Self {
            map: Scopes::new(),
            id_gen: IdGen::new(),
        }
    }
}

impl IdMap for FuncIdMap {
    type Id = Spanned<FuncId>;

    fn get(&self, name: &'static str) -> Option<&Spanned<FuncId>> {
        self.map.get(&name)
    }

    fn insert(&mut self, name: Spanned<&'static str>) -> Spanned<FuncId> {
        let id = Spanned(FuncId(self.id_gen.next().unwrap()), name.1);
        self.map.insert(name.0, id);
        id
    }
}

pub fn build(ast: TypedAst) -> Mir {
    MirBuilder::new().build(ast)
}

struct MirBuilder {
    var_id_map: VarIdMap,
    func_id_map: FuncIdMap,
}

impl MirBuilder {
    fn new() -> Self {
        Self {
            var_id_map: VarIdMap::new(),
            func_id_map: FuncIdMap::new(),
        }
    }

    fn build(&mut self, ast: TypedAst) -> Mir {
        for top_level in &ast.top_levels {
            match &top_level.0 {
                TypedTopLevel::Function(function) => {
                    self.func_id_map.insert(function.0.name);

                    for arg in &function.0.params.0 {
                        self.var_id_map.insert(arg.0);
                    }
                }
            }
        }

        let top_levels = ast
            .top_levels
            .into_iter()
            .map(|top_level| self.build_mir_top_level(top_level))
            .collect();

        Mir { top_levels }
    }

    fn build_mir_top_level(&mut self, top_level: Spanned<TypedTopLevel>) -> Spanned<TopLevel> {
        top_level.map(|top_level| match top_level {
            TypedTopLevel::Function(function) => {
                TopLevel::Function(self.build_mir_function(function))
            }
        })
    }

    fn build_mir_function(&mut self, function: Spanned<TypedFunction>) -> Spanned<Function> {
        function.map(|function| Function {
            id: *self.func_id_map.get(function.name.0).unwrap(),
            name: function.name,
            params: function.params.map(|params| {
                params
                    .into_iter()
                    .map(|param| {
                        (
                            *self.var_id_map.get(param.0 .0).unwrap(),
                            Self::lower_ty(param.1),
                        )
                    })
                    .collect()
            }),
            return_ty: Self::lower_ty(function.return_ty),
            body: self.build_statements(function.body),
        })
    }

    fn lower_ty(ty: Spanned<ast::Type>) -> Spanned<Type> {
        ty.map(|ty| match ty {
            ast::Type::Error => Type::Error,
            ast::Type::Primitive(primitive) => match primitive {
                Primitive::Boolean => Primitive::Boolean.into(),
                Primitive::Integer => Primitive::Integer.into(),
                Primitive::Float => Primitive::Float.into(),
            },
            ast::Type::Pointer(inner) => {
                let inner = Self::lower_ty(inner.into_inner());

                Type::Pointer(inner.boxed())
            }
        })
    }

    fn build_statements(
        &mut self,
        statements: Spanned<Vec<Spanned<TypedStatement>>>,
    ) -> Spanned<Vec<Spanned<Statement>>> {
        statements.map(|statements| {
            statements
                .into_iter()
                .map(|stmt| self.build_mir_statement(stmt))
                .collect()
        })
    }

    fn build_mir_statement(&mut self, statement: Spanned<TypedStatement>) -> Spanned<Statement> {
        statement.map(|statement| match statement {
            TypedStatement::Error => Statement::Error,
            TypedStatement::Expr(expr) => Statement::Expr(self.build_mir_expr(expr)),
            TypedStatement::Block(statements) => {
                Statement::Block(self.build_statements(statements))
            }
            TypedStatement::Loop(statements) => Statement::Loop(self.build_statements(statements)),
            TypedStatement::If {
                condition,
                then_branch,
                else_branch,
            } => Statement::If {
                condition: self.build_mir_expr(condition),
                then_branch: self.build_statements(then_branch),
                else_branch: else_branch.map(|stmts| self.build_statements(stmts)),
            },
            TypedStatement::For {
                name,
                start,
                end,
                inclusive,
                body,
            } => {
                let start = self.build_mir_expr(start);
                let end = self.build_mir_expr(end);
                let name = self.var_id_map.insert(name);
                let body = self.build_statements(body);

                Statement::For {
                    name,
                    start,
                    end,
                    inclusive,
                    body,
                }
            }
            TypedStatement::Let { name, value } => {
                let value = self.build_mir_expr(value);
                let name = self.var_id_map.insert(name);

                Statement::Let { name, value }
            }
            TypedStatement::Assign { name, value } => Statement::Assign {
                name: *self.var_id_map.get(name.0).unwrap(),
                value: self.build_mir_expr(value),
            },
            TypedStatement::Break => Statement::Break,
            TypedStatement::Continue => Statement::Continue,
            TypedStatement::Return(expr) => Statement::Return(self.build_mir_expr(expr)),
        })
    }

    fn build_mir_expr(&mut self, expr: Spanned<TypedExpr>) -> Spanned<TypedExpression> {
        expr.map_with_span(|expr, span| TypedExpression {
            expr: match expr.expr {
                Expr::Error => Expression::Error,
                Expr::Variable(name) => Expression::Variable(*self.var_id_map.get(name.0).unwrap()),
                Expr::Boolean(value) => Expression::Boolean(value),
                Expr::Integer(value) => Expression::Integer(value),
                Expr::Float(value) => Expression::Float(value),
                Expr::Unary(op, rhs) => {
                    let rhs = self.build_mir_expr(rhs.into_inner());

                    Expression::Unary {
                        op,
                        rhs: rhs.boxed(),
                    }
                }
                Expr::Binary(lhs, op, rhs) => {
                    let lhs = self.build_mir_expr(lhs.into_inner());
                    let rhs = self.build_mir_expr(rhs.into_inner());

                    Expression::Binary {
                        lhs: lhs.boxed(),
                        op,
                        rhs: rhs.boxed(),
                    }
                }
                Expr::Convert { ty, expr } => {
                    let expr = self.build_mir_expr(expr.into_inner());

                    Expression::Convert {
                        ty: Self::lower_ty(ty),
                        expr: expr.boxed(),
                    }
                }
                Expr::Call { func, args } => Expression::Call {
                    func: *self.func_id_map.get(func.0).unwrap(),
                    args: args.map(|args| {
                        args.into_iter()
                            .map(|arg| self.build_mir_expr(arg))
                            .collect()
                    }),
                },
            },
            ty: Self::lower_ty(Spanned(expr.ty, span)).0,
        })
    }
}
