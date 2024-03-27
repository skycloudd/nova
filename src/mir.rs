use crate::{
    ast::{
        typed::{Expr, TypedExpr, TypedFunction, TypedStatement, TypedTopLevel},
        BinaryOp, Type, UnaryOp,
    },
    scopes::Scopes,
    span::Spanned,
    FloatTy, IdGen, IntTy,
};

#[derive(Debug)]
pub enum TopLevel<'src> {
    Function(Spanned<Function<'src>>),
}

#[derive(Debug)]
pub struct Function<'src> {
    pub id: Spanned<FuncId>,
    pub name: Spanned<&'src str>,
    pub params: Spanned<Vec<(Spanned<VarId>, Spanned<Type>)>>,
    pub return_ty: Spanned<Type>,
    pub body: Spanned<Vec<Spanned<Statement>>>,
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
    String(String),
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

trait IdMap<'src> {
    type Id;

    fn get<'a>(&'a self, name: &'a str) -> Option<&Self::Id>;
    fn insert(&mut self, name: Spanned<&'src str>) -> Self::Id;
}

struct VarIdMap<'src> {
    map: Scopes<&'src str, Spanned<VarId>>,
    id_gen: IdGen,
}

impl VarIdMap<'_> {
    fn new() -> Self {
        Self {
            map: Scopes::new(),
            id_gen: IdGen::new(),
        }
    }
}

impl<'src> IdMap<'src> for VarIdMap<'src> {
    type Id = Spanned<VarId>;

    fn get<'a>(&'a self, name: &'a str) -> Option<&Spanned<VarId>> {
        self.map.get(&name)
    }

    fn insert(&mut self, name: Spanned<&'src str>) -> Spanned<VarId> {
        let id = Spanned(VarId(self.id_gen.next().unwrap()), name.1);
        self.map.insert(name.0, id);
        id
    }
}

struct FuncIdMap<'src> {
    map: Scopes<&'src str, Spanned<FuncId>>,
    id_gen: IdGen,
}

impl FuncIdMap<'_> {
    fn new() -> Self {
        Self {
            map: Scopes::new(),
            id_gen: IdGen::new(),
        }
    }
}

impl<'src> IdMap<'src> for FuncIdMap<'src> {
    type Id = Spanned<FuncId>;

    fn get<'a>(&'a self, name: &'a str) -> Option<&Spanned<FuncId>> {
        self.map.get(&name)
    }

    fn insert(&mut self, name: Spanned<&'src str>) -> Spanned<FuncId> {
        let id = Spanned(FuncId(self.id_gen.next().unwrap()), name.1);
        self.map.insert(name.0, id);
        id
    }
}

pub fn build(ast: Vec<Spanned<TypedTopLevel>>) -> Vec<Spanned<TopLevel>> {
    MirBuilder::new().build(ast)
}

struct MirBuilder<'src> {
    var_id_map: VarIdMap<'src>,
    func_id_map: FuncIdMap<'src>,
}

impl<'src> MirBuilder<'src> {
    fn new() -> Self {
        Self {
            var_id_map: VarIdMap::new(),
            func_id_map: FuncIdMap::new(),
        }
    }

    fn build(&mut self, ast: Vec<Spanned<TypedTopLevel<'src>>>) -> Vec<Spanned<TopLevel<'src>>> {
        for top_level in &ast {
            match &top_level.0 {
                TypedTopLevel::Function(function) => {
                    self.func_id_map.insert(function.0.name);

                    for arg in &function.0.params.0 {
                        self.var_id_map.insert(arg.0);
                    }
                }
            }
        }

        ast.into_iter()
            .map(|top_level| self.build_mir_top_level(top_level))
            .collect()
    }

    fn build_mir_top_level(
        &mut self,
        top_level: Spanned<TypedTopLevel<'src>>,
    ) -> Spanned<TopLevel<'src>> {
        Spanned(
            match top_level.0 {
                TypedTopLevel::Function(function) => {
                    TopLevel::Function(self.build_mir_function(function))
                }
            },
            top_level.1,
        )
    }

    fn build_mir_function(
        &mut self,
        function: Spanned<TypedFunction<'src>>,
    ) -> Spanned<Function<'src>> {
        Spanned(
            Function {
                id: *self.func_id_map.get(function.0.name.0).unwrap(),
                name: function.0.name,
                params: Spanned(
                    function
                        .0
                        .params
                        .0
                        .iter()
                        .map(|arg| (*self.var_id_map.get(arg.0 .0).unwrap(), arg.1.clone()))
                        .collect(),
                    function.0.params.1,
                ),
                return_ty: function.0.return_ty,
                body: self.build_statements(function.0.body),
            },
            function.1,
        )
    }

    fn build_statements(
        &mut self,
        statements: Spanned<Vec<Spanned<TypedStatement<'src>>>>,
    ) -> Spanned<Vec<Spanned<Statement>>> {
        Spanned(
            statements
                .0
                .into_iter()
                .map(|stmt| self.build_mir_statement(stmt))
                .collect(),
            statements.1,
        )
    }

    fn build_mir_statement(
        &mut self,
        statement: Spanned<TypedStatement<'src>>,
    ) -> Spanned<Statement> {
        Spanned(
            match statement.0 {
                TypedStatement::Error => Statement::Error,
                TypedStatement::Expr(expr) => Statement::Expr(self.build_mir_expr(expr)),
                TypedStatement::Block(statements) => {
                    Statement::Block(self.build_statements(statements))
                }
                TypedStatement::Loop(statements) => {
                    Statement::Loop(self.build_statements(statements))
                }
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
            },
            statement.1,
        )
    }

    fn build_mir_expr(&mut self, expr: Spanned<TypedExpr<'src>>) -> Spanned<TypedExpression> {
        Spanned(
            TypedExpression {
                expr: match expr.0.expr {
                    Expr::Error => Expression::Error,
                    Expr::Variable(name) => {
                        Expression::Variable(*self.var_id_map.get(name.0).unwrap())
                    }
                    Expr::Boolean(value) => Expression::Boolean(value),
                    Expr::Integer(value) => Expression::Integer(value),
                    Expr::Float(value) => Expression::Float(value),
                    Expr::String(value) => Expression::String(value),
                    Expr::Unary(op, rhs) => {
                        let rhs = self.build_mir_expr(Spanned(*rhs.0, rhs.1));

                        Expression::Unary {
                            op,
                            rhs: Spanned(Box::new(rhs.0), expr.1),
                        }
                    }
                    Expr::Binary(lhs, op, rhs) => {
                        let lhs = self.build_mir_expr(Spanned(*lhs.0, lhs.1));
                        let rhs = self.build_mir_expr(Spanned(*rhs.0, rhs.1));

                        Expression::Binary {
                            lhs: Spanned(Box::new(lhs.0), expr.1),
                            op,
                            rhs: Spanned(Box::new(rhs.0), expr.1),
                        }
                    }
                    Expr::Convert { ty, expr } => {
                        let expr = self.build_mir_expr(Spanned(*expr.0, expr.1));

                        Expression::Convert {
                            ty,
                            expr: Spanned(Box::new(expr.0), expr.1),
                        }
                    }
                    Expr::Call { func, args } => Expression::Call {
                        func: *self.func_id_map.get(func.0).unwrap(),
                        args: Spanned(
                            args.0
                                .into_iter()
                                .map(|arg| self.build_mir_expr(arg))
                                .collect(),
                            args.1,
                        ),
                    },
                },
                ty: expr.0.ty,
            },
            expr.1,
        )
    }
}
