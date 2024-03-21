use crate::{
    ast::{
        self,
        typed::{Expr, TypedExpr, TypedFunction, TypedStatement, TypedTopLevel},
        BinaryOp, UnaryOp,
    },
    scopes::Scopes,
    span::Spanned,
    FloatTy, IdGen, IntTy,
};

#[derive(Debug)]
pub enum TopLevel<'file> {
    Function(Function<'file>),
}

#[derive(Debug)]
pub struct Function<'file> {
    pub name: Spanned<'file, FuncId>,
    pub args: Spanned<'file, Vec<(Spanned<'file, VarId>, Spanned<'file, Type>)>>,
    pub return_ty: Spanned<'file, Type>,
    pub body: Spanned<'file, Vec<Spanned<'file, Statement<'file>>>>,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct FuncId(pub usize);

#[derive(Debug)]
pub enum Statement<'file> {
    Expr(Spanned<'file, TypedExpression<'file>>),
    Block(Spanned<'file, Vec<Spanned<'file, Self>>>),
    Loop(Spanned<'file, Vec<Spanned<'file, Self>>>),
    If {
        condition: Spanned<'file, TypedExpression<'file>>,
        then_branch: Spanned<'file, Vec<Spanned<'file, Self>>>,
        else_branch: Option<Spanned<'file, Vec<Spanned<'file, Self>>>>,
    },
    For {
        name: Spanned<'file, VarId>,
        start: Spanned<'file, TypedExpression<'file>>,
        end: Spanned<'file, TypedExpression<'file>>,
        inclusive: bool,
        body: Spanned<'file, Vec<Spanned<'file, Self>>>,
    },
    Let {
        name: Spanned<'file, VarId>,
        value: Spanned<'file, TypedExpression<'file>>,
    },
    Assign {
        name: Spanned<'file, VarId>,
        value: Spanned<'file, TypedExpression<'file>>,
    },
    Break,
    Continue,
    Return(Spanned<'file, TypedExpression<'file>>),
}

#[derive(Debug)]
pub struct TypedExpression<'file> {
    pub expr: Expression<'file>,
    pub ty: Type,
}

#[derive(Debug)]
pub enum Expression<'file> {
    Variable(Spanned<'file, VarId>),
    Boolean(bool),
    Integer(IntTy),
    Float(FloatTy),
    String(String),
    Unary {
        op: Spanned<'file, UnaryOp>,
        rhs: Spanned<'file, Box<TypedExpression<'file>>>,
    },
    Binary {
        lhs: Spanned<'file, Box<TypedExpression<'file>>>,
        op: Spanned<'file, BinaryOp>,
        rhs: Spanned<'file, Box<TypedExpression<'file>>>,
    },
    Convert {
        ty: Spanned<'file, Type>,
        expr: Spanned<'file, Box<TypedExpression<'file>>>,
    },
    Call {
        func: Spanned<'file, FuncId>,
        args: Spanned<'file, Vec<Spanned<'file, TypedExpression<'file>>>>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Type {
    Integer,
    Float,
    Boolean,
    String,
}

impl TryFrom<&ast::Type> for Type {
    type Error = ();

    fn try_from(ty: &ast::Type) -> Result<Self, Self::Error> {
        match ty {
            ast::Type::Error => Err(()),
            ast::Type::Integer => Ok(Self::Integer),
            ast::Type::Float => Ok(Self::Float),
            ast::Type::Boolean => Ok(Self::Boolean),
            ast::Type::String => Ok(Self::String),
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer => write!(f, "int"),
            Self::Float => write!(f, "float"),
            Self::Boolean => write!(f, "bool"),
            Self::String => write!(f, "str"),
        }
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct VarId(pub usize);

trait IdMap<'src, 'file> {
    type Id;

    fn get<'a>(&'a self, name: &'a str) -> Option<&Self::Id>;
    fn insert(&mut self, name: Spanned<'file, &'src str>) -> Self::Id;
}

struct VarIdMap<'src, 'file> {
    map: Scopes<&'src str, Spanned<'file, VarId>>,
    id_gen: IdGen,
}

impl VarIdMap<'_, '_> {
    fn new() -> Self {
        Self {
            map: Scopes::new(),
            id_gen: IdGen::new(),
        }
    }
}

impl<'src, 'file> IdMap<'src, 'file> for VarIdMap<'src, 'file> {
    type Id = Spanned<'file, VarId>;

    fn get<'a>(&'a self, name: &'a str) -> Option<&Spanned<'file, VarId>> {
        self.map.get(&name)
    }

    fn insert(&mut self, name: Spanned<'file, &'src str>) -> Spanned<'file, VarId> {
        let id = Spanned(VarId(self.id_gen.next()), name.1);
        self.map.insert(name.0, id);
        id
    }
}

struct FuncIdMap<'src, 'file> {
    map: Scopes<&'src str, Spanned<'file, FuncId>>,
    id_gen: IdGen,
}

impl FuncIdMap<'_, '_> {
    fn new() -> Self {
        Self {
            map: Scopes::new(),
            id_gen: IdGen::new(),
        }
    }
}

impl<'src, 'file> IdMap<'src, 'file> for FuncIdMap<'src, 'file> {
    type Id = Spanned<'file, FuncId>;

    fn get<'a>(&'a self, name: &'a str) -> Option<&Spanned<'file, FuncId>> {
        self.map.get(&name)
    }

    fn insert(&mut self, name: Spanned<'file, &'src str>) -> Spanned<'file, FuncId> {
        let id = Spanned(FuncId(self.id_gen.next()), name.1);
        self.map.insert(name.0, id);
        id
    }
}

pub fn build<'src: 'file, 'file>(
    ast: Vec<Spanned<'file, TypedTopLevel<'src, 'file>>>,
) -> Vec<Spanned<'file, TopLevel<'file>>> {
    MirBuilder::new().build(ast)
}

struct MirBuilder<'src, 'file> {
    var_id_map: VarIdMap<'src, 'file>,
    func_id_map: FuncIdMap<'src, 'file>,
}

impl<'src, 'file: 'src> MirBuilder<'src, 'file> {
    fn new() -> Self {
        Self {
            var_id_map: VarIdMap::new(),
            func_id_map: FuncIdMap::new(),
        }
    }

    fn build(
        &mut self,
        ast: Vec<Spanned<'file, TypedTopLevel<'src, 'file>>>,
    ) -> Vec<Spanned<'file, TopLevel<'file>>> {
        for top_level in &ast {
            match &top_level.0 {
                TypedTopLevel::Function(function) => {
                    self.func_id_map.insert(function.name);

                    for arg in &function.args.0 {
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
        top_level: Spanned<'file, TypedTopLevel<'src, 'file>>,
    ) -> Spanned<'file, TopLevel<'file>> {
        Spanned(
            match top_level.0 {
                TypedTopLevel::Function(function) => {
                    TopLevel::Function(self.build_mir_function(function))
                }
            },
            top_level.1,
        )
    }

    fn build_mir_function(&mut self, function: TypedFunction<'src, 'file>) -> Function<'file> {
        Function {
            name: *self.func_id_map.get(function.name.0).unwrap(),
            args: Spanned(
                function
                    .args
                    .0
                    .iter()
                    .map(|arg| {
                        let name = arg.0;
                        let ty = arg.1;

                        (
                            *self.var_id_map.get(name.0).unwrap(),
                            Spanned((&ty.0).try_into().unwrap(), ty.1),
                        )
                    })
                    .collect(),
                function.args.1,
            ),
            return_ty: Spanned(
                (&function.return_ty.0).try_into().unwrap(),
                function.return_ty.1,
            ),
            body: self.build_statements(function.body),
        }
    }

    fn build_statements(
        &mut self,
        statements: Spanned<'file, Vec<Spanned<'file, TypedStatement<'src, 'file>>>>,
    ) -> Spanned<'file, Vec<Spanned<'file, Statement<'file>>>> {
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
        statement: Spanned<'file, TypedStatement<'src, 'file>>,
    ) -> Spanned<'file, Statement<'file>> {
        Spanned(
            match statement.0 {
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

    fn build_mir_expr(
        &mut self,
        expr: Spanned<'file, TypedExpr<'src, 'file>>,
    ) -> Spanned<'file, TypedExpression<'file>> {
        Spanned(
            TypedExpression {
                expr: match expr.0.expr {
                    Expr::Error => unreachable!(),
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
                            ty: Spanned((&ty.0).try_into().unwrap(), ty.1),
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
                ty: (&expr.0.ty).try_into().unwrap(),
            },
            expr.1,
        )
    }
}
