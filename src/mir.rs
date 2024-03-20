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
pub enum TopLevel<'ast> {
    Function(Function<'ast>),
}

#[derive(Debug)]
pub struct Function<'ast> {
    pub name: FuncId,
    pub args: Vec<(VarId, Type)>,
    pub return_ty: Type,
    pub body: Vec<Statement<'ast>>,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct FuncId(pub usize);

#[derive(Debug)]
pub enum Statement<'ast> {
    Expr(TypedExpression<'ast>),
    Block(Vec<Self>),
    Loop(Vec<Self>),
    If {
        condition: TypedExpression<'ast>,
        then_branch: Vec<Self>,
        else_branch: Option<Vec<Self>>,
    },
    Let {
        name: VarId,
        value: TypedExpression<'ast>,
    },
    Assign {
        name: VarId,
        value: TypedExpression<'ast>,
    },
    Break,
    Continue,
    Return(TypedExpression<'ast>),
}

#[derive(Debug)]
pub struct TypedExpression<'ast> {
    pub expr: Expression<'ast>,
    pub ty: Type,
}

#[derive(Debug)]
pub enum Expression<'ast> {
    Variable(VarId),
    Boolean(bool),
    Integer(IntTy),
    Float(FloatTy),
    String(&'ast str),
    Unary {
        op: UnaryOp,
        rhs: Box<TypedExpression<'ast>>,
    },
    Binary {
        lhs: Box<TypedExpression<'ast>>,
        op: BinaryOp,
        rhs: Box<TypedExpression<'ast>>,
    },
    Convert {
        ty: Type,
        expr: Box<TypedExpression<'ast>>,
    },
    Call {
        func: FuncId,
        args: Vec<TypedExpression<'ast>>,
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

trait IdMap<'src> {
    type Id;

    fn get<'a>(&'a self, name: &'a str) -> Option<&Self::Id>;
    fn insert(&mut self, name: &'src str) -> Self::Id;
}

struct VarIdMap<'src> {
    map: Scopes<&'src str, VarId>,
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
    type Id = VarId;

    fn get<'a>(&'a self, name: &'a str) -> Option<&VarId> {
        self.map.get(&name)
    }

    fn insert(&mut self, name: &'src str) -> VarId {
        let id = VarId(self.id_gen.next());
        self.map.insert(name, id);
        id
    }
}

struct FuncIdMap<'src> {
    map: Scopes<&'src str, FuncId>,
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
    type Id = FuncId;

    fn get<'a>(&'a self, name: &'a str) -> Option<&FuncId> {
        self.map.get(&name)
    }

    fn insert(&mut self, name: &'src str) -> FuncId {
        let id = FuncId(self.id_gen.next());
        self.map.insert(name, id);
        id
    }
}

pub fn build<'file, 'ast>(
    ast: &'ast [Spanned<'file, TypedTopLevel<'_, 'file>>],
) -> Vec<TopLevel<'ast>> {
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

    fn build<'file: 'src, 'ast>(
        &mut self,
        ast: &'ast [Spanned<'file, TypedTopLevel<'src, 'file>>],
    ) -> Vec<TopLevel<'ast>> {
        for top_level in ast {
            match &top_level.0 {
                TypedTopLevel::Function(function) => {
                    self.func_id_map.insert(function.name.0);

                    for arg in &function.args.0 {
                        self.var_id_map.insert(arg.0 .0);
                    }
                }
            }
        }

        ast.iter()
            .map(|top_level| self.build_mir_top_level(&top_level.0))
            .collect()
    }

    fn build_mir_top_level<'file: 'src, 'ast>(
        &mut self,
        top_level: &'ast TypedTopLevel<'src, 'file>,
    ) -> TopLevel<'ast> {
        match top_level {
            TypedTopLevel::Function(function) => {
                TopLevel::Function(self.build_mir_function(function))
            }
        }
    }

    fn build_mir_function<'file: 'src, 'ast>(
        &mut self,
        function: &'ast TypedFunction<'src, 'file>,
    ) -> Function<'ast> {
        Function {
            name: *self.func_id_map.get(function.name.0).unwrap(),
            args: function
                .args
                .0
                .iter()
                .map(|arg| {
                    (
                        *self.var_id_map.get(arg.0 .0).unwrap(),
                        (&arg.1 .0).try_into().unwrap(),
                    )
                })
                .collect(),
            return_ty: (&function.return_ty.0).try_into().unwrap(),
            body: self.build_statements(&function.body.0),
        }
    }

    fn build_statements<'file: 'src, 'ast>(
        &mut self,
        statements: &'ast [Spanned<'src, TypedStatement<'src, 'file>>],
    ) -> Vec<Statement<'ast>> {
        statements
            .iter()
            .map(|stmt| self.build_mir_statement(&stmt.0))
            .collect()
    }

    fn build_mir_statement<'file: 'src, 'ast>(
        &mut self,
        statement: &'ast TypedStatement<'src, 'file>,
    ) -> Statement<'ast> {
        match statement {
            TypedStatement::Expr(expr) => Statement::Expr(self.build_mir_expr(&expr.0)),
            TypedStatement::Block(statements) => {
                Statement::Block(self.build_statements(&statements.0))
            }
            TypedStatement::Loop(statements) => {
                Statement::Loop(self.build_statements(&statements.0))
            }
            TypedStatement::If {
                condition,
                then_branch,
                else_branch,
            } => Statement::If {
                condition: self.build_mir_expr(&condition.0),
                then_branch: self.build_statements(&then_branch.0),
                else_branch: else_branch
                    .as_ref()
                    .map(|stmts| self.build_statements(&stmts.0)),
            },
            TypedStatement::For {
                name,
                start,
                end,
                inclusive,
                body,
            } => {
                let start = self.build_mir_expr(&start.0);
                let end = self.build_mir_expr(&end.0);
                let name = self.var_id_map.insert(name.0);
                let body = self.build_statements(&body.0);

                Statement::Block(vec![
                    Statement::Let { name, value: start },
                    Statement::Loop(vec![Statement::If {
                        condition: TypedExpression {
                            expr: Expression::Binary {
                                lhs: Box::new(TypedExpression {
                                    expr: Expression::Variable(name),
                                    ty: Type::Integer,
                                }),
                                op: if *inclusive {
                                    BinaryOp::LessThanEquals
                                } else {
                                    BinaryOp::LessThan
                                },
                                rhs: Box::new(end),
                            },
                            ty: Type::Boolean,
                        },
                        then_branch: {
                            let mut stmts = body;

                            stmts.push(Statement::Assign {
                                name,
                                value: TypedExpression {
                                    expr: Expression::Binary {
                                        lhs: Box::new(TypedExpression {
                                            expr: Expression::Variable(name),
                                            ty: Type::Integer,
                                        }),
                                        op: BinaryOp::Plus,
                                        rhs: Box::new(TypedExpression {
                                            expr: Expression::Integer(1),
                                            ty: Type::Integer,
                                        }),
                                    },
                                    ty: Type::Integer,
                                },
                            });

                            stmts
                        },
                        else_branch: Some(vec![Statement::Break]),
                    }]),
                ])
            }
            TypedStatement::Let { name, value } => {
                let value = self.build_mir_expr(&value.0);
                let name = self.var_id_map.insert(name.0);

                Statement::Let { name, value }
            }
            TypedStatement::Assign { name, value } => Statement::Assign {
                name: *self.var_id_map.get(name.0).unwrap(),
                value: self.build_mir_expr(&value.0),
            },
            TypedStatement::Break => Statement::Break,
            TypedStatement::Continue => Statement::Continue,
            TypedStatement::Return(expr) => Statement::Return(self.build_mir_expr(&expr.0)),
        }
    }

    fn build_mir_expr<'ast>(&mut self, expr: &'ast TypedExpr<'src, '_>) -> TypedExpression<'ast> {
        TypedExpression {
            expr: match &expr.expr {
                Expr::Error => unreachable!(),
                Expr::Variable(name) => Expression::Variable(*self.var_id_map.get(name.0).unwrap()),
                Expr::Boolean(value) => Expression::Boolean(*value),
                Expr::Integer(value) => Expression::Integer(*value),
                Expr::Float(value) => Expression::Float(*value),
                Expr::String(value) => Expression::String(value),
                Expr::Unary(op, rhs) => {
                    let rhs = self.build_mir_expr(&rhs.0);

                    Expression::Unary {
                        op: op.0,
                        rhs: Box::new(rhs),
                    }
                }
                Expr::Binary(lhs, op, rhs) => {
                    let lhs = self.build_mir_expr(&lhs.0);
                    let rhs = self.build_mir_expr(&rhs.0);

                    Expression::Binary {
                        lhs: Box::new(lhs),
                        op: op.0,
                        rhs: Box::new(rhs),
                    }
                }
                Expr::Convert { ty, expr } => {
                    let expr = self.build_mir_expr(&expr.0);

                    Expression::Convert {
                        ty: (&ty.0).try_into().unwrap(),
                        expr: Box::new(expr),
                    }
                }
                Expr::Call { func, args } => Expression::Call {
                    func: *self.func_id_map.get(func.0).unwrap(),
                    args: args
                        .0
                        .iter()
                        .map(|arg| self.build_mir_expr(&arg.0))
                        .collect(),
                },
            },
            ty: (&expr.ty).try_into().unwrap(),
        }
    }
}
