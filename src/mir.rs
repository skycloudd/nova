use crate::{
    ast::{
        typed::{Expr, TypedExpr, TypedProcedure, TypedStatement, TypedTopLevel},
        BinaryOp, Type, UnaryOp,
    },
    scopes::Scopes,
    span::Spanned,
    FloatTy, IdGen, IntTy,
};

#[derive(Debug)]
pub enum TopLevel {
    Procedure(Procedure),
}

#[derive(Debug)]
pub struct Procedure {
    pub name: ProcId,
    pub args: Vec<(VarId, Type)>,
    pub body: Vec<Statement>,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct ProcId(pub usize);

#[derive(Debug)]
pub enum Statement {
    Expr(TypedExpression),
    Block(Vec<Self>),
    Loop(Vec<Self>),
    If {
        condition: TypedExpression,
        then_branch: Vec<Self>,
        else_branch: Option<Vec<Self>>,
    },
    Let {
        name: VarId,
        value: TypedExpression,
    },
    Assign {
        name: VarId,
        value: TypedExpression,
    },
    Break,
    Continue,
    Return,
    Action {
        action: Action,
        args: Vec<TypedExpression>,
    },
    Call {
        proc: ProcId,
        args: Vec<TypedExpression>,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub enum Action {
    /// `wait <float>`
    Wait,
    /// `waitframes <integer>`
    WaitFrames,
    /// `print <int/float/bool>`
    Print,
}

#[derive(Debug)]
pub struct TypedExpression {
    pub expr: Expression,
    pub ty: Type,
}

#[derive(Debug)]
pub enum Expression {
    Variable(VarId),
    Boolean(bool),
    Integer(IntTy),
    Float(FloatTy),
    Colour {
        r: u8,
        g: u8,
        b: u8,
        a: u8,
    },
    Vector {
        x: Box<TypedExpression>,
        y: Box<TypedExpression>,
    },
    Unary {
        op: UnaryOp,
        rhs: Box<TypedExpression>,
    },
    Binary {
        lhs: Box<TypedExpression>,
        op: BinaryOp,
        rhs: Box<TypedExpression>,
    },
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

impl<'src> VarIdMap<'src> {
    fn new() -> Self {
        Self {
            map: Scopes::new(),
            id_gen: IdGen::default(),
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

struct ProcIdMap<'src> {
    map: Scopes<&'src str, ProcId>,
    id_gen: IdGen,
}

impl ProcIdMap<'_> {
    fn new() -> Self {
        Self {
            map: Scopes::new(),
            id_gen: IdGen::default(),
        }
    }
}

impl<'src> IdMap<'src> for ProcIdMap<'src> {
    type Id = ProcId;

    fn get<'a>(&'a self, name: &'a str) -> Option<&ProcId> {
        self.map.get(&name)
    }

    fn insert(&mut self, name: &'src str) -> ProcId {
        let id = ProcId(self.id_gen.next());
        self.map.insert(name, id);
        id
    }
}

pub fn build<'file>(ast: &[Spanned<'file, TypedTopLevel<'_, 'file>>]) -> Vec<TopLevel> {
    let mut var_id_map = VarIdMap::new();
    let mut proc_id_map = ProcIdMap::new();

    ast.iter()
        .map(|top_level| build_mir_top_level(&mut var_id_map, &mut proc_id_map, &top_level.0))
        .collect()
}

fn build_mir_top_level<'src>(
    var_id_map: &mut VarIdMap<'src>,
    proc_id_map: &mut ProcIdMap<'src>,
    top_level: &TypedTopLevel<'src, '_>,
) -> TopLevel {
    match top_level {
        TypedTopLevel::Procedure(procedure) => {
            TopLevel::Procedure(build_mir_procedure(var_id_map, proc_id_map, procedure))
        }
    }
}

fn build_mir_procedure<'src>(
    var_id_map: &mut VarIdMap<'src>,
    proc_id_map: &mut ProcIdMap<'src>,
    procedure: &TypedProcedure<'src, '_>,
) -> Procedure {
    Procedure {
        name: proc_id_map.insert(procedure.name.0),
        args: procedure
            .args
            .iter()
            .map(|arg| (var_id_map.insert(&arg.0), arg.1 .0))
            .collect(),
        body: procedure
            .body
            .iter()
            .map(|stmt| build_mir_statement(var_id_map, proc_id_map, &stmt.0))
            .collect(),
    }
}

fn build_mir_statement<'src>(
    var_id_map: &mut VarIdMap<'src>,
    proc_id_map: &mut ProcIdMap<'src>,
    statement: &TypedStatement<'src, '_>,
) -> Statement {
    match statement {
        TypedStatement::Expr(expr) => Statement::Expr(build_mir_expr(var_id_map, &expr.0)),
        TypedStatement::Block(statements) => Statement::Block(
            statements
                .iter()
                .map(|stmt| build_mir_statement(var_id_map, proc_id_map, &stmt.0))
                .collect(),
        ),
        TypedStatement::Loop(statements) => Statement::Loop(
            statements
                .iter()
                .map(|stmt| build_mir_statement(var_id_map, proc_id_map, &stmt.0))
                .collect(),
        ),
        TypedStatement::If {
            condition,
            then_branch,
            else_branch,
        } => Statement::If {
            condition: build_mir_expr(var_id_map, &condition.0),
            then_branch: then_branch
                .iter()
                .map(|stmt| build_mir_statement(var_id_map, proc_id_map, &stmt.0))
                .collect(),
            else_branch: else_branch.as_ref().map(|s| {
                s.iter()
                    .map(|stmt| build_mir_statement(var_id_map, proc_id_map, &stmt.0))
                    .collect()
            }),
        },
        TypedStatement::For {
            name,
            start,
            end,
            inclusive,
            body,
        } => {
            let start = build_mir_expr(var_id_map, &start.0);
            let end = build_mir_expr(var_id_map, &end.0);
            let name = var_id_map.insert(name);
            let body = body
                .iter()
                .map(|stmt| build_mir_statement(var_id_map, proc_id_map, &stmt.0))
                .collect::<Vec<_>>();

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
            let value = build_mir_expr(var_id_map, &value.0);
            let name = var_id_map.insert(name);

            Statement::Let { name, value }
        }
        TypedStatement::Assign { name, value } => Statement::Assign {
            name: *var_id_map.get(name).unwrap(),
            value: build_mir_expr(var_id_map, &value.0),
        },
        TypedStatement::Break => Statement::Break,
        TypedStatement::Continue => Statement::Continue,
        TypedStatement::Return => Statement::Return,
        TypedStatement::Action { name, args } => Statement::Action {
            action: match name.0 {
                "wait" => Action::Wait,
                "waitframes" => Action::WaitFrames,
                "print" => Action::Print,
                _ => unreachable!(),
            },
            args: args
                .iter()
                .map(|arg| build_mir_expr(var_id_map, &arg.0))
                .collect(),
        },
        TypedStatement::Call { proc, args } => Statement::Call {
            proc: *proc_id_map.get(proc.0).unwrap(),
            args: args
                .iter()
                .map(|arg| build_mir_expr(var_id_map, &arg.0))
                .collect(),
        },
    }
}

fn build_mir_expr<'src>(
    var_id_map: &mut VarIdMap<'src>,
    expr: &TypedExpr<'src, '_>,
) -> TypedExpression {
    TypedExpression {
        expr: match &expr.expr {
            Expr::Variable(name) => Expression::Variable(*var_id_map.get(name).unwrap()),
            Expr::Boolean(value) => Expression::Boolean(*value),
            Expr::Integer(value) => Expression::Integer(*value),
            Expr::Float(value) => Expression::Float(*value),
            Expr::Colour { r, g, b, a } => Expression::Colour {
                r: *r,
                g: *g,
                b: *b,
                a: *a,
            },
            Expr::Vector { x, y } => Expression::Vector {
                x: Box::new(build_mir_expr(var_id_map, &x.0)),
                y: Box::new(build_mir_expr(var_id_map, &y.0)),
            },
            Expr::Unary(op, rhs) => {
                let rhs = build_mir_expr(var_id_map, &rhs.0);

                Expression::Unary {
                    op: op.0,
                    rhs: Box::new(rhs),
                }
            }
            Expr::Binary(lhs, op, rhs) => {
                let lhs = build_mir_expr(var_id_map, &lhs.0);
                let rhs = build_mir_expr(var_id_map, &rhs.0);

                Expression::Binary {
                    lhs: Box::new(lhs),
                    op: op.0,
                    rhs: Box::new(rhs),
                }
            }
        },
        ty: expr.ty,
    }
}
