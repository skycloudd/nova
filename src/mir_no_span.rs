use crate::{
    ast::{BinaryOp, UnaryOp},
    mir::{self, FuncId, VarId},
    span::Spanned,
    FloatTy, IntTy,
};

#[derive(Debug)]
pub enum TopLevel<'src> {
    Function(Function<'src>),
}

#[derive(Debug)]
pub struct Function<'src> {
    pub id: FuncId,
    pub name: &'src str,
    pub params: Vec<(VarId, Type)>,
    pub return_ty: Type,
    pub body: Vec<Statement>,
}

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
    Return(TypedExpression),
}

#[derive(Clone, Debug)]
pub struct TypedExpression {
    pub expr: Expression,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub enum Expression {
    Variable(VarId),
    Boolean(bool),
    Integer(IntTy),
    Float(FloatTy),
    String(String),
    Unary {
        op: UnaryOp,
        rhs: Box<TypedExpression>,
    },
    Binary {
        lhs: Box<TypedExpression>,
        op: BinaryOp,
        rhs: Box<TypedExpression>,
    },
    Convert {
        ty: Type,
        expr: Box<TypedExpression>,
    },
    Call {
        func: FuncId,
        args: Vec<TypedExpression>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Type {
    Integer,
    Float,
    Boolean,
    String,
}

impl TryFrom<mir::Type> for Type {
    type Error = ();

    fn try_from(ty: mir::Type) -> Result<Self, Self::Error> {
        match ty {
            mir::Type::Error => Err(()),
            mir::Type::Integer => Ok(Self::Integer),
            mir::Type::Float => Ok(Self::Float),
            mir::Type::Boolean => Ok(Self::Boolean),
            mir::Type::String => Ok(Self::String),
        }
    }
}

pub fn build(ast: Vec<Spanned<mir::TopLevel>>) -> Vec<TopLevel> {
    ast.into_iter().map(build_mir_top_level).collect()
}

fn build_mir_top_level(top_level: Spanned<mir::TopLevel>) -> TopLevel {
    match top_level.0 {
        mir::TopLevel::Function(function) => TopLevel::Function(build_mir_function(function)),
    }
}

fn build_mir_function(function: Spanned<mir::Function>) -> Function {
    Function {
        id: function.0.id.0,
        name: function.0.name.0,
        params: function
            .0
            .params
            .0
            .into_iter()
            .map(|(name, ty)| (name.0, ty.0.try_into().unwrap()))
            .collect(),
        return_ty: function.0.return_ty.0.try_into().unwrap(),
        body: build_statements(function.0.body),
    }
}

fn build_statements(statements: Spanned<Vec<Spanned<mir::Statement>>>) -> Vec<Statement> {
    statements.0.into_iter().map(build_mir_statement).collect()
}

fn build_mir_statement(statement: Spanned<mir::Statement>) -> Statement {
    match statement.0 {
        mir::Statement::Error => unreachable!(),
        mir::Statement::Expr(expr) => Statement::Expr(build_mir_expr(expr)),
        mir::Statement::Block(statements) => Statement::Block(build_statements(statements)),
        mir::Statement::Loop(statements) => Statement::Loop(build_statements(statements)),
        mir::Statement::If {
            condition,
            then_branch,
            else_branch,
        } => Statement::If {
            condition: build_mir_expr(condition),
            then_branch: build_statements(then_branch),
            else_branch: else_branch.map(build_statements),
        },
        mir::Statement::For {
            name,
            start,
            end,
            inclusive,
            body,
        } => {
            let var_expr = TypedExpression {
                expr: Expression::Variable(name.0),
                ty: Type::Integer,
            };

            Statement::Block(vec![
                Statement::Let {
                    name: name.0,
                    value: build_mir_expr(start),
                },
                Statement::Loop(vec![
                    Statement::If {
                        condition: TypedExpression {
                            expr: Expression::Binary {
                                lhs: Box::new(var_expr.clone()),
                                op: if inclusive {
                                    BinaryOp::LessThanEquals
                                } else {
                                    BinaryOp::LessThan
                                },
                                rhs: Box::new(build_mir_expr(end)),
                            },
                            ty: Type::Boolean,
                        },
                        then_branch: build_statements(body),
                        else_branch: Some(vec![Statement::Break]),
                    },
                    Statement::Assign {
                        name: name.0,
                        value: TypedExpression {
                            expr: Expression::Binary {
                                lhs: Box::new(var_expr),
                                op: BinaryOp::Plus,
                                rhs: Box::new(TypedExpression {
                                    expr: Expression::Integer(1),
                                    ty: Type::Integer,
                                }),
                            },
                            ty: Type::Integer,
                        },
                    },
                ]),
            ])
        }
        mir::Statement::Let { name, value } => Statement::Let {
            name: name.0,
            value: build_mir_expr(value),
        },
        mir::Statement::Assign { name, value } => Statement::Assign {
            name: name.0,
            value: build_mir_expr(value),
        },
        mir::Statement::Break => Statement::Break,
        mir::Statement::Continue => Statement::Continue,
        mir::Statement::Return(expr) => Statement::Return(build_mir_expr(expr)),
    }
}

fn build_mir_expr(expr: Spanned<mir::TypedExpression>) -> TypedExpression {
    TypedExpression {
        expr: match expr.0.expr {
            mir::Expression::Error => unreachable!(),
            mir::Expression::Variable(name) => Expression::Variable(name.0),
            mir::Expression::Boolean(value) => Expression::Boolean(value),
            mir::Expression::Integer(value) => Expression::Integer(value),
            mir::Expression::Float(value) => Expression::Float(value),
            mir::Expression::String(value) => Expression::String(value),
            mir::Expression::Unary { op, rhs } => Expression::Unary {
                op: op.0,
                rhs: Box::new(build_mir_expr(Spanned(*rhs.0, rhs.1))),
            },
            mir::Expression::Binary { lhs, op, rhs } => Expression::Binary {
                lhs: Box::new(build_mir_expr(Spanned(*lhs.0, lhs.1))),
                op: op.0,
                rhs: Box::new(build_mir_expr(Spanned(*rhs.0, rhs.1))),
            },
            mir::Expression::Convert { ty, expr } => Expression::Convert {
                ty: ty.0.try_into().unwrap(),
                expr: Box::new(build_mir_expr(Spanned(*expr.0, expr.1))),
            },
            mir::Expression::Call { func, args } => Expression::Call {
                func: func.0,
                args: args.0.into_iter().map(build_mir_expr).collect(),
            },
        },
        ty: expr.0.ty.try_into().unwrap(),
    }
}
