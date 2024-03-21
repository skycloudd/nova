use crate::{
    ast::{BinaryOp, UnaryOp},
    mir::{self, FuncId, Type, VarId},
    span::Spanned,
    FloatTy, IntTy,
};

#[derive(Debug)]
pub enum TopLevel {
    Function(Function),
}

#[derive(Debug)]
pub struct Function {
    pub name: FuncId,
    pub args: Vec<(VarId, Type)>,
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

pub fn build<'src: 'file, 'file>(ast: Vec<Spanned<'file, mir::TopLevel<'file>>>) -> Vec<TopLevel> {
    ast.into_iter()
        .map(|top_level| build_mir_top_level(top_level))
        .collect()
}

fn build_mir_top_level<'file>(top_level: Spanned<'file, mir::TopLevel<'file>>) -> TopLevel {
    match top_level.0 {
        mir::TopLevel::Function(function) => TopLevel::Function(build_mir_function(function)),
    }
}

fn build_mir_function(function: mir::Function) -> Function {
    Function {
        name: function.name.0,
        args: function
            .args
            .0
            .into_iter()
            .map(|(name, ty)| (name.0, ty.0))
            .collect(),
        return_ty: function.return_ty.0,
        body: build_statements(function.body),
    }
}

fn build_statements<'file>(
    statements: Spanned<'file, Vec<Spanned<'file, mir::Statement<'file>>>>,
) -> Vec<Statement> {
    statements
        .0
        .into_iter()
        .map(|stmt| build_mir_statement(stmt))
        .collect()
}

fn build_mir_statement<'file>(statement: Spanned<'file, mir::Statement<'file>>) -> Statement {
    match statement.0 {
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
            else_branch: else_branch.map(|stmts| build_statements(stmts)),
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
                        else_branch: None,
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

fn build_mir_expr<'file>(expr: Spanned<'file, mir::TypedExpression<'file>>) -> TypedExpression {
    TypedExpression {
        expr: match expr.0.expr {
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
                ty: ty.0,
                expr: Box::new(build_mir_expr(Spanned(*expr.0, expr.1))),
            },
            mir::Expression::Call { func, args } => Expression::Call {
                func: func.0,
                args: args.0.into_iter().map(|arg| build_mir_expr(arg)).collect(),
            },
        },
        ty: expr.0.ty,
    }
}
