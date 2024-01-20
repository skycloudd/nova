use crate::{
    ast::{
        typed::{self, Expr, TypedExpr, TypedStatement as Statement},
        BinaryOp, UnaryOp,
    },
    scopes::Scopes,
    span::Spanned,
    FloatTy, IntTy,
};

#[derive(Debug)]
pub enum TypedStatement<'file> {
    Expr(Spanned<'file, TypedExpression<'file>>),
    Print(Spanned<'file, TypedExpression<'file>>),
    Loop(Spanned<'file, Vec<Spanned<'file, TypedStatement<'file>>>>),
    If {
        condition: Spanned<'file, TypedExpression<'file>>,
        then_branch: Spanned<'file, Vec<Spanned<'file, TypedStatement<'file>>>>,
        else_branch: Option<Spanned<'file, Vec<Spanned<'file, TypedStatement<'file>>>>>,
    },
    For {
        name: Spanned<'file, VarId>,
        start: Spanned<'file, TypedExpression<'file>>,
        end: Spanned<'file, TypedExpression<'file>>,
        inclusive: bool,
        body: Spanned<'file, Vec<Spanned<'file, TypedStatement<'file>>>>,
    },
    Let {
        name: Spanned<'file, VarId>,
        value: Spanned<'file, TypedExpression<'file>>,
    },
    Const {
        name: Spanned<'file, VarId>,
        value: Spanned<'file, TypedExpression<'file>>,
    },
    Assign {
        name: Spanned<'file, VarId>,
        value: Spanned<'file, TypedExpression<'file>>,
    },
    Break,
    Continue,
}

#[derive(Debug)]
pub struct TypedExpression<'file> {
    pub expr: Expression<'file>,
    pub ty: Type,
}

#[derive(Debug)]
pub enum Expression<'file> {
    Variable(VarId),
    Boolean(bool),
    Integer(IntTy),
    Float(FloatTy),
    Colour {
        r: u8,
        g: u8,
        b: u8,
    },
    Vector {
        x: Spanned<'file, Box<TypedExpression<'file>>>,
        y: Spanned<'file, Box<TypedExpression<'file>>>,
    },
    Operation(Box<Operation<'file>>),
}

pub type VarId = usize;

#[derive(Debug)]
pub enum Operation<'file> {
    IntegerEquals(
        Spanned<'file, TypedExpression<'file>>,
        Spanned<'file, TypedExpression<'file>>,
    ),
    IntegerNotEquals(
        Spanned<'file, TypedExpression<'file>>,
        Spanned<'file, TypedExpression<'file>>,
    ),
    IntegerPlus(
        Spanned<'file, TypedExpression<'file>>,
        Spanned<'file, TypedExpression<'file>>,
    ),
    IntegerMinus(
        Spanned<'file, TypedExpression<'file>>,
        Spanned<'file, TypedExpression<'file>>,
    ),
    IntegerMultiply(
        Spanned<'file, TypedExpression<'file>>,
        Spanned<'file, TypedExpression<'file>>,
    ),
    IntegerDivide(
        Spanned<'file, TypedExpression<'file>>,
        Spanned<'file, TypedExpression<'file>>,
    ),
    IntegerGreaterThanEquals(
        Spanned<'file, TypedExpression<'file>>,
        Spanned<'file, TypedExpression<'file>>,
    ),
    IntegerLessThanEquals(
        Spanned<'file, TypedExpression<'file>>,
        Spanned<'file, TypedExpression<'file>>,
    ),
    IntegerGreaterThan(
        Spanned<'file, TypedExpression<'file>>,
        Spanned<'file, TypedExpression<'file>>,
    ),
    IntegerLessThan(
        Spanned<'file, TypedExpression<'file>>,
        Spanned<'file, TypedExpression<'file>>,
    ),

    FloatEquals(
        Spanned<'file, TypedExpression<'file>>,
        Spanned<'file, TypedExpression<'file>>,
    ),
    FloatNotEquals(
        Spanned<'file, TypedExpression<'file>>,
        Spanned<'file, TypedExpression<'file>>,
    ),
    FloatPlus(
        Spanned<'file, TypedExpression<'file>>,
        Spanned<'file, TypedExpression<'file>>,
    ),
    FloatMinus(
        Spanned<'file, TypedExpression<'file>>,
        Spanned<'file, TypedExpression<'file>>,
    ),
    FloatMultiply(
        Spanned<'file, TypedExpression<'file>>,
        Spanned<'file, TypedExpression<'file>>,
    ),
    FloatDivide(
        Spanned<'file, TypedExpression<'file>>,
        Spanned<'file, TypedExpression<'file>>,
    ),
    FloatGreaterThanEquals(
        Spanned<'file, TypedExpression<'file>>,
        Spanned<'file, TypedExpression<'file>>,
    ),
    FloatLessThanEquals(
        Spanned<'file, TypedExpression<'file>>,
        Spanned<'file, TypedExpression<'file>>,
    ),
    FloatGreaterThan(
        Spanned<'file, TypedExpression<'file>>,
        Spanned<'file, TypedExpression<'file>>,
    ),
    FloatLessThan(
        Spanned<'file, TypedExpression<'file>>,
        Spanned<'file, TypedExpression<'file>>,
    ),

    BooleanEquals(
        Spanned<'file, TypedExpression<'file>>,
        Spanned<'file, TypedExpression<'file>>,
    ),
    BooleanNotEquals(
        Spanned<'file, TypedExpression<'file>>,
        Spanned<'file, TypedExpression<'file>>,
    ),

    IntegerNegate(Spanned<'file, TypedExpression<'file>>),
    FloatNegate(Spanned<'file, TypedExpression<'file>>),
    BooleanNot(Spanned<'file, TypedExpression<'file>>),
}

#[derive(Clone, Copy, Debug)]
pub enum Type {
    Boolean,
    Integer,
    Float,
    Colour,
    Vector,
}

impl From<typed::Type> for Type {
    fn from(ty: typed::Type) -> Self {
        match ty {
            typed::Type::Boolean => Self::Boolean,
            typed::Type::Integer => Self::Integer,
            typed::Type::Float => Self::Float,
            typed::Type::Colour => Self::Colour,
            typed::Type::Vector => Self::Vector,
        }
    }
}

struct VarIdMap<'src> {
    map: Scopes<&'src str, VarId>,
    next_id: VarId,
}

impl<'src> VarIdMap<'src> {
    fn new() -> Self {
        Self {
            map: Scopes::new(),
            next_id: 0,
        }
    }

    fn get_or_insert(&mut self, name: &'src str) -> VarId {
        if let Some(id) = self.map.get(&name) {
            *id
        } else {
            let id = self.next_id;
            self.next_id += 1;
            self.map.insert(name, id);
            id
        }
    }
}

pub fn build<'file>(
    ast: Vec<Spanned<'file, Statement<'_, 'file>>>,
) -> Vec<Spanned<'file, TypedStatement<'file>>> {
    let mut var_id_map = VarIdMap::new();

    ast.into_iter()
        .map(|stmt| build_mir_statement(&mut var_id_map, stmt))
        .collect()
}

fn build_mir_statement<'src, 'file>(
    var_id_map: &mut VarIdMap<'src>,
    statement: Spanned<'file, Statement<'src, 'file>>,
) -> Spanned<'file, TypedStatement<'file>> {
    statement.map(|statement| match statement {
        Statement::Expr(expr) => TypedStatement::Expr(build_mir_expr(var_id_map, expr)),
        Statement::Print(expr) => TypedStatement::Print(build_mir_expr(var_id_map, expr)),
        Statement::Loop(statements) => TypedStatement::Loop(statements.map(|s| {
            s.into_iter()
                .map(|stmt| build_mir_statement(var_id_map, stmt))
                .collect()
        })),
        Statement::If {
            condition,
            then_branch,
            else_branch,
        } => TypedStatement::If {
            condition: build_mir_expr(var_id_map, condition),
            then_branch: then_branch.map(|s| {
                s.into_iter()
                    .map(|stmt| build_mir_statement(var_id_map, stmt))
                    .collect()
            }),
            else_branch: else_branch.map(|s| {
                s.map(|s| {
                    s.into_iter()
                        .map(|stmt| build_mir_statement(var_id_map, stmt))
                        .collect()
                })
            }),
        },
        Statement::For {
            name,
            start,
            end,
            inclusive,
            body,
        } => TypedStatement::For {
            name: name.map(|name| var_id_map.get_or_insert(name)),
            start: build_mir_expr(var_id_map, start),
            end: build_mir_expr(var_id_map, end),
            inclusive,
            body: body.map(|s| {
                s.into_iter()
                    .map(|stmt| build_mir_statement(var_id_map, stmt))
                    .collect()
            }),
        },
        Statement::Let { name, value } => TypedStatement::Let {
            name: name.map(|name| var_id_map.get_or_insert(name)),
            value: build_mir_expr(var_id_map, value),
        },
        Statement::Const { name, value } => TypedStatement::Const {
            name: name.map(|name| var_id_map.get_or_insert(name)),
            value: build_mir_expr(var_id_map, value),
        },
        Statement::Assign { name, value } => TypedStatement::Assign {
            name: name.map(|name| var_id_map.get_or_insert(name)),
            value: build_mir_expr(var_id_map, value),
        },
        Statement::Break => TypedStatement::Break,
        Statement::Continue => TypedStatement::Continue,
    })
}

fn build_mir_expr<'src, 'file>(
    var_id_map: &mut VarIdMap<'src>,
    expr: Spanned<'file, TypedExpr<'src, 'file>>,
) -> Spanned<'file, TypedExpression<'file>> {
    expr.map(|expr| TypedExpression {
        expr: match expr.expr {
            Expr::Variable(name) => Expression::Variable(var_id_map.get_or_insert(name)),
            Expr::Boolean(value) => Expression::Boolean(value),
            Expr::Integer(value) => Expression::Integer(value),
            Expr::Float(value) => Expression::Float(value),
            Expr::Colour { r, g, b } => Expression::Colour { r, g, b },
            Expr::Vector { x, y } => Expression::Vector {
                x: build_mir_expr(var_id_map, x.map(|x| *x)).map(Box::new),
                y: build_mir_expr(var_id_map, y.map(|y| *y)).map(Box::new),
            },
            Expr::Binary(lhs, op, rhs) => {
                let lhs = build_mir_expr(var_id_map, lhs.map(|l| *l));
                let rhs = build_mir_expr(var_id_map, rhs.map(|r| *r));

                Expression::Operation(Box::new(match (&lhs.0.ty, &rhs.0.ty) {
                    (Type::Integer, Type::Integer) => match op.0 {
                        BinaryOp::Equals => Operation::IntegerEquals(lhs, rhs),
                        BinaryOp::NotEquals => Operation::IntegerNotEquals(lhs, rhs),
                        BinaryOp::Plus => Operation::IntegerPlus(lhs, rhs),
                        BinaryOp::Minus => Operation::IntegerMinus(lhs, rhs),
                        BinaryOp::Multiply => Operation::IntegerMultiply(lhs, rhs),
                        BinaryOp::Divide => Operation::IntegerDivide(lhs, rhs),
                        BinaryOp::GreaterThanEquals => {
                            Operation::IntegerGreaterThanEquals(lhs, rhs)
                        }
                        BinaryOp::LessThanEquals => Operation::IntegerLessThanEquals(lhs, rhs),
                        BinaryOp::GreaterThan => Operation::IntegerGreaterThan(lhs, rhs),
                        BinaryOp::LessThan => Operation::IntegerLessThan(lhs, rhs),
                    },

                    (Type::Float, Type::Float) => match op.0 {
                        BinaryOp::Equals => Operation::FloatEquals(lhs, rhs),
                        BinaryOp::NotEquals => Operation::FloatNotEquals(lhs, rhs),
                        BinaryOp::Plus => Operation::FloatPlus(lhs, rhs),
                        BinaryOp::Minus => Operation::FloatMinus(lhs, rhs),
                        BinaryOp::Multiply => Operation::FloatMultiply(lhs, rhs),
                        BinaryOp::Divide => Operation::FloatDivide(lhs, rhs),
                        BinaryOp::GreaterThanEquals => Operation::FloatGreaterThanEquals(lhs, rhs),
                        BinaryOp::LessThanEquals => Operation::FloatLessThanEquals(lhs, rhs),
                        BinaryOp::GreaterThan => Operation::FloatGreaterThan(lhs, rhs),
                        BinaryOp::LessThan => Operation::FloatLessThan(lhs, rhs),
                    },

                    (Type::Boolean, Type::Boolean) => match op.0 {
                        BinaryOp::Equals => Operation::BooleanEquals(lhs, rhs),
                        BinaryOp::NotEquals => Operation::BooleanNotEquals(lhs, rhs),

                        _ => unreachable!(),
                    },

                    _ => unreachable!(),
                }))
            }
            Expr::Unary(op, rhs) => {
                let rhs = build_mir_expr(var_id_map, rhs.map(|r| *r));

                #[allow(clippy::match_wildcard_for_single_variants)]
                Expression::Operation(Box::new(match &rhs.0.ty {
                    Type::Integer => match op.0 {
                        UnaryOp::Negate => Operation::IntegerNegate(rhs),

                        _ => unreachable!(),
                    },
                    Type::Float => match op.0 {
                        UnaryOp::Negate => Operation::FloatNegate(rhs),

                        _ => unreachable!(),
                    },
                    Type::Boolean => match op.0 {
                        UnaryOp::Not => Operation::BooleanNot(rhs),

                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                }))
            }
        },
        ty: expr.ty.into(),
    })
}
