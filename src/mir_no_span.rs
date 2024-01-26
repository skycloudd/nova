use crate::{mir, span::Spanned, FloatTy, IntTy};

#[derive(Debug)]
pub enum TypedStatement {
    Expr(TypedExpression),
    Print(TypedExpression),
    Loop(Vec<TypedStatement>),
    Block(Vec<TypedStatement>),
    If {
        condition: TypedExpression,
        then_branch: Vec<TypedStatement>,
        else_branch: Option<Vec<TypedStatement>>,
    },
    Let {
        name: VarId,
        value: TypedExpression,
    },
    Const {
        name: VarId,
        value: TypedExpression,
    },
    Assign {
        name: VarId,
        value: TypedExpression,
    },
    Break,
    Continue,
    Action {
        name: Action,
        args: Vec<TypedExpression>,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Action {
    Wait,
    WaitFrames,
    Move,
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
    Object(Object),
    Operation(Box<Operation>),
}

#[derive(Debug)]
pub enum Object {
    Player,
}

pub type VarId = usize;

#[derive(Debug)]
pub enum Operation {
    IntegerEquals(TypedExpression, TypedExpression),
    IntegerNotEquals(TypedExpression, TypedExpression),
    IntegerPlus(TypedExpression, TypedExpression),
    IntegerMinus(TypedExpression, TypedExpression),
    IntegerMultiply(TypedExpression, TypedExpression),
    IntegerDivide(TypedExpression, TypedExpression),
    IntegerGreaterThanEquals(TypedExpression, TypedExpression),
    IntegerLessThanEquals(TypedExpression, TypedExpression),
    IntegerGreaterThan(TypedExpression, TypedExpression),
    IntegerLessThan(TypedExpression, TypedExpression),

    FloatEquals(TypedExpression, TypedExpression),
    FloatNotEquals(TypedExpression, TypedExpression),
    FloatPlus(TypedExpression, TypedExpression),
    FloatMinus(TypedExpression, TypedExpression),
    FloatMultiply(TypedExpression, TypedExpression),
    FloatDivide(TypedExpression, TypedExpression),
    FloatGreaterThanEquals(TypedExpression, TypedExpression),
    FloatLessThanEquals(TypedExpression, TypedExpression),
    FloatGreaterThan(TypedExpression, TypedExpression),
    FloatLessThan(TypedExpression, TypedExpression),

    BooleanEquals(TypedExpression, TypedExpression),
    BooleanNotEquals(TypedExpression, TypedExpression),

    IntegerNegate(TypedExpression),
    FloatNegate(TypedExpression),
    BooleanNot(TypedExpression),
}

#[derive(Debug)]
pub enum Type {
    Boolean,
    Integer,
    Float,
    Colour,
    Vector,
    Object,
    ObjectSet,
}

impl From<mir::Type> for Type {
    fn from(ty: mir::Type) -> Self {
        match ty {
            mir::Type::Boolean => Self::Boolean,
            mir::Type::Integer => Self::Integer,
            mir::Type::Float => Self::Float,
            mir::Type::Colour => Self::Colour,
            mir::Type::Vector => Self::Vector,
            mir::Type::Object => Self::Object,
            mir::Type::ObjectSet => Self::ObjectSet,
        }
    }
}

impl std::fmt::Display for Action {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Wait => write!(f, "wait"),
            Self::WaitFrames => write!(f, "waitframes"),
            Self::Move => write!(f, "move"),
        }
    }
}

pub fn mir_remove_span(mir: Vec<Spanned<mir::TypedStatement<'_>>>) -> Vec<TypedStatement> {
    mir.into_iter().map(|s| statement_remove_span(s)).collect()
}

fn statement_remove_span(statement: Spanned<mir::TypedStatement<'_>>) -> TypedStatement {
    match statement.0 {
        mir::TypedStatement::Expr(expr) => TypedStatement::Expr(expression_remove_span(expr)),
        mir::TypedStatement::Print(expr) => TypedStatement::Print(expression_remove_span(expr)),
        mir::TypedStatement::Loop(statements) => TypedStatement::Loop(
            statements
                .0
                .into_iter()
                .map(|s| statement_remove_span(s))
                .collect(),
        ),
        mir::TypedStatement::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let condition = expression_remove_span(condition);

            if let Expression::Boolean(value) = condition.expr {
                TypedStatement::Block(
                    if value {
                        then_branch.0
                    } else {
                        else_branch.map_or(vec![], |statements| statements.0)
                    }
                    .into_iter()
                    .map(|s| statement_remove_span(s))
                    .collect(),
                )
            } else {
                let then_branch = then_branch
                    .0
                    .into_iter()
                    .map(|s| statement_remove_span(s))
                    .collect();

                let else_branch = else_branch.map(|statements| {
                    statements
                        .0
                        .into_iter()
                        .map(|s| statement_remove_span(s))
                        .collect()
                });

                TypedStatement::If {
                    condition,
                    then_branch,
                    else_branch,
                }
            }
        }
        mir::TypedStatement::For {
            name,
            start,
            end,
            inclusive,
            body,
        } => {
            let start = expression_remove_span(start);
            let end = expression_remove_span(end);

            let body = body
                .0
                .into_iter()
                .map(|s| statement_remove_span(s))
                .chain(std::iter::once(TypedStatement::Assign {
                    name: name.0,
                    value: TypedExpression {
                        expr: Expression::Operation(Box::new(Operation::IntegerPlus(
                            TypedExpression {
                                expr: Expression::Variable(name.0),
                                ty: Type::Integer,
                            },
                            TypedExpression {
                                expr: Expression::Integer(1),
                                ty: Type::Integer,
                            },
                        ))),
                        ty: Type::Integer,
                    },
                }))
                .collect();

            let condition = TypedExpression {
                expr: Expression::Operation(Box::new(if inclusive {
                    Operation::IntegerLessThanEquals(
                        TypedExpression {
                            expr: Expression::Variable(name.0),
                            ty: Type::Integer,
                        },
                        end,
                    )
                } else {
                    Operation::IntegerLessThan(
                        TypedExpression {
                            expr: Expression::Variable(name.0),
                            ty: Type::Integer,
                        },
                        end,
                    )
                })),
                ty: Type::Boolean,
            };

            TypedStatement::Block(vec![
                TypedStatement::Let {
                    name: name.0,
                    value: start,
                },
                TypedStatement::Loop(vec![TypedStatement::If {
                    condition,
                    then_branch: body,
                    else_branch: Some(vec![TypedStatement::Break]),
                }]),
            ])
        }
        mir::TypedStatement::Let { name, value } => TypedStatement::Let {
            name: name.0,
            value: expression_remove_span(value),
        },
        mir::TypedStatement::Const { name, value } => TypedStatement::Const {
            name: name.0,
            value: expression_remove_span(value),
        },
        mir::TypedStatement::Assign { name, value } => TypedStatement::Assign {
            name: name.0,
            value: expression_remove_span(value),
        },
        mir::TypedStatement::Break => TypedStatement::Break,
        mir::TypedStatement::Continue => TypedStatement::Continue,
        mir::TypedStatement::Action { name, args } => TypedStatement::Action {
            name: match name.0 {
                "wait" => Action::Wait,
                "waitframes" => Action::WaitFrames,
                "move" => Action::Move,
                _ => unreachable!(),
            },
            args: args
                .0
                .into_iter()
                .map(|e| expression_remove_span(e))
                .collect(),
        },
    }
}

fn expression_remove_span(expression: Spanned<mir::TypedExpression<'_>>) -> TypedExpression {
    TypedExpression {
        expr: match expression.0.expr {
            mir::Expression::Variable(name) => Expression::Variable(name),
            mir::Expression::Boolean(value) => Expression::Boolean(value),
            mir::Expression::Integer(value) => Expression::Integer(value),
            mir::Expression::Float(value) => Expression::Float(value),
            mir::Expression::Colour { r, g, b, a } => Expression::Colour { r, g, b, a },
            mir::Expression::Vector { x, y } => Expression::Vector {
                x: Box::new(expression_remove_span(x.map(|x| *x))),
                y: Box::new(expression_remove_span(y.map(|y| *y))),
            },
            mir::Expression::Object(object) => Expression::Object(match object {
                mir::Object::Player => Object::Player,
            }),
            mir::Expression::Operation(operation) => {
                Expression::Operation(Box::new(operation_remove_span(*operation)))
            }
        },
        ty: expression.0.ty.into(),
    }
}

fn operation_remove_span(operation: mir::Operation<'_>) -> Operation {
    match operation {
        mir::Operation::IntegerEquals(lhs, rhs) => {
            Operation::IntegerEquals(expression_remove_span(lhs), expression_remove_span(rhs))
        }
        mir::Operation::IntegerNotEquals(lhs, rhs) => {
            Operation::IntegerNotEquals(expression_remove_span(lhs), expression_remove_span(rhs))
        }
        mir::Operation::IntegerPlus(lhs, rhs) => {
            Operation::IntegerPlus(expression_remove_span(lhs), expression_remove_span(rhs))
        }
        mir::Operation::IntegerMinus(lhs, rhs) => {
            Operation::IntegerMinus(expression_remove_span(lhs), expression_remove_span(rhs))
        }
        mir::Operation::IntegerMultiply(lhs, rhs) => {
            Operation::IntegerMultiply(expression_remove_span(lhs), expression_remove_span(rhs))
        }
        mir::Operation::IntegerDivide(lhs, rhs) => {
            Operation::IntegerDivide(expression_remove_span(lhs), expression_remove_span(rhs))
        }
        mir::Operation::IntegerGreaterThanEquals(lhs, rhs) => Operation::IntegerGreaterThanEquals(
            expression_remove_span(lhs),
            expression_remove_span(rhs),
        ),
        mir::Operation::IntegerLessThanEquals(lhs, rhs) => Operation::IntegerLessThanEquals(
            expression_remove_span(lhs),
            expression_remove_span(rhs),
        ),
        mir::Operation::IntegerGreaterThan(lhs, rhs) => {
            Operation::IntegerGreaterThan(expression_remove_span(lhs), expression_remove_span(rhs))
        }
        mir::Operation::IntegerLessThan(lhs, rhs) => {
            Operation::IntegerLessThan(expression_remove_span(lhs), expression_remove_span(rhs))
        }

        mir::Operation::FloatEquals(lhs, rhs) => {
            Operation::FloatEquals(expression_remove_span(lhs), expression_remove_span(rhs))
        }
        mir::Operation::FloatNotEquals(lhs, rhs) => {
            Operation::FloatNotEquals(expression_remove_span(lhs), expression_remove_span(rhs))
        }
        mir::Operation::FloatPlus(lhs, rhs) => {
            Operation::FloatPlus(expression_remove_span(lhs), expression_remove_span(rhs))
        }
        mir::Operation::FloatMinus(lhs, rhs) => {
            Operation::FloatMinus(expression_remove_span(lhs), expression_remove_span(rhs))
        }
        mir::Operation::FloatMultiply(lhs, rhs) => {
            Operation::FloatMultiply(expression_remove_span(lhs), expression_remove_span(rhs))
        }
        mir::Operation::FloatDivide(lhs, rhs) => {
            Operation::FloatDivide(expression_remove_span(lhs), expression_remove_span(rhs))
        }
        mir::Operation::FloatGreaterThanEquals(lhs, rhs) => Operation::FloatGreaterThanEquals(
            expression_remove_span(lhs),
            expression_remove_span(rhs),
        ),
        mir::Operation::FloatLessThanEquals(lhs, rhs) => {
            Operation::FloatLessThanEquals(expression_remove_span(lhs), expression_remove_span(rhs))
        }
        mir::Operation::FloatGreaterThan(lhs, rhs) => {
            Operation::FloatGreaterThan(expression_remove_span(lhs), expression_remove_span(rhs))
        }
        mir::Operation::FloatLessThan(lhs, rhs) => {
            Operation::FloatLessThan(expression_remove_span(lhs), expression_remove_span(rhs))
        }

        mir::Operation::BooleanEquals(lhs, rhs) => {
            Operation::BooleanEquals(expression_remove_span(lhs), expression_remove_span(rhs))
        }
        mir::Operation::BooleanNotEquals(lhs, rhs) => {
            Operation::BooleanNotEquals(expression_remove_span(lhs), expression_remove_span(rhs))
        }

        mir::Operation::IntegerNegate(value) => {
            Operation::IntegerNegate(expression_remove_span(value))
        }
        mir::Operation::FloatNegate(value) => Operation::FloatNegate(expression_remove_span(value)),
        mir::Operation::BooleanNot(value) => Operation::BooleanNot(expression_remove_span(value)),
    }
}
