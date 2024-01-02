use crate::Spanned;

macro_rules! ast_statement {
    ($name:ident, $expr:ident) => {
        #[derive(Debug)]
        pub enum $name<'src> {
            Expr(Spanned<$expr<'src>>),
            BuiltinPrint(Spanned<$expr<'src>>),
            Loop(Spanned<Vec<Spanned<Self>>>),
            If {
                condition: Spanned<$expr<'src>>,
                then_branch: Spanned<Vec<Spanned<Self>>>,
                else_branch: Option<Spanned<Vec<Spanned<Self>>>>,
            },
            Let {
                name: Spanned<&'src str>,
                value: Spanned<$expr<'src>>,
            },
            Const {
                name: Spanned<&'src str>,
                value: Spanned<$expr<'src>>,
            },
        }
    };
}

ast_statement!(Statement, Expr);

macro_rules! ast_expr {
    ($name:ident, $self_expr:ident) => {
        #[derive(Debug)]
        pub enum $name<'src> {
            Variable(&'src str),
            Boolean(bool),
            Integer(i32),
            Null,
            Binary(
                Box<Spanned<$self_expr<'src>>>,
                Spanned<BinaryOp>,
                Box<Spanned<$self_expr<'src>>>,
            ),
            Unary(Spanned<UnaryOp>, Box<Spanned<$self_expr<'src>>>),
        }
    };
}

ast_expr!(Expr, Expr);

#[derive(Clone, Copy, Debug)]
pub enum BinaryOp {
    Equals,
    NotEquals,
    Plus,
    Minus,
    Multiply,
    Divide,
    GreaterThanEquals,
    LessThanEquals,
    GreaterThan,
    LessThan,
}

impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOp::Equals => write!(f, "=="),
            BinaryOp::NotEquals => write!(f, "!="),
            BinaryOp::Plus => write!(f, "+"),
            BinaryOp::Minus => write!(f, "-"),
            BinaryOp::Multiply => write!(f, "*"),
            BinaryOp::Divide => write!(f, "/"),
            BinaryOp::GreaterThanEquals => write!(f, ">="),
            BinaryOp::LessThanEquals => write!(f, "<="),
            BinaryOp::GreaterThan => write!(f, ">"),
            BinaryOp::LessThan => write!(f, "<"),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum UnaryOp {
    Negate,
    Not,
}

impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Negate => write!(f, "-"),
            UnaryOp::Not => write!(f, "!"),
        }
    }
}

pub mod typed {
    use crate::ast::{BinaryOp, UnaryOp};
    use crate::Spanned;

    ast_statement!(TypedStatement, TypedExpr);

    #[derive(Debug)]
    pub struct TypedExpr<'src> {
        pub expr: Expr<'src>,
        pub ty: Type,
    }

    ast_expr!(Expr, TypedExpr);

    #[derive(Clone, Copy, Debug)]
    pub enum Type {
        Boolean,
        Integer,
        Null,
    }
}
