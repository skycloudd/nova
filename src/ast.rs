use crate::span::Spanned;

macro_rules! ast_statement {
    ($name:ident, $expr:ident) => {
        #[derive(Debug)]
        pub enum $name<'src, 'file> {
            Expr(Spanned<'file, $expr<'src, 'file>>),
            BuiltinPrint(Spanned<'file, $expr<'src, 'file>>),
            Loop(Spanned<'file, Vec<Spanned<'file, Self>>>),
            If {
                condition: Spanned<'file, $expr<'src, 'file>>,
                then_branch: Spanned<'file, Vec<Spanned<'file, Self>>>,
                else_branch: Option<Spanned<'file, Vec<Spanned<'file, Self>>>>,
            },
            Let {
                name: Spanned<'file, &'src str>,
                value: Spanned<'file, $expr<'src, 'file>>,
            },
            Const {
                name: Spanned<'file, &'src str>,
                value: Spanned<'file, $expr<'src, 'file>>,
            },
            Assign {
                name: Spanned<'file, &'src str>,
                value: Spanned<'file, $expr<'src, 'file>>,
            },
            Break,
            Continue,
        }
    };
}

ast_statement!(Statement, Expr);

macro_rules! ast_expr {
    ($name:ident, $self_expr:ident) => {
        #[derive(Debug)]
        pub enum $name<'src, 'file> {
            Variable(&'src str),
            Boolean(bool),
            Integer(i32),
            Float(f32),
            Colour {
                r: u8,
                g: u8,
                b: u8,
            },
            Vector {
                x: Spanned<'file, Box<$self_expr<'src, 'file>>>,
                y: Spanned<'file, Box<$self_expr<'src, 'file>>>,
            },
            Binary(
                Spanned<'file, Box<$self_expr<'src, 'file>>>,
                Spanned<'file, BinaryOp>,
                Spanned<'file, Box<$self_expr<'src, 'file>>>,
            ),
            Unary(
                Spanned<'file, UnaryOp>,
                Spanned<'file, Box<$self_expr<'src, 'file>>>,
            ),
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
            Self::Equals => write!(f, "=="),
            Self::NotEquals => write!(f, "!="),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Multiply => write!(f, "*"),
            Self::Divide => write!(f, "/"),
            Self::GreaterThanEquals => write!(f, ">="),
            Self::LessThanEquals => write!(f, "<="),
            Self::GreaterThan => write!(f, ">"),
            Self::LessThan => write!(f, "<"),
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
            Self::Negate => write!(f, "-"),
            Self::Not => write!(f, "!"),
        }
    }
}

pub mod typed {
    use super::{BinaryOp, UnaryOp};
    use crate::span::Spanned;

    ast_statement!(TypedStatement, TypedExpr);

    #[allow(clippy::module_name_repetitions)]
    #[derive(Debug)]
    pub struct TypedExpr<'src, 'file> {
        pub expr: Expr<'src, 'file>,
        pub ty: Type,
    }

    ast_expr!(Expr, TypedExpr);

    #[derive(Clone, Copy, Debug)]
    pub enum Type {
        Boolean,
        Integer,
        Float,
        Colour,
        Vector,
    }

    impl std::fmt::Display for Type {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Boolean => write!(f, "boolean"),
                Self::Integer => write!(f, "integer"),
                Self::Float => write!(f, "float"),
                Self::Colour => write!(f, "colour"),
                Self::Vector => write!(f, "vector"),
            }
        }
    }
}
