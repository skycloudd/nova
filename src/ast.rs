use crate::span::Spanned;

macro_rules! top_level {
    ($name:ident, $proc:ident) => {
        #[derive(Debug)]
        pub enum $name<'src, 'file> {
            Procedure($proc<'src, 'file>),
        }
    };
}

macro_rules! ast_procedure {
    ($name:ident, $stmt:ident) => {
        #[derive(Debug)]
        pub struct $name<'src, 'file> {
            pub name: Spanned<'file, &'src str>,
            pub args: Spanned<'file, Vec<(Spanned<'file, &'src str>, Spanned<'file, Type>)>>,
            pub body: Spanned<'file, Vec<Spanned<'file, $stmt<'src, 'file>>>>,
        }
    };
}

macro_rules! ast_statement {
    ($name:ident, $expr:ident) => {
        #[derive(Debug)]
        pub enum $name<'src, 'file> {
            Expr(Spanned<'file, $expr<'src, 'file>>),
            Block(Spanned<'file, Vec<Spanned<'file, Self>>>),
            Loop(Spanned<'file, Vec<Spanned<'file, Self>>>),
            If {
                condition: Spanned<'file, $expr<'src, 'file>>,
                then_branch: Spanned<'file, Vec<Spanned<'file, Self>>>,
                else_branch: Option<Spanned<'file, Vec<Spanned<'file, Self>>>>,
            },
            For {
                name: Spanned<'file, &'src str>,
                start: Spanned<'file, $expr<'src, 'file>>,
                end: Spanned<'file, $expr<'src, 'file>>,
                inclusive: bool,
                body: Spanned<'file, Vec<Spanned<'file, Self>>>,
            },
            Let {
                name: Spanned<'file, &'src str>,
                value: Spanned<'file, $expr<'src, 'file>>,
            },
            Assign {
                name: Spanned<'file, &'src str>,
                value: Spanned<'file, $expr<'src, 'file>>,
            },
            Break,
            Continue,
            Return,
            Action {
                name: Spanned<'file, &'src str>,
                args: Spanned<'file, Vec<Spanned<'file, $expr<'src, 'file>>>>,
            },
            Call {
                proc: Spanned<'file, &'src str>,
                args: Spanned<'file, Vec<Spanned<'file, $expr<'src, 'file>>>>,
            },
        }
    };
}

macro_rules! ast_expr {
    ($name:ident, $self_expr:ident) => {
        #[derive(Debug)]
        pub enum $name<'src, 'file> {
            Variable(&'src str),
            Boolean(bool),
            Integer(crate::IntTy),
            Float(crate::FloatTy),
            Colour {
                r: u8,
                g: u8,
                b: u8,
                a: u8,
            },
            Vector {
                x: Spanned<'file, Box<$self_expr<'src, 'file>>>,
                y: Spanned<'file, Box<$self_expr<'src, 'file>>>,
            },
            Unary(
                Spanned<'file, UnaryOp>,
                Spanned<'file, Box<$self_expr<'src, 'file>>>,
            ),
            Binary(
                Spanned<'file, Box<$self_expr<'src, 'file>>>,
                Spanned<'file, BinaryOp>,
                Spanned<'file, Box<$self_expr<'src, 'file>>>,
            ),
        }
    };
}

top_level!(TopLevel, Procedure);

ast_procedure!(Procedure, Statement);

ast_statement!(Statement, Expr);

ast_expr!(Expr, Expr);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Type {
    Boolean,
    Integer,
    Float,
    Colour,
    Vector,
}

pub mod typed {
    use super::{BinaryOp, Type, UnaryOp};
    use crate::span::Spanned;

    top_level!(TypedTopLevel, TypedProcedure);

    ast_procedure!(TypedProcedure, TypedStatement);

    ast_statement!(TypedStatement, TypedExpr);

    ast_expr!(Expr, TypedExpr);

    #[allow(clippy::module_name_repetitions)]
    #[derive(Debug)]
    pub struct TypedExpr<'src, 'file> {
        pub expr: Expr<'src, 'file>,
        pub ty: Type,
    }

    impl std::fmt::Display for Type {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Boolean => write!(f, "bool"),
                Self::Integer => write!(f, "int"),
                Self::Float => write!(f, "float"),
                Self::Colour => write!(f, "colour"),
                Self::Vector => write!(f, "vector"),
            }
        }
    }
}
