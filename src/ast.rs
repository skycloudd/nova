use crate::span::Spanned;

macro_rules! top_level {
    ($name:ident, $proc:ident) => {
        #[derive(Debug)]
        pub enum $name<'src, 'file> {
            #[allow(dead_code)]
            Error,
            Procedure($proc<'src, 'file>),
            Run(Spanned<'file, &'src str>),
        }
    };
}

macro_rules! ast_procedure {
    ($name:ident, $stmt:ident) => {
        #[derive(Debug)]
        pub struct $name<'src, 'file> {
            pub name: Spanned<'file, &'src str>,
            pub args: Spanned<'file, Vec<(Spanned<'file, &'src str>, Spanned<'file, Type<'file>>)>>,
            pub body: Spanned<'file, Vec<Spanned<'file, $stmt<'src, 'file>>>>,
        }
    };
}

macro_rules! ast_statement {
    ($name:ident, $expr:ident) => {
        #[derive(Debug)]
        pub enum $name<'src, 'file> {
            #[allow(dead_code)]
            Error,
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
                name: Spanned<'file, Action>,
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
            Error,
            Variable(Spanned<'file, &'src str>),
            Boolean(bool),
            Integer(crate::IntTy),
            Float(crate::FloatTy),
            String(String),
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
            Convert {
                ty: Spanned<'file, Type<'file>>,
                expr: Spanned<'file, Box<$self_expr<'src, 'file>>>,
            },
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
    Addr,
}

impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Negate => write!(f, "-"),
            Self::Not => write!(f, "!"),
            Self::Addr => write!(f, "&"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type<'file> {
    Error,
    Pointer(Spanned<'file, Box<Type<'file>>>),
    Boolean,
    Integer,
    Float,
    String,
    Colour,
    Vector,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Action {
    Error,
    Wait,
    WaitFrames,
    Print,
}

pub mod typed {
    use super::{Action, BinaryOp, Type, UnaryOp};
    use crate::span::Spanned;

    top_level!(TypedTopLevel, TypedProcedure);

    ast_procedure!(TypedProcedure, TypedStatement);

    ast_statement!(TypedStatement, TypedExpr);

    ast_expr!(Expr, TypedExpr);

    #[allow(clippy::module_name_repetitions)]
    #[derive(Debug)]
    pub struct TypedExpr<'src, 'file> {
        pub expr: Expr<'src, 'file>,
        pub ty: Type<'file>,
    }

    impl std::fmt::Display for Type<'_> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Error => write!(f, "error"),
                Self::Pointer(ty) => write!(f, "ptr<{}>", ty.0),
                Self::Boolean => write!(f, "bool"),
                Self::Integer => write!(f, "int"),
                Self::Float => write!(f, "float"),
                Self::String => write!(f, "str"),
                Self::Colour => write!(f, "colour"),
                Self::Vector => write!(f, "vector"),
            }
        }
    }
}
