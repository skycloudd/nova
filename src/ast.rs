use crate::span::Spanned;

macro_rules! top_level {
    ($name:ident, $func:ident) => {
        #[derive(Debug)]
        pub enum $name {
            Function(Spanned<$func>),
        }
    };
}

macro_rules! ast_function {
    ($name:ident, $stmt:ident) => {
        #[derive(Debug)]
        pub struct $name {
            pub name: Spanned<&'static str>,
            pub params: Spanned<Vec<(Spanned<&'static str>, Spanned<Type>)>>,
            pub return_ty: Spanned<Type>,
            pub body: Spanned<Vec<Spanned<$stmt>>>,
        }
    };
}

macro_rules! ast_statement {
    ($name:ident, $expr:ident) => {
        #[derive(Clone, Debug)]
        pub enum $name {
            #[allow(dead_code)]
            Error,
            Expr(Spanned<$expr>),
            Block(Spanned<Vec<Spanned<Self>>>),
            Loop(Spanned<Vec<Spanned<Self>>>),
            If {
                condition: Spanned<$expr>,
                then_branch: Spanned<Vec<Spanned<Self>>>,
                else_branch: Option<Spanned<Vec<Spanned<Self>>>>,
            },
            For {
                name: Spanned<&'static str>,
                start: Spanned<$expr>,
                end: Spanned<$expr>,
                inclusive: bool,
                body: Spanned<Vec<Spanned<Self>>>,
            },
            Let {
                name: Spanned<&'static str>,
                value: Spanned<$expr>,
            },
            Assign {
                name: Spanned<&'static str>,
                value: Spanned<$expr>,
            },
            Break,
            Continue,
            Return(Spanned<$expr>),
        }
    };
}

macro_rules! ast_expr {
    ($name:ident, $self_expr:ident) => {
        #[derive(Clone, Debug)]
        pub enum $name {
            Error,
            Variable(Spanned<&'static str>),
            Boolean(bool),
            Integer(crate::IntTy),
            Float(crate::FloatTy),
            String(String),
            Unary(Spanned<UnaryOp>, Spanned<Box<$self_expr>>),
            Binary(
                Spanned<Box<$self_expr>>,
                Spanned<BinaryOp>,
                Spanned<Box<$self_expr>>,
            ),
            Convert {
                ty: Spanned<Type>,
                expr: Spanned<Box<$self_expr>>,
            },
            Call {
                func: Spanned<&'static str>,
                args: Spanned<Vec<Spanned<$self_expr>>>,
            },
        }
    };
}

top_level!(TopLevel, Function);

ast_function!(Function, Statement);

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

impl core::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
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
    Ref,
    Deref,
}

impl core::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Negate => write!(f, "-"),
            Self::Not => write!(f, "!"),
            Self::Ref => write!(f, "&"),
            Self::Deref => write!(f, "*"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Error,
    Boolean,
    Integer,
    Float,
    String,
    Pointer(Spanned<Box<Type>>),
}

pub mod typed {
    use super::{BinaryOp, Type, UnaryOp};
    use crate::span::Spanned;

    top_level!(TypedTopLevel, TypedFunction);

    ast_function!(TypedFunction, TypedStatement);

    ast_statement!(TypedStatement, TypedExpr);

    ast_expr!(Expr, TypedExpr);

    #[allow(clippy::module_name_repetitions)]
    #[derive(Clone, Debug)]
    pub struct TypedExpr {
        pub expr: Expr,
        pub ty: Type,
    }

    impl core::fmt::Display for Type {
        fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
            match self {
                Self::Error => write!(f, "error"),
                Self::Boolean => write!(f, "bool"),
                Self::Integer => write!(f, "int"),
                Self::Float => write!(f, "float"),
                Self::String => write!(f, "str"),
                Self::Pointer(ty) => write!(f, "ptr<{}>", ty.0),
            }
        }
    }
}
