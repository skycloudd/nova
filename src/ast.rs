use crate::span::Spanned;

macro_rules! top_level {
    ($name:ident, $func:ident) => {
        #[derive(Debug)]
        pub enum $name<'src> {
            Function(Spanned<$func<'src>>),
        }
    };
}

macro_rules! ast_function {
    ($name:ident, $stmt:ident) => {
        #[derive(Debug)]
        pub struct $name<'src> {
            pub name: Spanned<&'src str>,
            pub params: Spanned<Vec<(Spanned<&'src str>, Spanned<Type>)>>,
            pub return_ty: Spanned<Type>,
            pub body: Spanned<Vec<Spanned<$stmt<'src>>>>,
        }
    };
}

macro_rules! ast_statement {
    ($name:ident, $expr:ident) => {
        #[derive(Clone, Debug)]
        pub enum $name<'src> {
            #[allow(dead_code)]
            Error,
            Expr(Spanned<$expr<'src>>),
            Block(Spanned<Vec<Spanned<Self>>>),
            Loop(Spanned<Vec<Spanned<Self>>>),
            If {
                condition: Spanned<$expr<'src>>,
                then_branch: Spanned<Vec<Spanned<Self>>>,
                else_branch: Option<Spanned<Vec<Spanned<Self>>>>,
            },
            For {
                name: Spanned<&'src str>,
                start: Spanned<$expr<'src>>,
                end: Spanned<$expr<'src>>,
                inclusive: bool,
                body: Spanned<Vec<Spanned<Self>>>,
            },
            Let {
                name: Spanned<&'src str>,
                value: Spanned<$expr<'src>>,
            },
            Assign {
                name: Spanned<&'src str>,
                value: Spanned<$expr<'src>>,
            },
            Break,
            Continue,
            Return(Spanned<$expr<'src>>),
        }
    };
}

macro_rules! ast_expr {
    ($name:ident, $self_expr:ident) => {
        #[derive(Clone, Debug)]
        pub enum $name<'src> {
            Error,
            Variable(Spanned<&'src str>),
            Boolean(bool),
            Integer(crate::IntTy),
            Float(crate::FloatTy),
            String(String),
            Unary(Spanned<UnaryOp>, Spanned<Box<$self_expr<'src>>>),
            Binary(
                Spanned<Box<$self_expr<'src>>>,
                Spanned<BinaryOp>,
                Spanned<Box<$self_expr<'src>>>,
            ),
            Convert {
                ty: Spanned<Type>,
                expr: Spanned<Box<$self_expr<'src>>>,
            },
            Call {
                func: Spanned<&'src str>,
                args: Spanned<Vec<Spanned<$self_expr<'src>>>>,
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
    pub struct TypedExpr<'src> {
        pub expr: Expr<'src>,
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
