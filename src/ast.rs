use crate::Spanned;

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
                x: Box<Spanned<'file, $self_expr<'src, 'file>>>,
                y: Box<Spanned<'file, $self_expr<'src, 'file>>>,
            },
            Binary(
                Box<Spanned<'file, $self_expr<'src, 'file>>>,
                Spanned<'file, BinaryOp>,
                Box<Spanned<'file, $self_expr<'src, 'file>>>,
            ),
            Unary(
                Spanned<'file, UnaryOp>,
                Box<Spanned<'file, $self_expr<'src, 'file>>>,
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
                Type::Boolean => write!(f, "boolean"),
                Type::Integer => write!(f, "integer"),
                Type::Float => write!(f, "float"),
                Type::Colour => write!(f, "colour"),
                Type::Vector => write!(f, "vector"),
            }
        }
    }
}
