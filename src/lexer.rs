use crate::{Span, Spanned};
use chumsky::prelude::*;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Token<'src> {
    Variable(&'src str),
    Boolean(bool),
    Integer(i32),
    Null,
    Kw(Kw),
    Ctrl(Ctrl),
    Op(Op),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Kw {
    Print,
    Func,
    End,
    Return,
    Loop,
    Break,
    Continue,
    If,
    Else,
    Then,
    Let,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Ctrl {
    LeftParen,
    RightParen,
    SemiColon,
    Comma,
    Pipe,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Op {
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

    Not,
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Variable(v) => write!(f, "{}", v),
            Token::Boolean(b) => write!(f, "{}", b),
            Token::Integer(n) => write!(f, "{}", n),
            Token::Null => write!(f, "null"),
            Token::Kw(k) => write!(f, "{}", k),
            Token::Ctrl(c) => write!(f, "{}", c),
            Token::Op(o) => write!(f, "{}", o),
        }
    }
}

impl std::fmt::Display for Kw {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Kw::Print => write!(f, "print"),
            Kw::Func => write!(f, "func"),
            Kw::End => write!(f, "end"),
            Kw::Return => write!(f, "return"),
            Kw::Loop => write!(f, "loop"),
            Kw::Break => write!(f, "break"),
            Kw::Continue => write!(f, "continue"),
            Kw::If => write!(f, "if"),
            Kw::Else => write!(f, "else"),
            Kw::Then => write!(f, "then"),
            Kw::Let => write!(f, "let"),
        }
    }
}

impl std::fmt::Display for Ctrl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ctrl::LeftParen => write!(f, "("),
            Ctrl::RightParen => write!(f, ")"),
            Ctrl::SemiColon => write!(f, ";"),
            Ctrl::Comma => write!(f, ","),
            Ctrl::Pipe => write!(f, "|"),
        }
    }
}

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Equals => write!(f, "=="),
            Op::NotEquals => write!(f, "!="),

            Op::Plus => write!(f, "+"),
            Op::Minus => write!(f, "-"),
            Op::Multiply => write!(f, "*"),
            Op::Divide => write!(f, "/"),

            Op::GreaterThanEquals => write!(f, ">="),
            Op::LessThanEquals => write!(f, "<="),
            Op::GreaterThan => write!(f, ">"),
            Op::LessThan => write!(f, "<"),

            Op::Not => write!(f, "!"),
        }
    }
}

pub fn lexer<'src>(
) -> impl Parser<'src, &'src str, Vec<Spanned<Token<'src>>>, extra::Err<Rich<'src, char, Span>>> {
    let variable = text::ident().map(Token::Variable);

    let integer = text::int(10)
        .validate(|n: &str, e, emitter| match n.parse() {
            Ok(n) => n,
            Err(err) => {
                emitter.emit(Rich::custom(e.span(), err));
                0
            }
        })
        .map(Token::Integer);

    let keyword = choice((
        text::keyword("true").to(Token::Boolean(true)),
        text::keyword("false").to(Token::Boolean(false)),
        text::keyword("null").to(Token::Null),
        text::keyword("print").to(Token::Kw(Kw::Print)),
        text::keyword("func").to(Token::Kw(Kw::Func)),
        text::keyword("end").to(Token::Kw(Kw::End)),
        text::keyword("return").to(Token::Kw(Kw::Return)),
        text::keyword("loop").to(Token::Kw(Kw::Loop)),
        text::keyword("break").to(Token::Kw(Kw::Break)),
        text::keyword("continue").to(Token::Kw(Kw::Continue)),
        text::keyword("if").to(Token::Kw(Kw::If)),
        text::keyword("else").to(Token::Kw(Kw::Else)),
        text::keyword("then").to(Token::Kw(Kw::Then)),
        text::keyword("let").to(Token::Kw(Kw::Let)),
    ));

    let ctrl = choice((
        just('(').to(Token::Ctrl(Ctrl::LeftParen)),
        just(')').to(Token::Ctrl(Ctrl::RightParen)),
        just(';').to(Token::Ctrl(Ctrl::SemiColon)),
        just(',').to(Token::Ctrl(Ctrl::Comma)),
        just('|').to(Token::Ctrl(Ctrl::Pipe)),
    ));

    let operator = choice((
        just("==").to(Token::Op(Op::Equals)),
        just("!=").to(Token::Op(Op::NotEquals)),
        just('+').to(Token::Op(Op::Plus)),
        just('-').to(Token::Op(Op::Minus)),
        just('*').to(Token::Op(Op::Multiply)),
        just('/').to(Token::Op(Op::Divide)),
        just(">=").to(Token::Op(Op::GreaterThanEquals)),
        just("<=").to(Token::Op(Op::LessThanEquals)),
        just('>').to(Token::Op(Op::GreaterThan)),
        just('<').to(Token::Op(Op::LessThan)),
        just('!').to(Token::Op(Op::Not)),
    ));

    let token = choice((keyword, variable, integer, ctrl, operator));

    token
        .map_with(|tok, e| (tok, e.span()))
        .padded()
        .repeated()
        .collect()
        .then_ignore(end())
}
