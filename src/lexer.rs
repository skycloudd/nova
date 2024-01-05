use crate::{Span, Spanned};
use chumsky::prelude::*;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Token<'src> {
    Variable(&'src str),
    Boolean(bool),
    Integer(i32),
    Float(f32),
    Null,
    HexCode(u8, u8, u8),
    Kw(Kw),
    Ctrl(Ctrl),
    Op(Op),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Kw {
    BuiltinPrint,
    End,
    Loop,
    If,
    Else,
    Then,
    Let,
    Const,
    Break,
    Continue,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Ctrl {
    LeftParen,
    RightParen,
    LeftCurly,
    RightCurly,
    SemiColon,
    Comma,
    Pipe,
    Equals,
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
            Token::Float(n) => write!(f, "{}", n),
            Token::Null => write!(f, "null"),
            Token::Kw(k) => write!(f, "{}", k),
            Token::Ctrl(c) => write!(f, "{}", c),
            Token::Op(o) => write!(f, "{}", o),
            Token::HexCode(r, g, b) => write!(f, "#{:02x}{:02x}{:02x}", r, g, b),
        }
    }
}

impl std::fmt::Display for Kw {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Kw::BuiltinPrint => write!(f, "print"),
            Kw::End => write!(f, "end"),
            Kw::Loop => write!(f, "loop"),
            Kw::If => write!(f, "if"),
            Kw::Else => write!(f, "else"),
            Kw::Then => write!(f, "then"),
            Kw::Let => write!(f, "let"),
            Kw::Const => write!(f, "const"),
            Kw::Break => write!(f, "break"),
            Kw::Continue => write!(f, "continue"),
        }
    }
}

impl std::fmt::Display for Ctrl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Ctrl::LeftParen => write!(f, "("),
            Ctrl::RightParen => write!(f, ")"),
            Ctrl::LeftCurly => write!(f, "{{"),
            Ctrl::RightCurly => write!(f, "}}"),
            Ctrl::SemiColon => write!(f, ";"),
            Ctrl::Comma => write!(f, ","),
            Ctrl::Pipe => write!(f, "|"),
            Ctrl::Equals => write!(f, "="),
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

    let bool = choice((
        text::keyword("true").to(Token::Boolean(true)),
        text::keyword("false").to(Token::Boolean(false)),
    ));

    let integer = text::int(10)
        .validate(|n: &str, e, emitter| match n.parse() {
            Ok(n) => n,
            Err(err) => {
                emitter.emit(Rich::custom(e.span(), err));
                0
            }
        })
        .map(Token::Integer);

    let float = text::int(10)
        .then_ignore(just('.'))
        .then(text::digits(10))
        .to_slice()
        .validate(|n: &str, e, emitter| match n.parse::<f32>() {
            Ok(n) => n,
            Err(err) => {
                emitter.emit(Rich::custom(e.span(), err));
                0.0
            }
        })
        .map(Token::Float);

    let hex_byte = choice((
        just('0'),
        just('1'),
        just('2'),
        just('3'),
        just('4'),
        just('5'),
        just('6'),
        just('7'),
        just('8'),
        just('9'),
        just('a'),
        just('b'),
        just('c'),
        just('d'),
        just('e'),
        just('f'),
        just('A'),
        just('B'),
        just('C'),
        just('D'),
        just('E'),
        just('F'),
    ))
    .repeated()
    .exactly(2)
    .collect::<String>();

    let colour = just('#')
        .ignore_then(hex_byte.repeated().exactly(3).collect::<Vec<String>>())
        .map(|bytes| {
            let r = u8::from_str_radix(&bytes[0], 16).unwrap();
            let g = u8::from_str_radix(&bytes[1], 16).unwrap();
            let b = u8::from_str_radix(&bytes[2], 16).unwrap();

            Token::HexCode(r, g, b)
        });

    let keyword = choice((
        text::keyword("null").to(Token::Null),
        text::keyword("builtin_print__").to(Token::Kw(Kw::BuiltinPrint)),
        text::keyword("end").to(Token::Kw(Kw::End)),
        text::keyword("loop").to(Token::Kw(Kw::Loop)),
        text::keyword("if").to(Token::Kw(Kw::If)),
        text::keyword("else").to(Token::Kw(Kw::Else)),
        text::keyword("then").to(Token::Kw(Kw::Then)),
        text::keyword("let").to(Token::Kw(Kw::Let)),
        text::keyword("const").to(Token::Kw(Kw::Const)),
        text::keyword("break").to(Token::Kw(Kw::Break)),
        text::keyword("continue").to(Token::Kw(Kw::Continue)),
    ));

    let ctrl = choice((
        just('(').to(Token::Ctrl(Ctrl::LeftParen)),
        just(')').to(Token::Ctrl(Ctrl::RightParen)),
        just('{').to(Token::Ctrl(Ctrl::LeftCurly)),
        just('}').to(Token::Ctrl(Ctrl::RightCurly)),
        just(';').to(Token::Ctrl(Ctrl::SemiColon)),
        just(',').to(Token::Ctrl(Ctrl::Comma)),
        just('|').to(Token::Ctrl(Ctrl::Pipe)),
        just('=').to(Token::Ctrl(Ctrl::Equals)),
    ));

    let operator = choice((
        just("==").to(Token::Op(Op::Equals)),
        just("!=").to(Token::Op(Op::NotEquals)),
        just(">=").to(Token::Op(Op::GreaterThanEquals)),
        just("<=").to(Token::Op(Op::LessThanEquals)),
        just('+').to(Token::Op(Op::Plus)),
        just('-').to(Token::Op(Op::Minus)),
        just('*').to(Token::Op(Op::Multiply)),
        just('/').to(Token::Op(Op::Divide)),
        just('>').to(Token::Op(Op::GreaterThan)),
        just('<').to(Token::Op(Op::LessThan)),
        just('!').to(Token::Op(Op::Not)),
    ));

    let token = choice((
        keyword, bool, variable, float, integer, colour, operator, ctrl,
    ));

    token
        .map_with(|tok, e| (tok, e.span()))
        .padded()
        .repeated()
        .collect()
        .then_ignore(end())
}
