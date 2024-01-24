use crate::{FloatTy, IntTy, Span};
use chumsky::{input::WithContext, prelude::*};
use std::num::ParseIntError;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Token<'src> {
    Error,
    Variable(&'src str),
    Boolean(bool),
    Integer(IntTy),
    Float(FloatTy),
    HexCode(u8, u8, u8, Option<u8>),
    Kw(Kw),
    Ctrl(Ctrl),
    Op(Op),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kw {
    Print,
    End,
    Loop,
    If,
    Else,
    Then,
    Let,
    Const,
    Break,
    Continue,
    For,
    Do,
    In,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Ctrl {
    LeftParen,
    RightParen,
    LeftCurly,
    RightCurly,
    SemiColon,
    Comma,
    Equals,
    Range,
    RangeInclusive,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
    #[allow(clippy::many_single_char_names)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Error => write!(f, "error"),
            Token::Variable(v) => write!(f, "{v}"),
            Token::Boolean(b) => write!(f, "{b}"),
            Token::Integer(n) => write!(f, "{n}"),
            Token::Float(n) => write!(f, "{n}"),
            Token::Kw(k) => write!(f, "{k}"),
            Token::Ctrl(c) => write!(f, "{c}"),
            Token::Op(o) => write!(f, "{o}"),
            Token::HexCode(r, g, b, a) => write!(
                f,
                "#{r:02x}{g:02x}{b:02x}{}",
                if let Some(a) = a {
                    format!("{a:02x}")
                } else {
                    String::new()
                },
            ),
        }
    }
}

impl std::fmt::Display for Kw {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Print => write!(f, "print"),
            Self::End => write!(f, "end"),
            Self::Loop => write!(f, "loop"),
            Self::If => write!(f, "if"),
            Self::Else => write!(f, "else"),
            Self::Then => write!(f, "then"),
            Self::Let => write!(f, "let"),
            Self::Const => write!(f, "const"),
            Self::Break => write!(f, "break"),
            Self::Continue => write!(f, "continue"),
            Self::For => write!(f, "for"),
            Self::Do => write!(f, "do"),
            Self::In => write!(f, "in"),
        }
    }
}

impl std::fmt::Display for Ctrl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LeftParen => write!(f, "("),
            Self::RightParen => write!(f, ")"),
            Self::LeftCurly => write!(f, "{{"),
            Self::RightCurly => write!(f, "}}"),
            Self::SemiColon => write!(f, ";"),
            Self::Comma => write!(f, ","),
            Self::Equals => write!(f, "="),
            Self::Range => write!(f, ".."),
            Self::RangeInclusive => write!(f, "..="),
        }
    }
}

impl std::fmt::Display for Op {
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

            Self::Not => write!(f, "!"),
        }
    }
}

type LexerInput<'tokens, 'src, 'file> = WithContext<Span<'file>, &'src str>;

type LexerOutput<'tokens, 'src, 'file> = Vec<(Token<'src>, Span<'file>)>;

type LexerError<'tokens, 'src, 'file> = extra::Err<Rich<'src, char, Span<'file>>>;

pub fn lexer<'src, 'file: 'src>() -> impl Parser<
    'src,
    LexerInput<'src, 'src, 'file>,
    LexerOutput<'src, 'src, 'file>,
    LexerError<'src, 'src, 'file>,
> {
    let variable = text::ident().map(Token::Variable);

    let bool = choice((
        text::keyword("true").to(Token::Boolean(true)),
        text::keyword("false").to(Token::Boolean(false)),
    ))
    .boxed();

    let integer = text::int(10)
        .validate(|n: &str, e, emitter| match n.parse() {
            Ok(n) => n,
            Err(err) => {
                emitter.emit(Rich::custom(e.span(), err));
                0
            }
        })
        .map(Token::Integer)
        .boxed();

    let float = text::int(10)
        .then_ignore(just('.'))
        .then(text::digits(10))
        .to_slice()
        .validate(|n: &str, e, emitter| match n.parse() {
            Ok(n) => n,
            Err(err) => {
                emitter.emit(Rich::custom(e.span(), err));
                0.0
            }
        })
        .map(Token::Float)
        .boxed();

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
    .collect::<String>()
    .boxed();

    let colour = just('#')
        // .ignore_then(hex_byte.repeated().exactly(3).collect::<Vec<String>>())
        .ignore_then(hex_byte.clone())
        .then(hex_byte.clone())
        .then(hex_byte.clone())
        .then(hex_byte.or_not())
        .map(|(((r, g), b), a)| {
            let r = u8::from_str_radix(&r, 16).unwrap();
            let g = u8::from_str_radix(&g, 16).unwrap();
            let b = u8::from_str_radix(&b, 16).unwrap();
            let a = a.map(|a| u8::from_str_radix(&a, 16).unwrap());

            Ok(Token::HexCode(r, g, b, a))
        })
        .validate(|res: Result<_, ParseIntError>, e, emitter| match res {
            Ok(token) => token,
            Err(err) => {
                emitter.emit(Rich::custom(
                    e.span(),
                    format!("ICE: parsed invalid hex code: {err}"),
                ));

                Token::Error
            }
        })
        .boxed();

    let keyword = choice((
        text::keyword("print").to(Token::Kw(Kw::Print)),
        text::keyword("end").to(Token::Kw(Kw::End)),
        text::keyword("loop").to(Token::Kw(Kw::Loop)),
        text::keyword("if").to(Token::Kw(Kw::If)),
        text::keyword("else").to(Token::Kw(Kw::Else)),
        text::keyword("then").to(Token::Kw(Kw::Then)),
        text::keyword("let").to(Token::Kw(Kw::Let)),
        text::keyword("const").to(Token::Kw(Kw::Const)),
        text::keyword("break").to(Token::Kw(Kw::Break)),
        text::keyword("continue").to(Token::Kw(Kw::Continue)),
        text::keyword("for").to(Token::Kw(Kw::For)),
        text::keyword("do").to(Token::Kw(Kw::Do)),
        text::keyword("in").to(Token::Kw(Kw::In)),
    ))
    .boxed();

    let ctrl = choice((
        just("..=").to(Token::Ctrl(Ctrl::RangeInclusive)),
        just("..").to(Token::Ctrl(Ctrl::Range)),
        just('(').to(Token::Ctrl(Ctrl::LeftParen)),
        just(')').to(Token::Ctrl(Ctrl::RightParen)),
        just('{').to(Token::Ctrl(Ctrl::LeftCurly)),
        just('}').to(Token::Ctrl(Ctrl::RightCurly)),
        just(';').to(Token::Ctrl(Ctrl::SemiColon)),
        just(',').to(Token::Ctrl(Ctrl::Comma)),
        just('=').to(Token::Ctrl(Ctrl::Equals)),
    ))
    .boxed();

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
    ))
    .boxed();

    let token = choice((
        keyword, bool, variable, float, integer, colour, operator, ctrl,
    ))
    .boxed();

    let comment = just("//")
        .then(any().and_is(just('\n').not()).repeated())
        .padded()
        .boxed();

    token
        .map_with(|tok, e| (tok, e.span()))
        .padded_by(comment.repeated())
        .padded()
        .repeated()
        .collect()
        .then_ignore(end())
        .boxed()
}
