use crate::{FloatTy, IntTy, Span};
use chumsky::{input::WithContext, prelude::*};
use std::num::ParseIntError;

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'src> {
    Error,
    Variable(&'src str),
    Boolean(bool),
    Integer(IntTy),
    Float(FloatTy),
    String(String),
    Colour(u8, u8, u8, Option<u8>),
    Kw(Kw),
    Ctrl(Ctrl),
    Op(Op),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kw {
    End,
    Loop,
    If,
    Else,
    Then,
    Let,
    Break,
    Continue,
    For,
    Do,
    In,
    Action,
    Proc,
    Return,
    Run,
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
    Colon,
    At,
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
            Token::String(s) => write!(f, "\"{s}\""),
            Token::Colour(r, g, b, a) => write!(
                f,
                "#{r:02x}{g:02x}{b:02x}{}",
                a.as_ref().map_or_else(String::new, |a| format!("{a:02x}")),
            ),
            Token::Kw(k) => write!(f, "{k}"),
            Token::Ctrl(c) => write!(f, "{c}"),
            Token::Op(o) => write!(f, "{o}"),
        }
    }
}

impl std::fmt::Display for Kw {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::End => write!(f, "end"),
            Self::Loop => write!(f, "loop"),
            Self::If => write!(f, "if"),
            Self::Else => write!(f, "else"),
            Self::Then => write!(f, "then"),
            Self::Let => write!(f, "let"),
            Self::Break => write!(f, "break"),
            Self::Continue => write!(f, "continue"),
            Self::For => write!(f, "for"),
            Self::Do => write!(f, "do"),
            Self::In => write!(f, "in"),
            Self::Action => write!(f, "action"),
            Self::Proc => write!(f, "proc"),
            Self::Return => write!(f, "return"),
            Self::Run => write!(f, "run"),
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
            Self::Colon => write!(f, ":"),
            Self::At => write!(f, "@"),
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
    let variable = text::ascii::ident().map(Token::Variable);

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

    let escape = just('\\')
        .then(choice((
            just('\\'),
            just('/'),
            just('"'),
            just('n').to('\n'),
            just('u').ignore_then(text::digits(16).exactly(4).to_slice().validate(
                |digits, e, emitter| {
                    char::from_u32(u32::from_str_radix(digits, 16).unwrap()).unwrap_or_else(|| {
                        emitter.emit(Rich::custom(e.span(), "invalid unicode character"));

                        '\u{FFFD}'
                    })
                },
            )),
        )))
        .ignored()
        .boxed();

    let string = none_of("\\\"")
        .ignored()
        .or(escape)
        .repeated()
        .to_slice()
        .map(ToString::to_string)
        .delimited_by(just('"'), just('"'))
        .map(Token::String)
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
        .ignore_then(hex_byte.clone())
        .then(hex_byte.clone())
        .then(hex_byte.clone())
        .then(hex_byte.or_not())
        .map(|(((r, g), b), a)| {
            let r = u8::from_str_radix(&r, 16)?;
            let g = u8::from_str_radix(&g, 16)?;
            let b = u8::from_str_radix(&b, 16)?;
            let a = match a {
                Some(a) => Some(u8::from_str_radix(&a, 16)?),
                None => None,
            };

            Ok(Token::Colour(r, g, b, a))
        })
        .validate(|res: Result<_, ParseIntError>, e, emitter| {
            res.unwrap_or_else(|err| {
                emitter.emit(Rich::custom(
                    e.span(),
                    format!("ICE: parsed invalid hex code: {err}"),
                ));

                Token::Error
            })
        })
        .boxed();

    let keyword = choice((
        text::keyword("end").to(Token::Kw(Kw::End)),
        text::keyword("loop").to(Token::Kw(Kw::Loop)),
        text::keyword("if").to(Token::Kw(Kw::If)),
        text::keyword("else").to(Token::Kw(Kw::Else)),
        text::keyword("then").to(Token::Kw(Kw::Then)),
        text::keyword("let").to(Token::Kw(Kw::Let)),
        text::keyword("break").to(Token::Kw(Kw::Break)),
        text::keyword("continue").to(Token::Kw(Kw::Continue)),
        text::keyword("for").to(Token::Kw(Kw::For)),
        text::keyword("do").to(Token::Kw(Kw::Do)),
        text::keyword("in").to(Token::Kw(Kw::In)),
        text::keyword("action").to(Token::Kw(Kw::Action)),
        text::keyword("proc").to(Token::Kw(Kw::Proc)),
        text::keyword("return").to(Token::Kw(Kw::Return)),
        text::keyword("run").to(Token::Kw(Kw::Run)),
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
        just(':').to(Token::Ctrl(Ctrl::Colon)),
        just('@').to(Token::Ctrl(Ctrl::At)),
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
        keyword, bool, variable, float, integer, string, colour, operator, ctrl,
    ))
    .boxed();

    let comment = just("//")
        .then(any().and_is(just('\n').not()).repeated())
        .padded()
        .boxed();

    token
        .map_with(|tok, e| (tok, e.span()))
        .padded_by(comment.clone().repeated())
        .padded()
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
        .padded_by(comment.repeated())
        .padded()
        .then_ignore(end())
        .boxed()
}
