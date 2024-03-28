use crate::{FloatTy, IntTy, Span};
use chumsky::{input::WithContext, prelude::*};

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    // Error,
    Variable(&'static str),
    Boolean(bool),
    Integer(IntTy),
    Float(FloatTy),
    String(String),
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
    Func,
    Return,
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
    Star,
    Slash,

    GreaterThanEquals,
    LessThanEquals,
    GreaterThan,
    LessThan,

    Not,
    Ref,
}

impl core::fmt::Display for Token {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            // Token::Error => write!(f, "error"),
            Token::Variable(v) => write!(f, "{v}"),
            Token::Boolean(b) => write!(f, "{b}"),
            Token::Integer(n) => write!(f, "{n}"),
            Token::Float(n) => write!(f, "{n}"),
            Token::String(s) => write!(f, "\"{s}\""),
            Token::Kw(k) => write!(f, "{k}"),
            Token::Ctrl(c) => write!(f, "{c}"),
            Token::Op(o) => write!(f, "{o}"),
        }
    }
}

impl core::fmt::Display for Kw {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
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
            Self::Func => write!(f, "func"),
            Self::Return => write!(f, "return"),
        }
    }
}

impl core::fmt::Display for Ctrl {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
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

impl core::fmt::Display for Op {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Equals => write!(f, "=="),
            Self::NotEquals => write!(f, "!="),

            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Star => write!(f, "*"),
            Self::Slash => write!(f, "/"),

            Self::GreaterThanEquals => write!(f, ">="),
            Self::LessThanEquals => write!(f, "<="),
            Self::GreaterThan => write!(f, ">"),
            Self::LessThan => write!(f, "<"),

            Self::Not => write!(f, "!"),
            Self::Ref => write!(f, "&"),
        }
    }
}

type LexerInput = WithContext<Span, &'static str>;

type LexerOutput = Vec<(Token, Span)>;

type LexerError = extra::Err<Rich<'static, char, Span>>;

#[allow(clippy::too_many_lines)]
pub fn lexer() -> impl Parser<'static, LexerInput, LexerOutput, LexerError> {
    let variable = text::ascii::ident().map(Token::Variable).boxed();

    let bool = choice((
        text::keyword("true").to(Token::Boolean(true)),
        text::keyword("false").to(Token::Boolean(false)),
    ))
    .boxed();

    let sign = choice((just('+'), just('-'))).or_not().boxed();

    let integer = sign
        .clone()
        .then(text::int(10))
        .to_slice()
        .validate(|n: &str, e, emitter| match n.parse() {
            Ok(n) => n,
            Err(err) => {
                emitter.emit(Rich::custom(e.span(), err));
                0
            }
        })
        .map(Token::Integer)
        .boxed();

    let float = sign
        .then(text::int(10))
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
        text::keyword("func").to(Token::Kw(Kw::Func)),
        text::keyword("return").to(Token::Kw(Kw::Return)),
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
        just('*').to(Token::Op(Op::Star)),
        just('/').to(Token::Op(Op::Slash)),
        just('>').to(Token::Op(Op::GreaterThan)),
        just('<').to(Token::Op(Op::LessThan)),
        just('!').to(Token::Op(Op::Not)),
        just('&').to(Token::Op(Op::Ref)),
    ))
    .boxed();

    let comment = just("#")
        .then(any().and_is(just('\n').not()).repeated())
        .padded()
        .boxed();

    let token = choice((
        keyword, bool, variable, float, integer, string, operator, ctrl,
    ))
    .map_with(|tok, e| (tok, e.span()))
    .padded_by(comment.clone().repeated())
    .padded()
    .recover_with(skip_then_retry_until(any().ignored(), end()))
    .boxed();

    token
        .repeated()
        .collect()
        .padded_by(comment.repeated())
        .padded()
        .then_ignore(end())
        .boxed()
}
