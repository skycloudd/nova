use crate::{Span, Spanned};
use chumsky::prelude::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'src> {
    Variable(&'src str),
    Number(i32),
    LeftParen,
    RightParen,
    Equals,
    NotEquals,
    Plus,
    Minus,
    Multiply,
    Divide,
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Variable(name) => write!(f, "{}", name),
            Token::Number(n) => write!(f, "{}", n),
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::Equals => write!(f, "=="),
            Token::NotEquals => write!(f, "!="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Multiply => write!(f, "*"),
            Token::Divide => write!(f, "/"),
        }
    }
}

pub fn lexer<'src>(
) -> impl Parser<'src, &'src str, Vec<Spanned<Token<'src>>>, extra::Err<Rich<'src, char, Span>>> {
    let variable = any()
        .filter(|c: &char| c.is_alphabetic())
        .to_slice()
        .map(Token::Variable);

    let number = text::int(10)
        .map(|n: &str| n.parse().unwrap())
        .map(Token::Number);

    let token = choice((
        variable,
        number,
        just('(').to(Token::LeftParen),
        just(')').to(Token::RightParen),
        just("==").to(Token::Equals),
        just("!=").to(Token::NotEquals),
        just('+').to(Token::Plus),
        just('-').to(Token::Minus),
        just('*').to(Token::Multiply),
        just('/').to(Token::Divide),
    ));

    token
        .map_with(|tok, e| (tok, e.span()))
        .padded()
        .repeated()
        .collect()
}
