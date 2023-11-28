use crate::{lexer::Token, Span, Spanned};
use chumsky::{input::SpannedInput, prelude::*};

#[derive(Clone)]
pub enum Expr<'src> {
    Variable(&'src str),
    Number(i32),
    Binary(
        Box<Spanned<Expr<'src>>>,
        Spanned<BinaryOp>,
        Box<Spanned<Expr<'src>>>,
    ),
    Unary(Spanned<UnaryOp>, Box<Spanned<Expr<'src>>>),
}

#[derive(Clone, Copy)]
pub enum BinaryOp {
    Multiply,
    Divide,
    Equals,
    NotEquals,
    Plus,
    Minus,
}

#[derive(Clone, Copy)]
pub enum UnaryOp {
    Negate,
}

type ParserInput<'tokens, 'src> = SpannedInput<Token<'src>, Span, &'tokens [(Token<'src>, Span)]>;

type ParserError<'tokens, 'src> = extra::Err<Rich<'tokens, Token<'src>, Span>>;

pub fn parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Expr<'src>>, ParserError<'tokens, 'src>>
{
    expr_parser().then_ignore(end())
}

fn expr_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Expr<'src>>, ParserError<'tokens, 'src>>
{
    recursive(|expression| {
        let var = select! {
            Token::Variable(name) => Expr::Variable(name)
        }
        .map_with(|variable, e| (variable, e.span()));

        let number = select! {
            Token::Number(n) => Expr::Number(n)
        }
        .map_with(|number, e| (number, e.span()));

        let parenthesized_expr =
            expression.delimited_by(just(Token::LeftParen), just(Token::RightParen));

        let atom = choice((var, number, parenthesized_expr)).boxed();

        let negate_op = just(Token::Minus)
            .to(UnaryOp::Negate)
            .map_with(|op, e| (op, e.span()));

        let negate = negate_op
            .repeated()
            .foldr(atom, |op: (_, SimpleSpan), expr: (_, SimpleSpan)| {
                let span = op.1.start..expr.1.end;

                (Expr::Unary(op, Box::new(expr)), span.into())
            })
            .boxed();

        let factor_op = choice((
            just(Token::Multiply).to(BinaryOp::Multiply),
            just(Token::Divide).to(BinaryOp::Divide),
        ))
        .map_with(|t, e| (t, e.span()));

        let factor = negate
            .clone()
            .foldl(factor_op.then(negate).repeated(), |lhs, (op, rhs)| {
                let span = lhs.1.start..rhs.1.end;

                (Expr::Binary(Box::new(lhs), op, Box::new(rhs)), span.into())
            })
            .boxed();

        let sum_op = choice((
            just(Token::Plus).to(BinaryOp::Plus),
            just(Token::Minus).to(BinaryOp::Minus),
        ))
        .map_with(|t, e| (t, e.span()));

        let sum = factor
            .clone()
            .foldl(sum_op.then(factor).repeated(), |lhs, (op, rhs)| {
                let span = lhs.1.start..rhs.1.end;

                (Expr::Binary(Box::new(lhs), op, Box::new(rhs)), span.into())
            })
            .boxed();

        let equality_op = choice((
            just(Token::Equals).to(BinaryOp::Equals),
            just(Token::NotEquals).to(BinaryOp::NotEquals),
        ))
        .map_with(|t, e| (t, e.span()));

        sum.clone()
            .foldl(equality_op.then(sum).repeated(), |lhs, (op, rhs)| {
                let span = lhs.1.start..rhs.1.end;

                (Expr::Binary(Box::new(lhs), op, Box::new(rhs)), span.into())
            })
            .boxed()
    })
}
