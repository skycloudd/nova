use crate::{
    ast::{BinaryOp, Expr, Statement, UnaryOp},
    lexer::{Ctrl, Kw, Op, Token},
    Span, Spanned,
};
use chumsky::{input::SpannedInput, prelude::*};

type ParserInput<'tokens, 'src> = SpannedInput<Token<'src>, Span, &'tokens [(Token<'src>, Span)]>;

type ParserError<'tokens, 'src> = extra::Err<Rich<'tokens, Token<'src>, Span>>;

pub fn parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Vec<Spanned<Statement<'src>>>,
    ParserError<'tokens, 'src>,
> {
    statement_parser().repeated().collect().then_ignore(end())
}

fn statement_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Statement<'src>>, ParserError<'tokens, 'src>>
{
    recursive(|statement| {
        let expr = expr_parser()
            .then_ignore(just(Token::Ctrl(Ctrl::SemiColon)))
            .map_with(|expr, e| (Statement::Expr(expr), e.span()))
            .boxed();

        let print = just(Token::Kw(Kw::BuiltinPrint))
            .ignore_then(expr_parser())
            .then_ignore(just(Token::Ctrl(Ctrl::SemiColon)))
            .map_with(|expr, e| (Statement::BuiltinPrint(expr), e.span()))
            .boxed();

        let loop_ = just(Token::Kw(Kw::Loop))
            .ignore_then(
                statement
                    .clone()
                    .repeated()
                    .collect()
                    .then_ignore(just(Token::Kw(Kw::End)))
                    .map_with(|body, e| (body, e.span())),
            )
            .map_with(|body, e| (Statement::Loop(body), e.span()))
            .boxed();

        let if_ = just(Token::Kw(Kw::If))
            .ignore_then(expr_parser())
            .then_ignore(just(Token::Kw(Kw::Then)))
            .then(
                statement
                    .clone()
                    .repeated()
                    .collect()
                    .map_with(|body, e| (body, e.span()))
                    .boxed(),
            )
            .then(
                just(Token::Kw(Kw::Else))
                    .ignore_then(
                        statement
                            .clone()
                            .repeated()
                            .collect()
                            .map_with(|body, e| (body, e.span())),
                    )
                    .or_not()
                    .boxed(),
            )
            .then_ignore(just(Token::Kw(Kw::End)))
            .map_with(|((condition, then_branch), else_branch), e| {
                (
                    Statement::If {
                        condition,
                        then_branch,
                        else_branch,
                    },
                    e.span(),
                )
            })
            .boxed();

        let let_ = just(Token::Kw(Kw::Let))
            .ignore_then(ident())
            .then_ignore(just(Token::Op(Op::Equals)))
            .then(expr_parser())
            .then_ignore(just(Token::Ctrl(Ctrl::SemiColon)))
            .map_with(|(name, value), e| (Statement::Let { name, value }, e.span()))
            .boxed();

        choice((expr, print, loop_, if_, let_)).boxed()
    })
}

fn expr_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Expr<'src>>, ParserError<'tokens, 'src>>
{
    recursive(|expression| {
        let variable = select! {
            Token::Variable(name) => name
        }
        .map_with(|variable, e| (Expr::Variable((variable, e.span())), e.span()));

        let boolean = select! {
            Token::Boolean(b) => b
        }
        .map_with(|boolean, e| (Expr::Boolean((boolean, e.span())), e.span()));

        let integer = select! {
            Token::Integer(n) => n
        }
        .map_with(|integer, e| (Expr::Integer((integer, e.span())), e.span()));

        let null = just(Token::Null)
            .ignored()
            .map_with(|_, e| (Expr::Null, e.span()))
            .boxed();

        let parenthesized_expr = expression.clone().delimited_by(
            just(Token::Ctrl(Ctrl::LeftParen)),
            just(Token::Ctrl(Ctrl::RightParen)),
        );

        let atom = choice((variable, boolean, integer, null, parenthesized_expr)).boxed();

        let unary_op = choice((
            just(Token::Op(Op::Minus)).to(UnaryOp::Negate),
            just(Token::Op(Op::Not)).to(UnaryOp::Not),
        ))
        .map_with(|t, e| (t, e.span()));

        let unary = unary_op
            .repeated()
            .foldr(atom, |op: (_, SimpleSpan), expr: (_, SimpleSpan)| {
                let span = op.1.start..expr.1.end;

                (Expr::Unary(op, Box::new(expr)), span.into())
            })
            .boxed();

        let factor_op = choice((
            just(Token::Op(Op::Multiply)).to(BinaryOp::Multiply),
            just(Token::Op(Op::Divide)).to(BinaryOp::Divide),
        ))
        .map_with(|t, e| (t, e.span()));

        let factor = unary
            .clone()
            .foldl(factor_op.then(unary).repeated(), |lhs, (op, rhs)| {
                let span = lhs.1.start..rhs.1.end;

                (Expr::Binary(Box::new(lhs), op, Box::new(rhs)), span.into())
            })
            .boxed();

        let sum_op = choice((
            just(Token::Op(Op::Plus)).to(BinaryOp::Plus),
            just(Token::Op(Op::Minus)).to(BinaryOp::Minus),
        ))
        .map_with(|t, e| (t, e.span()));

        let sum = factor
            .clone()
            .foldl(sum_op.then(factor).repeated(), |lhs, (op, rhs)| {
                let span = lhs.1.start..rhs.1.end;

                (Expr::Binary(Box::new(lhs), op, Box::new(rhs)), span.into())
            })
            .boxed();

        let relational_op = choice((
            just(Token::Op(Op::GreaterThanEquals)).to(BinaryOp::GreaterThanEquals),
            just(Token::Op(Op::LessThanEquals)).to(BinaryOp::LessThanEquals),
            just(Token::Op(Op::GreaterThan)).to(BinaryOp::GreaterThan),
            just(Token::Op(Op::LessThan)).to(BinaryOp::LessThan),
        ))
        .map_with(|t, e| (t, e.span()));

        let relational = sum
            .clone()
            .foldl(relational_op.then(sum).repeated(), |lhs, (op, rhs)| {
                let span = lhs.1.start..rhs.1.end;

                (Expr::Binary(Box::new(lhs), op, Box::new(rhs)), span.into())
            })
            .boxed();

        let equality_op = choice((
            just(Token::Op(Op::Equals)).to(BinaryOp::Equals),
            just(Token::Op(Op::NotEquals)).to(BinaryOp::NotEquals),
        ))
        .map_with(|t, e| (t, e.span()));

        relational
            .clone()
            .foldl(equality_op.then(relational).repeated(), |lhs, (op, rhs)| {
                let span = lhs.1.start..rhs.1.end;

                (Expr::Binary(Box::new(lhs), op, Box::new(rhs)), span.into())
            })
            .boxed()
    })
}

fn ident<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<&'src str>, ParserError<'tokens, 'src>>
{
    select! {
        Token::Variable(name) => name
    }
    .map_with(|name, e| (name, e.span()))
}
