use crate::{
    ast::{BinaryOp, Expr, Statement, UnaryOp},
    lexer::{Ctrl, Kw, Op, Token},
    Span, Spanned,
};
use chumsky::{input::SpannedInput, prelude::*};

type ParserInput<'tokens, 'src, 'file> =
    SpannedInput<Token<'src>, Span<'file>, &'tokens [(Token<'src>, Span<'file>)]>;

type ParserOutput<'tokens, 'src, 'file> = Vec<Spanned<'file, Statement<'src, 'file>>>;

type ParserError<'tokens, 'src, 'file> = extra::Err<Rich<'tokens, Token<'src>, Span<'file>>>;

pub fn parser<'tokens, 'src: 'tokens, 'file: 'src>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src, 'file>,
    ParserOutput<'tokens, 'src, 'file>,
    ParserError<'tokens, 'src, 'file>,
> {
    statement_parser().repeated().collect().then_ignore(end())
}

fn statement_parser<'tokens, 'src: 'tokens, 'file: 'src>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src, 'file>,
    Spanned<'file, Statement<'src, 'file>>,
    ParserError<'tokens, 'src, 'file>,
> {
    recursive(|statement| {
        let expr = expr_parser()
            .then_ignore(just(Token::Ctrl(Ctrl::SemiColon)))
            .map(Statement::Expr)
            .boxed();

        let print = just(Token::Kw(Kw::BuiltinPrint))
            .ignore_then(expr_parser())
            .then_ignore(just(Token::Ctrl(Ctrl::SemiColon)))
            .map(Statement::BuiltinPrint)
            .boxed();

        let loop_ = just(Token::Kw(Kw::Loop))
            .ignore_then(
                statement
                    .clone()
                    .repeated()
                    .collect()
                    .then_ignore(just(Token::Kw(Kw::End)))
                    .map_with(|body, e| Spanned(body, e.span())),
            )
            .map(Statement::Loop)
            .boxed();

        let if_ = just(Token::Kw(Kw::If))
            .ignore_then(expr_parser())
            .then_ignore(just(Token::Kw(Kw::Then)))
            .then(
                statement
                    .clone()
                    .repeated()
                    .collect()
                    .map_with(|body, e| Spanned(body, e.span()))
                    .boxed(),
            )
            .then(
                just(Token::Kw(Kw::Else))
                    .ignore_then(
                        statement
                            .repeated()
                            .collect()
                            .map_with(|body, e| Spanned(body, e.span())),
                    )
                    .or_not()
                    .boxed(),
            )
            .then_ignore(just(Token::Kw(Kw::End)))
            .map(|((condition, then_branch), else_branch)| Statement::If {
                condition,
                then_branch,
                else_branch,
            })
            .boxed();

        let let_ = just(Token::Kw(Kw::Let))
            .ignore_then(ident())
            .then_ignore(just(Token::Ctrl(Ctrl::Equals)))
            .then(expr_parser())
            .then_ignore(just(Token::Ctrl(Ctrl::SemiColon)))
            .map(|(name, value)| Statement::Let { name, value })
            .boxed();

        let const_ = just(Token::Kw(Kw::Const))
            .ignore_then(ident())
            .then_ignore(just(Token::Ctrl(Ctrl::Equals)))
            .then(expr_parser())
            .then_ignore(just(Token::Ctrl(Ctrl::SemiColon)))
            .map(|(name, value)| Statement::Const { name, value })
            .boxed();

        let assign = ident()
            .then_ignore(just(Token::Ctrl(Ctrl::Equals)))
            .then(expr_parser())
            .then_ignore(just(Token::Ctrl(Ctrl::SemiColon)))
            .map(|(name, value)| Statement::Assign { name, value })
            .boxed();

        let break_ = just(Token::Kw(Kw::Break))
            .then_ignore(just(Token::Ctrl(Ctrl::SemiColon)))
            .map(|_| Statement::Break)
            .boxed();

        let continue_ = just(Token::Kw(Kw::Continue))
            .then_ignore(just(Token::Ctrl(Ctrl::SemiColon)))
            .map(|_| Statement::Continue)
            .boxed();

        choice((
            expr, print, loop_, if_, let_, const_, assign, break_, continue_,
        ))
        .map_with(|statement, e| Spanned(statement, e.span()))
        .boxed()
    })
}

fn expr_parser<'tokens, 'src: 'tokens, 'file: 'src>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src, 'file>,
    Spanned<'file, Expr<'src, 'file>>,
    ParserError<'tokens, 'src, 'file>,
> {
    recursive(|expression| {
        let variable = select! {
            Token::Variable(name) => name
        }
        .map_with(|variable, e| Spanned(Expr::Variable(variable), e.span()));

        let boolean = select! {
            Token::Boolean(b) => b
        }
        .map_with(|boolean, e| Spanned(Expr::Boolean(boolean), e.span()));

        let integer = select! {
            Token::Integer(n) => n
        }
        .map_with(|integer, e| Spanned(Expr::Integer(integer), e.span()));

        let float = select! {
            Token::Float(n) => n
        }
        .map_with(|float, e| Spanned(Expr::Float(float), e.span()));

        let colour = select! {
            Token::HexCode(r, g, b) => (r, g, b)
        }
        .map_with(|(r, g, b), e| Spanned(Expr::Colour { r, g, b }, e.span()))
        .boxed();

        let vector = expression
            .clone()
            .then_ignore(just(Token::Ctrl(Ctrl::Comma)))
            .then(expression.clone())
            .then_ignore(just(Token::Ctrl(Ctrl::Comma)).or_not())
            .delimited_by(
                just(Token::Ctrl(Ctrl::LeftCurly)),
                just(Token::Ctrl(Ctrl::RightCurly)),
            )
            .map_with(|(x, y): (Spanned<Expr>, _), e| {
                Spanned(
                    Expr::Vector {
                        x: x.map(Box::new),
                        y: y.map(Box::new),
                    },
                    e.span(),
                )
            })
            .boxed();

        let parenthesized_expr = expression.delimited_by(
            just(Token::Ctrl(Ctrl::LeftParen)),
            just(Token::Ctrl(Ctrl::RightParen)),
        );

        let atom = choice((
            variable,
            boolean,
            integer,
            float,
            colour,
            vector,
            parenthesized_expr,
        ))
        .boxed();

        let unary_op = choice((
            just(Token::Op(Op::Minus)).to(UnaryOp::Negate),
            just(Token::Op(Op::Not)).to(UnaryOp::Not),
        ))
        .map_with(|t, e| Spanned(t, e.span()));

        let unary = unary_op
            .repeated()
            .foldr(atom, |op, expr| {
                let span = Span::union(op.1, expr.1);

                Spanned(Expr::Unary(op, expr.map(Box::new)), span)
            })
            .boxed();

        let factor_op = choice((
            just(Token::Op(Op::Multiply)).to(BinaryOp::Multiply),
            just(Token::Op(Op::Divide)).to(BinaryOp::Divide),
        ))
        .map_with(|t, e| Spanned(t, e.span()));

        let factor = unary
            .clone()
            .foldl(factor_op.then(unary).repeated(), |lhs, (op, rhs)| {
                let span = Span::union(lhs.1, rhs.1);

                Spanned(Expr::Binary(lhs.map(Box::new), op, rhs.map(Box::new)), span)
            })
            .boxed();

        let sum_op = choice((
            just(Token::Op(Op::Plus)).to(BinaryOp::Plus),
            just(Token::Op(Op::Minus)).to(BinaryOp::Minus),
        ))
        .map_with(|t, e| Spanned(t, e.span()));

        let sum = factor
            .clone()
            .foldl(sum_op.then(factor).repeated(), |lhs, (op, rhs)| {
                let span = Span::union(lhs.1, rhs.1);

                Spanned(Expr::Binary(lhs.map(Box::new), op, rhs.map(Box::new)), span)
            })
            .boxed();

        let relational_op = choice((
            just(Token::Op(Op::GreaterThanEquals)).to(BinaryOp::GreaterThanEquals),
            just(Token::Op(Op::LessThanEquals)).to(BinaryOp::LessThanEquals),
            just(Token::Op(Op::GreaterThan)).to(BinaryOp::GreaterThan),
            just(Token::Op(Op::LessThan)).to(BinaryOp::LessThan),
        ))
        .map_with(|t, e| Spanned(t, e.span()));

        let relational = sum
            .clone()
            .foldl(relational_op.then(sum).repeated(), |lhs, (op, rhs)| {
                let span = Span::union(lhs.1, rhs.1);

                Spanned(Expr::Binary(lhs.map(Box::new), op, rhs.map(Box::new)), span)
            })
            .boxed();

        let equality_op = choice((
            just(Token::Op(Op::Equals)).to(BinaryOp::Equals),
            just(Token::Op(Op::NotEquals)).to(BinaryOp::NotEquals),
        ))
        .map_with(|t, e| Spanned(t, e.span()));

        relational
            .clone()
            .foldl(equality_op.then(relational).repeated(), |lhs, (op, rhs)| {
                let span = Span::union(lhs.1, rhs.1);

                Spanned(Expr::Binary(lhs.map(Box::new), op, rhs.map(Box::new)), span)
            })
            .boxed()
    })
}

fn ident<'tokens, 'src: 'tokens, 'file: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src, 'file>,
    Spanned<'file, &'src str>,
    ParserError<'tokens, 'src, 'file>,
> {
    select! {
        Token::Variable(name) => name
    }
    .map_with(|name, e| Spanned(name, e.span()))
}
