use crate::{
    ast::{BinaryOp, Expr, Procedure, Statement, TopLevel, Type, UnaryOp},
    lexer::{Ctrl, Kw, Op, Token},
    span::{Span, Spanned},
};
use chumsky::{input::SpannedInput, prelude::*};

type ParserInput<'tokens, 'src, 'file> =
    SpannedInput<Token<'src>, Span<'file>, &'tokens [(Token<'src>, Span<'file>)]>;

type ParserOutput<'tokens, 'src, 'file> = Vec<Spanned<'file, TopLevel<'src, 'file>>>;

type ParserError<'tokens, 'src, 'file> = extra::Err<Rich<'tokens, Token<'src>, Span<'file>>>;

pub fn parser<'tokens, 'src: 'tokens, 'file: 'src>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src, 'file>,
    ParserOutput<'tokens, 'src, 'file>,
    ParserError<'tokens, 'src, 'file>,
> {
    toplevel_parser().repeated().collect().then_ignore(end())
}

fn toplevel_parser<'tokens, 'src: 'tokens, 'file: 'src>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src, 'file>,
    Spanned<'file, TopLevel<'src, 'file>>,
    ParserError<'tokens, 'src, 'file>,
> {
    procedure_parser()
}

fn procedure_parser<'tokens, 'src: 'tokens, 'file: 'src>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src, 'file>,
    Spanned<'file, TopLevel<'src, 'file>>,
    ParserError<'tokens, 'src, 'file>,
> {
    let ty_parser = select! {
        Token::Variable("bool") => Type::Boolean,
        Token::Variable("int") => Type::Integer,
        Token::Variable("float") => Type::Float,
        Token::Variable("colour" | "color") => Type::Colour,
        Token::Variable("vector") => Type::Vector,
    }
    .map_with(|ty, e| Spanned(ty, e.span()));

    just(Token::Kw(Kw::Proc))
        .ignore_then(ident())
        .then_ignore(just(Token::Ctrl(Ctrl::LeftParen)))
        .then(
            ident()
                .then_ignore(just(Token::Ctrl(Ctrl::Colon)))
                .then(ty_parser)
                .separated_by(just(Token::Ctrl(Ctrl::Comma)))
                .allow_trailing()
                .collect()
                .map_with(|args, e| Spanned(args, e.span()))
                .boxed(),
        )
        .then_ignore(just(Token::Ctrl(Ctrl::RightParen)))
        .then_ignore(just(Token::Kw(Kw::Do)))
        .then(
            statement_parser()
                .repeated()
                .collect()
                .map_with(|body, e| Spanned(body, e.span()))
                .boxed(),
        )
        .then_ignore(just(Token::Kw(Kw::End)))
        .map(|((name, args), body)| Procedure { name, args, body })
        .map_with(|procedure, e| Spanned(TopLevel::Procedure(procedure), e.span()))
        .boxed()
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

        let block = just(Token::Kw(Kw::Do))
            .ignore_then(
                statement
                    .clone()
                    .repeated()
                    .collect()
                    .map_with(|body, e| Spanned(body, e.span())),
            )
            .then_ignore(just(Token::Kw(Kw::End)))
            .map(Statement::Block)
            .boxed();

        let loop_ = just(Token::Kw(Kw::Loop))
            .ignore_then(just(Token::Kw(Kw::Do)))
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
                            .clone()
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

        let for_ = just(Token::Kw(Kw::For))
            .ignore_then(ident())
            .then_ignore(just(Token::Kw(Kw::In)))
            .then(
                expr_parser()
                    .then(choice((
                        just(Token::Ctrl(Ctrl::Range)).to(false),
                        just(Token::Ctrl(Ctrl::RangeInclusive)).to(true),
                    )))
                    .then(expr_parser())
                    .boxed(),
            )
            .then_ignore(just(Token::Kw(Kw::Do)))
            .then(
                statement
                    .repeated()
                    .collect()
                    .map_with(|body, e| Spanned(body, e.span()))
                    .boxed(),
            )
            .then_ignore(just(Token::Kw(Kw::End)))
            .map(|((name, ((start, inclusive), end)), body)| Statement::For {
                name,
                start,
                end,
                inclusive,
                body,
            })
            .boxed();

        let let_ = just(Token::Kw(Kw::Let))
            .ignore_then(ident())
            .then_ignore(just(Token::Ctrl(Ctrl::Equals)))
            .then(expr_parser())
            .then_ignore(just(Token::Ctrl(Ctrl::SemiColon)))
            .map(|(name, value)| Statement::Let { name, value })
            .boxed();

        let assign = ident()
            .then_ignore(just(Token::Ctrl(Ctrl::Equals)))
            .then(expr_parser())
            .then_ignore(just(Token::Ctrl(Ctrl::SemiColon)))
            .map(|(name, value)| Statement::Assign { name, value })
            .boxed();

        let break_ = just(Token::Kw(Kw::Break))
            .ignored()
            .then_ignore(just(Token::Ctrl(Ctrl::SemiColon)))
            .map(|()| Statement::Break)
            .boxed();

        let continue_ = just(Token::Kw(Kw::Continue))
            .ignored()
            .then_ignore(just(Token::Ctrl(Ctrl::SemiColon)))
            .map(|()| Statement::Continue)
            .boxed();

        let return_ = just(Token::Kw(Kw::Return))
            .ignored()
            .then_ignore(just(Token::Ctrl(Ctrl::SemiColon)))
            .map(|()| Statement::Return)
            .boxed();

        let action = just(Token::Kw(Kw::Action))
            .ignore_then(ident())
            .then_ignore(just(Token::Ctrl(Ctrl::Colon)))
            .then(
                expr_parser()
                    .separated_by(just(Token::Ctrl(Ctrl::Comma)))
                    .collect()
                    .map_with(|args, e| Spanned(args, e.span()))
                    .boxed(),
            )
            .then_ignore(just(Token::Ctrl(Ctrl::SemiColon)))
            .map(|(name, args)| Statement::Action { name, args })
            .boxed();

        let call = ident()
            .then_ignore(just(Token::Ctrl(Ctrl::LeftParen)))
            .then(
                expr_parser()
                    .separated_by(just(Token::Ctrl(Ctrl::Comma)))
                    .collect()
                    .map_with(|args, e| Spanned(args, e.span()))
                    .boxed(),
            )
            .then_ignore(just(Token::Ctrl(Ctrl::RightParen)))
            .then_ignore(just(Token::Ctrl(Ctrl::SemiColon)))
            .map(|(proc, args)| Statement::Call { proc, args })
            .boxed();

        choice((
            expr, block, loop_, if_, for_, let_, assign, break_, continue_, return_, action, call,
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
        let variable = ident()
            .map_with(|variable, e| Spanned(Expr::Variable(variable.0), e.span()))
            .boxed();

        let boolean = select! {
            Token::Boolean(b) => b
        }
        .map_with(|boolean, e| Spanned(Expr::Boolean(boolean), e.span()))
        .boxed();

        let integer = select! {
            Token::Integer(n) => n
        }
        .map_with(|integer, e| Spanned(Expr::Integer(integer), e.span()))
        .boxed();

        let float = select! {
            Token::Float(n) => n
        }
        .map_with(|float, e| Spanned(Expr::Float(float), e.span()))
        .boxed();

        let colour = select! {
            Token::HexCode(r, g, b, a) => (r, g, b, a)
        }
        .map_with(|(r, g, b, a), e| {
            Spanned(
                Expr::Colour {
                    r,
                    g,
                    b,
                    a: a.unwrap_or(255),
                },
                e.span(),
            )
        })
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

        let parenthesized_expr = expression
            .clone()
            .delimited_by(
                just(Token::Ctrl(Ctrl::LeftParen)),
                just(Token::Ctrl(Ctrl::RightParen)),
            )
            .boxed();

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
    .boxed()
}
