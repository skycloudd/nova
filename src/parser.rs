use crate::{
    ast::{Ast, BinaryOp, Expr, Function, Primitive, Statement, TopLevel, Type, UnaryOp},
    lexer::{Ctrl, Kw, Op, Token},
    span::{Span, Spanned},
};
use chumsky::{input::SpannedInput, prelude::*};

type ParserInput<'tok> = SpannedInput<Token, Span, &'tok [(Token, Span)]>;

type ParserError<'tok> = extra::Err<Rich<'tok, Token, Span>>;

pub fn parser<'tok>() -> impl Parser<'tok, ParserInput<'tok>, Ast, ParserError<'tok>> {
    toplevel_parser()
        .repeated()
        .collect()
        .then_ignore(end())
        .map(|top_levels| Ast { top_levels })
        .boxed()
}

fn toplevel_parser<'tok>(
) -> impl Parser<'tok, ParserInput<'tok>, Spanned<TopLevel>, ParserError<'tok>> {
    function_parser()
}

fn function_parser<'tok>(
) -> impl Parser<'tok, ParserInput<'tok>, Spanned<TopLevel>, ParserError<'tok>> {
    just(Token::Kw(Kw::Func))
        .ignore_then(ident())
        .then(
            ident()
                .then_ignore(just(Token::Ctrl(Ctrl::Colon)))
                .then(type_parser())
                .separated_by(just(Token::Ctrl(Ctrl::Comma)))
                .allow_trailing()
                .collect()
                .delimited_by(
                    just(Token::Ctrl(Ctrl::LeftParen)),
                    just(Token::Ctrl(Ctrl::RightParen)),
                )
                .map_with(|args, e| Spanned(args, e.span()))
                .boxed(),
        )
        .then(just(Token::Ctrl(Ctrl::Colon)).ignore_then(type_parser()))
        .then(
            statement_parser()
                .repeated()
                .collect()
                .delimited_by(just(Token::Kw(Kw::Do)), just(Token::Kw(Kw::End)))
                .map_with(|body, e| Spanned(body, e.span()))
                .recover_with(via_parser(nested_delimiters(
                    Token::Kw(Kw::Do),
                    Token::Kw(Kw::End),
                    [
                        (Token::Ctrl(Ctrl::LeftParen), Token::Ctrl(Ctrl::RightParen)),
                        (Token::Ctrl(Ctrl::LeftCurly), Token::Ctrl(Ctrl::RightCurly)),
                    ],
                    |span| Spanned(vec![], span),
                )))
                .boxed(),
        )
        .map_with(|(((name, params), return_ty), body), e| {
            Spanned(
                Function {
                    name,
                    params,
                    return_ty,
                    body,
                },
                e.span(),
            )
        })
        .map_with(|function, e| Spanned(TopLevel::Function(function), e.span()))
        .boxed()
}

#[allow(clippy::too_many_lines)]
fn statement_parser<'tok>(
) -> impl Parser<'tok, ParserInput<'tok>, Spanned<Statement>, ParserError<'tok>> {
    recursive(|statement| {
        let expr = expr_parser()
            .then_ignore(just(Token::Ctrl(Ctrl::SemiColon)))
            .map(Statement::Expr)
            .boxed();

        let statements = statement
            .clone()
            .repeated()
            .collect()
            .map_with(|statements, e| Spanned(statements, e.span()))
            .boxed();

        let block = statements
            .clone()
            .delimited_by(just(Token::Kw(Kw::Do)), just(Token::Kw(Kw::End)))
            .recover_with(via_parser(nested_delimiters(
                Token::Kw(Kw::Do),
                Token::Kw(Kw::End),
                [
                    (Token::Ctrl(Ctrl::LeftParen), Token::Ctrl(Ctrl::RightParen)),
                    (Token::Ctrl(Ctrl::LeftCurly), Token::Ctrl(Ctrl::RightCurly)),
                ],
                |span| Spanned(vec![], span),
            )))
            .map(Statement::Block)
            .boxed();

        let loop_ = just(Token::Kw(Kw::Loop))
            .ignore_then(
                statements
                    .clone()
                    .delimited_by(just(Token::Kw(Kw::Do)), just(Token::Kw(Kw::End)))
                    .recover_with(via_parser(nested_delimiters(
                        Token::Kw(Kw::Do),
                        Token::Kw(Kw::End),
                        [
                            (Token::Ctrl(Ctrl::LeftParen), Token::Ctrl(Ctrl::RightParen)),
                            (Token::Ctrl(Ctrl::LeftCurly), Token::Ctrl(Ctrl::RightCurly)),
                        ],
                        |span| Spanned(vec![], span),
                    ))),
            )
            .map(Statement::Loop)
            .boxed();

        let if_ = recursive(|if_| {
            let if_part = just(Token::Kw(Kw::If))
                .ignore_then(expr_parser())
                .then_ignore(just(Token::Kw(Kw::Then)))
                .then(statements.clone())
                .boxed();

            let if_else = if_part
                .clone()
                .then(
                    just(Token::Kw(Kw::Else))
                        .ignore_then(statements.clone())
                        .or_not(),
                )
                .then_ignore(just(Token::Kw(Kw::End)))
                .map(|((condition, then_branch), else_branch)| Statement::If {
                    condition,
                    then_branch,
                    else_branch,
                })
                .boxed();

            let if_else_if_else = if_part
                .then_ignore(just(Token::Kw(Kw::Else)))
                .then(choice((
                    if_.map_with(|if_, e| Spanned(if_, e.span()))
                        .map_with(|if_, e| Spanned(vec![if_], e.span()))
                        .boxed(),
                    statements.clone().then_ignore(just(Token::Kw(Kw::End))),
                )))
                .map(|((condition, then_branch), else_branch)| Statement::If {
                    condition,
                    then_branch,
                    else_branch: Some(else_branch),
                })
                .boxed();

            choice((if_else_if_else, if_else))
        });

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
            .then(
                statements
                    .delimited_by(just(Token::Kw(Kw::Do)), just(Token::Kw(Kw::End)))
                    .recover_with(via_parser(nested_delimiters(
                        Token::Kw(Kw::Do),
                        Token::Kw(Kw::End),
                        [
                            (Token::Ctrl(Ctrl::LeftParen), Token::Ctrl(Ctrl::RightParen)),
                            (Token::Ctrl(Ctrl::LeftCurly), Token::Ctrl(Ctrl::RightCurly)),
                        ],
                        |span| Spanned(vec![], span),
                    )))
                    .boxed(),
            )
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
            .ignore_then(expr_parser())
            .then_ignore(just(Token::Ctrl(Ctrl::SemiColon)))
            .map(Statement::Return)
            .boxed();

        choice((
            expr, block, loop_, if_, for_, let_, assign, break_, continue_, return_,
        ))
        .map_with(|statement, e| Spanned(statement, e.span()))
        .boxed()
    })
}

#[allow(clippy::too_many_lines)]
fn expr_parser<'tok>() -> impl Parser<'tok, ParserInput<'tok>, Spanned<Expr>, ParserError<'tok>> {
    recursive(|expression| {
        let variable = ident()
            .map_with(|variable, e| Spanned(Expr::Variable(variable), e.span()))
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

        let parenthesized_expr = expression
            .clone()
            .delimited_by(
                just(Token::Ctrl(Ctrl::LeftParen)),
                just(Token::Ctrl(Ctrl::RightParen)),
            )
            .recover_with(via_parser(nested_delimiters(
                Token::Ctrl(Ctrl::LeftParen),
                Token::Ctrl(Ctrl::RightParen),
                [
                    (Token::Ctrl(Ctrl::LeftCurly), Token::Ctrl(Ctrl::RightCurly)),
                    (Token::Kw(Kw::Do), Token::Kw(Kw::End)),
                ],
                |span| Spanned(Expr::Error, span),
            )))
            .boxed();

        let convert = just(Token::Ctrl(Ctrl::At))
            .ignore_then(type_parser())
            .then(expression.clone().delimited_by(
                just(Token::Ctrl(Ctrl::LeftParen)),
                just(Token::Ctrl(Ctrl::RightParen)),
            ))
            .map_with(|(ty, expr), e| {
                Spanned(
                    Expr::Convert {
                        ty,
                        expr: expr.boxed(),
                    },
                    e.span(),
                )
            })
            .boxed();

        let call = ident()
            .then(
                expression
                    .clone()
                    .separated_by(just(Token::Ctrl(Ctrl::Comma)))
                    .allow_trailing()
                    .collect()
                    .map_with(|args, e| Spanned(args, e.span()))
                    .delimited_by(
                        just(Token::Ctrl(Ctrl::LeftParen)),
                        just(Token::Ctrl(Ctrl::RightParen)),
                    )
                    .recover_with(via_parser(nested_delimiters(
                        Token::Ctrl(Ctrl::LeftParen),
                        Token::Ctrl(Ctrl::RightParen),
                        [
                            (Token::Ctrl(Ctrl::LeftCurly), Token::Ctrl(Ctrl::RightCurly)),
                            (Token::Kw(Kw::Do), Token::Kw(Kw::End)),
                        ],
                        |span| Spanned(vec![], span),
                    ))),
            )
            .map_with(|(func, args), e| Spanned(Expr::Call { func, args }, e.span()))
            .boxed();

        let atom = choice((
            call,
            variable,
            boolean,
            integer,
            float,
            parenthesized_expr,
            convert,
        ))
        .boxed();

        let unary_op = choice((
            just(Token::Op(Op::Minus)).to(UnaryOp::Negate),
            just(Token::Op(Op::Not)).to(UnaryOp::Not),
            just(Token::Op(Op::Ref)).to(UnaryOp::Ref),
            just(Token::Op(Op::Star)).to(UnaryOp::Deref),
        ))
        .map_with(|t, e| Spanned(t, e.span()));

        let unary = unary_op
            .repeated()
            .foldr(atom, |op, expr| {
                let span = Span::union(op.1, expr.1);

                Spanned(Expr::Unary(op, expr.boxed()), span)
            })
            .boxed();

        let factor_op = choice((
            just(Token::Op(Op::Star)).to(BinaryOp::Multiply),
            just(Token::Op(Op::Slash)).to(BinaryOp::Divide),
        ))
        .map_with(|t, e| Spanned(t, e.span()));

        let factor = unary
            .clone()
            .foldl(factor_op.then(unary).repeated(), |lhs, (op, rhs)| {
                let span = Span::union(lhs.1, rhs.1);

                Spanned(Expr::Binary(lhs.boxed(), op, rhs.boxed()), span)
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

                Spanned(Expr::Binary(lhs.boxed(), op, rhs.boxed()), span)
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

                Spanned(Expr::Binary(lhs.boxed(), op, rhs.boxed()), span)
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

                Spanned(Expr::Binary(lhs.boxed(), op, rhs.boxed()), span)
            })
            .boxed()
    })
    .boxed()
}

fn ident<'tok>() -> impl Parser<'tok, ParserInput<'tok>, Spanned<&'static str>, ParserError<'tok>> {
    select! {
        Token::Variable(name) => name
    }
    .map_with(|name, e| Spanned(name, e.span()))
    .boxed()
}

fn type_parser<'tok>() -> impl Parser<'tok, ParserInput<'tok>, Spanned<Type>, ParserError<'tok>> {
    recursive(|type_| {
        let ptr = select! {
            Token::Variable("ptr") => ()
        }
        .ignore_then(type_.delimited_by(
            just(Token::Op(Op::LessThan)),
            just(Token::Op(Op::GreaterThan)),
        ))
        .map(|ty: Spanned<Type>| Type::Pointer(ty.boxed()));

        let primitive = select! {
            Token::Variable("bool") => Primitive::Boolean,
            Token::Variable("int") => Primitive::Integer,
            Token::Variable("float") => Primitive::Float,
        }
        .map(Type::Primitive)
        .boxed();

        choice((ptr, primitive))
            .map_with(|ty, e| Spanned(ty, e.span()))
            .boxed()
    })
}
