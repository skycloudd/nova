use crate::{
    ast::{BinaryOp, Expr, Function, Statement, TopLevel, Type, UnaryOp},
    lexer::{Ctrl, Kw, Op, Token},
    span::{Span, Spanned},
};
use chumsky::{input::SpannedInput, prelude::*};

type ParserInput<'tokens, 'src> = SpannedInput<Token<'src>, Span, &'tokens [(Token<'src>, Span)]>;

type ParserOutput<'src> = Vec<Spanned<TopLevel<'src>>>;

type ParserError<'tokens, 'src> = extra::Err<Rich<'tokens, Token<'src>, Span>>;

pub fn parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, ParserOutput<'src>, ParserError<'tokens, 'src>>
{
    toplevel_parser()
        .repeated()
        .collect()
        .then_ignore(end())
        .boxed()
}

fn toplevel_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<TopLevel<'src>>, ParserError<'tokens, 'src>>
{
    function_parser()
}

fn function_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<TopLevel<'src>>, ParserError<'tokens, 'src>>
{
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
                .recover_with(via_parser(nested_delimiters(
                    Token::Ctrl(Ctrl::LeftParen),
                    Token::Ctrl(Ctrl::RightParen),
                    [
                        (Token::Ctrl(Ctrl::LeftCurly), Token::Ctrl(Ctrl::RightCurly)),
                        (Token::Kw(Kw::Do), Token::Kw(Kw::End)),
                    ],
                    |span| Spanned(vec![], span),
                )))
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
        .map(|(((name, params), return_ty), body)| Function {
            name,
            params,
            return_ty,
            body,
        })
        .map_with(|function, e| Spanned(TopLevel::Function(function), e.span()))
        .boxed()
}

#[allow(clippy::too_many_lines)]
fn statement_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Statement<'src>>, ParserError<'tokens, 'src>>
{
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
            let just_if = just(Token::Kw(Kw::If))
                .ignore_then(expr_parser())
                .then_ignore(just(Token::Kw(Kw::Then)))
                .then(statements.clone())
                .then_ignore(just(Token::Kw(Kw::End)))
                .map(|(condition, then_branch)| Statement::If {
                    condition,
                    then_branch,
                    else_branch: None,
                })
                .boxed();

            let if_else = just(Token::Kw(Kw::If))
                .ignore_then(expr_parser())
                .then_ignore(just(Token::Kw(Kw::Then)))
                .then(statements.clone())
                .then_ignore(just(Token::Kw(Kw::Else)))
                .then(statements.clone())
                .then_ignore(just(Token::Kw(Kw::End)))
                .map(|((condition, then_branch), else_branch)| Statement::If {
                    condition,
                    then_branch,
                    else_branch: Some(else_branch),
                })
                .boxed();

            let if_else_if_else = just(Token::Kw(Kw::If))
                .ignore_then(expr_parser())
                .then_ignore(just(Token::Kw(Kw::Then)))
                .then(statements.clone())
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

            choice((if_else_if_else, if_else, just_if))
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
fn expr_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Expr<'src>>, ParserError<'tokens, 'src>>
{
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

        let string = select! {
            Token::String(s) => s
        }
        .map_with(|string, e| Spanned(Expr::String(string), e.span()))
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
                        expr: Spanned(Box::new(expr.0), expr.1),
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
            string,
            parenthesized_expr,
            convert,
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

                Spanned(Expr::Unary(op, Spanned(Box::new(expr.0), expr.1)), span)
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

                Spanned(
                    Expr::Binary(
                        Spanned(Box::new(lhs.0), lhs.1),
                        op,
                        Spanned(Box::new(rhs.0), rhs.1),
                    ),
                    span,
                )
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

                Spanned(
                    Expr::Binary(
                        Spanned(Box::new(lhs.0), lhs.1),
                        op,
                        Spanned(Box::new(rhs.0), rhs.1),
                    ),
                    span,
                )
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

                Spanned(
                    Expr::Binary(
                        Spanned(Box::new(lhs.0), lhs.1),
                        op,
                        Spanned(Box::new(rhs.0), rhs.1),
                    ),
                    span,
                )
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

                Spanned(
                    Expr::Binary(
                        Spanned(Box::new(lhs.0), lhs.1),
                        op,
                        Spanned(Box::new(rhs.0), rhs.1),
                    ),
                    span,
                )
            })
            .boxed()
    })
    .boxed()
}

fn ident<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<&'src str>, ParserError<'tokens, 'src>>
{
    select! {
        Token::Variable(name) => name
    }
    .map_with(|name, e| Spanned(name, e.span()))
    .boxed()
}

fn type_parser<'tokens, 'src: 'tokens>(
) -> impl Parser<'tokens, ParserInput<'tokens, 'src>, Spanned<Type>, ParserError<'tokens, 'src>> {
    select! {
        Token::Variable("bool") => Type::Boolean,
        Token::Variable("int") => Type::Integer,
        Token::Variable("float") => Type::Float,
        Token::Variable("str") => Type::String,

    }
    .map_with(|ty, e| Spanned(ty, e.span()))
    .boxed()
}
