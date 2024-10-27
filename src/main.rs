use std::rc::Rc;

use ast::ASTNode;
use ast::FungoImport;
use ast::GoImport;
use ast::Identifier;
use ast::IdentifierType;
use ast::LetExpression;
use ast::RecordDefinition;
use ast::RecordDefinitionField;
use ast::Type;
use ast::TypeDef;
use chumsky::prelude::*;
use lexer::{Token, TokenKind};
mod ast;
mod lexer;

fn main() {
    env_logger::builder()
        .filter_level(log::LevelFilter::Debug)
        .init();
    println!("{}", log::log_enabled!(log::Level::Debug));
    let tokens = lexer::lex("./test_file")
        .unwrap()
        .into_iter()
        .map(|x| {
            log::debug!("{:?}", x);
            x
        })
        .filter(|x| x.is_ok())
        .map(|x| x.unwrap())
        .collect::<Vec<_>>();
    let _ = parser().parse(tokens);
}

fn token(kind: TokenKind) -> impl Parser<Token, Token, Error = Simple<Token>> {
    filter(move |(t, _, _)| t == &kind)
}

#[derive(Clone, Copy)]
enum StrValueType {
    String,
    Identifier,
    Number,
}

fn identifier(t_: StrValueType) -> impl Parser<Token, Rc<String>, Error = Simple<Token>> {
    filter_map(
        move |span, (kind, inner_span, ctx): Token| match (t_, &kind) {
            (StrValueType::Identifier, TokenKind::Identifier(s)) => Ok(s.clone()),
            (StrValueType::String, TokenKind::StringLiteral(s)) => Ok(s.clone()),
            (StrValueType::Number, TokenKind::NumberLiteral(s)) => Ok(s.clone()),
            _ => Err(Simple::expected_input_found(
                span,
                Vec::new(),
                Some((kind, inner_span, ctx)),
            )),
        },
    )
}

fn comma() -> BoxedParser<'static, Token, Token, Simple<Token>> {
    token(TokenKind::Comma).boxed()
}

fn lbrace() -> BoxedParser<'static, Token, Token, Simple<Token>> {
    token(TokenKind::LBrace).boxed()
}

fn rbrace() -> BoxedParser<'static, Token, Token, Simple<Token>> {
    token(TokenKind::RBrace).boxed()
}

fn lbracket() -> BoxedParser<'static, Token, Token, Simple<Token>> {
    token(TokenKind::LBracket).boxed()
}

fn rbracket() -> BoxedParser<'static, Token, Token, Simple<Token>> {
    token(TokenKind::RBracket).boxed()
}

fn lparen() -> BoxedParser<'static, Token, Token, Simple<Token>> {
    token(TokenKind::LParen).boxed()
}

fn rparen() -> BoxedParser<'static, Token, Token, Simple<Token>> {
    token(TokenKind::RParen).boxed()
}

fn parser() -> impl Parser<Token, Vec<ASTNode>, Error = Simple<Token>> {
    let ident = identifier(StrValueType::Identifier)
        .map(|s| {
            ASTNode::Identifier(IdentifierType::Identifier(
                Identifier { value: s.clone() },
                None,
            ))
        })
        .boxed();

    let str_ = identifier(StrValueType::String)
        .map(|s| ASTNode::StringLiteral(s))
        .boxed();

    let digit = identifier(StrValueType::Number).map(|s| ASTNode::NumberLiteral(s));
    let type_literal = recursive(|t| {
        (token(TokenKind::Deref)
            .boxed()
            .ignore_then(t.clone())
            .map(|x| Type { pointer: true, ..x }))
        .or(lbracket()
            .ignore_then(rbracket())
            .ignore_then(t.clone())
            .map(|x: Type| Type { slice: true, ..x }))
        .or(ident
            .clone()
            .then(
                token(TokenKind::Dot)
                    .boxed()
                    .then(ident.clone())
                    .or_not()
                    .map(|x| match x {
                        None => None,
                        Some((
                            _,
                            ASTNode::Identifier(IdentifierType::Identifier(
                                Identifier { value, .. },
                                _,
                            )),
                        )) => Some(Type {
                            name: value.clone(),
                            module: None,
                            pointer: false,
                            slice: false,
                        }),
                        _ => None,
                    }),
            )
            .map(|(a, x)| {
                log::debug!("{:?}", a);
                match x {
                    Some(x) => x,
                    None => Type {
                        name: Rc::new("unknown".to_string()),
                        module: None,
                        pointer: false,
                        slice: false,
                    },
                }
            }))
    })
    .map(|x| TypeDef::Type(x));

    let record_definition = recursive(|r| {
        ident
            .clone()
            .then_ignore(token(TokenKind::Colon).boxed())
            .then(type_literal.clone().or(r.clone()))
            .separated_by(comma().ignored())
            .delimited_by(lbrace(), rbrace())
            .map(|x| RecordDefinition {
                fields: x
                    .into_iter()
                    .map(|(ident, t_)| match ident {
                        (ASTNode::Identifier(IdentifierType::Identifier(
                            Identifier { value, .. },
                            _,
                        )),) => RecordDefinitionField {
                            name: value,
                            type_: t,
                        },
                        _ => panic!(),
                    })
                    .collect(),
            })
    });

    let type_ = token(TokenKind::TypeKeyword)
        .ignore_then(ident.clone())
        .then_ignore(token(TokenKind::Assign))
        .then(choice((type_literal, record_definition)))
        .map(|(ident, t_)| match ident {
            ASTNode::Identifier(IdentifierType::Identifier(Identifier { value, .. }, _)) => {
                ASTNode::TypeDefinition(value.clone(), t_)
            }
            _ => panic!(),
        });

    let expr = recursive(|expr| any().map(|_| ASTNode::NoOp));

    let stmt = recursive(|stmt| {
        let record_destructure = (ident
            .clone()
            .separated_by(comma())
            .delimited_by(lbrace(), rbrace()))
        .map(|s| {
            let filtered = s
                .into_iter()
                .map(|x| match x {
                    ASTNode::Identifier(ident @ IdentifierType::Identifier(_, _)) => ident,
                    _ => unreachable!(),
                })
                .collect();
            ASTNode::Identifier(IdentifierType::RecordDestructure(filtered, None))
        });

        let array_destructure = (ident
            .clone()
            .separated_by(comma())
            .delimited_by(lbracket(), rbracket()))
        .map(|s| {
            let filtered = s
                .into_iter()
                .map(|x| match x {
                    ASTNode::Identifier(ident @ IdentifierType::Identifier(_, _)) => ident,
                    _ => unreachable!(),
                })
                .collect();
            ASTNode::Identifier(IdentifierType::ArrayDestructure(filtered, None))
        });

        let tuple_destructure = (ident
            .clone()
            .separated_by(comma())
            .delimited_by(lparen(), rparen()))
        .map(|s| {
            let filtered = s
                .into_iter()
                .map(|x| match x {
                    ASTNode::Identifier(ident @ IdentifierType::Identifier(_, _)) => ident,
                    _ => unreachable!(),
                })
                .collect();
            ASTNode::Identifier(IdentifierType::TupleDestructure(filtered, None))
        });

        let let_identifier = choice((
            ident.clone(),
            record_destructure.clone(),
            array_destructure.clone(),
            tuple_destructure.clone(),
        ))
        .then(token(TokenKind::Colon).or_not().map(|x| x.is_some()));

        let let_stmt = token(TokenKind::Let)
            .ignore_then(token(TokenKind::Mut).or_not().map(|x| x.is_some()))
            .then(let_identifier)
            .then_ignore(token(TokenKind::Assign))
            .then(expr.clone())
            .map(
                |((mutable, (name, _)), rhs): ((bool, (ASTNode, bool)), ASTNode)| {
                    let identifier = match name {
                        ASTNode::Identifier(n) => n,
                        _ => panic!(),
                    };
                    ASTNode::LetExpression(LetExpression {
                        identifier,
                        value: Box::new(rhs),
                        mutable,
                    })
                },
            );
        let_stmt.or(type_).or(expr.clone())
    });

    let import = token(TokenKind::Import).boxed();

    let go_import = (import.clone().ignore_then(str_.clone())).map(|s| match s {
        ASTNode::StringLiteral(value) => ASTNode::GoImport(GoImport {
            module: value,
            alias: None,
        }),
        _ => panic!(),
    });

    let fungo_import = import.clone().ignore_then(ident.clone()).map(|s| match s {
        ASTNode::Identifier(IdentifierType::Identifier(Identifier { value, .. }, _)) => {
            ASTNode::FungoImport(FungoImport { module: value })
        }
        _ => panic!(),
    });

    let token = choice((
        go_import,
        fungo_import,
        stmt,
        str_,
        digit,
        ident,
        expr.clone(),
    ));

    token
        // .recover_with(skip_then_retry_until([any().ignored(), end()]))
        .repeated()
        .map(|tokens| {
            let mut tokens = tokens.into_iter();
            let mut exprs = vec![];
            while let Some(token) = tokens.next() {
                println!("{:?}", token);
                exprs.push(token);
                // match token {
                //     ASTNode::GoImport(value) => {
                //         exprs.push(ASTNode::GoImport(value));
                //     }
                //     ASTNode::Var(value) => {
                //         if let Some(Expr::GoImport(_)) = tokens.next() {
                //             exprs.push(Expr::GoImport(value));
                //         } else {
                //             exprs.push(Expr::Var(value));
                //         }
                //     }
                //     Expr::Ident(value) => exprs.push(Expr::Ident(value)),
                //     Expr::FungoImport(value) => exprs.push(Expr::FungoImport(value)),
                //     _ => panic!("unexpected token {:?}", token),
                // }
            }
            exprs
        })
}
