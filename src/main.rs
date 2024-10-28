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

fn token<'a>(kind: TokenKind) -> BoxedParser<'a, Token, Token, Simple<Token>> {
    filter(move |(t, _, _)| t == &kind).boxed()
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

fn colon() -> BoxedParser<'static, Token, Token, Simple<Token>> {
    token(TokenKind::Colon).boxed()
}

fn assign() -> BoxedParser<'static, Token, Token, Simple<Token>> {
    token(TokenKind::Assign).boxed()
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

    let digit = identifier(StrValueType::Number)
        .map(|s| ASTNode::NumberLiteral(s))
        .boxed();

    let bool = choice((token(TokenKind::True), token(TokenKind::False)))
        .map(|(kind, _, _)| match kind {
            TokenKind::True => true,
            TokenKind::False => false,
            _ => panic!(),
        })
        .map(|x| ASTNode::BoolLiteral(x))
        .boxed();

    let type_literal = recursive(|t| {
        choice((
            token(TokenKind::Deref)
                .boxed()
                .ignore_then(t.clone())
                .map(|x| Type::Pointer(Box::new(x))),
            lbracket()
                .ignore_then(rbracket())
                .ignore_then(t.clone())
                .map(|x: Type| Type::Slice(Box::new(x))),
            ident
                .clone()
                .then(
                    token(TokenKind::Dot)
                        .boxed()
                        .ignore_then(ident.clone())
                        .map(|x| match x {
                            ASTNode::Identifier(IdentifierType::Identifier(
                                Identifier { value, .. },
                                _,
                            )) => Type::Type {
                                name: value.clone(),
                                module: None,
                            },
                            _ => panic!(),
                        })
                        .or_not(),
                )
                .map(|(a, x)| {
                    log::debug!("INSIDE THE TYPE {:?} ------ {:?}", a, x);
                    match x {
                        Some(x) => match x {
                            Type::Type { name, .. } => Type::Type {
                                name,
                                module: a.get_ident_name(),
                            },
                            _ => panic!(),
                        },
                        None => match a.get_ident_name() {
                            Some(value) => Type::Type {
                                name: value.clone(),
                                module: None,
                            },
                            _ => panic!(),
                        },
                    }
                }),
        ))
    })
    .map(|x| TypeDef::Type(x))
    .boxed();

    let record_definition = recursive(|r| {
        lbrace()
            .ignore_then(
                ident
                    .clone()
                    .then_ignore(colon())
                    .then(type_literal.clone().or(r.clone()))
                    .repeated(), // .separated_by(comma().ignored()),
            )
            .then_ignore(rbrace())
            .map(|x| RecordDefinition {
                fields: x
                    .into_iter()
                    .map(|(ident, t_)| match ident {
                        ASTNode::Identifier(IdentifierType::Identifier(
                            Identifier { value, .. },
                            _,
                        )) => RecordDefinitionField {
                            name: value,
                            type_: t_,
                        },
                        _ => panic!(),
                    })
                    .collect(),
            })
            .map(|x| TypeDef::RecordDefinition(x))
    })
    .boxed();

    let tuple_definition = recursive(|t| {
        lparen()
            .ignore_then(
                choice((type_literal.clone(), record_definition.clone(), t.clone()))
                    .separated_by(comma().ignored())
                    .map(|x| TypeDef::TupleDefinition {
                        length: x.len(),
                        types: x,
                    }),
            )
            .then_ignore(rparen())
    })
    .boxed();

    let variant_definition = token(TokenKind::Pipe)
        .ignore_then(
            ident
                .clone()
                .then((token(TokenKind::Of)).then(choice((
                    record_definition.clone(),
                    tuple_definition.clone(),
                    type_literal.clone(),
                ))))
                .or_not(),
        )
        .boxed();

    let type_ = token(TokenKind::TypeKeyword)
        .ignore_then(ident.clone())
        .then_ignore(assign())
        .then(choice((
            record_definition.clone(),
            tuple_definition.clone(),
            type_literal.clone(),
        )))
        .map(|(ident, t_)| match ident {
            ASTNode::Identifier(IdentifierType::Identifier(Identifier { value, .. }, _)) => {
                ASTNode::TypeDefinition(value.clone(), t_)
            }
            _ => panic!(),
        });

    let expr = recursive(|expr| {
        choice((
            ident.clone(),
            digit.clone(),
            str_.clone(),
            bool.clone(),
            token(TokenKind::LParen)
                .ignore_then(expr.clone())
                .then_ignore(token(TokenKind::RParen)),
        ))
    });

    let stmt = {
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

        let ident_types = choice((
            ident.clone(),
            record_destructure.clone(),
            array_destructure.clone(),
            tuple_destructure.clone(),
        ))
        .boxed();

        let typed_ident = lparen()
            .ignore_then(ident_types.clone())
            .then_ignore(token(TokenKind::Colon).boxed())
            .then(type_literal.clone())
            .then_ignore(rparen())
            .map(|(node, def)| match node {
                ASTNode::Identifier(ident) => match (ident, def) {
                    (IdentifierType::Identifier(n, _), TypeDef::Type(t)) => {
                        ASTNode::Identifier(IdentifierType::Identifier(n, Some(t)))
                    }
                    (IdentifierType::RecordDestructure(n, _), TypeDef::Type(t)) => {
                        ASTNode::Identifier(IdentifierType::RecordDestructure(n, Some(t)))
                    }
                    (IdentifierType::ArrayDestructure(n, _), TypeDef::Type(t)) => {
                        ASTNode::Identifier(IdentifierType::ArrayDestructure(n, Some(t)))
                    }
                    (IdentifierType::TupleDestructure(n, _), TypeDef::Type(t)) => {
                        ASTNode::Identifier(IdentifierType::TupleDestructure(n, Some(t)))
                    }
                    (x @ IdentifierType::Bucket, _) => ASTNode::Identifier(x),
                    _ => panic!(),
                },

                _ => panic!(),
            })
            .boxed();

        let let_identifier = choice((typed_ident.clone(), ident_types.clone())).boxed();

        let let_stmt = token(TokenKind::Let)
            .ignore_then(token(TokenKind::Mut).or_not().map(|x| x.is_some()))
            .then(let_identifier)
            .then_ignore(token(TokenKind::Assign))
            .then(expr.clone())
            .map(|((mutable, name), value)| {
                let identifier = match name {
                    ASTNode::Identifier(n) => n,
                    _ => panic!(),
                };
                ASTNode::LetExpression(LetExpression {
                    identifier,
                    value: Box::new(value),
                    mutable,
                })
            });
        let_stmt.or(type_.clone()).or(expr.clone())
    };

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
        // record_definition.map(|x| ASTNode::TypeDefinition(Rc::new("ASER".to_string()), x)),
        stmt,
    ))
    .boxed();

    token
        .repeated()
        // .recover_with(skip_then_retry_until([any().ignored(), end()]))
        .map_err(|x| {
            log::debug!("{:?}", x);
            x
        })
        .map(|tokens| {
            // let mut tokens = tokens.into_iter();
            let mut exprs = vec![];
            for token in tokens {
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
