use std::rc::Rc;

use ariadne::Label;
use ariadne::Report;
use ariadne::ReportKind;
use ariadne::Source;
use ast::*;
use chumsky::prelude::*;
use lexer::{Token, TokenKind};
use std::cmp;
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

fn identifier<'a>(t_: StrValueType) -> BoxedParser<'a, Token, Rc<String>, Simple<Token>> {
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
    .boxed()
}

fn comma() -> BoxedParser<'static, Token, Token, Simple<Token>> {
    token(TokenKind::Comma).labelled("comma").boxed()
}

fn lbrace() -> BoxedParser<'static, Token, Token, Simple<Token>> {
    token(TokenKind::LBrace).labelled("lbrace").boxed()
}

fn rbrace() -> BoxedParser<'static, Token, Token, Simple<Token>> {
    token(TokenKind::RBrace).labelled("rbrace").boxed()
}

fn lbracket() -> BoxedParser<'static, Token, Token, Simple<Token>> {
    token(TokenKind::LBracket).labelled("lbracket").boxed()
}

fn rbracket() -> BoxedParser<'static, Token, Token, Simple<Token>> {
    token(TokenKind::RBracket).labelled("rbracket").boxed()
}

fn lparen() -> BoxedParser<'static, Token, Token, Simple<Token>> {
    token(TokenKind::LParen).labelled("lparen").boxed()
}

fn rparen() -> BoxedParser<'static, Token, Token, Simple<Token>> {
    token(TokenKind::RParen).labelled("rparen").boxed()
}

fn colon() -> BoxedParser<'static, Token, Token, Simple<Token>> {
    token(TokenKind::Colon).labelled("colon").boxed()
}

fn assign() -> BoxedParser<'static, Token, Token, Simple<Token>> {
    token(TokenKind::Assign).labelled("assign").boxed()
}

fn unit<'a>() -> BoxedParser<'a, Token, (), Simple<Token>> {
    (lparen().then(rparen())).ignored().labelled("unit").boxed()
}

fn pointer<'a>() -> BoxedParser<'a, Token, bool, Simple<Token>> {
    token(TokenKind::Pointer)
        .or_not()
        .map(|x| x.is_some())
        .labelled("pointer")
        .boxed()
}

fn parser() -> impl Parser<Token, Vec<ASTNode>, Error = Simple<Token>> {
    let ident_token = identifier(StrValueType::Identifier);

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
            ident_token
                .clone()
                .then(
                    token(TokenKind::Dot)
                        .boxed()
                        .ignore_then(ident_token.clone())
                        .map(|value| Type::Type {
                            name: value.clone(),
                            module: None,
                        })
                        .or_not(),
                )
                .map(|(a, x)| match x {
                    Some(x) => match x {
                        Type::Type { name, .. } => Type::Type {
                            name,
                            module: Some(a.clone()),
                        },
                        _ => panic!(),
                    },
                    None => Type::Type {
                        name: a.clone(),
                        module: None,
                    },
                }),
        ))
    })
    .map(|x| TypeDef::Type(x))
    .boxed();

    let ident = choice((
        (pointer()
            .then(ident_token)
            .then(
                recursive(|i| {
                    token(TokenKind::Dot)
                        .boxed()
                        .ignore_then(ident_token.clone())
                        .then_with(i.or_not())
                })
                .repeated(),
            )
            .map(|x| Accessor::Property(value, next))
            .boxed()
            .map(|x| {})
            .labelled("Accessor")
            .boxed()),
        (pointer().then(ident_token.clone()).map(|(is_pointer, s)| {
            if is_pointer {
                IdentifierType::Pointer(s.clone(), None)
            } else {
                IdentifierType::Identifier(s.clone(), None)
            }
        }))
        .labelled("ident")
        .boxed(),
        (lparen()
            .ignore_then(pointer().then(ident_token.clone()).boxed())
            .then_ignore(colon().ignored())
            .then(type_literal.clone())
            .then_ignore(rparen())
            .map(|((is_pointer, s), t)| match t {
                TypeDef::Type(t) => {
                    if is_pointer {
                        IdentifierType::Pointer(s.clone(), Some(t))
                    } else {
                        IdentifierType::Identifier(s.clone(), Some(t))
                    }
                }
                _ => panic!(),
            })
            .labelled("typed_ident"))
        .boxed(),
    ));

    let ident_node = ident.clone().map(|x| ASTNode::Identifier(x));

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
                        IdentifierType::Identifier(value, _) => RecordDefinitionField {
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
            ident_token
                .clone()
                .then(
                    token(TokenKind::Of)
                        .boxed()
                        .ignore_then(choice((
                            record_definition.clone(),
                            tuple_definition.clone(),
                            type_literal.clone(),
                        )))
                        .or_not(),
                )
                .map(|(name, type_)| (name, type_)),
        )
        .repeated()
        .map(|x| TypeDef::VariantDefinition { fields: x })
        .boxed();

    let type_ = token(TokenKind::TypeKeyword)
        .ignore_then(ident_token.clone())
        .then_ignore(assign())
        .then(choice((
            record_definition.clone(),
            tuple_definition.clone(),
            variant_definition.clone(),
            type_literal.clone(),
        )))
        .map(|(value, t_)| ASTNode::TypeDefinition(value, t_))
        .boxed();

    let expr = recursive(|expr| {
        choice((
            ident_node.clone(),
            digit.clone(),
            str_.clone(),
            bool.clone(),
            token(TokenKind::LParen)
                .ignore_then(expr.clone())
                .then_ignore(token(TokenKind::RParen)),
        ))
    });

    let stmt = recursive(|stmt| {
        let record_destructure = (ident
            .clone()
            .separated_by(comma())
            .delimited_by(lbrace(), rbrace()))
        .map(|s| ASTNode::Identifier(IdentifierType::RecordDestructure(s, None)));

        let array_destructure = (ident
            .clone()
            .separated_by(comma())
            .delimited_by(lbracket(), rbracket()))
        .map(|s| ASTNode::Identifier(IdentifierType::ArrayDestructure(s, None)));

        let tuple_destructure = (ident
            .clone()
            .separated_by(comma())
            .delimited_by(lparen(), rparen()))
        .map(|s| ASTNode::Identifier(IdentifierType::TupleDestructure(s, None)));

        let ident_types = choice((
            ident_node.clone(),
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

        let let_stmt = ({
            let val = token(TokenKind::Mut)
                .or_not()
                .map(|x| x.is_some())
                .then(let_identifier)
                .then_ignore(assign())
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
                })
                .boxed();
            let func = {
                let exprs = choice((
                    expr.clone().map(|x| vec![x]).boxed(),
                    lparen()
                        .ignore_then(stmt.clone().repeated())
                        .then_ignore(rparen())
                        .boxed(),
                ))
                .boxed();
                let idents = ident_token
                    .clone()
                    .then(
                        ident
                            .clone()
                            .repeated()
                            .or(unit().clone().map(|_| vec![]))
                            .boxed(),
                    )
                    .boxed();
                let return_type = colon()
                    .ignored()
                    .then(type_literal.clone())
                    .or_not()
                    .boxed();
                idents
                    .then(return_type)
                    .then(assign().ignored())
                    .then(exprs)
                    .map(|((((name, args), return_), _), body)| {
                        ASTNode::FunctionDefinition(FunctionDefinition {
                            name: Some(name),
                            return_type: match return_ {
                                Some((_, t)) => match t {
                                    TypeDef::Type(t) => Some(t),
                                    _ => panic!(),
                                },
                                _ => None,
                            },
                            arguments: args,
                            body,
                        })
                    })
            };
            token(TokenKind::Let).ignore_then(choice((val, func)))
        })
        .boxed();

        let x = choice((let_stmt, type_.clone(), expr.clone())).boxed();
        x
    });

    let import = token(TokenKind::Import).boxed();

    let go_import = (import.clone().ignore_then(str_.clone()))
        .map(|s| match s {
            ASTNode::StringLiteral(value) => ASTNode::GoImport(GoImport {
                module: value,
                alias: None,
            }),
            _ => panic!(),
        })
        .boxed();

    let fungo_import = import
        .clone()
        .ignore_then(ident_token.clone())
        .map(|s| ASTNode::FungoImport(FungoImport { module: s }))
        .boxed();

    let token = choice((
        go_import,
        fungo_import,
        // record_definition.map(|x| ASTNode::TypeDefinition(Rc::new("ASER".to_string()), x)),
        stmt,
    ))
    .map_err(|x| {
        let (token, range, context) = x.found().unwrap();
        let expected = x.expected();
        let mut colors = ariadne::ColorGenerator::new();
        let a = colors.next();
        let b = colors.next();
        let first = range.start;
        let second = range.end;
        Report::build(ReportKind::Error, (context.clone(), first..second))
            .with_code(1)
            .with_note("Unexpected Token")
            .with_label(
                Label::new((context.clone(), range.clone()))
                    .with_message(format!("Unexpected Token: {:?}", token))
                    .with_color(a),
            )
            .with_label(
                Label::new((context.clone(), (cmp::max(first - 20, 0)..second + 20)))
                    .with_message(format!(
                        "expected one of {:?}",
                        expected.into_iter().map(|x| x).collect::<Vec<_>>()
                    ))
                    .with_color(b),
            )
            .finish()
            .print((context.clone(), Source::from(include_str!("../test_file"))))
            .unwrap();
        x
    })
    .boxed();

    token
        .repeated()
        // .recover_with(skip_then_retry_until([any().ignored(), end()]))
        .map(|tokens| {
            let tokens = tokens.into_iter();
            tokens
                .map(|token| {
                    log::debug!("{:?}", token);
                    token
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
                })
                .collect()
        })
}
