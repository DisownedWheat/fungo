use std::rc::Rc;

use ariadne::Label;
use ariadne::Report;
use ariadne::ReportKind;
use ariadne::Source;
use ast::*;
use chumsky::prelude::*;
use colored::*;
use lexer::{Token, TokenKind};
use serde_json::to_string_pretty;
use std::cmp;
mod ast;
mod lexer;

fn main() {
    env_logger::builder()
        .filter_level(log::LevelFilter::Debug)
        .init();
    let tokens = lexer::lex("./test_file")
        .unwrap()
        .into_iter()
        .filter(|x| x.is_ok())
        .map(|x| x.unwrap())
        .collect::<Vec<_>>();
    let _ = parser().parse(tokens);
}

fn token<'a>(kind: TokenKind) -> BoxedParser<'a, Token, Token, Simple<Token>> {
    filter(move |(t, _, _, _)| t == &kind).boxed()
}

#[derive(Clone, Copy)]
enum StrValueType {
    String,
    Identifier,
    Number,
}

fn identifier<'a>(t_: StrValueType) -> BoxedParser<'a, Token, String, Simple<Token>> {
    filter_map(
        move |span, (kind, inner_span, source, ctx): Token| match (t_, kind.clone()) {
            (StrValueType::Identifier, TokenKind::Identifier(s)) => Ok(s),
            (StrValueType::String, TokenKind::StringLiteral(s)) => Ok(s),
            (StrValueType::Number, TokenKind::NumberLiteral(s)) => Ok(s),
            _ => Err(Simple::expected_input_found(
                span,
                Vec::new(),
                Some((kind, inner_span, source, ctx)),
            )),
        },
    )
    .labelled("Identifier")
    .boxed()
}

fn comma() -> BoxedParser<'static, Token, Token, Simple<Token>> {
    token(TokenKind::Comma).labelled("Comma").boxed()
}

fn lbrace() -> BoxedParser<'static, Token, Token, Simple<Token>> {
    token(TokenKind::LBrace).labelled("LBrace").boxed()
}

fn rbrace() -> BoxedParser<'static, Token, Token, Simple<Token>> {
    token(TokenKind::RBrace).labelled("RBrace").boxed()
}

fn lbracket() -> BoxedParser<'static, Token, Token, Simple<Token>> {
    token(TokenKind::LBracket).labelled("LBracket").boxed()
}

fn rbracket() -> BoxedParser<'static, Token, Token, Simple<Token>> {
    token(TokenKind::RBracket).labelled("RBracket").boxed()
}

fn lparen() -> BoxedParser<'static, Token, Token, Simple<Token>> {
    token(TokenKind::LParen).labelled("LParen").boxed()
}

fn rparen() -> BoxedParser<'static, Token, Token, Simple<Token>> {
    token(TokenKind::RParen).labelled("RParen").boxed()
}

fn colon() -> BoxedParser<'static, Token, Token, Simple<Token>> {
    token(TokenKind::Colon).labelled("Colon").boxed()
}

fn assign() -> BoxedParser<'static, Token, (), Simple<Token>> {
    let x = token(TokenKind::Assign)
        .labelled("Assign")
        .ignored()
        .boxed();
    x
}

fn unit<'a>() -> BoxedParser<'a, Token, (), Simple<Token>> {
    (lparen().then(rparen())).ignored().labelled("Unit").boxed()
}

fn pointer<'a>() -> BoxedParser<'a, Token, bool, Simple<Token>> {
    token(TokenKind::Pointer)
        .or_not()
        .map(|x| x.is_some())
        .labelled("Pointer")
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
        .map(|(kind, _, _, _)| match kind {
            TokenKind::True => true,
            TokenKind::False => false,
            _ => panic!(),
        })
        .map(|x| ASTNode::BoolLiteral(x))
        .boxed();

    let type_literal = recursive(|t| {
        choice((
            unit().map(|_| Type::Unit),
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

    let ident = {
        let base_ident = pointer()
            .then(ident_token.clone())
            .map(|(is_pointer, s)| {
                if is_pointer {
                    IdentifierType::Pointer(Box::new(IdentifierType::Identifier(s, None)))
                } else {
                    IdentifierType::Identifier(s, None)
                }
            })
            .labelled("Ident")
            .boxed();

        let ident = choice((
            base_ident.clone(),
            // Regular identifier with optional pointer
            // Typed identifier with optional pointer
            lparen()
                .ignore_then(pointer().then(ident_token.clone()))
                .then_ignore(colon())
                .then(type_literal.clone())
                .then_ignore(rparen())
                .map(|((is_pointer, s), t)| match t {
                    TypeDef::Type(t) => {
                        if is_pointer {
                            IdentifierType::Pointer(Box::new(IdentifierType::Identifier(
                                s,
                                Some(t),
                            )))
                        } else {
                            IdentifierType::Identifier(s, Some(t))
                        }
                    }
                    _ => panic!("Expected type in typed identifier"),
                })
                .labelled("Typed_Ident"),
        ))
        .boxed();

        // Property accessor chain
        let accessor_chain = (token(TokenKind::Dot).ignore_then(ident_token.clone()))
            .repeated()
            .boxed();

        // Combine base identifier with optional accessor chain
        ident
            .clone()
            .then(accessor_chain)
            .map(|(base, accessors)| {
                if accessors.is_empty() {
                    base
                } else {
                    // Convert chain of accessors into nested Accessor types
                    let initial = IdentifierType::Accessor {
                        left: Box::new(base),
                        right: None,
                    };
                    accessors
                        .into_iter()
                        .fold(initial, |acc, accessor| match acc {
                            IdentifierType::Accessor { right, left } => {
                                if right.is_some() {
                                    IdentifierType::Accessor {
                                        left: Box::new(IdentifierType::Accessor { left, right }),
                                        right: Some(accessor),
                                    }
                                } else {
                                    IdentifierType::Accessor {
                                        left,
                                        right: Some(accessor),
                                    }
                                }
                            }
                            _ => unreachable!(),
                        })
                }
            })
            .labelled("Identifier_With_Accessors")
    };

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
        let array_literal = (expr.clone().separated_by(comma()).boxed())
            .delimited_by(lbracket(), rbracket())
            .map(|x| ASTNode::ArrayLiteral(x));
        let record_literal = (ident_token
            .clone()
            .then_ignore(assign())
            .then(expr.clone())
            .separated_by(comma()))
        .delimited_by(lbrace(), rbrace())
        .map(|x: Vec<(String, ASTNode)>| ASTNode::RecordLiteral {
            fields: x
                .into_iter()
                .map(|(name, value)| RecordField { name, value })
                .collect(),
        });

        choice((
            array_literal,
            record_literal,
            ident_node.clone(),
            digit.clone(),
            str_.clone(),
            unit().map(|_| ASTNode::Identifier(IdentifierType::Unit)),
            bool.clone(),
            token(TokenKind::LParen)
                .ignore_then(expr.clone())
                .then_ignore(token(TokenKind::RParen)),
        ))
        .padded_by(token(TokenKind::Comment).or_not().ignored())
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
                        (unit().clone().map(|_| vec![]))
                            .or(ident.clone().repeated())
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

        let x = choice((let_stmt, type_.clone(), expr.clone()))
            .padded_by(token(TokenKind::Comment).or_not().ignored())
            .boxed();
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

    let token_ast = choice((go_import, fungo_import, stmt))
        .padded_by(token(TokenKind::Comment).or_not().ignored())
        .repeated()
        .then_ignore(token(TokenKind::EOF))
        .then_ignore(end())
        .map_err(|x| {
            let (token, range, source, context) = x.found().unwrap();
            log::error!("Unexpected token: {:?}", token);
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
                            "expected one of {}",
                            expected
                                .into_iter()
                                .map(|x| match x {
                                    Some(kind) => format!("{:?}", kind),
                                    None => "end of input".to_string(),
                                })
                                .collect::<Vec<_>>()
                                .join(", ")
                        ))
                        .with_color(b),
                )
                .finish()
                .print((context.clone(), Source::from(source.clone().as_str())))
                .unwrap();
            x
        });

    token_ast.map(|tokens| {
        let tokens = tokens.into_iter();
        tokens
            .map(|token| {
                let pretty = to_string_pretty(&token).unwrap();
                println!("{}", pretty.cyan());
                token
            })
            .collect()
    })
}
