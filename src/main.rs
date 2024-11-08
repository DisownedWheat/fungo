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
    let tokens = lexer::lex("./test_file").unwrap();
    let mut count = 0;
    tokens.iter().for_each(move |x| {
        if x.kind == TokenKind::Indent {
            count += 1;
        } else if x.kind == TokenKind::Dedent {
            count -= 1;
        }
        log::info!("Indent: {}, Token:  {:?}", count, x.kind)
    });
    let _ = parser().parse_recovery(tokens);
}

fn token<'a>(kind: TokenKind) -> BoxedParser<'a, Token, Token, Simple<Token>> {
    filter(move |token: &Token| token.kind == kind).boxed()
}

#[derive(Clone, Copy)]
enum StrValueType {
    String,
    Identifier,
    Number,
}

fn identifier<'a>(t_: StrValueType) -> BoxedParser<'a, Token, String, Simple<Token>> {
    filter_map(move |span, token: Token| match (t_, token.kind.clone()) {
        (StrValueType::Identifier, TokenKind::Identifier(s)) => Ok(s),
        (StrValueType::String, TokenKind::StringLiteral(s)) => Ok(s),
        (StrValueType::Number, TokenKind::NumberLiteral(s)) => Ok(s),
        _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
    })
    .labelled("Identifier")
    .boxed()
}

fn indent() -> impl Parser<Token, (), Error = Simple<Token>> {
    token(TokenKind::Indent).ignored()
}

fn dedent() -> impl Parser<Token, (), Error = Simple<Token>> {
    token(TokenKind::Dedent).ignored()
}

fn comma() -> impl Parser<Token, Token, Error = Simple<Token>> {
    token(TokenKind::Comma).labelled("Comma")
}

fn lbrace() -> impl Parser<Token, Token, Error = Simple<Token>> {
    token(TokenKind::LBrace).labelled("LBrace")
}

fn rbrace() -> impl Parser<Token, Token, Error = Simple<Token>> {
    token(TokenKind::RBrace).labelled("RBrace")
}

fn lbracket() -> impl Parser<Token, Token, Error = Simple<Token>> {
    token(TokenKind::LBracket).labelled("LBracket")
}

fn rbracket() -> impl Parser<Token, Token, Error = Simple<Token>> {
    token(TokenKind::RBracket).labelled("RBracket")
}

fn lparen() -> impl Parser<Token, Token, Error = Simple<Token>> {
    token(TokenKind::LParen).labelled("LParen")
}

fn rparen() -> impl Parser<Token, Token, Error = Simple<Token>> {
    token(TokenKind::RParen).labelled("RParen")
}

fn colon() -> impl Parser<Token, Token, Error = Simple<Token>> {
    token(TokenKind::Colon).labelled("Colon")
}

fn assign() -> impl Parser<Token, (), Error = Simple<Token>> {
    token(TokenKind::Assign).labelled("Assign").ignored()
}

fn unit() -> impl Parser<Token, (), Error = Simple<Token>> {
    (lparen().then(rparen())).ignored().labelled("Unit")
}

fn pointer() -> impl Parser<Token, bool, Error = Simple<Token>> {
    token(TokenKind::Pointer)
        .or_not()
        .map(|x| x.is_some())
        .labelled("Pointer")
}

fn import() -> impl Parser<Token, Token, Error = Simple<Token>> {
    token(TokenKind::Import)
}

fn ident_token() -> impl Parser<Token, String, Error = Simple<Token>> {
    identifier(StrValueType::Identifier).labelled("Ident token")
}

fn str_() -> impl Parser<Token, ASTNode, Error = Simple<Token>> {
    identifier(StrValueType::String)
        .map(|s| ASTNode::StringLiteral(s))
        .labelled("String Literal")
}

fn digit() -> impl Parser<Token, ASTNode, Error = Simple<Token>> {
    identifier(StrValueType::Number)
        .map(|s| ASTNode::NumberLiteral(s))
        .labelled("Number Literal")
}

fn bool() -> impl Parser<Token, ASTNode, Error = Simple<Token>> {
    choice((token(TokenKind::True), token(TokenKind::False)))
        .map(|Token { kind, .. }| match kind {
            TokenKind::True => true,
            TokenKind::False => false,
            _ => panic!(),
        })
        .map(|x| ASTNode::BoolLiteral(x))
        .labelled("Bool Literal")
}

fn type_literal() -> impl Parser<Token, TypeDef, Error = Simple<Token>> {
    recursive(|t| {
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
            ident_token()
                .then(
                    token(TokenKind::Dot)
                        .boxed()
                        .ignore_then(ident_token())
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
    .labelled("Type Literal")
}

fn ident() -> impl Parser<Token, IdentifierType, Error = Simple<Token>> {
    let base_ident = pointer()
        .then(ident_token())
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
            .ignore_then(pointer().then(ident_token()))
            .then_ignore(colon())
            .then(type_literal())
            .then_ignore(rparen())
            .map(|((is_pointer, s), t)| match t {
                TypeDef::Type(t) => {
                    if is_pointer {
                        IdentifierType::Pointer(Box::new(IdentifierType::Identifier(s, Some(t))))
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
    let accessor_chain = (token(TokenKind::Dot).ignore_then(ident_token()))
        .repeated()
        .labelled("Accessor Chain")
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
        .labelled("Identifier With Accessors")
}

fn ident_node() -> impl Parser<Token, ASTNode, Error = Simple<Token>> {
    ident()
        .map(|x| ASTNode::Identifier(x))
        .labelled("Base Ident")
}

fn record_definition() -> impl Parser<Token, TypeDef, Error = Simple<Token>> {
    recursive(|r| {
        lbrace()
            .ignore_then(
                ident()
                    .then_ignore(colon())
                    .then(type_literal().or(r.clone()))
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
            .labelled("Record Definition")
    })
}

fn tuple_definition() -> impl Parser<Token, TypeDef, Error = Simple<Token>> {
    recursive(|t| {
        lparen()
            .ignore_then(
                choice((type_literal(), record_definition(), t.clone()))
                    .separated_by(comma().ignored())
                    .map(|x| TypeDef::TupleDefinition {
                        length: x.len(),
                        types: x,
                    }),
            )
            .then_ignore(rparen())
            .labelled("Tuple Definition")
    })
}

fn variant_definition() -> impl Parser<Token, TypeDef, Error = Simple<Token>> {
    token(TokenKind::Pipe)
        .ignore_then(
            ident_token()
                .then(
                    token(TokenKind::Of)
                        .boxed()
                        .ignore_then(choice((
                            record_definition(),
                            tuple_definition(),
                            type_literal(),
                        )))
                        .or_not(),
                )
                .map(|(name, type_)| (name, type_)),
        )
        .repeated()
        .delimited_by(indent().boxed(), dedent().boxed())
        .map(|x| TypeDef::VariantDefinition { fields: x })
        .labelled("Variant Definition")
}

fn type_definition() -> impl Parser<Token, ASTNode, Error = Simple<Token>> {
    token(TokenKind::TypeKeyword)
        .ignore_then(ident_token())
        .then_ignore(assign())
        .then(choice((
            record_definition(),
            tuple_definition(),
            variant_definition(),
            type_literal(),
        )))
        .map(|(value, t_)| ASTNode::TypeDefinition(value, t_))
        .labelled("Type Definition")
}

fn expr<'a>() -> BoxedParser<'a, Token, ASTNode, Simple<Token>> {
    recursive(|expr| {
        let array_literal = (expr.clone().separated_by(comma().boxed()))
            .delimited_by(lbracket().boxed(), rbracket().boxed())
            .map(|x| ASTNode::ArrayLiteral(x))
            .labelled("Array Literal");

        let record_literal = (ident_token().boxed())
            .then_ignore(assign().boxed())
            .then(expr.clone())
            .separated_by(comma().boxed())
            .delimited_by(lbrace().boxed(), rbrace().boxed())
            .map(|x| {
                // log::info!("Record Literal: {:?}", x);
                x
            })
            .map(|x: Vec<(String, ASTNode)>| ASTNode::RecordLiteral {
                fields: x
                    .into_iter()
                    .map(|(name, value)| RecordField { name, value })
                    .collect(),
            })
            .labelled("Record Literal");

        let tuple_literal = ((expr.clone().then_ignore(comma()))
            .then(expr.clone().separated_by(comma()))
            .boxed())
        .delimited_by(lparen().boxed(), rparen().boxed())
        .map(|(first, rest): (ASTNode, Vec<ASTNode>)| {
            let mut v = Vec::with_capacity(rest.len() + 1);
            v.push(first);
            v.extend(rest);
            ASTNode::TupleLiteral(v)
        })
        .labelled("Tuple Literal");

        let func_call = (ident_token().then(expr.clone().repeated()).boxed())
            .map(|(name, arguments)| ASTNode::FunctionCall { name, arguments })
            .labelled("Function Call");

        choice((
            array_literal.boxed(),
            record_literal.boxed(),
            tuple_literal.boxed(),
            digit().boxed(),
            str_().boxed(),
            bool().boxed(),
            unit()
                .map(|_| ASTNode::Identifier(IdentifierType::Unit))
                .boxed(),
            // func_call,
            ident_node().boxed(),
            // expr.clone()
            //     .repeated()
            //     .delimited_by(lparen().boxed(), rparen().boxed())
            //     .map(|x| ASTNode::LogicBlock(x)),
            // indent()
            //     .ignore_then(expr.clone().repeated())
            //     .then_ignore(dedent())
            //     .map(|x| ASTNode::LogicBlock(x)),
            expr.delimited_by(lparen().boxed(), rparen().boxed()),
        ))
        .labelled("Expression")
        .padded_by(token(TokenKind::Comment).or_not().ignored())
    })
    .boxed()
}

fn stmt<'a>() -> BoxedParser<'a, Token, ASTNode, Simple<Token>> {
    // Prepare all the parsers inside the statement function
    let ident = ident().boxed();
    let ident_node = ident_node().boxed();
    let ident_token = ident_token().boxed();
    let comma = comma().boxed();
    let lbrace = lbrace().boxed();
    let rbrace = rbrace().boxed();
    let lparen = lparen().boxed();
    let rparen = rparen().boxed();
    let lbracket = lbracket().boxed();
    let rbracket = rbracket().boxed();
    let type_literal = type_literal().boxed();
    let expr = expr().boxed();
    let assign = assign().boxed();
    let type_ = type_definition().boxed();
    let indent = indent().boxed();
    let dedent = dedent().boxed();

    recursive(move |stmt| {
        let record_destructure = (ident
            .clone()
            .separated_by(comma.clone())
            .delimited_by(lbrace.clone(), rbrace.clone()))
        .map(|s| ASTNode::Identifier(IdentifierType::RecordDestructure(s, None)))
        .labelled("Record Destructure");

        let array_destructure = (ident
            .clone()
            .separated_by(comma.clone())
            .delimited_by(lbracket.clone(), rbracket.clone()))
        .map(|s| ASTNode::Identifier(IdentifierType::ArrayDestructure(s, None)))
        .labelled("Array Destructure");

        let tuple_destructure = (ident
            .clone()
            .separated_by(comma.clone())
            .delimited_by(lparen.clone(), rparen.clone()))
        .map(|s| ASTNode::Identifier(IdentifierType::TupleDestructure(s, None)))
        .labelled("Tuple Destructure");

        let ident_types = choice((
            ident_node.clone(),
            record_destructure,
            array_destructure,
            tuple_destructure,
        ))
        .boxed();

        let typed_ident = lparen
            .clone()
            .ignore_then(ident_types.clone())
            .then_ignore(token(TokenKind::Colon).boxed())
            .then(type_literal.clone())
            .then_ignore(rparen.clone())
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
            .labelled("Typed Ident")
            .boxed();

        let let_identifier = choice((typed_ident.clone(), ident_types.clone())).boxed();

        let let_stmt = ({
            let val = token(TokenKind::Mut)
                .or_not()
                .map(|x| x.is_some())
                .then(let_identifier)
                .then_ignore(assign.clone())
                .then(choice((
                    // expr.clone()
                    //     .repeated()
                    //     .delimited_by(ident.clone(), dedent.clone())
                    //     .map(|x| ASTNode::LogicBlock(x)),
                    expr.clone(),
                )))
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
                .labelled("Let value")
                .boxed();
            let func = {
                let exprs = choice((
                    indent
                        .clone()
                        .ignore_then(stmt.clone().repeated())
                        .then_ignore(dedent.clone())
                        .boxed(),
                    expr.clone().map(|x| vec![x]).boxed(),
                ))
                .map_err(error("Function Expressions"))
                .labelled("Function Body")
                .boxed();

                let idents = ident_token
                    .clone()
                    .then(
                        (unit().boxed().map(|_| vec![]))
                            .or(ident.clone().repeated())
                            .boxed(),
                    )
                    .labelled("Function Arguments")
                    .boxed();

                let return_type = colon()
                    .ignored()
                    .then(type_literal.clone())
                    .or_not()
                    .labelled("Return Type")
                    .boxed();

                idents
                    .then(return_type)
                    .then(assign.clone().ignored())
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
                    .labelled("Function")
            };
            token(TokenKind::Let)
                .ignore_then(choice((val, func)).map_err(error("Inside Let Statement")))
        })
        .labelled("Let Stmt")
        .boxed();

        choice((let_stmt.clone(), type_.clone(), expr.clone()))
            .map_err(|x| {
                log::error!("Inside Statements");
                error_report(x)
            })
            .padded_by(token(TokenKind::Comment).or_not().ignored())
            .labelled("Statement")
            .boxed()
    })
    .boxed()
}

fn go_import() -> impl Parser<Token, ASTNode, Error = Simple<Token>> {
    (import().ignore_then(str_())).map(|s| match s {
        ASTNode::StringLiteral(value) => ASTNode::GoImport(GoImport {
            module: value,
            alias: None,
        }),
        _ => panic!(),
    })
}

fn fungo_import() -> impl Parser<Token, ASTNode, Error = Simple<Token>> {
    import()
        .ignore_then(ident_token())
        .map(|s| ASTNode::FungoImport(FungoImport { module: s }))
}

fn parser() -> impl Parser<Token, Vec<ASTNode>, Error = Simple<Token>> {
    let token_ast = choice((go_import(), fungo_import(), stmt()))
        .padded_by(token(TokenKind::Comment).or_not().ignored())
        .repeated()
        .then_ignore(token(TokenKind::EOF))
        .then_ignore(end())
        .boxed()
        .map_err(error("Inside regular parser"))
        .boxed();

    token_ast.map(|x| {
        let pretty = to_string_pretty(&x).unwrap();
        println!("{}", pretty.cyan());
        x
    })
}

fn error(msg: &str) -> fn(Simple<Token>) -> Simple<Token> {
    log::error!("{}", msg);
    error_report
}

fn error_report(err: Simple<Token>) -> Simple<Token> {
    let Token {
        kind,
        span,
        source,
        file,
    } = err.found().unwrap();
    // let reason = err.reason();
    let expected = err.expected().into_iter().map(|x| x).collect::<Vec<_>>();
    log::error!("Unexpected token: {:?}", kind);
    log::error!("Expected: {:?}", expected);

    let mut colors = ariadne::ColorGenerator::new();
    let a = colors.next();
    let b = colors.next();
    let first = span.start;
    let second = span.end;
    Report::build(ReportKind::Error, (file.clone(), first..second))
        .with_code(1)
        .with_note("Unexpected Token")
        .with_label(
            Label::new((file.clone(), span.clone()))
                .with_message(format!("Unexpected Token: {:?}", kind))
                .with_color(a),
        )
        .with_label(
            Label::new((file.clone(), (cmp::max(first - 10, 0)..second + 10)))
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
        .print((file.clone(), Source::from(source.clone().as_str())))
        .unwrap();
    err
}
