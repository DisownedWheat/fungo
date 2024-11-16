use ariadne::Label;
use ariadne::Report;
use ariadne::ReportKind;
use ariadne::Source;
use ast::*;
use chumsky::prelude::*;
use colored::*;
use lexer::{Token, TokenKind};
use serde_json::to_string_pretty;
mod ast;
mod lexer;

fn main() {
    env_logger::builder()
        .filter_level(log::LevelFilter::Debug)
        .init();
    let tokens = lexer::lex("./test_file").unwrap();
    match parser().parse(tokens) {
        Ok(ast) => {
            let pretty = to_string_pretty(&ast).unwrap();
            println!("{}", pretty.cyan());
        }
        Err(e) => {
            e.into_iter().for_each(|e| {
                error_report(e);
            });
        }
    }
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

fn str_() -> impl Parser<Token, Expr, Error = Simple<Token>> {
    identifier(StrValueType::String)
        .map(|s| Expr::StringLiteral(s))
        .labelled("String Literal")
}

fn digit() -> impl Parser<Token, Expr, Error = Simple<Token>> {
    identifier(StrValueType::Number)
        .map(|s| Expr::IntLiteral(s))
        .labelled("Number Literal")
}

fn bool() -> impl Parser<Token, Expr, Error = Simple<Token>> {
    choice((token(TokenKind::True), token(TokenKind::False)))
        .map(|Token { kind, .. }| match kind {
            TokenKind::True => true,
            TokenKind::False => false,
            _ => panic!(),
        })
        .map(|x| Expr::BoolLiteral(x))
        .labelled("Bool Literal")
}

fn type_literal() -> impl Parser<Token, TypeDef, Error = Simple<Token>> {
    log::trace!("Building type_literal parser");
    let t = recursive(move |t| {
        log::trace!("Moving into Type Literal");
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
    .labelled("Type Literal");
    log::trace!("Completed type literal parser");
    t
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

    choice((
        base_ident.clone(),
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
    .boxed()
}

fn ident_node() -> impl Parser<Token, Expr, Error = Simple<Token>> {
    ident().map(|x| Expr::Identifier(x)).labelled("Base Ident")
}

fn record_definition() -> impl Parser<Token, TypeDef, Error = Simple<Token>> {
    recursive(move |r| {
        log::trace!("Moving into record definition");
        lbrace()
            .ignore_then(
                ident()
                    .then_ignore(colon())
                    .then(type_literal().or(r.clone().boxed()))
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
    recursive(move |t| {
        log::trace!("Moving into tuple defintiion");
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

fn type_definition() -> impl Parser<Token, Stmt, Error = Simple<Token>> {
    token(TokenKind::TypeKeyword)
        .ignore_then(ident_token())
        .then_ignore(assign())
        .then(choice((
            record_definition(),
            tuple_definition(),
            variant_definition(),
            type_literal(),
        )))
        .map(|(value, t_)| Stmt::TypeDefinition(value, t_))
        .labelled("Type Definition")
}

fn expr<'a>(
    stmt: BoxedParser<'a, Token, Stmt, Simple<Token>>,
) -> BoxedParser<'a, Token, Expr, Simple<Token>> {
    log::trace!("Inside the expr parser");
    recursive(move |expr| {
        log::trace!("moving into expr");
        let array_literal = (expr.clone().separated_by(comma().boxed()))
            .delimited_by(lbracket().boxed(), rbracket().boxed())
            .map(|x| Expr::ArrayLiteral(x))
            .labelled("Array Literal");

        log::trace!("Record Literal");
        let record_literal = (ident_token().boxed())
            .then_ignore(assign().boxed())
            .then(expr.clone())
            .separated_by(comma().boxed())
            .delimited_by(lbrace().boxed(), rbrace().boxed())
            .map(|x: Vec<(String, Expr)>| Expr::RecordLiteral {
                fields: x
                    .into_iter()
                    .map(|(name, value)| RecordField { name, value })
                    .collect(),
            })
            .labelled("Record Literal");

        log::trace!("Tuple Literal");
        let tuple_literal = ((expr.clone().then_ignore(comma()))
            .then(expr.clone().separated_by(comma()))
            .boxed())
        .delimited_by(lparen().boxed(), rparen().boxed())
        .map(|(first, rest): (Expr, Vec<Expr>)| {
            let mut v = Vec::with_capacity(rest.len() + 1);
            v.push(first);
            v.extend(rest);
            Expr::TupleLiteral(v)
        })
        .labelled("Tuple Literal");

        log::trace!("Block Expr");
        let block_expr = indent()
            .ignore_then(stmt.repeated())
            .then_ignore(dedent())
            .map(|x| Expr::Block(x))
            .labelled("Block Expression");

        let func_call = (ident_token().then(expr.clone().repeated()).boxed())
            .map(|(name, args)| Expr::FunctionCall { name, args })
            .labelled("Function Call")
            .boxed();

        log::trace!("Paren Expression");
        let paren_expression = lparen()
            .ignore_then(expr.clone())
            .then_ignore(rparen())
            .boxed();

        log::trace!("Expr Choice");
        let accessor_idents = choice((
            array_literal.clone().boxed(),
            record_literal.clone().boxed(),
            tuple_literal.clone().boxed(),
            ident_node().boxed(),
            paren_expression.clone(),
        ));
        let accessor = accessor_idents
            .clone()
            .then((token(TokenKind::Dot).boxed().ignore_then(accessor_idents)).repeated())
            .map(|(first, rest)| {
                let mut v = Vec::with_capacity(rest.len() + 1);
                v.push(first);
                v.extend(rest);
                v.reverse();
                let first = v.pop().unwrap();
                v.into_iter().fold(first, |acc, x| Expr::Accessor {
                    left: Box::new(x),
                    right: Box::new(acc),
                })
            });
        choice((
            accessor.boxed(),
            array_literal.boxed(),
            record_literal.boxed(),
            tuple_literal.boxed(),
            digit().boxed(),
            str_().boxed(),
            bool().boxed(),
            unit()
                .map(|_| Expr::Identifier(IdentifierType::Unit))
                .boxed(),
            func_call,
            ident_node().boxed(),
            block_expr.boxed(),
            paren_expression.boxed(),
        ))
        .labelled("Expression")
        .padded_by(token(TokenKind::Comment).or_not().ignored())
    })
    .boxed()
}

fn stmt() -> impl Parser<Token, Stmt, Error = Simple<Token>> {
    log::trace!("Starting the stmt parser");
    // Prepare all the parsers inside the statement function
    let ident = ident().boxed();
    let ident_node = ident_node().map(Stmt::Expr).boxed();
    let ident_token = ident_token().boxed();
    let comma = comma().boxed();
    let lbrace = lbrace().boxed();
    let rbrace = rbrace().boxed();
    let lparen = lparen().boxed();
    let rparen = rparen().boxed();
    let lbracket = lbracket().boxed();
    let rbracket = rbracket().boxed();
    let type_literal = type_literal().boxed();
    let assign = assign().boxed();
    let type_ = type_definition().boxed();
    let indent = indent().boxed();
    let dedent = dedent().boxed();

    recursive(move |rstmt| {
        log::trace!("Moving into stmt");

        // Because the stmt and expr parsers are mutually recursive the expr parser needs to be
        // instantiated here with the recurst stmt parser
        let expr = expr(rstmt.clone().boxed()).boxed();

        let record_destructure = (ident
            .clone()
            .separated_by(comma.clone())
            .delimited_by(lbrace.clone(), rbrace.clone()))
        .map(|s| Stmt::Expr(Expr::Identifier(IdentifierType::RecordDestructure(s, None))))
        .labelled("Record Destructure");

        let array_destructure = (ident
            .clone()
            .separated_by(comma.clone())
            .delimited_by(lbracket.clone(), rbracket.clone()))
        .map(|s| Stmt::Expr(Expr::Identifier(IdentifierType::ArrayDestructure(s, None))))
        .labelled("Array Destructure");

        let tuple_destructure = (ident
            .clone()
            .separated_by(comma.clone())
            .delimited_by(lparen.clone(), rparen.clone()))
        .map(|s| Stmt::Expr(Expr::Identifier(IdentifierType::TupleDestructure(s, None))))
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
                Stmt::Expr(Expr::Identifier(ident)) => match (ident, def) {
                    (IdentifierType::Identifier(n, _), TypeDef::Type(t)) => {
                        Stmt::Expr(Expr::Identifier(IdentifierType::Identifier(n, Some(t))))
                    }
                    (IdentifierType::RecordDestructure(n, _), TypeDef::Type(t)) => Stmt::Expr(
                        Expr::Identifier(IdentifierType::RecordDestructure(n, Some(t))),
                    ),
                    (IdentifierType::ArrayDestructure(n, _), TypeDef::Type(t)) => Stmt::Expr(
                        Expr::Identifier(IdentifierType::ArrayDestructure(n, Some(t))),
                    ),
                    (IdentifierType::TupleDestructure(n, _), TypeDef::Type(t)) => Stmt::Expr(
                        Expr::Identifier(IdentifierType::TupleDestructure(n, Some(t))),
                    ),
                    (x @ IdentifierType::Bucket, _) => Stmt::Expr(Expr::Identifier(x)),
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
                    rstmt
                        .clone()
                        .repeated()
                        .delimited_by(ident.clone(), dedent.clone())
                        .map(|x| Expr::Block(x)),
                    expr.clone(),
                )))
                .map(|((mutable, name), value)| {
                    let identifier = match name {
                        Stmt::Expr(Expr::Identifier(n)) => n,
                        _ => panic!(),
                    };
                    Stmt::LetStatement(LetExpression {
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
                        .ignore_then(rstmt.clone().repeated())
                        .then_ignore(dedent.clone())
                        .boxed(),
                    expr.clone().map(|x| vec![Stmt::Expr(x)]).boxed(),
                ))
                // .recover_with(skip_parser(expr.clone().map(|x| vec![x])))
                .labelled("Function Body")
                .boxed();

                let idents = ident_token
                    .clone()
                    .then(choice((
                        (unit().boxed().map(|_| vec![])),
                        (ident.clone().repeated()).boxed(),
                    )))
                    .labelled("Function Arguments")
                    .boxed();

                let return_type = colon()
                    .ignore_then(type_literal.clone())
                    .or_not()
                    .labelled("Return Type")
                    .boxed();

                idents
                    .then(return_type)
                    .then(assign.clone().ignored())
                    .then(exprs)
                    .map(|((((name, args), return_), _), body)| {
                        Stmt::FunctionDefinition(FunctionDefinition {
                            name: Some(name),
                            return_type: match return_ {
                                Some(t) => match t {
                                    TypeDef::Type(t) => Some(t),
                                    _ => panic!(),
                                },
                                _ => None,
                            },
                            arguments: args,
                            body,
                        })
                    })
                    .boxed()
            };
            token(TokenKind::Let)
                .ignore_then(choice((val, func)))
                .boxed()
        })
        .labelled("Let Stmt")
        .boxed();

        choice((
            let_stmt.clone(),
            type_.clone(),
            expr.clone().map(Stmt::Expr),
        ))
        .padded_by(token(TokenKind::Comment).or_not().ignored())
        .labelled("Statement")
    })
}

fn go_import() -> impl Parser<Token, TopLevel, Error = Simple<Token>> {
    log::trace!("Go import parser");
    (import().ignore_then(str_())).map(|s| match s {
        Expr::StringLiteral(value) => TopLevel::GoImport(GoImport {
            module: value,
            alias: None,
        }),
        _ => panic!(),
    })
}

fn fungo_import() -> impl Parser<Token, TopLevel, Error = Simple<Token>> {
    log::trace!("FunGo import parser");
    import().ignore_then(ident_token()).map(|s| {
        return TopLevel::FungoImport(FungoImport { module: s });
    })
}

fn parser() -> impl Parser<Token, Vec<TopLevel>, Error = Simple<Token>> {
    let go_import = go_import();
    let fungo_import = fungo_import();
    let stmt = stmt().boxed();
    let top_level_stmt = choice((
        go_import.boxed(),
        fungo_import.boxed(),
        stmt.map(TopLevel::Stmt).boxed(),
    ))
    .padded_by(token(TokenKind::Comment).or_not());
    // .recover_with(skip_parser(stmt().boxed()));

    top_level_stmt
        .repeated()
        .then_ignore(token(TokenKind::EOF))
        .then_ignore(end())
}

fn error_report(err: Simple<Token>) -> Simple<Token> {
    if let Some(Token {
        kind,
        span,
        source,
        file,
    }) = err.found()
    {
        let expected = err.expected().into_iter().map(|x| x).collect::<Vec<_>>();
        log::error!("Unexpected token: {:?}", kind);
        log::error!("Expected: {:?}", expected);

        let mut colors = ariadne::ColorGenerator::new();
        let a = colors.next();
        let first = span.start;
        let second = span.end;
        Report::build(ReportKind::Error, (file.clone(), first..second))
            .with_code(1)
            .with_note("Unexpected Token")
            .with_note(format!("Label: {:?}", err.label()))
            .with_label(
                Label::new((file.clone(), span.clone()))
                    .with_message(format!("Unexpected Token: {:?}", kind))
                    .with_color(a),
            )
            .finish()
            .print((file.clone(), Source::from(source.clone().as_str())))
            .unwrap();
    } else {
        log::error!("Error: {:?}", err);
    }
    err
}
