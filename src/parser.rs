use super::ast::*;
use super::lexer::{Token, TokenKind};
use ariadne::{Label, Report, ReportKind, Source};
use chumsky::prelude::*;

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

fn newline<'a>() -> BoxedParser<'a, Token, (), Simple<Token>> {
    token(TokenKind::NewLine)
        .ignored()
        .labelled("Newline")
        .boxed()
}

fn indent<'a>(ignore_newline: bool) -> BoxedParser<'a, Token, (), Simple<Token>> {
    if ignore_newline {
        token(TokenKind::Indent)
            .ignored()
            .labelled("Indent")
            .boxed()
    } else {
        (newline().or_not())
            .ignore_then(token(TokenKind::Indent))
            .ignored()
            .labelled("Indent")
            .boxed()
    }
}

fn dedent<'a>(ignore_newline: bool) -> BoxedParser<'a, Token, (), Simple<Token>> {
    if ignore_newline {
        token(TokenKind::Dedent)
            .ignored()
            .labelled("Dedent")
            .boxed()
    } else {
        (newline().or_not())
            .ignore_then(token(TokenKind::Dedent))
            .ignored()
            .labelled("Dedent")
            .boxed()
    }
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

fn semicolon() -> impl Parser<Token, (), Error = Simple<Token>> {
    token(TokenKind::SemiColon).labelled("Semicolon").ignored()
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
    token(TokenKind::Import).labelled("import")
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
    let t = recursive(move |t| {
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
    let newline = newline().boxed();
    recursive(move |r| {
        (indent(false).or_not())
            .ignore_then(lbrace())
            .ignore_then(
                choice((
                    newline
                        .clone()
                        .ignore_then(indent(true))
                        .ignore_then(
                            ident()
                                .then_ignore(colon())
                                .then(type_literal().or(r.clone().boxed()))
                                .separated_by(newline.clone()),
                        )
                        .then_ignore(dedent(false).or_not()),
                    ident()
                        .then_ignore(colon())
                        .then(type_literal().or(r.clone().boxed()))
                        .separated_by(semicolon()),
                ))
                .boxed(),
            )
            .then_ignore(rbrace())
            .then_ignore(choice((newline, dedent(false))).or_not())
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
        choice((
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
                .then_ignore(newline().or_not()),
            indent(true).ignore_then(
                lparen()
                    .ignore_then(
                        choice((type_literal(), record_definition(), t.clone()))
                            .separated_by(
                                (comma().then(newline()).ignored())
                                    .or(newline().then(comma()).ignored()),
                            )
                            .map(|x| TypeDef::TupleDefinition {
                                length: x.len(),
                                types: x,
                            }),
                    )
                    .then_ignore(rparen())
                    .then_ignore(dedent(true)),
            ),
        ))
        .labelled("Tuple Definition")
    })
}

fn variant_definition() -> impl Parser<Token, TypeDef, Error = Simple<Token>> {
    choice((
        indent(false)
            .ignore_then(
                token(TokenKind::Pipe)
                    .ignore_then(ident_token())
                    .then(
                        token(TokenKind::Of)
                            .ignore_then(choice((
                                record_definition(),
                                tuple_definition(),
                                type_literal(),
                            )))
                            .or_not(),
                    )
                    .then_ignore(newline())
                    .map(|x| {
                        log::debug!("Inside the indented variant type thingo");
                        x
                    })
                    .repeated()
                    .at_least(1),
            )
            .map(|x| {
                log::debug!("Passed from variant, now checking dedent");
                x
            })
            .then_ignore(dedent(true))
            .debug("Something in here?")
            .map(|x| {
                log::debug!("We have found the dedent");
                x
            }),
        token(TokenKind::Pipe)
            .ignore_then(
                ident_token().then(
                    token(TokenKind::Of)
                        .ignore_then(choice((
                            record_definition(),
                            tuple_definition(),
                            type_literal(),
                        )))
                        .or_not(),
                ),
            )
            .repeated()
            .at_least(1),
    ))
    .map(|x| {
        log::debug!("Variant has been built");
        TypeDef::VariantDefinition { fields: x }
    })
    .labelled("Variant Definition")
}

fn type_definition() -> impl Parser<Token, Stmt, Error = Simple<Token>> {
    token(TokenKind::TypeKeyword)
        .map(|x| {
            log::debug!("Found type keyword");
            x
        })
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
    recursive(move |expr| {
        let array_literal = (expr.clone().separated_by(comma().boxed()))
            .delimited_by(lbracket().boxed(), rbracket().boxed())
            .map(|x| Expr::ArrayLiteral(x))
            .labelled("Array Literal");

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

        let block_expr = indent(false)
            .ignore_then(stmt.repeated())
            .then_ignore(dedent(false))
            .map(|x| Expr::Block(x))
            .labelled("Block Expression");

        let paren_expression = lparen()
            .ignore_then(expr.clone())
            .then_ignore(rparen())
            .boxed()
            .labelled("Paren Expression");

        let atom = choice((
            array_literal.clone().boxed(),
            record_literal.clone().boxed(),
            tuple_literal.clone().boxed(),
            ident_node().boxed(),
            paren_expression.clone(),
        ))
        .labelled("Atom");

        let base_expr = atom
            .clone()
            .then(
                (token(TokenKind::Dot).boxed().ignore_then(atom))
                    .repeated()
                    .or_not(),
            )
            .map(|(first, chain)| match chain {
                None => first,
                Some(rest) => {
                    let mut v = Vec::with_capacity(rest.len() + 1);
                    v.push(first);
                    v.extend(rest.into_iter());
                    let next = v.pop().unwrap();
                    let rev = v.into_iter().rev().collect::<Vec<_>>();
                    rev.into_iter().fold(next, |acc, x| Expr::Accessor {
                        left: Box::new(x),
                        right: Box::new(acc),
                    })
                }
            })
            .labelled("Base Expr");

        let func_call = ident_token()
            .then(choice((
                indent(false)
                    .ignore_then(expr.clone().separated_by(newline()))
                    .then_ignore(dedent(false)),
                expr.clone()
                    .repeated()
                    .at_least(1)
                    .then_ignore(newline())
                    .boxed(),
            )))
            .map(|(name, args)| Expr::FunctionCall { name, args })
            .labelled("Function Call");

        choice([
            func_call.boxed(),
            base_expr.boxed(),
            digit().boxed(),
            str_().boxed(),
            bool().boxed(),
            unit()
                .map(|_| Expr::Identifier(IdentifierType::Unit))
                .boxed(),
            block_expr.boxed(),
        ])
        .labelled("Expression")
        .padded_by(token(TokenKind::Comment).or_not().ignored())
    })
    .labelled("Expression")
    .boxed()
}

fn stmt() -> impl Parser<Token, Stmt, Error = Simple<Token>> {
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
    let indent = indent(false).boxed();
    let dedent = dedent(false).boxed();

    recursive(move |rstmt| {
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
        .map(|x| {
            log::debug!("Finished stmt");
            x
        })
        .padded_by(token(TokenKind::Comment).or_not().ignored())
        .labelled("Statement")
    })
}

fn go_import() -> impl Parser<Token, TopLevel, Error = Simple<Token>> {
    (import().ignore_then(str_()).then_ignore(newline())).map(|s| match s {
        Expr::StringLiteral(value) => TopLevel::GoImport(GoImport {
            module: value,
            alias: None,
        }),
        _ => panic!(),
    })
}

fn fungo_import() -> impl Parser<Token, TopLevel, Error = Simple<Token>> {
    import()
        .ignore_then(ident_token())
        .then_ignore(newline())
        .map(|s| {
            return TopLevel::FungoImport(FungoImport { module: s });
        })
}

pub fn parser() -> impl Parser<Token, Vec<TopLevel>, Error = Simple<Token>> {
    let go_import = go_import();
    let fungo_import = fungo_import();
    let stmt = stmt().boxed();
    let top_level_stmt = choice((
        go_import.boxed(),
        fungo_import.boxed(),
        stmt.map(TopLevel::Stmt).boxed(),
    ))
    .padded_by(token(TokenKind::Comment).or_not());

    top_level_stmt
        .repeated()
        .then_ignore(token(TokenKind::EOF))
        .then_ignore(end())
}

pub fn error_report(err: &Simple<Token>) {
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
        // let first = span.start;
        // let second = span.end;
        // let first_i = first as isize;
        // let second_i = second as isize;
        // let mapped_span =
        //     (std::cmp::max(first_i - 50, 0) as usize)..(std::cmp::max(second_i + 50, 0) as usize);

        let label = err.label().unwrap_or("");
        Report::build(ReportKind::Error, (file.clone(), span.clone()))
            .with_code(1)
            .with_note("Unexpected Token")
            .with_note(format!("Combinator Label: {:?}", label))
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
}

#[cfg(test)]
mod test {
    use super::super::lexer;
    use super::*;
    use std::sync::Once;
    static INIT: Once = Once::new();

    fn setup() {
        INIT.call_once(|| {
            let _ = env_logger::builder()
                .filter_level(log::LevelFilter::Debug)
                .try_init();
        })
    }

    fn map_kinds(tokens: &Vec<lexer::Token>) -> Vec<lexer::TokenKind> {
        tokens.iter().map(|x| x.kind.clone()).collect()
    }

    #[test]
    fn test_func_calls() {
        setup();
        let input = "
x 1 2
y
	5
	4
	\"Hello World\"
";
        let tokens = lexer::lex_raw(input);
        assert!(tokens.is_ok());
        let expected_tokens = vec![
            TokenKind::Identifier("x".to_string()),
            TokenKind::NumberLiteral("1".to_string()),
            TokenKind::NumberLiteral("2".to_string()),
            TokenKind::NewLine,
            TokenKind::Identifier("y".to_string()),
            TokenKind::NewLine,
            TokenKind::Indent,
            TokenKind::NumberLiteral("5".to_string()),
            TokenKind::NewLine,
            TokenKind::NumberLiteral("4".to_string()),
            TokenKind::NewLine,
            TokenKind::StringLiteral("\"Hello World\"".to_string()),
            TokenKind::NewLine,
            TokenKind::Dedent,
            TokenKind::EOF,
        ];
        assert_eq!(map_kinds(&tokens.as_ref().unwrap()), expected_tokens);

        let output = parser().parse(tokens.unwrap()).inspect_err(|errs| {
            errs.iter().for_each(|e| error_report(e));
        });
        assert!(output.is_ok());
        let expected_output = vec![
            TopLevel::Stmt(Stmt::Expr(Expr::FunctionCall {
                name: "x".to_owned(),
                args: vec![
                    Expr::IntLiteral("1".to_owned()),
                    Expr::IntLiteral("2".to_owned()),
                ],
            })),
            TopLevel::Stmt(Stmt::Expr(Expr::FunctionCall {
                name: "y".to_owned(),
                args: vec![
                    Expr::IntLiteral("5".to_owned()),
                    Expr::IntLiteral("4".to_owned()),
                    Expr::StringLiteral("\"Hello World\"".to_owned()),
                ],
            })),
        ];
        let unwrapped = output.unwrap();
        assert_eq!(unwrapped, expected_output);
    }

    #[test]
    fn test_opens() {
        setup();
        let input = "
open \"fmt\"
open Test
testCall
	(1, 2)
	3
	4
";
        let expected_tokens = vec![
            TokenKind::Import,
            TokenKind::StringLiteral("\"fmt\"".to_string()),
            TokenKind::NewLine,
            TokenKind::Import,
            TokenKind::Identifier("Test".to_string()),
            TokenKind::NewLine,
            TokenKind::Identifier("testCall".to_string()),
            TokenKind::NewLine,
            TokenKind::Indent,
            TokenKind::LParen,
            TokenKind::NumberLiteral("1".to_string()),
            TokenKind::Comma,
            TokenKind::NumberLiteral("2".to_string()),
            TokenKind::RParen,
            TokenKind::NewLine,
            TokenKind::NumberLiteral("3".to_string()),
            TokenKind::NewLine,
            TokenKind::NumberLiteral("4".to_string()),
            TokenKind::NewLine,
            TokenKind::Dedent,
            TokenKind::EOF,
        ];

        let tokens_result = lexer::lex_raw(input);
        assert!(tokens_result.is_ok());
        let tokens = tokens_result.unwrap();
        let kinds = map_kinds(&tokens);
        assert_eq!(kinds, expected_tokens);

        let output_result = parser().parse(tokens);
        assert!(output_result.is_ok());
        let output = output_result.unwrap();

        let expected_output = vec![
            TopLevel::GoImport(GoImport {
                module: "\"fmt\"".to_string(),
                alias: None,
            }),
            TopLevel::FungoImport(FungoImport {
                module: "Test".to_string(),
            }),
            TopLevel::Stmt(Stmt::Expr(Expr::FunctionCall {
                name: "testCall".to_string(),
                args: vec![
                    Expr::TupleLiteral(vec![
                        Expr::IntLiteral("1".to_string()),
                        Expr::IntLiteral("2".to_string()),
                    ]),
                    Expr::IntLiteral("3".to_string()),
                    Expr::IntLiteral("4".to_string()),
                ],
            })),
        ];

        assert_eq!(output, expected_output);
    }

    #[test]
    fn block_expression() {
        setup();
        let input = "
let x =
	testFunc 0 1 \"a\"
";
        let token_result = lexer::lex_raw(input);
        assert!(token_result.is_ok());
        let tokens = token_result.unwrap();

        let expected_tokens = vec![
            TokenKind::Let,
            TokenKind::Identifier("x".to_string()),
            TokenKind::Assign,
            TokenKind::NewLine,
            TokenKind::Indent,
            TokenKind::Identifier("testFunc".to_string()),
            TokenKind::NumberLiteral("0".to_string()),
            TokenKind::NumberLiteral("1".to_string()),
            TokenKind::StringLiteral("\"a\"".to_string()),
            TokenKind::NewLine,
            TokenKind::Dedent,
            TokenKind::EOF,
        ];

        let kinds: Vec<TokenKind> = map_kinds(&tokens);
        assert_eq!(kinds, expected_tokens);

        let output_result = parser().parse(tokens);
        assert!(output_result.is_ok());
        let output = output_result.unwrap();
        let expected_output = vec![TopLevel::Stmt(Stmt::LetStatement(LetExpression {
            identifier: IdentifierType::Identifier("x".to_string(), None),
            mutable: false,
            value: Box::new(Expr::Block(vec![Stmt::Expr(Expr::FunctionCall {
                name: "testFunc".to_string(),
                args: vec![
                    Expr::IntLiteral("0".to_string()),
                    Expr::IntLiteral("1".to_string()),
                    Expr::StringLiteral("\"a\"".to_string()),
                ],
            })])),
        }))];

        assert_eq!(output, expected_output);
    }

    #[test]
    fn type_definitions() {
        setup();
        let parser_ = parser();
        {
            let input = "
type Testing = {x: int; y: int}
type TestRecord = {
	TestVal: []*int
	AnotherTest: {
		InteriorTest: string
	}
}
";

            let token_result = lexer::lex_raw(input);
            assert!(token_result.is_ok());
            let tokens = token_result.unwrap();
            let kinds = map_kinds(&tokens);
            let expected_tokens = vec![
                TokenKind::TypeKeyword,
                TokenKind::Identifier("Testing".to_string()),
                TokenKind::Assign,
                TokenKind::LBrace,
                TokenKind::Identifier("x".to_string()),
                TokenKind::Colon,
                TokenKind::Identifier("int".to_string()),
                TokenKind::SemiColon,
                TokenKind::Identifier("y".to_string()),
                TokenKind::Colon,
                TokenKind::Identifier("int".to_string()),
                TokenKind::RBrace,
                TokenKind::NewLine,
                TokenKind::TypeKeyword,
                TokenKind::Identifier("TestRecord".to_string()),
                TokenKind::Assign,
                TokenKind::LBrace,
                TokenKind::NewLine,
                TokenKind::Indent,
                TokenKind::Identifier("TestVal".to_string()),
                TokenKind::Colon,
                TokenKind::LBracket,
                TokenKind::RBracket,
                TokenKind::Deref,
                TokenKind::Identifier("int".to_string()),
                TokenKind::NewLine,
                TokenKind::Identifier("AnotherTest".to_string()),
                TokenKind::Colon,
                TokenKind::LBrace,
                TokenKind::NewLine,
                TokenKind::Indent,
                TokenKind::Identifier("InteriorTest".to_string()),
                TokenKind::Colon,
                TokenKind::Identifier("string".to_string()),
                TokenKind::NewLine,
                TokenKind::Dedent,
                TokenKind::RBrace,
                TokenKind::NewLine,
                TokenKind::Dedent,
                TokenKind::RBrace,
                TokenKind::NewLine,
                TokenKind::EOF,
            ];
            assert_eq!(kinds, expected_tokens);
            let output_result = parser_.parse(tokens);
            let _ = output_result
                .as_ref()
                .map_err(|x| x.iter().for_each(error_report));
            assert!(output_result.is_ok());
            let output = output_result.unwrap();
            let expected_output = vec![
                TopLevel::Stmt(Stmt::TypeDefinition(
                    "Testing".to_string(),
                    TypeDef::RecordDefinition(RecordDefinition {
                        fields: vec![
                            RecordDefinitionField {
                                name: "x".to_string(),
                                type_: TypeDef::Type(Type::Type {
                                    name: "int".to_string(),
                                    module: None,
                                }),
                            },
                            RecordDefinitionField {
                                name: "y".to_string(),
                                type_: TypeDef::Type(Type::Type {
                                    name: "int".to_string(),
                                    module: None,
                                }),
                            },
                        ],
                    }),
                )),
                TopLevel::Stmt(Stmt::TypeDefinition(
                    "TestRecord".to_string(),
                    TypeDef::RecordDefinition(RecordDefinition {
                        fields: vec![
                            RecordDefinitionField {
                                name: "TestVal".to_string(),
                                type_: TypeDef::Type(Type::Slice(Box::new(Type::Pointer(
                                    Box::new(Type::Type {
                                        name: "int".to_string(),
                                        module: None,
                                    }),
                                )))),
                            },
                            RecordDefinitionField {
                                name: "AnotherTest".to_string(),
                                type_: TypeDef::RecordDefinition(RecordDefinition {
                                    fields: vec![RecordDefinitionField {
                                        name: "InteriorTest".to_string(),
                                        type_: TypeDef::Type(Type::Type {
                                            name: "string".to_string(),
                                            module: None,
                                        }),
                                    }],
                                }),
                            },
                        ],
                    }),
                )),
            ];
            assert_eq!(output, expected_output);
        }
        {
            let input = "
type TestTuple = (int, string, TestRecord)
";
            let token_result = lexer::lex_raw(input);
            assert!(token_result.is_ok());
            let tokens = token_result.unwrap();
            let kinds = map_kinds(&tokens);
            let expected_tokens = vec![
                TokenKind::TypeKeyword,
                TokenKind::Identifier("TestTuple".to_string()),
                TokenKind::Assign,
                TokenKind::LParen,
                TokenKind::Identifier("int".to_string()),
                TokenKind::Comma,
                TokenKind::Identifier("string".to_string()),
                TokenKind::Comma,
                TokenKind::Identifier("TestRecord".to_string()),
                TokenKind::RParen,
                TokenKind::NewLine,
                TokenKind::EOF,
            ];
            assert_eq!(kinds, expected_tokens);
            let output_result = parser_.parse(tokens);
            let _ = output_result
                .as_ref()
                .map_err(|x| x.iter().for_each(error_report));
            assert!(output_result.is_ok());
        }
        {
            let input = "
type TestADT =
	| Test
	| Testing of string

let x = 1

type Test2 = | Test | Testing of int
";
            let token_result = lexer::lex_raw(input);
            assert!(token_result.is_ok());
            let tokens = token_result.unwrap();
            let kinds = map_kinds(&tokens);
            kinds.iter().for_each(|x| {
                log::debug!("{:?}", x);
            });
            let expected_tokens = vec![
                TokenKind::TypeKeyword,
                TokenKind::Identifier("TestADT".to_string()),
                TokenKind::Assign,
                TokenKind::NewLine,
                TokenKind::Indent,
                TokenKind::Pipe,
                TokenKind::Identifier("Test".to_string()),
                TokenKind::NewLine,
                TokenKind::Pipe,
                TokenKind::Identifier("Testing".to_string()),
                TokenKind::Of,
                TokenKind::Identifier("string".to_string()),
                TokenKind::NewLine,
                TokenKind::Dedent,
                TokenKind::Let,
                TokenKind::Identifier("x".to_string()),
                TokenKind::Assign,
                TokenKind::NumberLiteral("1".to_string()),
                TokenKind::NewLine,
                TokenKind::TypeKeyword,
                TokenKind::Identifier("Test2".to_string()),
                TokenKind::Assign,
                TokenKind::Pipe,
                TokenKind::Identifier("Test".to_string()),
                TokenKind::Pipe,
                TokenKind::Identifier("Testing".to_string()),
                TokenKind::Of,
                TokenKind::Identifier("int".to_string()),
                TokenKind::NewLine,
                TokenKind::EOF,
            ];
            assert_eq!(kinds, expected_tokens);
            let output_result = parser_.parse(tokens);
            let _ = output_result
                .as_ref()
                .map_err(|x| x.iter().for_each(error_report));
            assert!(output_result.is_ok());
        }
    }
}
