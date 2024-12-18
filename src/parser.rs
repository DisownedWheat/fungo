use super::ast::*;
use super::lexer::{Token, TokenKind};
use ariadne::{Label, Report, ReportKind, Source};
use chumsky::prelude::*;

fn token(kind: TokenKind, path: &str) -> BoxedParser<'_, Token, Token, Simple<Token>> {
    filter(move |token: &Token| {
        log::debug!(
            "Inside {} matching token {:?} against {:?}",
            path,
            token.kind,
            kind
        );
        token.kind == kind
    })
    .boxed()
}

#[derive(Clone, Copy, Debug)]
enum StrValueType {
    String,
    Identifier,
    Number,
    Operator,
}

fn identifier<'a>(t_: StrValueType) -> BoxedParser<'a, Token, String, Simple<Token>> {
    filter_map(move |span, token: Token| {
        log::debug!("Attempting to match {:?} as {:?}", token.kind, t_);
        match (t_, token.kind.clone()) {
            (StrValueType::Identifier, TokenKind::Identifier(s)) => Ok(s),
            (StrValueType::String, TokenKind::StringLiteral(s)) => Ok(s),
            (StrValueType::Number, TokenKind::NumberLiteral(s)) => Ok(s),
            (StrValueType::Operator, TokenKind::Operator(s)) => Ok(s),
            _ => Err(Simple::expected_input_found(span, Vec::new(), Some(token))),
        }
    })
    .labelled("Identifier")
    .boxed()
}

fn newline<'a>(path: &'static str) -> BoxedParser<'a, Token, (), Simple<Token>> {
    token(TokenKind::NewLine, path)
        .ignored()
        .labelled("Newline")
        .boxed()
}

fn indent<'a>(path: &'static str) -> BoxedParser<'a, Token, (), Simple<Token>> {
    (newline(path).or_not())
        .ignore_then(token(TokenKind::Indent, path))
        .labelled("Indent")
        .ignored()
        .boxed()
}

fn dedent<'a>(path: &'static str) -> BoxedParser<'a, Token, (), Simple<Token>> {
    (newline(path).or_not())
        .ignore_then(token(TokenKind::Dedent, path))
        .labelled("Dedent")
        .ignored()
        .boxed()
}

fn comma(path: &'static str) -> impl Parser<Token, Token, Error = Simple<Token>> {
    token(TokenKind::Comma, path).labelled("Comma")
}

fn lbrace(path: &'static str) -> impl Parser<Token, Token, Error = Simple<Token>> {
    token(TokenKind::LBrace, path).labelled("LBrace")
}

fn rbrace(path: &'static str) -> impl Parser<Token, Token, Error = Simple<Token>> {
    token(TokenKind::RBrace, path).labelled("RBrace")
}

fn lbracket(path: &'static str) -> impl Parser<Token, Token, Error = Simple<Token>> {
    token(TokenKind::LBracket, path).labelled("LBracket")
}

fn rbracket(path: &'static str) -> impl Parser<Token, Token, Error = Simple<Token>> {
    token(TokenKind::RBracket, path).labelled("RBracket")
}

fn lparen(path: &'static str) -> impl Parser<Token, Token, Error = Simple<Token>> {
    token(TokenKind::LParen, path).labelled("LParen")
}

fn rparen(path: &'static str) -> impl Parser<Token, Token, Error = Simple<Token>> {
    token(TokenKind::RParen, path).labelled("RParen")
}

fn colon(path: &'static str) -> impl Parser<Token, Token, Error = Simple<Token>> {
    token(TokenKind::Colon, path).labelled("Colon")
}

fn semicolon(path: &'static str) -> impl Parser<Token, (), Error = Simple<Token>> {
    token(TokenKind::SemiColon, path)
        .labelled("Semicolon")
        .ignored()
}

fn assign(path: &'static str) -> impl Parser<Token, (), Error = Simple<Token>> {
    token(TokenKind::Assign, path).labelled("Assign").ignored()
}

fn unit(path: &'static str) -> impl Parser<Token, (), Error = Simple<Token>> {
    (lparen(path).then(rparen(path))).ignored().labelled("Unit")
}

fn pointer(path: &'static str) -> impl Parser<Token, bool, Error = Simple<Token>> {
    token(TokenKind::Pointer, path)
        .or_not()
        .map(|x| x.is_some())
        .labelled("Pointer")
}

fn import(path: &'static str) -> impl Parser<Token, Token, Error = Simple<Token>> {
    token(TokenKind::Import, path).labelled("import")
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

fn bool(path: &'static str) -> impl Parser<Token, Expr, Error = Simple<Token>> {
    choice((token(TokenKind::True, path), token(TokenKind::False, path)))
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
            unit("Type literal").map(|_| Type::Unit),
            token(TokenKind::Deref, "type literal")
                .boxed()
                .ignore_then(t.clone())
                .map(|x| Type::Pointer(Box::new(x))),
            lbracket("Type literal")
                .ignore_then(rbracket("Type literal"))
                .ignore_then(t.clone())
                .map(|x: Type| Type::Slice(Box::new(x))),
            ident_token()
                .then(
                    token(TokenKind::Dot, "Type literal")
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
    let base_ident = pointer("Ident")
        .then(ident_token())
        .map(|(is_pointer, s)| match (is_pointer, s.as_str()) {
            (false, "_") => IdentifierType::Bucket,
            (true, _) => IdentifierType::Pointer(Box::new(IdentifierType::Identifier(s, None))),
            _ => IdentifierType::Identifier(s, None),
        })
        .labelled("Ident")
        .boxed();

    choice((
        base_ident.clone(),
        lparen("Ident")
            .ignore_then(pointer("Ident").then(ident_token()))
            .then_ignore(colon("Ident"))
            .then(type_literal())
            .then_ignore(rparen("Ident"))
            .map(|((is_pointer, s), t)| match t {
                TypeDef::Type(t) => match (is_pointer, s.as_str()) {
                    (false, "_") => IdentifierType::Bucket,
                    (true, _) => {
                        IdentifierType::Pointer(Box::new(IdentifierType::Identifier(s, Some(t))))
                    }
                    _ => IdentifierType::Identifier(s, Some(t)),
                },
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
    let newline = newline("Record Definition").boxed();
    recursive(move |r| {
        indent("Record Definition")
            .or_not()
            .ignore_then(lbrace("Record Definition"))
            .ignore_then(
                choice((
                    newline
                        .clone()
                        .ignore_then(indent("Record Definition"))
                        .ignore_then(
                            ident()
                                .then_ignore(colon("Record Definition"))
                                .then(type_literal().or(r.clone().boxed()))
                                .separated_by(newline.clone()),
                        )
                        .then_ignore(dedent("Record Definition").or_not()),
                    ident()
                        .then_ignore(colon("Record Definition"))
                        .then(type_literal().or(r.clone().boxed()))
                        .separated_by(semicolon("Record Definition")),
                ))
                .boxed(),
            )
            .then_ignore(rbrace("Record Definition"))
            .then_ignore(choice((newline, dedent("Record Definition"))).or_not())
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
            lparen("Tuple Definition")
                .ignore_then(
                    choice((type_literal(), record_definition(), t.clone()))
                        .separated_by(comma("Tuple Definition").ignored())
                        .map(|x| TypeDef::TupleDefinition {
                            length: x.len(),
                            types: x,
                        }),
                )
                .then_ignore(rparen("Tuple Definition"))
                .then_ignore(newline("Tuple Definition").or_not()),
            indent("Tuple Definition").ignore_then(
                lparen("Tuple Definition")
                    .ignore_then(
                        choice((type_literal(), record_definition(), t.clone()))
                            .separated_by(
                                (comma("Tuple Definition")
                                    .then(newline("Tuple Definition"))
                                    .ignored())
                                .or(newline("Tuple Definition")
                                    .then(comma("Tuple Definition"))
                                    .ignored()),
                            )
                            .map(|x| TypeDef::TupleDefinition {
                                length: x.len(),
                                types: x,
                            }),
                    )
                    .then_ignore(rparen("Tuple Definition"))
                    .then_ignore(dedent("Tuple Definition")),
            ),
        ))
        .labelled("Tuple Definition")
    })
}

fn variant_definition() -> impl Parser<Token, TypeDef, Error = Simple<Token>> {
    let variant_case = token(TokenKind::op("|"), "Variant Definition")
        .ignore_then(ident_token())
        .then(
            token(TokenKind::Of, "Variant Definition")
                .ignore_then(type_literal())
                .or_not(),
        )
        .boxed();

    choice((
        indent("Variant Definition")
            .ignore_then(
                variant_case
                    .clone()
                    .then_ignore(newline("Variant Definition"))
                    .repeated()
                    .at_least(1),
            )
            .then_ignore(dedent("Variant Definition")),
        variant_case.repeated().at_least(1),
    ))
    .map(|x| TypeDef::VariantDefinition { fields: x })
}

fn type_definition() -> impl Parser<Token, Stmt, Error = Simple<Token>> {
    token(TokenKind::TypeKeyword, "Type Definition")
        .ignore_then(ident_token())
        .then_ignore(assign("Type Definition"))
        .then(choice((
            record_definition(),
            tuple_definition(),
            variant_definition(),
            type_literal(),
        )))
        .map(|(value, t_)| Stmt::TypeDefinition(value, t_))
        .labelled("Type Definition")
}

fn match_parser<'a>(
    expr: BoxedParser<'a, Token, Expr, Simple<Token>>,
) -> BoxedParser<'a, Token, Expr, Simple<Token>> {
    token(TokenKind::Match, "Match Expression")
        .map(|_| Expr::Identifier(IdentifierType::Bucket))
        .boxed()
}

fn expr<'a>(
    stmt: BoxedParser<'a, Token, Stmt, Simple<Token>>,
) -> BoxedParser<'a, Token, Expr, Simple<Token>> {
    let inner_expr = recursive(move |expr| {
        let match_parser = match_parser(expr.clone().boxed());
        let array_literal = (expr.clone().separated_by(
            semicolon("Array Literal")
                .or(newline("Array Literal"))
                .boxed(),
        ))
        .delimited_by(
            lbracket("Array Literal")
                .then_ignore(indent("Array Literal").or_not())
                .boxed(),
            dedent("Array Literal Multi")
                .or_not()
                .ignore_then(rbracket("Array Literal"))
                .boxed(),
        )
        .map(|x| Expr::ArrayLiteral(x))
        .labelled("Array Literal Single");

        let record_literal_single_line = (ident_token().boxed())
            .then_ignore(assign("Record Literal Single Line").boxed())
            .then(expr.clone())
            .separated_by(semicolon("Record Literal Single Line").boxed())
            .at_least(1)
            .delimited_by(
                lbrace("Record Literal Single Line").boxed(),
                rbrace("Record Literal Single Line").boxed(),
            )
            .map(|x: Vec<(String, Expr)>| Expr::RecordLiteral {
                fields: x
                    .into_iter()
                    .map(|(name, value)| RecordField { name, value })
                    .collect(),
            })
            .labelled("Record Literal");

        let record_literal_multiline = (indent("Record Literal Multi Line").or_not())
            .ignore_then(
                ident_token()
                    .boxed()
                    .then_ignore(assign("Record Literal Multi Line"))
                    .then(expr.clone())
                    .separated_by(
                        newline("Record Literal Multi Line")
                            .or(semicolon("Record Literal Multi Line")),
                    )
                    .at_least(1),
            )
            .then_ignore(dedent("Record Literal Multi Line"))
            .delimited_by(
                lbrace("Record Literal Multi Line").boxed(),
                rbrace("Record Literal Multi Line").boxed(),
            )
            .map(|x: Vec<(String, Expr)>| Expr::RecordLiteral {
                fields: x
                    .into_iter()
                    .map(|(name, value)| RecordField { name, value })
                    .collect(),
            })
            .labelled("Record Literal");

        let record_literal = record_literal_single_line.or(record_literal_multiline);

        let tuple_literal = ((expr.clone().then_ignore(comma("Tuple Literal")))
            .then(expr.clone().separated_by(comma("Tuple Literal"))))
        .delimited_by(
            lparen("Tuple Literal").boxed(),
            rparen("Tuple Literal").boxed(),
        )
        .map(|(first, rest)| {
            let mut v = Vec::with_capacity(rest.len() + 1);
            v.push(first);
            v.extend(rest);
            Expr::TupleLiteral(v)
        })
        .labelled("Tuple Literal");

        let block_expr = indent("Block Expr")
            .ignore_then(stmt.repeated().at_least(1))
            .then_ignore(dedent("Block Expr"))
            .map(|x| Expr::Block(x))
            .labelled("Block Expression");

        let paren_expression = lparen("Paren Expression")
            .ignore_then(expr.clone())
            .then_ignore(rparen("Paren Expression"))
            .boxed()
            .labelled("Paren Expression");

        let atom = choice((
            array_literal.clone().boxed(),
            record_literal.boxed(),
            tuple_literal.boxed(),
            ident_node().boxed(),
            str_().boxed(),
            digit().boxed(),
            bool("atom").boxed(),
            unit("atom")
                .map(|_| Expr::Identifier(IdentifierType::Unit))
                .boxed(),
            paren_expression.clone(),
        ))
        .labelled("Atom");

        let chain_expr = atom
            .clone()
            .then(
                (token(TokenKind::Dot, "Chain Expr")
                    .boxed()
                    .ignore_then(atom.clone()))
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
            .labelled("Base Expr")
            .boxed();

        let func_call = ident_token()
            .then(choice((
                indent("Func Call")
                    .ignore_then(expr.clone().separated_by(newline("Func Call")))
                    .then_ignore(dedent("Func Call")),
                expr.clone()
                    .repeated()
                    .at_least(1)
                    .then_ignore(newline("Func Call"))
                    .boxed(),
            )))
            .map(|(name, args)| Expr::FunctionCall { name, args })
            .labelled("Function Call");

        choice((block_expr.boxed(), func_call.boxed(), chain_expr, atom))
            .labelled("Expression")
            .padded_by(token(TokenKind::Comment, "Expr").or_not().ignored())
    })
    .labelled("Expression");

    recursive(|binary| {
        inner_expr
            .clone()
            .then(
                choice((
                    (identifier(StrValueType::Operator)
                        .or(token(TokenKind::Deref, "Binary Expr").map(|_| "*".to_string()))
                        .then(
                            (newline("Binary Expr").or_not())
                                .ignore_then(binary.clone().or(inner_expr.clone())),
                        )),
                    (newline("Binary Expr").ignore_then(
                        identifier(StrValueType::Operator)
                            .or(token(TokenKind::Deref, "Binary Expr").map(|_| "*".to_string()))
                            .then(binary.clone().or(inner_expr.clone())),
                    )),
                ))
                .or_not(),
            )
            .map(|(left, is_op)| match is_op {
                Some((op, right)) => Expr::FunctionCall {
                    name: op,
                    args: vec![left, right],
                },
                None => left,
            })
    })
    .boxed()
}

fn stmt() -> impl Parser<Token, Stmt, Error = Simple<Token>> {
    // Prepare all the parsers inside the statement function
    let ident = ident().boxed();
    let ident_node = ident_node().map(Stmt::Expr).boxed();
    let ident_token = ident_token().boxed();
    let comma = comma("Stmt").boxed();
    let lbrace = lbrace("Stmt").boxed();
    let rbrace = rbrace("Stmt").boxed();
    let lparen = lparen("Stmt").boxed();
    let rparen = rparen("Stmt").boxed();
    let lbracket = lbracket("Stmt").boxed();
    let rbracket = rbracket("Stmt").boxed();
    let type_literal = type_literal().boxed();
    let assign = assign("Stmt").boxed();
    let type_ = type_definition().boxed();

    recursive(move |rstmt| {
        log::debug!("Processing next token in stmt parser");

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
            .then_ignore(token(TokenKind::Colon, "Typed Ident").boxed())
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
            let val = token(TokenKind::Mut, "Let Stmt")
                .or_not()
                .map(|x| x.is_some())
                .then(let_identifier)
                .then_ignore(assign.clone())
                .then(expr.clone())
                .map(|((mutable, name), value)| {
                    let identifier = match name {
                        Stmt::Expr(Expr::Identifier(n)) => n,
                        _ => panic!(),
                    };
                    Stmt::LetStatement {
                        identifier,
                        value,
                        mutable,
                    }
                })
                .labelled("Let value")
                .boxed();

            let func_definition = {
                let exprs = expr.clone().labelled("Function Body").boxed();

                let idents = ident_token
                    .clone()
                    .then(
                        (ident
                            .clone()
                            .or(unit("Func Def").map(|_| IdentifierType::Unit)))
                        .repeated()
                        .at_least(1),
                    )
                    .labelled("Function Arguments")
                    .boxed();

                let return_type = colon("Return Type")
                    .ignore_then(type_literal.clone())
                    .or_not()
                    .labelled("Return Type")
                    .boxed();

                idents
                    .then(return_type)
                    .then_ignore(assign.clone())
                    .then(exprs)
                    .map(|(((name, args), return_), body)| {
                        log::debug!("Parsing function {}\n{:?}\n{:?}", name, args, body);
                        Stmt::FunctionDefinition {
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
                        }
                    })
                    .boxed()
            };
            token(TokenKind::Let, "Let Stmt")
                .ignore_then(func_definition.or(val))
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
            log::debug!("Parsed Statement {:?}", x);
            x
        })
        .padded_by(token(TokenKind::Comment, "Stmt").or_not().ignored())
        .padded_by(newline("Stmt").or_not().ignored())
        .labelled("Statement")
    })
}

fn go_import() -> impl Parser<Token, TopLevel, Error = Simple<Token>> {
    (import("Go Import")
        .ignore_then(str_())
        .then_ignore(newline("Go Import")))
    .map(|s| match s {
        Expr::StringLiteral(value) => TopLevel::GoImport(GoImport {
            module: value,
            alias: None,
        }),
        _ => panic!(),
    })
}

fn fungo_import() -> impl Parser<Token, TopLevel, Error = Simple<Token>> {
    import("Import")
        .ignore_then(ident_token())
        .then_ignore(newline("Import"))
        .map(|s| {
            return TopLevel::FungoImport(FungoImport { module: s });
        })
}

pub fn parser() -> impl Parser<Token, Vec<TopLevel>, Error = Simple<Token>> {
    let go_import = go_import();
    let fungo_import = fungo_import();
    let stmt = stmt().boxed();
    let top_level_stmt = choice((
        stmt.map(TopLevel::Stmt).boxed(),
        go_import.boxed(),
        fungo_import.boxed(),
    ))
    .padded_by(newline("Top Level").or_not())
    .recover_with(skip_then_retry_until([]))
    .debug("Post top-level statement");

    top_level_stmt.repeated().at_least(1)
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
    use super::*;
    use crate::lexer;
    use colored::*;
    use serde_json;
    static INIT: std::sync::Once = std::sync::Once::new();

    fn setup(level: log::LevelFilter) {
        INIT.call_once(|| {
            let _ = env_logger::builder().filter_level(level).try_init();
        })
    }

    fn map_kinds(tokens: &Vec<lexer::Token>) -> Vec<lexer::TokenKind> {
        tokens.iter().map(|x| x.kind.clone()).collect()
    }

    fn lex_input(input: &str, expected: Vec<TokenKind>) -> Vec<Token> {
        let token_result = lexer::lex_raw(input);
        assert!(token_result.is_ok());
        let tokens = token_result.unwrap();
        let kinds = map_kinds(&tokens);
        assert_eq!(
            kinds,
            expected,
            "\nGot: {}\nExpected: {}\n",
            serde_json::to_string_pretty(&kinds).unwrap().red(),
            serde_json::to_string_pretty(&expected).unwrap().cyan()
        );
        tokens
    }

    fn parse_input(input: Vec<Token>) -> Vec<TopLevel> {
        let output_result = parser()
            .parse(input)
            .inspect_err(|errs| errs.iter().for_each(|e| error_report(e)));
        assert!(output_result.is_ok());
        output_result.unwrap()
    }

    fn match_output(tokens: Vec<Token>, expected: Vec<TopLevel>) {
        let output = parse_input(tokens);
        assert_eq!(
            output,
            expected,
            "\nGot: {}\nExpected: {}\n",
            serde_json::to_string_pretty(&output).unwrap().red(),
            serde_json::to_string_pretty(&expected).unwrap().cyan()
        );
    }

    #[test]
    fn test_func_calls() {
        setup(log::LevelFilter::Info);
        let input = "
x 1 2
y
	5
	4
	\"Hello World\"
";

        let tokens = lex_input(
            input,
            vec![
                TokenKind::ident("x"),
                TokenKind::num("1"),
                TokenKind::num("2"),
                TokenKind::NewLine,
                TokenKind::ident("y"),
                TokenKind::NewLine,
                TokenKind::Indent,
                TokenKind::num("5"),
                TokenKind::NewLine,
                TokenKind::num("4"),
                TokenKind::NewLine,
                TokenKind::str("Hello World"),
                TokenKind::NewLine,
                TokenKind::Dedent,
            ],
        );

        let _ = match_output(
            tokens,
            vec![
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
                        Expr::StringLiteral("Hello World".to_owned()),
                    ],
                })),
            ],
        );
    }

    #[test]
    fn test_opens() {
        setup(log::LevelFilter::Info);
        let input = "
open \"fmt\"
open Test
testCall
	(1, 2)
	3
	4
";
        let tokens = lex_input(
            input,
            vec![
                TokenKind::Import,
                TokenKind::str("fmt"),
                TokenKind::NewLine,
                TokenKind::Import,
                TokenKind::ident("Test"),
                TokenKind::NewLine,
                TokenKind::ident("testCall"),
                TokenKind::NewLine,
                TokenKind::Indent,
                TokenKind::LParen,
                TokenKind::num("1"),
                TokenKind::Comma,
                TokenKind::num("2"),
                TokenKind::RParen,
                TokenKind::NewLine,
                TokenKind::num("3"),
                TokenKind::NewLine,
                TokenKind::num("4"),
                TokenKind::NewLine,
                TokenKind::Dedent,
            ],
        );

        let _ = match_output(
            tokens,
            vec![
                TopLevel::GoImport(GoImport {
                    module: "fmt".to_string(),
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
            ],
        );
    }

    #[test]
    fn block_expression() {
        setup(log::LevelFilter::Info);
        let input = "
let x =
	testFunc 0 1 \"a\"
";
        let tokens = lex_input(
            input,
            vec![
                TokenKind::Let,
                TokenKind::ident("x"),
                TokenKind::Assign,
                TokenKind::NewLine,
                TokenKind::Indent,
                TokenKind::ident("testFunc"),
                TokenKind::num("0"),
                TokenKind::num("1"),
                TokenKind::str("a"),
                TokenKind::NewLine,
                TokenKind::Dedent,
            ],
        );

        let _ = match_output(
            tokens,
            vec![TopLevel::Stmt(Stmt::LetStatement {
                identifier: IdentifierType::Identifier("x".to_string(), None),
                mutable: false,
                value: Expr::Block(vec![Stmt::Expr(Expr::FunctionCall {
                    name: "testFunc".to_string(),
                    args: vec![
                        Expr::IntLiteral("0".to_string()),
                        Expr::IntLiteral("1".to_string()),
                        Expr::StringLiteral("a".to_string()),
                    ],
                })]),
            })],
        );
    }

    #[test]
    fn type_definitions() {
        setup(log::LevelFilter::Info);
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

            let tokens = lex_input(
                input,
                vec![
                    TokenKind::TypeKeyword,
                    TokenKind::ident("Testing"),
                    TokenKind::Assign,
                    TokenKind::LBrace,
                    TokenKind::ident("x"),
                    TokenKind::Colon,
                    TokenKind::ident("int"),
                    TokenKind::SemiColon,
                    TokenKind::ident("y"),
                    TokenKind::Colon,
                    TokenKind::ident("int"),
                    TokenKind::RBrace,
                    TokenKind::NewLine,
                    TokenKind::TypeKeyword,
                    TokenKind::ident("TestRecord"),
                    TokenKind::Assign,
                    TokenKind::LBrace,
                    TokenKind::NewLine,
                    TokenKind::Indent,
                    TokenKind::ident("TestVal"),
                    TokenKind::Colon,
                    TokenKind::LBracket,
                    TokenKind::RBracket,
                    TokenKind::Deref,
                    TokenKind::ident("int"),
                    TokenKind::NewLine,
                    TokenKind::ident("AnotherTest"),
                    TokenKind::Colon,
                    TokenKind::LBrace,
                    TokenKind::NewLine,
                    TokenKind::Indent,
                    TokenKind::ident("InteriorTest"),
                    TokenKind::Colon,
                    TokenKind::ident("string"),
                    TokenKind::NewLine,
                    TokenKind::Dedent,
                    TokenKind::RBrace,
                    TokenKind::NewLine,
                    TokenKind::Dedent,
                    TokenKind::RBrace,
                    TokenKind::NewLine,
                ],
            );
            let _ = match_output(
                tokens,
                vec![
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
                ],
            );
        }
        {
            let input = "
type TestTuple = (int, string, TestRecord)
";
            let tokens = lex_input(
                input,
                vec![
                    TokenKind::TypeKeyword,
                    TokenKind::ident("TestTuple"),
                    TokenKind::Assign,
                    TokenKind::LParen,
                    TokenKind::ident("int"),
                    TokenKind::Comma,
                    TokenKind::ident("string"),
                    TokenKind::Comma,
                    TokenKind::ident("TestRecord"),
                    TokenKind::RParen,
                    TokenKind::NewLine,
                ],
            );
            let _ = match_output(
                tokens,
                vec![TopLevel::Stmt(Stmt::TypeDefinition(
                    "TestTuple".to_string(),
                    TypeDef::TupleDefinition {
                        length: 3,
                        types: vec![
                            TypeDef::Type(Type::Type {
                                name: "int".to_string(),
                                module: None,
                            }),
                            TypeDef::Type(Type::Type {
                                name: "string".to_string(),
                                module: None,
                            }),
                            TypeDef::Type(Type::Type {
                                name: "TestRecord".to_string(),
                                module: None,
                            }),
                        ],
                    },
                ))],
            );
        }
        {
            let input = "
type TestADT =
	| Test
	| Testing of string

type Test2 = | Test	| Testing of int
";
            let tokens = lex_input(
                input,
                vec![
                    TokenKind::TypeKeyword,
                    TokenKind::ident("TestADT"),
                    TokenKind::Assign,
                    TokenKind::NewLine,
                    TokenKind::Indent,
                    TokenKind::op("|"),
                    TokenKind::ident("Test"),
                    TokenKind::NewLine,
                    TokenKind::op("|"),
                    TokenKind::ident("Testing"),
                    TokenKind::Of,
                    TokenKind::ident("string"),
                    TokenKind::NewLine,
                    TokenKind::Dedent,
                    TokenKind::TypeKeyword,
                    TokenKind::ident("Test2"),
                    TokenKind::Assign,
                    TokenKind::op("|"),
                    TokenKind::ident("Test"),
                    TokenKind::op("|"),
                    TokenKind::ident("Testing"),
                    TokenKind::Of,
                    TokenKind::ident("int"),
                    TokenKind::NewLine,
                ],
            );
            let output_result = parser().parse(tokens);
            let _ = output_result
                .as_ref()
                .map_err(|x| x.iter().for_each(error_report));
            assert!(output_result.is_ok());
        }
    }

    #[test]
    fn test_binary_expr() {
        setup(log::LevelFilter::Info);
        let input = "
x + 5
(\"test\") + \"Hello\"
x.InsideValue -
	5 + 2 % 3
";
        let tokens = lex_input(
            input,
            vec![
                TokenKind::ident("x"),
                TokenKind::op("+"),
                TokenKind::num("5"),
                TokenKind::NewLine,
                TokenKind::LParen,
                TokenKind::str("test"),
                TokenKind::RParen,
                TokenKind::op("+"),
                TokenKind::str("Hello"),
                TokenKind::NewLine,
                TokenKind::ident("x"),
                TokenKind::Dot,
                TokenKind::ident("InsideValue"),
                TokenKind::op("-"),
                TokenKind::NewLine,
                TokenKind::Indent,
                TokenKind::num("5"),
                TokenKind::op("+"),
                TokenKind::num("2"),
                TokenKind::op("%"),
                TokenKind::num("3"),
                TokenKind::NewLine,
                TokenKind::Dedent,
            ],
        );
        let _ = match_output(
            tokens,
            vec![
                TopLevel::Stmt(Stmt::Expr(Expr::FunctionCall {
                    name: "+".to_string(),
                    args: vec![
                        Expr::Identifier(IdentifierType::Identifier("x".to_string(), None)),
                        Expr::IntLiteral("5".to_string()),
                    ],
                })),
                TopLevel::Stmt(Stmt::Expr(Expr::FunctionCall {
                    name: "+".to_string(),
                    args: vec![
                        Expr::StringLiteral("test".to_string()),
                        Expr::StringLiteral("Hello".to_string()),
                    ],
                })),
                TopLevel::Stmt(Stmt::Expr(Expr::FunctionCall {
                    name: "-".to_string(),
                    args: vec![
                        Expr::Accessor {
                            left: Box::new(Expr::Identifier(IdentifierType::Identifier(
                                "x".to_string(),
                                None,
                            ))),
                            right: Box::new(Expr::Identifier(IdentifierType::Identifier(
                                "InsideValue".to_string(),
                                None,
                            ))),
                        },
                        Expr::Block(vec![Stmt::Expr(Expr::FunctionCall {
                            name: "+".to_string(),
                            args: vec![
                                Expr::IntLiteral("5".to_string()),
                                Expr::FunctionCall {
                                    name: "%".to_string(),
                                    args: vec![
                                        Expr::IntLiteral("2".to_string()),
                                        Expr::IntLiteral("3".to_string()),
                                    ],
                                },
                            ],
                        })]),
                    ],
                })),
            ],
        );
    }

    #[test]
    fn test_let_stmt() {
        setup(log::LevelFilter::Info);
        let input = "
let x = 1
let y = 2
let z = {TestValue = true}
let a = (\"Hello\", \"World\")
let b =
	let inner = 5 + 5
	inner
";

        let tokens = lex_input(
            input,
            vec![
                TokenKind::Let,
                TokenKind::ident("x"),
                TokenKind::Assign,
                TokenKind::num("1"),
                TokenKind::NewLine,
                TokenKind::Let,
                TokenKind::ident("y"),
                TokenKind::Assign,
                TokenKind::num("2"),
                TokenKind::NewLine,
                TokenKind::Let,
                TokenKind::ident("z"),
                TokenKind::Assign,
                TokenKind::LBrace,
                TokenKind::ident("TestValue"),
                TokenKind::Assign,
                TokenKind::True,
                TokenKind::RBrace,
                TokenKind::NewLine,
                TokenKind::Let,
                TokenKind::ident("a"),
                TokenKind::Assign,
                TokenKind::LParen,
                TokenKind::str("Hello"),
                TokenKind::Comma,
                TokenKind::str("World"),
                TokenKind::RParen,
                TokenKind::NewLine,
                TokenKind::Let,
                TokenKind::ident("b"),
                TokenKind::Assign,
                TokenKind::NewLine,
                TokenKind::Indent,
                TokenKind::Let,
                TokenKind::ident("inner"),
                TokenKind::Assign,
                TokenKind::num("5"),
                TokenKind::op("+"),
                TokenKind::num("5"),
                TokenKind::NewLine,
                TokenKind::ident("inner"),
                TokenKind::NewLine,
                TokenKind::Dedent,
            ],
        );

        let _ = match_output(
            tokens,
            vec![
                TopLevel::Stmt(Stmt::LetStatement {
                    identifier: IdentifierType::Identifier("x".to_string(), None),
                    value: Expr::IntLiteral("1".to_string()),
                    mutable: false,
                }),
                TopLevel::Stmt(Stmt::LetStatement {
                    identifier: IdentifierType::Identifier("y".to_string(), None),
                    value: Expr::IntLiteral("2".to_string()),
                    mutable: false,
                }),
                TopLevel::Stmt(Stmt::LetStatement {
                    identifier: IdentifierType::Identifier("z".to_string(), None),
                    value: Expr::RecordLiteral {
                        fields: vec![RecordField {
                            name: "TestValue".to_string(),
                            value: Expr::BoolLiteral(true),
                        }],
                    },
                    mutable: false,
                }),
                TopLevel::Stmt(Stmt::LetStatement {
                    identifier: IdentifierType::Identifier("a".to_string(), None),
                    value: Expr::TupleLiteral(vec![
                        Expr::StringLiteral("Hello".to_string()),
                        Expr::StringLiteral("World".to_string()),
                    ]),
                    mutable: false,
                }),
                TopLevel::Stmt(Stmt::LetStatement {
                    identifier: IdentifierType::Identifier("b".to_string(), None),
                    value: Expr::Block(vec![
                        Stmt::LetStatement {
                            identifier: IdentifierType::Identifier("inner".to_string(), None),
                            value: Expr::FunctionCall {
                                name: "+".to_string(),
                                args: vec![
                                    Expr::IntLiteral("5".to_string()),
                                    Expr::IntLiteral("5".to_string()),
                                ],
                            },
                            mutable: false,
                        },
                        Stmt::Expr(Expr::Identifier(IdentifierType::Identifier(
                            "inner".to_string(),
                            None,
                        ))),
                    ]),
                    mutable: false,
                }),
            ],
        );
    }

    #[test]
    fn test_func_definition() {
        setup(log::LevelFilter::Info);
        let input = "
let firstTest x y = x + y
let testFunc x (y: int) z =
	let result = x + 1
	result * z
";
        let tokens = lex_input(
            input,
            vec![
                TokenKind::Let,
                TokenKind::ident("firstTest"),
                TokenKind::ident("x"),
                TokenKind::ident("y"),
                TokenKind::Assign,
                TokenKind::ident("x"),
                TokenKind::op("+"),
                TokenKind::ident("y"),
                TokenKind::NewLine,
                TokenKind::Let,
                TokenKind::ident("testFunc"),
                TokenKind::ident("x"),
                TokenKind::LParen,
                TokenKind::ident("y"),
                TokenKind::Colon,
                TokenKind::ident("int"),
                TokenKind::RParen,
                TokenKind::ident("z"),
                TokenKind::Assign,
                TokenKind::NewLine,
                TokenKind::Indent,
                TokenKind::Let,
                TokenKind::ident("result"),
                TokenKind::Assign,
                TokenKind::ident("x"),
                TokenKind::op("+"),
                TokenKind::num("1"),
                TokenKind::NewLine,
                TokenKind::ident("result"),
                TokenKind::Deref,
                TokenKind::ident("z"),
                TokenKind::NewLine,
                TokenKind::Dedent,
            ],
        );

        let _ = match_output(
            tokens,
            vec![
                TopLevel::Stmt(Stmt::FunctionDefinition {
                    name: Some("firstTest".to_string()),
                    arguments: vec![
                        IdentifierType::Identifier("x".to_string(), None),
                        IdentifierType::Identifier("y".to_string(), None),
                    ],
                    return_type: None,
                    body: (Expr::FunctionCall {
                        name: "+".to_string(),
                        args: vec![
                            Expr::Identifier(IdentifierType::Identifier("x".to_string(), None)),
                            Expr::Identifier(IdentifierType::Identifier("y".to_string(), None)),
                        ],
                    }),
                }),
                TopLevel::Stmt(Stmt::FunctionDefinition {
                    name: Some("testFunc".to_string()),
                    arguments: vec![
                        IdentifierType::Identifier("x".to_string(), None),
                        IdentifierType::Identifier(
                            "y".to_string(),
                            Some(Type::Type {
                                name: "int".to_string(),
                                module: None,
                            }),
                        ),
                        IdentifierType::Identifier("z".to_string(), None),
                    ],
                    return_type: None,
                    body: (Expr::Block(vec![
                        Stmt::LetStatement {
                            identifier: IdentifierType::Identifier("result".to_string(), None),
                            mutable: false,
                            value: Expr::FunctionCall {
                                name: "+".to_string(),
                                args: vec![
                                    Expr::Identifier(IdentifierType::Identifier(
                                        "x".to_string(),
                                        None,
                                    )),
                                    Expr::IntLiteral("1".to_string()),
                                ],
                            },
                        },
                        Stmt::Expr(Expr::FunctionCall {
                            name: "*".to_string(),
                            args: vec![
                                Expr::Identifier(IdentifierType::Identifier(
                                    "result".to_string(),
                                    None,
                                )),
                                Expr::Identifier(IdentifierType::Identifier("z".to_string(), None)),
                            ],
                        }),
                    ])),
                }),
            ],
        );
    }

    #[test]
    fn test_literals() {
        setup(log::LevelFilter::Info);
        let input = "
let singleLine = {TestValue = \"Test\"; Val = true}
let multiLine =
	{
		TestValue = \"Hello World\"
		Val = false
	}
let singleTuple = (\"Hello World\", 15)
let multiTuple =
	(15, (\"Hello\", 20))
let singleList = [1;2;3;4]
let multiList =
	[
		1
		2
		3
		4
	]
";

        let tokens = lex_input(
            input,
            vec![
                TokenKind::Let,
                TokenKind::ident("singleLine"),
                TokenKind::Assign,
                TokenKind::LBrace,
                TokenKind::ident("TestValue"),
                TokenKind::Assign,
                TokenKind::str("Test"),
                TokenKind::SemiColon,
                TokenKind::ident("Val"),
                TokenKind::Assign,
                TokenKind::True,
                TokenKind::RBrace,
                TokenKind::NewLine,
                TokenKind::Let,
                TokenKind::ident("multiLine"),
                TokenKind::Assign,
                TokenKind::NewLine,
                TokenKind::Indent,
                TokenKind::LBrace,
                TokenKind::NewLine,
                TokenKind::Indent,
                TokenKind::ident("TestValue"),
                TokenKind::Assign,
                TokenKind::str("Hello World"),
                TokenKind::NewLine,
                TokenKind::ident("Val"),
                TokenKind::Assign,
                TokenKind::False,
                TokenKind::NewLine,
                TokenKind::Dedent,
                TokenKind::RBrace,
                TokenKind::NewLine,
                TokenKind::Dedent,
                TokenKind::Let,
                TokenKind::ident("singleTuple"),
                TokenKind::Assign,
                TokenKind::LParen,
                TokenKind::str("Hello World"),
                TokenKind::Comma,
                TokenKind::num("15"),
                TokenKind::RParen,
                TokenKind::NewLine,
                TokenKind::Let,
                TokenKind::ident("multiTuple"),
                TokenKind::Assign,
                TokenKind::NewLine,
                TokenKind::Indent,
                TokenKind::LParen,
                TokenKind::num("15"),
                TokenKind::Comma,
                TokenKind::LParen,
                TokenKind::str("Hello"),
                TokenKind::Comma,
                TokenKind::num("20"),
                TokenKind::RParen,
                TokenKind::RParen,
                TokenKind::NewLine,
                TokenKind::Dedent,
                TokenKind::Let,
                TokenKind::ident("singleList"),
                TokenKind::Assign,
                TokenKind::LBracket,
                TokenKind::num("1"),
                TokenKind::SemiColon,
                TokenKind::num("2"),
                TokenKind::SemiColon,
                TokenKind::num("3"),
                TokenKind::SemiColon,
                TokenKind::num("4"),
                TokenKind::RBracket,
                TokenKind::NewLine,
                TokenKind::Let,
                TokenKind::ident("multiList"),
                TokenKind::Assign,
                TokenKind::NewLine,
                TokenKind::Indent,
                TokenKind::LBracket,
                TokenKind::NewLine,
                TokenKind::Indent,
                TokenKind::num("1"),
                TokenKind::NewLine,
                TokenKind::num("2"),
                TokenKind::NewLine,
                TokenKind::num("3"),
                TokenKind::NewLine,
                TokenKind::num("4"),
                TokenKind::NewLine,
                TokenKind::Dedent,
                TokenKind::RBracket,
                TokenKind::NewLine,
                TokenKind::Dedent,
            ],
        );

        let _ = match_output(
            tokens,
            vec![
                TopLevel::Stmt(Stmt::LetStatement {
                    identifier: IdentifierType::Identifier("singleLine".to_string(), None),
                    value: Expr::RecordLiteral {
                        fields: vec![
                            RecordField {
                                name: "TestValue".to_string(),
                                value: Expr::StringLiteral("Test".to_string()),
                            },
                            RecordField {
                                name: "Val".to_string(),
                                value: Expr::BoolLiteral(true),
                            },
                        ],
                    },
                    mutable: false,
                }),
                TopLevel::Stmt(Stmt::LetStatement {
                    identifier: IdentifierType::Identifier("multiLine".to_string(), None),
                    mutable: false,
                    value: Expr::Block(vec![Stmt::Expr(Expr::RecordLiteral {
                        fields: vec![
                            RecordField {
                                name: "TestValue".to_string(),
                                value: Expr::StringLiteral("Hello World".to_string()),
                            },
                            RecordField {
                                name: "Val".to_string(),
                                value: Expr::BoolLiteral(false),
                            },
                        ],
                    })]),
                }),
                TopLevel::Stmt(Stmt::LetStatement {
                    identifier: IdentifierType::Identifier("singleTuple".to_string(), None),
                    value: Expr::TupleLiteral(vec![
                        Expr::StringLiteral("Hello World".to_string()),
                        Expr::IntLiteral("15".to_string()),
                    ]),
                    mutable: false,
                }),
                TopLevel::Stmt(Stmt::LetStatement {
                    identifier: IdentifierType::Identifier("multiTuple".to_string(), None),
                    value: Expr::Block(vec![Stmt::Expr(Expr::TupleLiteral(vec![
                        Expr::IntLiteral("15".to_string()),
                        Expr::TupleLiteral(vec![
                            Expr::StringLiteral("Hello".to_string()),
                            Expr::IntLiteral("20".to_string()),
                        ]),
                    ]))]),
                    mutable: false,
                }),
                TopLevel::Stmt(Stmt::LetStatement {
                    identifier: IdentifierType::Identifier("singleList".to_string(), None),
                    value: Expr::ArrayLiteral(vec![
                        Expr::IntLiteral("1".to_string()),
                        Expr::IntLiteral("2".to_string()),
                        Expr::IntLiteral("3".to_string()),
                        Expr::IntLiteral("4".to_string()),
                    ]),
                    mutable: false,
                }),
                TopLevel::Stmt(Stmt::LetStatement {
                    identifier: IdentifierType::Identifier("multiList".to_string(), None),
                    value: Expr::Block(vec![Stmt::Expr(Expr::ArrayLiteral(vec![
                        Expr::IntLiteral("1".to_string()),
                        Expr::IntLiteral("2".to_string()),
                        Expr::IntLiteral("3".to_string()),
                        Expr::IntLiteral("4".to_string()),
                    ]))]),
                    mutable: false,
                }),
            ],
        );
    }
}
