use crate::lexer::TokenState;

use super::ast::*;
use super::lexer::{Span, Token, TokenKind};
use ariadne::{Label, Report, ReportKind, Source};
use chumsky::prelude::*;

fn token(kind: TokenKind, path: &str) -> BoxedParser<'_, Token, Token, Simple<Token>> {
    filter(move |(inner_kind, _): &Token| {
        log::debug!(
            "Inside {} matching token {:?} against {:?}",
            path,
            inner_kind,
            kind
        );
        inner_kind == &kind
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

fn identifier<'a>(t_: StrValueType) -> BoxedParser<'a, Token, ASTString, Simple<Token>> {
    filter_map(move |span, token: Token| {
        let (kind, state) = token;
        log::debug!("Attempting to match {:?} as {:?}", kind, t_);
        match (t_, kind.clone()) {
            (StrValueType::Identifier, TokenKind::Identifier(_)) => {
                Ok(ASTString::from_token((kind, state)))
            }
            (StrValueType::String, TokenKind::StringLiteral(_)) => {
                Ok(ASTString::from_token((kind, state)))
            }
            (StrValueType::Number, TokenKind::NumberLiteral(_)) => {
                Ok(ASTString::from_token((kind, state)))
            }
            (StrValueType::Operator, TokenKind::Operator(_)) => {
                Ok(ASTString::from_token((kind, state)))
            }
            _ => Err(Simple::expected_input_found(
                span,
                Vec::new(),
                Some((kind, state)),
            )),
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

fn assign(path: &'static str) -> impl Parser<Token, Span, Error = Simple<Token>> {
    token(TokenKind::Assign, path)
        .labelled("Assign")
        .map(|(_, state)| state.span)
}

fn pipe(path: &'static str) -> impl Parser<Token, (), Error = Simple<Token>> {
    token(TokenKind::op("|"), path).ignored()
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

fn deref(path: &'static str) -> impl Parser<Token, bool, Error = Simple<Token>> {
    token(TokenKind::Deref, path)
        .or_not()
        .map(|x| x.is_some())
        .labelled("Deref")
}

fn import(path: &'static str) -> impl Parser<Token, Token, Error = Simple<Token>> {
    token(TokenKind::Import, path).labelled("import")
}

fn ident_token() -> impl Parser<Token, ASTString, Error = Simple<Token>> {
    identifier(StrValueType::Identifier).labelled("Ident token")
}

fn str_() -> impl Parser<Token, Expr, Error = Simple<Token>> {
    identifier(StrValueType::String)
        .map(|s| Expr::StringLiteral(s))
        .labelled("String Literal")
}

fn digit() -> impl Parser<Token, Expr, Error = Simple<Token>> {
    identifier(StrValueType::Number)
        .map(|s| {
            if s.value.contains(".") {
                Expr::FloatLiteral(s)
            } else {
                Expr::IntLiteral(s)
            }
        })
        .labelled("Number Literal")
}

fn bool(path: &'static str) -> impl Parser<Token, Expr, Error = Simple<Token>> {
    choice((token(TokenKind::True, path), token(TokenKind::False, path)))
        .map(|(kind, _)| match kind {
            TokenKind::True => true,
            TokenKind::False => false,
            _ => panic!(),
        })
        .map(|x| Expr::BoolLiteral(x))
        .labelled("Bool Literal")
}

fn record_destructure() -> impl Parser<Token, IdentifierType, Error = Simple<Token>> {
    lbrace("Record Destructure")
        .ignore_then(ident().separated_by(comma("Record Destructure")))
        .then_ignore(rbrace("Record Destructure"))
        .map(|s| (IdentifierType::RecordDestructure(s, None)))
}

fn array_destructure() -> impl Parser<Token, IdentifierType, Error = Simple<Token>> {
    lbracket("Array Destructure")
        .ignore_then(ident().separated_by(comma("Array Destructure")))
        .then_ignore(rparen("Array Destructure"))
        .map(|s| IdentifierType::ArrayDestructure(s, None))
        .labelled("Array Destructure")
}

fn tuple_destructure() -> impl Parser<Token, IdentifierType, Error = Simple<Token>> {
    lparen("Tuple Destructure")
        .ignore_then(ident().separated_by(comma("Tuple Destructure")))
        .then_ignore(rparen("Tuple Destructure"))
        .map(|s| IdentifierType::TupleDestructure(s, None))
        .labelled("Tuple Destructure")
}

fn type_literal() -> impl Parser<Token, Type, Error = Simple<Token>> {
    recursive(move |t| {
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
                            name: value,
                            module: None,
                        })
                        .or_not(),
                )
                .map(|(a, x)| match x {
                    Some(x) => match x {
                        Type::Type { name, .. } => Type::Type {
                            name,
                            module: Some(a),
                        },
                        _ => panic!(),
                    },
                    None => Type::Type {
                        name: a,
                        module: None,
                    },
                }),
        ))
    })
    .labelled("Type Literal")
}

fn ident() -> impl Parser<Token, IdentifierType, Error = Simple<Token>> {
    let base_ident = (pointer("Ident").then(deref("Ident")))
        .then(ident_token())
        .map(
            |((is_pointer, is_deref), s)| match (is_pointer, is_deref, s.value.as_str()) {
                (false, false, "_") => IdentifierType::Bucket,
                (true, false, _) => {
                    IdentifierType::Pointer(Box::new(IdentifierType::Identifier(s, None)))
                }
                (false, true, _) => {
                    IdentifierType::Deref(Box::new(IdentifierType::Identifier(s, None)))
                }
                _ => IdentifierType::Identifier(s, None),
            },
        )
        .labelled("Ident")
        .boxed();

    choice((
        base_ident.clone(),
        lparen("Ident")
            .ignore_then(pointer("Ident").then(ident_token()))
            .then_ignore(colon("Ident"))
            .then(type_literal())
            .then_ignore(rparen("Ident"))
            .map(
                |((is_pointer, s), t)| match (is_pointer, s.value.as_str()) {
                    (false, "_") => IdentifierType::Bucket,
                    (true, _) => {
                        IdentifierType::Pointer(Box::new(IdentifierType::Identifier(s, Some(t))))
                    }
                    _ => IdentifierType::Identifier(s, Some(t)),
                },
            )
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
                                .then(type_literal().map(TypeDef::Type).or(r.clone().boxed()))
                                .separated_by(newline.clone()),
                        )
                        .then_ignore(dedent("Record Definition").or_not()),
                    ident()
                        .then_ignore(colon("Record Definition"))
                        .then(type_literal().map(TypeDef::Type).or(r.clone().boxed()))
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
                    choice((
                        type_literal().map(TypeDef::Type),
                        record_definition(),
                        t.clone(),
                    ))
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
                        choice((
                            type_literal().map(TypeDef::Type),
                            record_definition(),
                            t.clone(),
                        ))
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
    let variant_case = pipe("Variant Definition")
        .ignore_then(ident_token())
        .then(
            token(TokenKind::Of, "Variant Definition")
                .ignore_then(type_literal().map(TypeDef::Type))
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
            type_literal().map(TypeDef::Type),
        )))
        .map(|(value, t_)| Stmt::TypeDefinition(value, t_))
        .labelled("Type Definition")
}

fn match_expr<'a>(
    expr: BoxedParser<'a, Token, Expr, Simple<Token>>,
) -> BoxedParser<'a, Token, Expr, Simple<Token>> {
    let match_handler = pipe("Match Expression");

    let first_elem = choice([
        token(TokenKind::Match, "Match Expression")
            .ignore_then(expr.clone())
            .then_ignore(token(TokenKind::With, "Match Expression"))
            .map(Some)
            .boxed(),
        token(TokenKind::FunctionMatch, "Match Expr")
            .map(|_| None)
            .boxed(),
    ]);

    first_elem
        .then_ignore(indent("Match Expression").or_not())
        .map(|_| Expr::Identifier(IdentifierType::Bucket))
        .boxed()
}

fn lambda_expression<'a>(
    expr: Recursive<'a, (TokenKind, TokenState), Expr, Simple<(TokenKind, TokenState)>>,
) -> BoxedParser<'a, Token, Expr, Simple<Token>> {
    let idents = choice((
        record_destructure(),
        tuple_destructure(),
        array_destructure(),
        ident(),
    ));
    token(TokenKind::Lambda, "Lambda Expression")
        .ignore_then(idents.repeated())
        .then(
            colon("Lambda Expression Return")
                .ignore_then(type_literal())
                .or_not(),
        )
        .then_ignore(token(TokenKind::op("->"), "Lambda Expression"))
        .then(expr.clone())
        .map(|((args, t), body)| Expr::Lambda {
            args,
            return_type: t,
            body: Box::new(body),
        })
        .boxed()
}

fn expr<'a>(
    stmt: BoxedParser<'a, Token, Stmt, Simple<Token>>,
) -> BoxedParser<'a, Token, Expr, Simple<Token>> {
    recursive(move |expr| {
        let match_expr = match_expr(expr.clone().boxed());
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
            .map(|x: Vec<(ASTString, Expr)>| Expr::RecordLiteral {
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
            .map(|x: Vec<(ASTString, Expr)>| Expr::RecordLiteral {
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
            .then_ignore(
                (rparen("Block Expr")
                    .ignored()
                    .then_ignore(dedent("Block Expr")))
                .rewind()
                .or(dedent("Block Expr")),
            )
            .map(|x| Expr::Block(x))
            .labelled("Block Expression");

        let paren_expression = lparen("Paren Expression")
            .ignore_then(expr.clone())
            .then_ignore(rparen("Paren Expression"))
            .then_ignore(dedent("Paren Expression").or_not())
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

        let if_expr = token(TokenKind::If, "If Expr")
            .ignore_then(expr.clone())
            .then_ignore(token(TokenKind::Then, "If Expr"))
            .then(expr.clone())
            .then((token(TokenKind::Else, "If Expr").ignore_then(expr.clone())).or_not())
            .map(|((condition, consequent), alternative)| Expr::IfExpr {
                condition: Box::new(condition),
                consequent: Box::new(consequent),
                alternative: match alternative {
                    Some(alt) => Some(Box::new(alt)),
                    None => None,
                },
            })
            .boxed()
            .labelled("If Expression");

        let func_call = ident_token()
            .then(choice((
                indent("Func Call")
                    .ignore_then(atom.clone().separated_by(newline("Func Call")))
                    .then_ignore(dedent("Func Call")),
                atom.clone()
                    .repeated()
                    .at_least(1)
                    .then_ignore(choice((
                        newline("Func Call"),
                        token(TokenKind::Do, "Func Call").rewind().ignored(),
                        rparen("Func Call").rewind().ignored(),
                    )))
                    .boxed(),
            )))
            .map(|(name, args)| Expr::FunctionCall { name, args })
            .labelled("Function Call");

        let binary_op = choice((
            identifier(StrValueType::Operator),
            assign("Binary Expr")
                .map(|span| ASTString {
                    value: "=".to_string(),
                    span,
                })
                .boxed(),
        ));

        choice((
            if_expr,
            match_expr.boxed(),
            block_expr.boxed(),
            func_call.boxed(),
            lambda_expression(expr.clone()),
            chain_expr,
            atom,
        ))
        .labelled("Expression")
        .padded_by(token(TokenKind::Comment, "Expr").or_not().ignored())
        .then(
            choice((
                binary_op
                    .clone()
                    .then((newline("Binary Expr").or_not()).ignore_then(expr.clone())),
                (newline("Binary Expr").ignore_then(binary_op.then(expr.clone()))),
            ))
            .or_not(),
        )
        .map(map_binary_expr)
    })
    .labelled("Expression")
    .map(|x| {
        log::debug!("Parsed EXPR: {:?}", x);
        x
    })
    .boxed()
}

// Recursively reverse the order of function calls to make it easier to evaluate later
fn map_binary_expr((left, right): (Expr, Option<(ASTString, Expr)>)) -> Expr {
    if let Some((name, expression)) = right {
        match expression {
            Expr::FunctionCall {
                name: inner_name,
                mut args,
            } => {
                let second_func_right_arg = args.pop().unwrap();
                let second_func_left_arg = args.pop().unwrap();
                Expr::FunctionCall {
                    name: inner_name,
                    args: vec![
                        map_binary_expr((left, Some((name, second_func_left_arg)))),
                        second_func_right_arg,
                    ],
                }
            }
            _ => Expr::FunctionCall {
                name,
                args: vec![left, expression],
            },
        }
    } else {
        left
    }
}

fn stmt() -> impl Parser<Token, Stmt, Error = Simple<Token>> {
    // Prepare all the parsers inside the statement function
    let ident = ident().boxed();
    let ident_token = ident_token().boxed();
    let lparen = lparen("Stmt").boxed();
    let rparen = rparen("Stmt").boxed();
    let type_literal = type_literal().boxed();
    let assign = assign("Stmt").boxed();
    let type_ = type_definition().boxed();
    let record_destructure = record_destructure().boxed();
    let array_destructure = array_destructure().boxed();
    let tuple_destructure = tuple_destructure().boxed();

    recursive(move |rstmt| {
        // Because the stmt and expr parsers are mutually recursive the expr parser needs to be
        // instantiated here with the recurst stmt parser
        let expr = expr(rstmt.clone().boxed()).boxed();
        let ident_types = choice((
            ident.clone(),
            record_destructure.clone(),
            array_destructure.clone(),
            tuple_destructure.clone(),
        ))
        .boxed();

        let typed_ident = lparen
            .clone()
            .ignore_then(ident_types.clone())
            .then_ignore(token(TokenKind::Colon, "Typed Ident").boxed())
            .then(type_literal.clone())
            .then_ignore(rparen.clone())
            .map(|(ident, t)| match ident {
                IdentifierType::Identifier(n, _) => IdentifierType::Identifier(n, Some(t)),
                IdentifierType::RecordDestructure(n, _) => {
                    IdentifierType::RecordDestructure(n, Some(t))
                }
                IdentifierType::ArrayDestructure(n, _) => {
                    IdentifierType::ArrayDestructure(n, Some(t))
                }
                IdentifierType::TupleDestructure(n, _) => {
                    IdentifierType::TupleDestructure(n, Some(t))
                }

                x @ IdentifierType::Bucket => x,
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
                .map(|((mutable, identifier), value)| Stmt::LetStatement {
                    identifier,
                    value,
                    mutable,
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
                    .map(|(((name, args), return_), body)| Stmt::FunctionDefinition {
                        name: Some(name),
                        return_type: return_,
                        arguments: args,
                        body,
                    })
                    .boxed()
            };
            token(TokenKind::Let, "Let Stmt")
                .ignore_then(func_definition.or(val))
                .boxed()
        })
        .labelled("Let Stmt")
        .boxed();

        let for_in_loop = token(TokenKind::For, "For In Loop")
            .ignore_then(choice([
                ident.clone(),
                record_destructure,
                array_destructure,
                tuple_destructure,
            ]))
            .then_ignore(token(TokenKind::In, "For In Loop").ignored())
            .then(expr.clone())
            .then_ignore(token(TokenKind::Do, "For In Loop"))
            .then(expr.clone())
            .map(|((i, cond), ex)| Stmt::ForInLoop {
                condition_arg: i,
                condition_expr: cond,
                consequent: ex,
            });

        let while_loop = token(TokenKind::While, "While Loop")
            .ignore_then(expr.clone())
            .then_ignore(token(TokenKind::Do, "While Loop"))
            .then(expr.clone())
            .map(|(cond, body)| Stmt::WhileLoop {
                condition: cond,
                consequent: body,
            });

        choice((
            let_stmt.clone(),
            for_in_loop,
            while_loop,
            type_.clone(),
            expr.clone().map(Stmt::Expr),
        ))
        .padded_by(token(TokenKind::Comment, "Stmt").or_not().ignored())
        .padded_by(newline("Stmt").or_not().ignored())
        .map(|x| {
            log::debug!("Parsed STMT: {:?}", x);
            x
        })
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
    if let Some((kind, TokenState { span, source, file })) = err.found() {
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
