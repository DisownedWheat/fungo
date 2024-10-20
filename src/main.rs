use core::panic;

use ariadne::*;
use ast::ASTNode;
use ast::FungoImport;
use ast::GoImport;
use ast::Identifier;
use ast::IdentifierType;
use ast::LetExpression;
use chumsky::prelude::*;
use chumsky::Error;
use text::digits;
mod ast;

// #[derive(Debug)]
// enum Expr {
//     Num(String),
//     Var(String),
//     Neg(Box<Expr>),
//     Add(Box<Expr>, Box<Expr>),
//     Sub(Box<Expr>, Box<Expr>),
//     Mul(Box<Expr>, Box<Expr>),
//     Div(Box<Expr>, Box<Expr>),
//     Ident(String),
//     String(String),
//     GoImport(String),
//     FungoImport(String),
//
//     Call(String, Vec<Expr>),
//     Let {
//         name: String,
//         rhs: Box<Expr>,
//         // then: Box<Expr>,
//     },
//     Fn {
//         name: String,
//         args: Vec<String>,
//         body: Box<Expr>,
//         then: Box<Expr>,
//     },
// }

fn main() {
    let input = std::fs::read_to_string("./test_file").unwrap();
    let result = parser().parse(input);
    println!("{:?}", result);
}

fn parser<'a>() -> impl Parser<char, Vec<ASTNode>, Error = Simple<char>> {
    let ident = text::ident()
        .padded()
        .map(|s| ASTNode::Identifier(IdentifierType::Identifier(Identifier { value: s }, None)));
    let expr = recursive(|expr| any().map(|_| ASTNode::NoOp));
    let record_destructure = just('{')
        .ignore_then(ident.then_ignore(just(',')).or_not())
        .repeated()
        .then_ignore(just('}'))
        .padded()
        .map(|s| {
            let filtered = s
                .into_iter()
                .map(|x| match x {
                    Some(ASTNode::Identifier(ident @ IdentifierType::Identifier(_, _))) => {
                        Some(ident)
                    }
                    _ => None,
                })
                .filter(|x| x.is_some())
                .map(|x| x.unwrap())
                .collect();
            ASTNode::Identifier(IdentifierType::RecordDestructure(filtered, None))
        });
    let array_destructure = just('[')
        .ignore_then(ident.then_ignore(just(',')).or_not())
        .repeated()
        .then_ignore(just(']'))
        .padded()
        .map(|s| {
            let filtered = s
                .into_iter()
                .map(|x| match x {
                    Some(ASTNode::Identifier(ident @ IdentifierType::Identifier(_, _))) => {
                        Some(ident)
                    }
                    _ => None,
                })
                .filter(|x| x.is_some())
                .map(|x| x.unwrap())
                .collect();
            ASTNode::Identifier(IdentifierType::ArrayDestructure(filtered, None))
        });
    let tuple_destructure = just('(')
        .ignore_then(ident.then_ignore(just(',')).or_not())
        .repeated()
        .then_ignore(just(')'))
        .padded()
        .map(|s| {
            let filtered = s
                .into_iter()
                .map(|x| match x {
                    Some(ASTNode::Identifier(ident @ IdentifierType::Identifier(_, _))) => {
                        Some(ident)
                    }
                    _ => None,
                })
                .filter(|x| x.is_some())
                .map(|x| x.unwrap())
                .collect();
            ASTNode::Identifier(IdentifierType::TupleDestructure(filtered, None))
        });

    let let_identifier = ident
        .or(record_destructure)
        .or(array_destructure)
        .or(tuple_destructure)
        .or(just('_').map(|_| ASTNode::NoOp));

    let let_stmt = text::keyword("let")
        .padded()
        .then(text::keyword("mut").padded().or_not())
        .then(let_identifier)
        .then_ignore(text::keyword("=").padded())
        .then(expr.clone())
        .map(|((x, name), rhs)| {
            println!("{:?}", x);
            let identifier = match name {
                ASTNode::Identifier(n) => n,
                _ => panic!(),
            };
            ASTNode::LetExpression(LetExpression {
                identifier,
                value: Box::new(rhs),
                mutable: false,
            })
        });

    let str_ = just('"')
        .ignore_then(filter(|c: &char| *c != '"').repeated())
        .then_ignore(just('"'))
        .map(|s| ASTNode::StringLiteral(s.iter().collect()));

    let import = text::keyword("import").padded();

    let go_import = import
        .clone()
        .padded_by(filter(|c: &char| c.is_whitespace()).repeated())
        .ignore_then(str_)
        .map(|s| match s {
            ASTNode::StringLiteral(value) => ASTNode::GoImport(GoImport {
                module: value,
                alias: None,
            }),
            _ => panic!(),
        });

    let fungo_import = import.padded().ignore_then(ident).map(|s| match s {
        ASTNode::Identifier(node) => match node {
            IdentifierType::Identifier(Identifier { value, .. }, _) => {
                ASTNode::FungoImport(FungoImport { module: value })
            }
            _ => panic!(),
        },
        _ => panic!(),
    });

    let digit = text::int(10).map(|s| ASTNode::NumberLiteral(s));

    let type_ = text::keyword("type").padded().map(|_| ASTNode::NoOp);
    let token = go_import
        .or(fungo_import)
        .or(let_stmt)
        .or(type_)
        .or(str_)
        .or(digit)
        .or(ident)
        .or(expr.clone())
        .padded();

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
