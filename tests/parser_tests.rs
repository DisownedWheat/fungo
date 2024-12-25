use chumsky::Parser;
use colored::*;
use fungo::ast::*;
use fungo::lexer::*;
use fungo::parser::*;
use serde_json;
static INIT: std::sync::Once = std::sync::Once::new();

pub fn setup() {
    INIT.call_once(|| {
        let _ = env_logger::builder()
            .filter_level(log::LevelFilter::Info)
            .try_init();
    })
}

fn map_kinds(tokens: &Vec<Token>) -> Vec<TokenKind> {
    tokens.iter().map(|(kind, _)| kind.clone()).collect()
}

fn lex_input(input: &str, expected: Vec<TokenKind>) -> Vec<Token> {
    let token_result = lex_raw(input);
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
    setup();
    let input = "
x y 2
y
	(5 +
	4)
	\"Hello World\"
";

    let tokens = lex_input(
        input,
        vec![
            TokenKind::ident("x"),
            TokenKind::ident("y"),
            TokenKind::num("2"),
            TokenKind::NewLine,
            TokenKind::ident("y"),
            TokenKind::NewLine,
            TokenKind::Indent,
            TokenKind::LParen,
            TokenKind::num("5"),
            TokenKind::op("+"),
            TokenKind::NewLine,
            TokenKind::num("4"),
            TokenKind::RParen,
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
                    Expr::Identifier(IdentifierType::Identifier("y".to_string(), None)),
                    Expr::IntLiteral("2".to_owned()),
                ],
            })),
            TopLevel::Stmt(Stmt::Expr(Expr::FunctionCall {
                name: "y".to_owned(),
                args: vec![
                    Expr::FunctionCall {
                        name: "+".to_string(),
                        args: vec![
                            Expr::IntLiteral("5".to_owned()),
                            Expr::IntLiteral("4".to_owned()),
                        ],
                    },
                    Expr::StringLiteral("Hello World".to_owned()),
                ],
            })),
        ],
    );
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
    setup();
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
    setup();
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
    setup();
    let input = "
x * 5
(\"test\") + \"Hello\"
x.InsideValue -
	5 + 2 % 3
x
|> testFunc
|> _.Value
5 + *test
|> square
|| sqrt
x *y
";
    let tokens = lex_input(
        input,
        vec![
            TokenKind::ident("x"),
            TokenKind::op("*"),
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
            TokenKind::ident("x"),
            TokenKind::NewLine,
            TokenKind::op("|>"),
            TokenKind::ident("testFunc"),
            TokenKind::NewLine,
            TokenKind::op("|>"),
            TokenKind::ident("_"),
            TokenKind::Dot,
            TokenKind::ident("Value"),
            TokenKind::NewLine,
            TokenKind::num("5"),
            TokenKind::op("+"),
            TokenKind::Deref,
            TokenKind::ident("test"),
            TokenKind::NewLine,
            TokenKind::op("|>"),
            TokenKind::ident("square"),
            TokenKind::NewLine,
            TokenKind::op("||"),
            TokenKind::ident("sqrt"),
            TokenKind::NewLine,
            TokenKind::ident("x"),
            TokenKind::Deref,
            TokenKind::ident("y"),
            TokenKind::NewLine,
        ],
    );
    let _ = match_output(
        tokens,
        vec![
            TopLevel::Stmt(Stmt::Expr(Expr::FunctionCall {
                name: "*".to_string(),
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
                        name: "%".to_string(),
                        args: vec![
                            Expr::FunctionCall {
                                name: "+".to_string(),
                                args: vec![
                                    Expr::IntLiteral("5".to_string()),
                                    Expr::IntLiteral("2".to_string()),
                                ],
                            },
                            Expr::IntLiteral("3".to_string()),
                        ],
                    })]),
                ],
            })),
            TopLevel::Stmt(Stmt::Expr(Expr::FunctionCall {
                name: "|>".to_string(),
                args: vec![
                    Expr::FunctionCall {
                        name: "|>".to_string(),
                        args: vec![
                            Expr::Identifier(IdentifierType::Identifier("x".to_string(), None)),
                            Expr::Identifier(IdentifierType::Identifier(
                                "testFunc".to_string(),
                                None,
                            )),
                        ],
                    },
                    Expr::Accessor {
                        left: Box::new(Expr::Identifier(IdentifierType::Bucket)),
                        right: Box::new(Expr::Identifier(IdentifierType::Identifier(
                            "Value".to_string(),
                            None,
                        ))),
                    },
                ],
            })),
            TopLevel::Stmt(Stmt::Expr(Expr::FunctionCall {
                name: "||".to_string(),
                args: vec![
                    Expr::FunctionCall {
                        name: "|>".to_string(),
                        args: vec![
                            Expr::FunctionCall {
                                name: "+".to_string(),
                                args: vec![
                                    Expr::IntLiteral("5".to_string()),
                                    Expr::Identifier(IdentifierType::Deref(Box::new(
                                        IdentifierType::Identifier("test".to_string(), None),
                                    ))),
                                ],
                            },
                            Expr::Identifier(IdentifierType::Identifier(
                                "square".to_string(),
                                None,
                            )),
                        ],
                    },
                    Expr::Identifier(IdentifierType::Identifier("sqrt".to_string(), None)),
                ],
            })),
            TopLevel::Stmt(Stmt::Expr(Expr::FunctionCall {
                name: "x".to_string(),
                args: vec![Expr::Identifier(IdentifierType::Deref(Box::new(
                    IdentifierType::Identifier("y".to_string(), None),
                )))],
            })),
        ],
    );
}

#[test]
fn test_let_stmt() {
    setup();
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
    setup();
    let input = "
let firstTest x y = x + y
let testFunc x (y: int) z: int =
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
            TokenKind::Colon,
            TokenKind::ident("int"),
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
            TokenKind::op("*"),
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
                return_type: Some(Type::Type {
                    name: "int".to_string(),
                    module: None,
                }),
                body: (Expr::Block(vec![
                    Stmt::LetStatement {
                        identifier: IdentifierType::Identifier("result".to_string(), None),
                        mutable: false,
                        value: Expr::FunctionCall {
                            name: "+".to_string(),
                            args: vec![
                                Expr::Identifier(IdentifierType::Identifier("x".to_string(), None)),
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
    setup();
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

#[test]
fn test_if_expr() {
    setup();
    let input = "
if x then x + 1
if x + 1 = 5 then x
if x then x + 1 else x + 2
if testRecord.Value > 5 then
	{Value = 5}

if testRecord.Value < 5 then
	testRecord
else
	{Value = 5}
";
    let tokens = lex_input(
        input,
        vec![
            TokenKind::If,
            TokenKind::ident("x"),
            TokenKind::Then,
            TokenKind::ident("x"),
            TokenKind::op("+"),
            TokenKind::num("1"),
            TokenKind::NewLine,
            TokenKind::If,
            TokenKind::ident("x"),
            TokenKind::op("+"),
            TokenKind::num("1"),
            TokenKind::Assign,
            TokenKind::num("5"),
            TokenKind::Then,
            TokenKind::ident("x"),
            TokenKind::NewLine,
            TokenKind::If,
            TokenKind::ident("x"),
            TokenKind::Then,
            TokenKind::ident("x"),
            TokenKind::op("+"),
            TokenKind::num("1"),
            TokenKind::Else,
            TokenKind::ident("x"),
            TokenKind::op("+"),
            TokenKind::num("2"),
            TokenKind::NewLine,
            TokenKind::If,
            TokenKind::ident("testRecord"),
            TokenKind::Dot,
            TokenKind::ident("Value"),
            TokenKind::op(">"),
            TokenKind::num("5"),
            TokenKind::Then,
            TokenKind::NewLine,
            TokenKind::Indent,
            TokenKind::LBrace,
            TokenKind::ident("Value"),
            TokenKind::Assign,
            TokenKind::num("5"),
            TokenKind::RBrace,
            TokenKind::NewLine,
            TokenKind::Dedent,
            TokenKind::If,
            TokenKind::ident("testRecord"),
            TokenKind::Dot,
            TokenKind::ident("Value"),
            TokenKind::op("<"),
            TokenKind::num("5"),
            TokenKind::Then,
            TokenKind::NewLine,
            TokenKind::Indent,
            TokenKind::ident("testRecord"),
            TokenKind::NewLine,
            TokenKind::Dedent,
            TokenKind::Else,
            TokenKind::NewLine,
            TokenKind::Indent,
            TokenKind::LBrace,
            TokenKind::ident("Value"),
            TokenKind::Assign,
            TokenKind::num("5"),
            TokenKind::RBrace,
            TokenKind::NewLine,
            TokenKind::Dedent,
        ],
    );

    let _ = match_output(
        tokens,
        vec![
            TopLevel::Stmt(Stmt::Expr(Expr::IfExpr {
                condition: Box::new(Expr::Identifier(IdentifierType::Identifier(
                    "x".to_string(),
                    None,
                ))),
                consequent: Box::new(Expr::FunctionCall {
                    name: "+".to_string(),
                    args: vec![
                        Expr::Identifier(IdentifierType::Identifier("x".to_string(), None)),
                        Expr::IntLiteral("1".to_string()),
                    ],
                }),
                alternative: None,
            })),
            TopLevel::Stmt(Stmt::Expr(Expr::IfExpr {
                condition: Box::new(Expr::FunctionCall {
                    name: "=".to_string(),
                    args: vec![
                        Expr::FunctionCall {
                            name: "+".to_string(),
                            args: vec![
                                Expr::Identifier(IdentifierType::Identifier("x".to_string(), None)),
                                Expr::IntLiteral("1".to_string()),
                            ],
                        },
                        Expr::IntLiteral("5".to_string()),
                    ],
                }),
                consequent: Box::new(Expr::Identifier(IdentifierType::Identifier(
                    "x".to_string(),
                    None,
                ))),
                alternative: None,
            })),
            TopLevel::Stmt(Stmt::Expr(Expr::IfExpr {
                condition: Box::new(Expr::Identifier(IdentifierType::Identifier(
                    "x".to_string(),
                    None,
                ))),
                consequent: Box::new(Expr::FunctionCall {
                    name: "+".to_string(),
                    args: vec![
                        Expr::Identifier(IdentifierType::Identifier("x".to_string(), None)),
                        Expr::IntLiteral("1".to_string()),
                    ],
                }),
                alternative: Some(Box::new(Expr::FunctionCall {
                    name: "+".to_string(),
                    args: vec![
                        Expr::Identifier(IdentifierType::Identifier("x".to_string(), None)),
                        Expr::IntLiteral("2".to_string()),
                    ],
                })),
            })),
            TopLevel::Stmt(Stmt::Expr(Expr::IfExpr {
                condition: Box::new(Expr::FunctionCall {
                    name: ">".to_string(),
                    args: vec![
                        Expr::Accessor {
                            left: Box::new(Expr::Identifier(IdentifierType::Identifier(
                                "testRecord".to_string(),
                                None,
                            ))),
                            right: Box::new(Expr::Identifier(IdentifierType::Identifier(
                                "Value".to_string(),
                                None,
                            ))),
                        },
                        Expr::IntLiteral("5".to_string()),
                    ],
                }),
                consequent: Box::new(Expr::Block(vec![Stmt::Expr(Expr::RecordLiteral {
                    fields: vec![RecordField {
                        name: "Value".to_string(),
                        value: Expr::IntLiteral("5".to_string()),
                    }],
                })])),
                alternative: None,
            })),
            TopLevel::Stmt(Stmt::Expr(Expr::IfExpr {
                condition: Box::new(Expr::FunctionCall {
                    name: "<".to_string(),
                    args: vec![
                        Expr::Accessor {
                            left: Box::new(Expr::Identifier(IdentifierType::Identifier(
                                "testRecord".to_string(),
                                None,
                            ))),
                            right: Box::new(Expr::Identifier(IdentifierType::Identifier(
                                "Value".to_string(),
                                None,
                            ))),
                        },
                        Expr::IntLiteral("5".to_string()),
                    ],
                }),
                consequent: Box::new(Expr::Block(vec![Stmt::Expr(Expr::Identifier(
                    IdentifierType::Identifier("testRecord".to_string(), None),
                ))])),
                alternative: Some(Box::new(Expr::Block(vec![Stmt::Expr(
                    Expr::RecordLiteral {
                        fields: vec![RecordField {
                            name: "Value".to_string(),
                            value: Expr::IntLiteral("5".to_string()),
                        }],
                    },
                )]))),
            })),
        ],
    );
}

#[test]
fn test_indexing() {
    setup();
    let input = "
arr.[1]
map.[\"Hello\"]
(x + 5 |> toSlice).[0]
arr.[5 * 10]
";
    let tokens = lex_input(
        input,
        vec![
            TokenKind::ident("arr"),
            TokenKind::Dot,
            TokenKind::LBracket,
            TokenKind::num("1"),
            TokenKind::RBracket,
            TokenKind::NewLine,
            TokenKind::ident("map"),
            TokenKind::Dot,
            TokenKind::LBracket,
            TokenKind::str("Hello"),
            TokenKind::RBracket,
            TokenKind::NewLine,
            TokenKind::LParen,
            TokenKind::ident("x"),
            TokenKind::op("+"),
            TokenKind::num("5"),
            TokenKind::op("|>"),
            TokenKind::ident("toSlice"),
            TokenKind::RParen,
            TokenKind::Dot,
            TokenKind::LBracket,
            TokenKind::num("0"),
            TokenKind::RBracket,
            TokenKind::NewLine,
            TokenKind::ident("arr"),
            TokenKind::Dot,
            TokenKind::LBracket,
            TokenKind::num("5"),
            TokenKind::op("*"),
            TokenKind::num("10"),
            TokenKind::RBracket,
            TokenKind::NewLine,
        ],
    );

    let _ = match_output(
        tokens,
        vec![
            TopLevel::Stmt(Stmt::Expr(Expr::Accessor {
                left: Box::new(Expr::Identifier(IdentifierType::Identifier(
                    "arr".to_string(),
                    None,
                ))),
                right: Box::new(Expr::ArrayLiteral(vec![Expr::IntLiteral("1".to_string())])),
            })),
            TopLevel::Stmt(Stmt::Expr(Expr::Accessor {
                left: Box::new(Expr::Identifier(IdentifierType::Identifier(
                    "map".to_string(),
                    None,
                ))),
                right: Box::new(Expr::ArrayLiteral(vec![Expr::StringLiteral(
                    "Hello".to_string(),
                )])),
            })),
            TopLevel::Stmt(Stmt::Expr(Expr::Accessor {
                left: Box::new(Expr::FunctionCall {
                    name: "|>".to_string(),
                    args: vec![
                        Expr::FunctionCall {
                            name: "+".to_string(),
                            args: vec![
                                Expr::Identifier(IdentifierType::Identifier("x".to_string(), None)),
                                Expr::IntLiteral("5".to_string()),
                            ],
                        },
                        Expr::Identifier(IdentifierType::Identifier("toSlice".to_string(), None)),
                    ],
                }),
                right: Box::new(Expr::ArrayLiteral(vec![Expr::IntLiteral("0".to_string())])),
            })),
            TopLevel::Stmt(Stmt::Expr(Expr::Accessor {
                left: Box::new(Expr::Identifier(IdentifierType::Identifier(
                    "arr".to_string(),
                    None,
                ))),
                right: Box::new(Expr::ArrayLiteral(vec![Expr::FunctionCall {
                    name: "*".to_string(),
                    args: vec![
                        Expr::IntLiteral("5".to_string()),
                        Expr::IntLiteral("10".to_string()),
                    ],
                }])),
            })),
        ],
    );
}

#[test]
fn test_lambda() {
    setup();
    let input = "
fun (x, y) -> x + y
(fun x ->
	square x)
fun _ -> 5
fun _ (x: int): int -> x + 5
";
    let tokens = lex_input(
        input,
        vec![
            TokenKind::Lambda,
            TokenKind::LParen,
            TokenKind::ident("x"),
            TokenKind::Comma,
            TokenKind::ident("y"),
            TokenKind::RParen,
            TokenKind::op("->"),
            TokenKind::ident("x"),
            TokenKind::op("+"),
            TokenKind::ident("y"),
            TokenKind::NewLine,
            TokenKind::LParen,
            TokenKind::Lambda,
            TokenKind::ident("x"),
            TokenKind::op("->"),
            TokenKind::NewLine,
            TokenKind::Indent,
            TokenKind::ident("square"),
            TokenKind::ident("x"),
            TokenKind::RParen,
            TokenKind::NewLine,
            TokenKind::Dedent,
            TokenKind::Lambda,
            TokenKind::ident("_"),
            TokenKind::op("->"),
            TokenKind::num("5"),
            TokenKind::NewLine,
            TokenKind::Lambda,
            TokenKind::ident("_"),
            TokenKind::LParen,
            TokenKind::ident("x"),
            TokenKind::Colon,
            TokenKind::ident("int"),
            TokenKind::RParen,
            TokenKind::Colon,
            TokenKind::ident("int"),
            TokenKind::op("->"),
            TokenKind::ident("x"),
            TokenKind::op("+"),
            TokenKind::num("5"),
            TokenKind::NewLine,
        ],
    );

    let _ = match_output(
        tokens,
        vec![
            TopLevel::Stmt(Stmt::Expr(Expr::Lambda {
                args: vec![IdentifierType::TupleDestructure(
                    vec![
                        IdentifierType::Identifier("x".to_string(), None),
                        IdentifierType::Identifier("y".to_string(), None),
                    ],
                    None,
                )],
                return_type: None,
                body: Box::new(Expr::FunctionCall {
                    name: "+".to_string(),
                    args: vec![
                        Expr::Identifier(IdentifierType::Identifier("x".to_string(), None)),
                        Expr::Identifier(IdentifierType::Identifier("y".to_string(), None)),
                    ],
                }),
            })),
            TopLevel::Stmt(Stmt::Expr(Expr::Lambda {
                args: vec![IdentifierType::Identifier("x".to_string(), None)],
                return_type: None,
                body: Box::new(Expr::Block(vec![Stmt::Expr(Expr::FunctionCall {
                    name: "square".to_string(),
                    args: vec![Expr::Identifier(IdentifierType::Identifier(
                        "x".to_string(),
                        None,
                    ))],
                })])),
            })),
            TopLevel::Stmt(Stmt::Expr(Expr::Lambda {
                args: vec![IdentifierType::Bucket],
                return_type: None,
                body: Box::new(Expr::IntLiteral("5".to_string())),
            })),
            TopLevel::Stmt(Stmt::Expr(Expr::Lambda {
                args: vec![
                    IdentifierType::Bucket,
                    IdentifierType::Identifier(
                        "x".to_string(),
                        Some(Type::Type {
                            name: "int".to_string(),
                            module: None,
                        }),
                    ),
                ],
                return_type: Some(Type::Type {
                    name: "int".to_string(),
                    module: None,
                }),
                body: Box::new(Expr::FunctionCall {
                    name: "+".to_string(),
                    args: vec![
                        Expr::Identifier(IdentifierType::Identifier("x".to_string(), None)),
                        Expr::IntLiteral("5".to_string()),
                    ],
                }),
            })),
        ],
    );
}

#[test]
fn test_for_in_loop() {
    setup();
    let input = "
for x in 1 .. 10 do x
for (a, b) in (func x y) do
	a + b
";
    let tokens = lex_input(
        input,
        vec![
            TokenKind::For,
            TokenKind::ident("x"),
            TokenKind::In,
            TokenKind::num("1"),
            TokenKind::op(".."),
            TokenKind::num("10"),
            TokenKind::Do,
            TokenKind::ident("x"),
            TokenKind::NewLine,
            TokenKind::For,
            TokenKind::LParen,
            TokenKind::ident("a"),
            TokenKind::Comma,
            TokenKind::ident("b"),
            TokenKind::RParen,
            TokenKind::In,
            TokenKind::LParen,
            TokenKind::ident("func"),
            TokenKind::ident("x"),
            TokenKind::ident("y"),
            TokenKind::RParen,
            TokenKind::Do,
            TokenKind::NewLine,
            TokenKind::Indent,
            TokenKind::ident("a"),
            TokenKind::op("+"),
            TokenKind::ident("b"),
            TokenKind::NewLine,
            TokenKind::Dedent,
        ],
    );

    let _ = match_output(
        tokens,
        vec![
            TopLevel::Stmt(Stmt::ForInLoop {
                condition_arg: IdentifierType::Identifier("x".to_string(), None),
                condition_expr: Expr::FunctionCall {
                    name: "..".to_string(),
                    args: vec![
                        Expr::IntLiteral("1".to_string()),
                        Expr::IntLiteral("10".to_string()),
                    ],
                },
                consequent: Expr::Identifier(IdentifierType::Identifier("x".to_string(), None)),
            }),
            TopLevel::Stmt(Stmt::ForInLoop {
                condition_arg: IdentifierType::TupleDestructure(
                    vec![
                        IdentifierType::Identifier("a".to_string(), None),
                        IdentifierType::Identifier("b".to_string(), None),
                    ],
                    None,
                ),
                condition_expr: Expr::FunctionCall {
                    name: "func".to_string(),
                    args: vec![
                        Expr::Identifier(IdentifierType::Identifier("x".to_string(), None)),
                        Expr::Identifier(IdentifierType::Identifier("y".to_string(), None)),
                    ],
                },
                consequent: Expr::Block(vec![Stmt::Expr(Expr::FunctionCall {
                    name: "+".to_string(),
                    args: vec![
                        Expr::Identifier(IdentifierType::Identifier("a".to_string(), None)),
                        Expr::Identifier(IdentifierType::Identifier("b".to_string(), None)),
                    ],
                })]),
            }),
        ],
    );
}
