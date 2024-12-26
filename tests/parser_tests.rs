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
    let tokens = token_result.unwrap().0;
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

fn parse_input(input: Vec<Token>) -> (Vec<ASTString>, Vec<TopLevel>) {
    let output_result = parser()
        .parse(input)
        .inspect_err(|errs| errs.iter().for_each(|e| error_report(e)));
    assert!(output_result.is_ok());
    output_result.unwrap()
}

fn match_output(tokens: Vec<Token>, expected: Vec<TopLevel>) {
    let output = parse_input(tokens);
    assert_eq!(
        output.1,
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
namespace test
x y 2
y
	(5 +
	4)
	\"Hello World\"
";

    let tokens = lex_input(
        input,
        vec![
            TokenKind::NameSpace,
            TokenKind::ident("test"),
            TokenKind::NewLine,
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
                name: ASTString::from_str("x"),
                args: vec![
                    Expr::Identifier(IdentifierType::Identifier(ASTString::from_str("y"), None)),
                    Expr::IntLiteral(ASTString::from_str("2")),
                ],
            })),
            TopLevel::Stmt(Stmt::Expr(Expr::FunctionCall {
                name: ASTString::from_str("y"),
                args: vec![
                    Expr::FunctionCall {
                        name: ASTString::from_str("+"),
                        args: vec![
                            Expr::IntLiteral(ASTString::from_str("5")),
                            Expr::IntLiteral(ASTString::from_str("4")),
                        ],
                    },
                    Expr::StringLiteral(ASTString::from_str("Hello World")),
                ],
            })),
        ],
    );
}

#[test]
fn test_opens() {
    setup();
    let input = "
namespace test
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
            TokenKind::NameSpace,
            TokenKind::ident("test"),
            TokenKind::NewLine,
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
            TopLevel::GoImport {
                module: ASTString::from_str("fmt"),
                alias: None,
            },
            TopLevel::FungoImport(FungoImport {
                module: ASTString::from_str("Test"),
            }),
            TopLevel::Stmt(Stmt::Expr(Expr::FunctionCall {
                name: ASTString::from_str("testCall"),
                args: vec![
                    Expr::TupleLiteral(vec![
                        Expr::IntLiteral(ASTString::from_str("1")),
                        Expr::IntLiteral(ASTString::from_str("2")),
                    ]),
                    Expr::IntLiteral(ASTString::from_str("3")),
                    Expr::IntLiteral(ASTString::from_str("4")),
                ],
            })),
        ],
    );
}

#[test]
fn block_expression() {
    setup();
    let input = "
namespace test
let x =
	testFunc 0 1 \"a\"
";
    let tokens = lex_input(
        input,
        vec![
            TokenKind::NameSpace,
            TokenKind::ident("test"),
            TokenKind::NewLine,
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
            identifier: IdentifierType::Identifier(ASTString::from_str("x"), None),
            mutable: false,
            value: Expr::Block(vec![Stmt::Expr(Expr::FunctionCall {
                name: ASTString::from_str("testFunc"),
                args: vec![
                    Expr::IntLiteral(ASTString::from_str("0")),
                    Expr::IntLiteral(ASTString::from_str("1")),
                    Expr::StringLiteral(ASTString::from_str("a")),
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
namespace test
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
                TokenKind::NameSpace,
                TokenKind::ident("test"),
                TokenKind::NewLine,
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
                    ASTString::from_str("Testing"),
                    TypeDef::RecordDefinition(RecordDefinition {
                        fields: vec![
                            RecordDefinitionField {
                                name: ASTString::from_str("x"),
                                type_: TypeDef::Type(Type::Type {
                                    name: ASTString::from_str("int"),
                                    module: None,
                                }),
                            },
                            RecordDefinitionField {
                                name: ASTString::from_str("y"),
                                type_: TypeDef::Type(Type::Type {
                                    name: ASTString::from_str("int"),
                                    module: None,
                                }),
                            },
                        ],
                    }),
                )),
                TopLevel::Stmt(Stmt::TypeDefinition(
                    ASTString::from_str("TestRecord"),
                    TypeDef::RecordDefinition(RecordDefinition {
                        fields: vec![
                            RecordDefinitionField {
                                name: ASTString::from_str("TestVal"),
                                type_: TypeDef::Type(Type::Slice(Box::new(Type::Pointer(
                                    Box::new(Type::Type {
                                        name: ASTString::from_str("int"),
                                        module: None,
                                    }),
                                )))),
                            },
                            RecordDefinitionField {
                                name: ASTString::from_str("AnotherTest"),
                                type_: TypeDef::RecordDefinition(RecordDefinition {
                                    fields: vec![RecordDefinitionField {
                                        name: ASTString::from_str("InteriorTest"),
                                        type_: TypeDef::Type(Type::Type {
                                            name: ASTString::from_str("string"),
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
namespace test
type TestTuple = (int, string, TestRecord)
";
        let tokens = lex_input(
            input,
            vec![
                TokenKind::NameSpace,
                TokenKind::ident("test"),
                TokenKind::NewLine,
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
                ASTString::from_str("TestTuple"),
                TypeDef::TupleDefinition {
                    length: 3,
                    types: vec![
                        TypeDef::Type(Type::Type {
                            name: ASTString::from_str("int"),
                            module: None,
                        }),
                        TypeDef::Type(Type::Type {
                            name: ASTString::from_str("string"),
                            module: None,
                        }),
                        TypeDef::Type(Type::Type {
                            name: ASTString::from_str("TestRecord"),
                            module: None,
                        }),
                    ],
                },
            ))],
        );
    }
    {
        let input = "
namespace test
type TestADT =
	| Test
	| Testing of string

type Test2 = | Test	| Testing of int
";
        let tokens = lex_input(
            input,
            vec![
                TokenKind::NameSpace,
                TokenKind::ident("test"),
                TokenKind::NewLine,
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
namespace test
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
            TokenKind::NameSpace,
            TokenKind::ident("test"),
            TokenKind::NewLine,
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
                name: ASTString::from_str("*"),
                args: vec![
                    Expr::Identifier(IdentifierType::Identifier(ASTString::from_str("x"), None)),
                    Expr::IntLiteral(ASTString::from_str("5")),
                ],
            })),
            TopLevel::Stmt(Stmt::Expr(Expr::FunctionCall {
                name: ASTString::from_str("+"),
                args: vec![
                    Expr::StringLiteral(ASTString::from_str("test")),
                    Expr::StringLiteral(ASTString::from_str("Hello")),
                ],
            })),
            TopLevel::Stmt(Stmt::Expr(Expr::FunctionCall {
                name: ASTString::from_str("-"),
                args: vec![
                    Expr::Accessor {
                        left: Box::new(Expr::Identifier(IdentifierType::Identifier(
                            ASTString::from_str("x"),
                            None,
                        ))),
                        right: Box::new(Expr::Identifier(IdentifierType::Identifier(
                            ASTString::from_str("InsideValue"),
                            None,
                        ))),
                    },
                    Expr::Block(vec![Stmt::Expr(Expr::FunctionCall {
                        name: ASTString::from_str("%"),
                        args: vec![
                            Expr::FunctionCall {
                                name: ASTString::from_str("+"),
                                args: vec![
                                    Expr::IntLiteral(ASTString::from_str("5")),
                                    Expr::IntLiteral(ASTString::from_str("2")),
                                ],
                            },
                            Expr::IntLiteral(ASTString::from_str("3")),
                        ],
                    })]),
                ],
            })),
            TopLevel::Stmt(Stmt::Expr(Expr::FunctionCall {
                name: ASTString::from_str("|>"),
                args: vec![
                    Expr::FunctionCall {
                        name: ASTString::from_str("|>"),
                        args: vec![
                            Expr::Identifier(IdentifierType::Identifier(
                                ASTString::from_str("x"),
                                None,
                            )),
                            Expr::Identifier(IdentifierType::Identifier(
                                ASTString::from_str("testFunc"),
                                None,
                            )),
                        ],
                    },
                    Expr::Accessor {
                        left: Box::new(Expr::Identifier(IdentifierType::Bucket)),
                        right: Box::new(Expr::Identifier(IdentifierType::Identifier(
                            ASTString::from_str("Value"),
                            None,
                        ))),
                    },
                ],
            })),
            TopLevel::Stmt(Stmt::Expr(Expr::FunctionCall {
                name: ASTString::from_str("||"),
                args: vec![
                    Expr::FunctionCall {
                        name: ASTString::from_str("|>"),
                        args: vec![
                            Expr::FunctionCall {
                                name: ASTString::from_str("+"),
                                args: vec![
                                    Expr::IntLiteral(ASTString::from_str("5")),
                                    Expr::Identifier(IdentifierType::Deref(Box::new(
                                        IdentifierType::Identifier(
                                            ASTString::from_str("test"),
                                            None,
                                        ),
                                    ))),
                                ],
                            },
                            Expr::Identifier(IdentifierType::Identifier(
                                ASTString::from_str("square"),
                                None,
                            )),
                        ],
                    },
                    Expr::Identifier(IdentifierType::Identifier(
                        ASTString::from_str("sqrt"),
                        None,
                    )),
                ],
            })),
            TopLevel::Stmt(Stmt::Expr(Expr::FunctionCall {
                name: ASTString::from_str("x"),
                args: vec![Expr::Identifier(IdentifierType::Deref(Box::new(
                    IdentifierType::Identifier(ASTString::from_str("y"), None),
                )))],
            })),
        ],
    );
}

#[test]
fn test_let_stmt() {
    setup();
    let input = "
namespace test
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
            TokenKind::NameSpace,
            TokenKind::ident("test"),
            TokenKind::NewLine,
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
                identifier: IdentifierType::Identifier(ASTString::from_str("x"), None),
                value: Expr::IntLiteral(ASTString::from_str("1")),
                mutable: false,
            }),
            TopLevel::Stmt(Stmt::LetStatement {
                identifier: IdentifierType::Identifier(ASTString::from_str("y"), None),
                value: Expr::IntLiteral(ASTString::from_str("2")),
                mutable: false,
            }),
            TopLevel::Stmt(Stmt::LetStatement {
                identifier: IdentifierType::Identifier(ASTString::from_str("z"), None),
                value: Expr::RecordLiteral {
                    fields: vec![RecordField {
                        name: ASTString::from_str("TestValue"),
                        value: Expr::BoolLiteral(true),
                    }],
                },
                mutable: false,
            }),
            TopLevel::Stmt(Stmt::LetStatement {
                identifier: IdentifierType::Identifier(ASTString::from_str("a"), None),
                value: Expr::TupleLiteral(vec![
                    Expr::StringLiteral(ASTString::from_str("Hello")),
                    Expr::StringLiteral(ASTString::from_str("World")),
                ]),
                mutable: false,
            }),
            TopLevel::Stmt(Stmt::LetStatement {
                identifier: IdentifierType::Identifier(ASTString::from_str("b"), None),
                value: Expr::Block(vec![
                    Stmt::LetStatement {
                        identifier: IdentifierType::Identifier(ASTString::from_str("inner"), None),
                        value: Expr::FunctionCall {
                            name: ASTString::from_str("+"),
                            args: vec![
                                Expr::IntLiteral(ASTString::from_str("5")),
                                Expr::IntLiteral(ASTString::from_str("5")),
                            ],
                        },
                        mutable: false,
                    },
                    Stmt::Expr(Expr::Identifier(IdentifierType::Identifier(
                        ASTString::from_str("inner"),
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
namespace test
let firstTest x y = x + y
let testFunc x (y: int) z: int =
	let result = x + 1
	result * z
";
    let tokens = lex_input(
        input,
        vec![
            TokenKind::NameSpace,
            TokenKind::ident("test"),
            TokenKind::NewLine,
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
                name: Some(ASTString::from_str("firstTest")),
                arguments: vec![
                    IdentifierType::Identifier(ASTString::from_str("x"), None),
                    IdentifierType::Identifier(ASTString::from_str("y"), None),
                ],
                return_type: None,
                body: (Expr::FunctionCall {
                    name: ASTString::from_str("+"),
                    args: vec![
                        Expr::Identifier(IdentifierType::Identifier(
                            ASTString::from_str("x"),
                            None,
                        )),
                        Expr::Identifier(IdentifierType::Identifier(
                            ASTString::from_str("y"),
                            None,
                        )),
                    ],
                }),
            }),
            TopLevel::Stmt(Stmt::FunctionDefinition {
                name: Some(ASTString::from_str("testFunc")),
                arguments: vec![
                    IdentifierType::Identifier(ASTString::from_str("x"), None),
                    IdentifierType::Identifier(
                        ASTString::from_str("y"),
                        Some(Type::Type {
                            name: ASTString::from_str("int"),
                            module: None,
                        }),
                    ),
                    IdentifierType::Identifier(ASTString::from_str("z"), None),
                ],
                return_type: Some(Type::Type {
                    name: ASTString::from_str("int"),
                    module: None,
                }),
                body: (Expr::Block(vec![
                    Stmt::LetStatement {
                        identifier: IdentifierType::Identifier(ASTString::from_str("result"), None),
                        mutable: false,
                        value: Expr::FunctionCall {
                            name: ASTString::from_str("+"),
                            args: vec![
                                Expr::Identifier(IdentifierType::Identifier(
                                    ASTString::from_str("x"),
                                    None,
                                )),
                                Expr::IntLiteral(ASTString::from_str("1")),
                            ],
                        },
                    },
                    Stmt::Expr(Expr::FunctionCall {
                        name: ASTString::from_str("*"),
                        args: vec![
                            Expr::Identifier(IdentifierType::Identifier(
                                ASTString::from_str("result"),
                                None,
                            )),
                            Expr::Identifier(IdentifierType::Identifier(
                                ASTString::from_str("z"),
                                None,
                            )),
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
namespace test
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
            TokenKind::NameSpace,
            TokenKind::ident("test"),
            TokenKind::NewLine,
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
                identifier: IdentifierType::Identifier(ASTString::from_str("singleLine"), None),
                value: Expr::RecordLiteral {
                    fields: vec![
                        RecordField {
                            name: ASTString::from_str("TestValue"),
                            value: Expr::StringLiteral(ASTString::from_str("Test")),
                        },
                        RecordField {
                            name: ASTString::from_str("Val"),
                            value: Expr::BoolLiteral(true),
                        },
                    ],
                },
                mutable: false,
            }),
            TopLevel::Stmt(Stmt::LetStatement {
                identifier: IdentifierType::Identifier(ASTString::from_str("multiLine"), None),
                mutable: false,
                value: Expr::Block(vec![Stmt::Expr(Expr::RecordLiteral {
                    fields: vec![
                        RecordField {
                            name: ASTString::from_str("TestValue"),
                            value: Expr::StringLiteral(ASTString::from_str("Hello World")),
                        },
                        RecordField {
                            name: ASTString::from_str("Val"),
                            value: Expr::BoolLiteral(false),
                        },
                    ],
                })]),
            }),
            TopLevel::Stmt(Stmt::LetStatement {
                identifier: IdentifierType::Identifier(ASTString::from_str("singleTuple"), None),
                value: Expr::TupleLiteral(vec![
                    Expr::StringLiteral(ASTString::from_str("Hello World")),
                    Expr::IntLiteral(ASTString::from_str("15")),
                ]),
                mutable: false,
            }),
            TopLevel::Stmt(Stmt::LetStatement {
                identifier: IdentifierType::Identifier(ASTString::from_str("multiTuple"), None),
                value: Expr::Block(vec![Stmt::Expr(Expr::TupleLiteral(vec![
                    Expr::IntLiteral(ASTString::from_str("15")),
                    Expr::TupleLiteral(vec![
                        Expr::StringLiteral(ASTString::from_str("Hello")),
                        Expr::IntLiteral(ASTString::from_str("20")),
                    ]),
                ]))]),
                mutable: false,
            }),
            TopLevel::Stmt(Stmt::LetStatement {
                identifier: IdentifierType::Identifier(ASTString::from_str("singleList"), None),
                value: Expr::ArrayLiteral(vec![
                    Expr::IntLiteral(ASTString::from_str("1")),
                    Expr::IntLiteral(ASTString::from_str("2")),
                    Expr::IntLiteral(ASTString::from_str("3")),
                    Expr::IntLiteral(ASTString::from_str("4")),
                ]),
                mutable: false,
            }),
            TopLevel::Stmt(Stmt::LetStatement {
                identifier: IdentifierType::Identifier(ASTString::from_str("multiList"), None),
                value: Expr::Block(vec![Stmt::Expr(Expr::ArrayLiteral(vec![
                    Expr::IntLiteral(ASTString::from_str("1")),
                    Expr::IntLiteral(ASTString::from_str("2")),
                    Expr::IntLiteral(ASTString::from_str("3")),
                    Expr::IntLiteral(ASTString::from_str("4")),
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
namespace test
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
            TokenKind::NameSpace,
            TokenKind::ident("test"),
            TokenKind::NewLine,
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
                    ASTString::from_str("x"),
                    None,
                ))),
                consequent: Box::new(Expr::FunctionCall {
                    name: ASTString::from_str("+"),
                    args: vec![
                        Expr::Identifier(IdentifierType::Identifier(
                            ASTString::from_str("x"),
                            None,
                        )),
                        Expr::IntLiteral(ASTString::from_str("1")),
                    ],
                }),
                alternative: None,
            })),
            TopLevel::Stmt(Stmt::Expr(Expr::IfExpr {
                condition: Box::new(Expr::FunctionCall {
                    name: ASTString::from_str("="),
                    args: vec![
                        Expr::FunctionCall {
                            name: ASTString::from_str("+"),
                            args: vec![
                                Expr::Identifier(IdentifierType::Identifier(
                                    ASTString::from_str("x"),
                                    None,
                                )),
                                Expr::IntLiteral(ASTString::from_str("1")),
                            ],
                        },
                        Expr::IntLiteral(ASTString::from_str("5")),
                    ],
                }),
                consequent: Box::new(Expr::Identifier(IdentifierType::Identifier(
                    ASTString::from_str("x"),
                    None,
                ))),
                alternative: None,
            })),
            TopLevel::Stmt(Stmt::Expr(Expr::IfExpr {
                condition: Box::new(Expr::Identifier(IdentifierType::Identifier(
                    ASTString::from_str("x"),
                    None,
                ))),
                consequent: Box::new(Expr::FunctionCall {
                    name: ASTString::from_str("+"),
                    args: vec![
                        Expr::Identifier(IdentifierType::Identifier(
                            ASTString::from_str("x"),
                            None,
                        )),
                        Expr::IntLiteral(ASTString::from_str("1")),
                    ],
                }),
                alternative: Some(Box::new(Expr::FunctionCall {
                    name: ASTString::from_str("+"),
                    args: vec![
                        Expr::Identifier(IdentifierType::Identifier(
                            ASTString::from_str("x"),
                            None,
                        )),
                        Expr::IntLiteral(ASTString::from_str("2")),
                    ],
                })),
            })),
            TopLevel::Stmt(Stmt::Expr(Expr::IfExpr {
                condition: Box::new(Expr::FunctionCall {
                    name: ASTString::from_str(">"),
                    args: vec![
                        Expr::Accessor {
                            left: Box::new(Expr::Identifier(IdentifierType::Identifier(
                                ASTString::from_str("testRecord"),
                                None,
                            ))),
                            right: Box::new(Expr::Identifier(IdentifierType::Identifier(
                                ASTString::from_str("Value"),
                                None,
                            ))),
                        },
                        Expr::IntLiteral(ASTString::from_str("5")),
                    ],
                }),
                consequent: Box::new(Expr::Block(vec![Stmt::Expr(Expr::RecordLiteral {
                    fields: vec![RecordField {
                        name: ASTString::from_str("Value"),
                        value: Expr::IntLiteral(ASTString::from_str("5")),
                    }],
                })])),
                alternative: None,
            })),
            TopLevel::Stmt(Stmt::Expr(Expr::IfExpr {
                condition: Box::new(Expr::FunctionCall {
                    name: ASTString::from_str("<"),
                    args: vec![
                        Expr::Accessor {
                            left: Box::new(Expr::Identifier(IdentifierType::Identifier(
                                ASTString::from_str("testRecord"),
                                None,
                            ))),
                            right: Box::new(Expr::Identifier(IdentifierType::Identifier(
                                ASTString::from_str("Value"),
                                None,
                            ))),
                        },
                        Expr::IntLiteral(ASTString::from_str("5")),
                    ],
                }),
                consequent: Box::new(Expr::Block(vec![Stmt::Expr(Expr::Identifier(
                    IdentifierType::Identifier(ASTString::from_str("testRecord"), None),
                ))])),
                alternative: Some(Box::new(Expr::Block(vec![Stmt::Expr(
                    Expr::RecordLiteral {
                        fields: vec![RecordField {
                            name: ASTString::from_str("Value"),
                            value: Expr::IntLiteral(ASTString::from_str("5")),
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
namespace test
arr.[1]
map.[\"Hello\"]
(x + 5 |> toSlice).[0]
arr.[5 * 10]
";
    let tokens = lex_input(
        input,
        vec![
            TokenKind::NameSpace,
            TokenKind::ident("test"),
            TokenKind::NewLine,
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
                    ASTString::from_str("arr"),
                    None,
                ))),
                right: Box::new(Expr::ArrayLiteral(vec![Expr::IntLiteral(
                    ASTString::from_str("1"),
                )])),
            })),
            TopLevel::Stmt(Stmt::Expr(Expr::Accessor {
                left: Box::new(Expr::Identifier(IdentifierType::Identifier(
                    ASTString::from_str("map"),
                    None,
                ))),
                right: Box::new(Expr::ArrayLiteral(vec![Expr::StringLiteral(
                    ASTString::from_str("Hello"),
                )])),
            })),
            TopLevel::Stmt(Stmt::Expr(Expr::Accessor {
                left: Box::new(Expr::FunctionCall {
                    name: ASTString::from_str("|>"),
                    args: vec![
                        Expr::FunctionCall {
                            name: ASTString::from_str("+"),
                            args: vec![
                                Expr::Identifier(IdentifierType::Identifier(
                                    ASTString::from_str("x"),
                                    None,
                                )),
                                Expr::IntLiteral(ASTString::from_str("5")),
                            ],
                        },
                        Expr::Identifier(IdentifierType::Identifier(
                            ASTString::from_str("toSlice"),
                            None,
                        )),
                    ],
                }),
                right: Box::new(Expr::ArrayLiteral(vec![Expr::IntLiteral(
                    ASTString::from_str("0"),
                )])),
            })),
            TopLevel::Stmt(Stmt::Expr(Expr::Accessor {
                left: Box::new(Expr::Identifier(IdentifierType::Identifier(
                    ASTString::from_str("arr"),
                    None,
                ))),
                right: Box::new(Expr::ArrayLiteral(vec![Expr::FunctionCall {
                    name: ASTString::from_str("*"),
                    args: vec![
                        Expr::IntLiteral(ASTString::from_str("5")),
                        Expr::IntLiteral(ASTString::from_str("10")),
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
namespace test
fun (x, y) -> x + y
(fun x ->
	square x)
fun _ -> 5
fun _ (x: int): int -> x + 5
";
    let tokens = lex_input(
        input,
        vec![
            TokenKind::NameSpace,
            TokenKind::ident("test"),
            TokenKind::NewLine,
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
                        IdentifierType::Identifier(ASTString::from_str("x"), None),
                        IdentifierType::Identifier(ASTString::from_str("y"), None),
                    ],
                    None,
                )],
                return_type: None,
                body: Box::new(Expr::FunctionCall {
                    name: ASTString::from_str("+"),
                    args: vec![
                        Expr::Identifier(IdentifierType::Identifier(
                            ASTString::from_str("x"),
                            None,
                        )),
                        Expr::Identifier(IdentifierType::Identifier(
                            ASTString::from_str("y"),
                            None,
                        )),
                    ],
                }),
            })),
            TopLevel::Stmt(Stmt::Expr(Expr::Lambda {
                args: vec![IdentifierType::Identifier(ASTString::from_str("x"), None)],
                return_type: None,
                body: Box::new(Expr::Block(vec![Stmt::Expr(Expr::FunctionCall {
                    name: ASTString::from_str("square"),
                    args: vec![Expr::Identifier(IdentifierType::Identifier(
                        ASTString::from_str("x"),
                        None,
                    ))],
                })])),
            })),
            TopLevel::Stmt(Stmt::Expr(Expr::Lambda {
                args: vec![IdentifierType::Bucket],
                return_type: None,
                body: Box::new(Expr::IntLiteral(ASTString::from_str("5"))),
            })),
            TopLevel::Stmt(Stmt::Expr(Expr::Lambda {
                args: vec![
                    IdentifierType::Bucket,
                    IdentifierType::Identifier(
                        ASTString::from_str("x"),
                        Some(Type::Type {
                            name: ASTString::from_str("int"),
                            module: None,
                        }),
                    ),
                ],
                return_type: Some(Type::Type {
                    name: ASTString::from_str("int"),
                    module: None,
                }),
                body: Box::new(Expr::FunctionCall {
                    name: ASTString::from_str("+"),
                    args: vec![
                        Expr::Identifier(IdentifierType::Identifier(
                            ASTString::from_str("x"),
                            None,
                        )),
                        Expr::IntLiteral(ASTString::from_str("5")),
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
namespace test
for x in 1 .. 10 do x
for (a, b) in (func x y) do
	a + b
";
    let tokens = lex_input(
        input,
        vec![
            TokenKind::NameSpace,
            TokenKind::ident("test"),
            TokenKind::NewLine,
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
                condition_arg: IdentifierType::Identifier(ASTString::from_str("x"), None),
                condition_expr: Expr::FunctionCall {
                    name: ASTString::from_str(".."),
                    args: vec![
                        Expr::IntLiteral(ASTString::from_str("1")),
                        Expr::IntLiteral(ASTString::from_str("10")),
                    ],
                },
                consequent: Expr::Identifier(IdentifierType::Identifier(
                    ASTString::from_str("x"),
                    None,
                )),
            }),
            TopLevel::Stmt(Stmt::ForInLoop {
                condition_arg: IdentifierType::TupleDestructure(
                    vec![
                        IdentifierType::Identifier(ASTString::from_str("a"), None),
                        IdentifierType::Identifier(ASTString::from_str("b"), None),
                    ],
                    None,
                ),
                condition_expr: Expr::FunctionCall {
                    name: ASTString::from_str("func"),
                    args: vec![
                        Expr::Identifier(IdentifierType::Identifier(
                            ASTString::from_str("x"),
                            None,
                        )),
                        Expr::Identifier(IdentifierType::Identifier(
                            ASTString::from_str("y"),
                            None,
                        )),
                    ],
                },
                consequent: Expr::Block(vec![Stmt::Expr(Expr::FunctionCall {
                    name: ASTString::from_str("+"),
                    args: vec![
                        Expr::Identifier(IdentifierType::Identifier(
                            ASTString::from_str("a"),
                            None,
                        )),
                        Expr::Identifier(IdentifierType::Identifier(
                            ASTString::from_str("b"),
                            None,
                        )),
                    ],
                })]),
            }),
        ],
    );
}

#[test]
fn test_namespaces() {
    setup();
    {
        let input = "
namespace test
let x = 1
";
        let tokens = lex_input(
            input,
            vec![
                TokenKind::NameSpace,
                TokenKind::ident("test"),
                TokenKind::NewLine,
                TokenKind::Let,
                TokenKind::ident("x"),
                TokenKind::Assign,
                TokenKind::num("1"),
                TokenKind::NewLine,
            ],
        );
        let output = parse_input(tokens);
        assert_eq!(output.0, vec![ASTString::from_str("test")])
    }
    {
        let input = "
namespace test.mod.base.path
let x = 1
";
        let tokens = lex_input(
            input,
            vec![
                TokenKind::NameSpace,
                TokenKind::ident("test"),
                TokenKind::Dot,
                TokenKind::ident("mod"),
                TokenKind::Dot,
                TokenKind::ident("base"),
                TokenKind::Dot,
                TokenKind::ident("path"),
                TokenKind::NewLine,
                TokenKind::Let,
                TokenKind::ident("x"),
                TokenKind::Assign,
                TokenKind::num("1"),
                TokenKind::NewLine,
            ],
        );
        let output = parse_input(tokens);
        assert_eq!(
            output.0,
            vec![
                ASTString::from_str("test"),
                ASTString::from_str("mod"),
                ASTString::from_str("base"),
                ASTString::from_str("path"),
            ]
        )
    }
}
