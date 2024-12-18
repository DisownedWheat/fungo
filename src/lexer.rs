use ariadne::{self, Label, Report, ReportKind, Source};
use logos::Logos;
use operators::OperatorFolder;
use serde::Serialize;
use std::{error::Error, fmt::Display, ops::Range, rc::Rc};

fn clean_string<'s>(lexer: &mut logos::Lexer<'s, TokenKind>) -> String {
    let value = lexer.slice();
    let mut new_str = value.chars().skip(1).collect::<String>();
    new_str.pop();
    new_str
}

#[derive(Debug, Logos, PartialEq, Eq, Clone, Hash, Serialize)]
// #[logos(skip r"[ \t\n\f]+")]
pub enum TokenKind {
    #[regex(r"[0-9]+", |lex| (lex.slice().to_string()))]
    NumberLiteral(String),
    #[token("let")]
    Let,
    #[token("open")]
    Import,
    #[regex(r#""[^"]*""#, clean_string)]
    StringLiteral(String),
    #[regex(r"//.*")]
    Comment,

    #[token("of")]
    Of,
    #[token("go")]
    Go,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("and")]
    And,
    #[token("or")]
    Or,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("match")]
    Match,
    #[token("with")]
    With,
    #[token("when")]
    When,
    #[token("interface")]
    Interface,
    #[token("type")]
    TypeKeyword,
    #[token("private")]
    Private,
    #[token("mutable")]
    Mut,
    #[token("module")]
    Module,

    #[regex(r"[\+\-\|\/\%\^<>]+", |lex| lex.slice().to_string())]
    Operator(String),

    #[regex(r"[a-zA-Z_$@][a-zA-Z0-9_$@]*", |lex| lex.slice().to_string())]
    Identifier(String),

    #[token("=")]
    Assign,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token(":")]
    Colon,
    #[token(";")]
    SemiColon,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token(",")]
    Comma,
    #[token(".")]
    Dot,
    #[token("&")]
    Pointer,
    #[token("*")]
    Deref,
    #[token("<-")]
    ChannelAssign,
    #[token("::")]
    Append,
    #[token("..")]
    Range,
    #[token("->")]
    ReturnType,
    #[regex(r"\n")]
    NewLine,
    #[regex(r"\t")]
    Tab,
    #[regex(" ")]
    Space,

    Indent,
    Dedent,
}

impl TokenKind {
    pub fn ident(input: &str) -> Self {
        Self::Identifier(input.to_string())
    }

    pub fn op(input: &str) -> Self {
        Self::Operator(input.to_string())
    }

    pub fn num(input: &str) -> Self {
        Self::NumberLiteral(input.to_string())
    }

    pub fn str(input: &str) -> Self {
        Self::StringLiteral(input.to_string())
    }
}

type Span = Range<usize>;

#[derive(Debug)]
pub enum LexerError {
    InvalidParseStep,
    FileNotFound(String),
}

impl Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Error for LexerError {}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub source: Rc<String>,
    pub file: Rc<String>,
}

pub fn lex_raw(input: &str) -> Result<Vec<Token>, LexerError> {
    let path = Rc::new("stdin".to_string());
    let input_rc = Rc::new(input.to_string());

    let mut lexer = TokenKind::lexer(input);
    let mut tokens = Vec::with_capacity(input.len() / 10);
    while let Some(token) = lexer.next() {
        let span = lexer.span();
        match token {
            Ok(x) => tokens.push(Ok(Token {
                kind: x,
                span,
                source: input_rc.clone(),
                file: path.clone(),
            })),
            Err(_) => tokens.push(Err(())),
        }
    }
    let mut range = 0..0;
    let mut context = Rc::new("".to_string());
    let error_src = input_rc.clone();
    let filtered_tokens = tokens
        .into_iter()
        .filter(move |token| match token {
            Ok(x) => {
                context = x.file.clone();
                range = x.span.clone();
                true
            }
            Err(_) => {
                let mut colors = ariadne::ColorGenerator::new();
                let a = colors.next();
                Report::build(ReportKind::Error, (context.clone(), range.clone()))
                    .with_code(2)
                    .with_note("Error parsing White Space")
                    .with_label(
                        Label::new((context.clone(), range.clone()))
                            .with_message("Unexpected token found")
                            .with_color(a),
                    )
                    .finish()
                    .print((context.clone(), Source::from(error_src.clone().as_str())))
                    .unwrap();
                false
            }
        })
        .map(|token| token.unwrap())
        .collect::<Vec<Token>>();

    let mut whitespace_parser =
        WhiteSpaceParser::new(filtered_tokens, input_rc.clone(), path.clone());
    whitespace_parser.parse();
    let parsed_output = whitespace_parser.get_output();
    return Ok(parsed_output);
}

pub fn lex(file_path: &str) -> Result<Vec<Token>, LexerError> {
    // Get the source code as a Rc<String>
    let input = match std::fs::read_to_string(file_path) {
        Ok(i) => Rc::new(i),
        Err(_) => return Err(LexerError::FileNotFound(file_path.to_string())),
    };
    let refcounted_file_path = Rc::new(file_path.to_string());

    // Lex the input
    let mut lexer = TokenKind::lexer(&input);
    let mut tokens = Vec::with_capacity(input.len() / 10);
    while let Some(token) = lexer.next() {
        let span = lexer.span();
        match token {
            Ok(x) => tokens.push(Ok(Token {
                kind: x,
                span,
                source: input.clone(),
                file: refcounted_file_path.clone(),
            })),
            Err(_) => tokens.push(Err(())),
        }
    }

    // Filter out the errors
    // TODO: Actually do something with the errors
    let mut range = 0..0;
    let mut context = Rc::new("".to_string());
    let error_src = input.clone();
    let len = tokens.len();
    let folder = OperatorFolder::new(len);
    let filtered_tokens = tokens
        .into_iter()
        .filter(move |token| match token {
            Ok(x) => {
                context = x.file.clone();
                range = x.span.clone();
                true
            }
            Err(_) => {
                let mut colors = ariadne::ColorGenerator::new();
                let a = colors.next();
                Report::build(ReportKind::Error, (context.clone(), range.clone()))
                    .with_code(2)
                    .with_note("Error parsing White Space")
                    .with_label(
                        Label::new((context.clone(), range.clone()))
                            .with_message("Unexpected token found")
                            .with_color(a),
                    )
                    .finish()
                    .print((context.clone(), Source::from(error_src.clone().as_str())))
                    .unwrap();
                false
            }
        })
        .map(|token| token.unwrap())
        .fold(folder, |acc, token| acc.fold(token))
        .tokens;

    let mut whitespace_parser =
        WhiteSpaceParser::new(filtered_tokens, input.clone(), refcounted_file_path.clone());
    whitespace_parser.parse();
    let parsed_output = whitespace_parser.get_output();
    return Ok(parsed_output);
}

mod operators {
    use super::*;

    #[derive(Debug, Clone, Copy)]
    enum PrevToken {
        Deref,
        Pointer,
        Other,
    }

    pub struct OperatorFolder {
        pub tokens: Vec<Token>,
        current: Option<Token>,
        prev: PrevToken,
    }

    impl OperatorFolder {
        pub fn new(size: usize) -> Self {
            OperatorFolder {
                tokens: Vec::with_capacity(size),
                current: None,
                prev: PrevToken::Other,
            }
        }

        pub fn fold(mut self, token: Token) -> Self {
            match &token.kind {
                TokenKind::Operator(op) => match self.current.as_ref().map(|x| x.kind) {
                    Some(TokenKind::Operator(current_op)) => {
                        self.current = Some(Token {
                            kind: TokenKind::Operator(format!("{}{}", current_op, op)),
                            ..self.current.unwrap()
                        });
                        self
                    }
                    None => {
                        self.current = Some(token);
                        self
                    }
                },
                _ => {
                    self.tokens.push(token);
                    self
                }
            }
        }
    }
}

struct WhiteSpaceParser {
    input: Vec<Token>,
    output: Vec<Token>,
    current_token: Option<TokenKind>,
    current_indent: usize,
    source: Rc<String>,
    file_name: Rc<String>,
    span: Range<usize>,
}

impl WhiteSpaceParser {
    pub fn new(mut input: Vec<Token>, source: Rc<String>, file_name: Rc<String>) -> Self {
        input.reverse();
        Self {
            output: Vec::with_capacity(input.len() * 2),
            current_token: None,
            input,
            current_indent: 0,
            source,
            file_name,
            span: 0..0,
        }
    }

    fn get_output(self) -> Vec<Token> {
        self.output
    }

    fn push_indent(&mut self) {
        let token = Token {
            kind: TokenKind::Indent,
            span: self.span.clone(),
            source: self.source.clone(),
            file: self.file_name.clone(),
        };
        self.output.push(token);
    }

    fn push_dedent(&mut self) {
        let token = Token {
            kind: TokenKind::Dedent,
            span: self.span.clone(),
            source: self.source.clone(),
            file: self.file_name.clone(),
        };
        self.output.push(token);
    }

    fn pop_to_root(&mut self) {
        while self.current_indent > 0 {
            self.push_dedent();
            self.current_indent -= 1;
        }
    }

    fn pop_token(&mut self) -> Option<Token> {
        if let Some(token) = self.input.pop() {
            let span = token.span.clone();
            self.span = span;
            self.current_token = Some(token.clone().kind);
            Some(token)
        } else {
            None
        }
    }

    pub fn parse(&mut self) {
        while let Some(current) = self.pop_token() {
            let Token { kind, .. } = &current;
            match kind {
                &TokenKind::NewLine => {
                    if self.output.len() == 0 {
                        continue;
                    }
                    self.output.push(current);
                    while let Some(current) = self.pop_token() {
                        let tok = &current.kind;
                        match tok {
                            &TokenKind::NewLine => continue,

                            &TokenKind::Space => {
                                let (token, new_indent) = self.parse_spaces();
                                if new_indent > self.current_indent {
                                    self.push_indent();
                                    self.current_indent = new_indent;
                                } else if new_indent < self.current_indent {
                                    self.push_dedent();
                                    self.current_indent = new_indent;
                                }
                                self.output.push(token);
                                break;
                            }

                            &TokenKind::Tab => {
                                let (token, new_indent) = self.parse_tabs();
                                if new_indent > self.current_indent {
                                    self.push_indent();
                                    self.current_indent = new_indent;
                                } else if new_indent < self.current_indent {
                                    self.push_dedent();
                                    self.current_indent = new_indent;
                                }
                                self.output.push(token);
                                break;
                            }

                            _ => {
                                self.pop_to_root();
                                self.output.push(current);
                                break;
                            }
                        }
                    }
                }

                &TokenKind::Space | &TokenKind::Tab => continue,
                _ => self.output.push(current),
            }
        }
        self.pop_to_root();
    }

    fn parse_spaces(&mut self) -> (Token, usize) {
        let mut indent = 1;
        while let Some(current) = self.pop_token() {
            let Token { kind, .. } = &current;
            match kind {
                &TokenKind::Tab => {
                    self.build_error_report(kind, "Found tab when parsing spaces");
                    panic!();
                }
                &TokenKind::Space => {
                    indent += 1;
                    continue;
                }
                _ => return (current, indent),
            };
        }

        panic!("No end to spaces!");
    }

    fn parse_tabs(&mut self) -> (Token, usize) {
        let mut indent = 1;
        while let Some(current) = self.pop_token() {
            let Token { kind, .. } = &current;
            match kind {
                TokenKind::Space => {
                    self.build_error_report(kind, "Found space when parsing tabs");
                    panic!();
                }
                TokenKind::Tab => {
                    indent += 1;
                    continue;
                }
                _ => {
                    return (current, indent);
                }
            };
        }
        panic!("No end to tabs!");
    }

    fn build_error_report(&self, token: &TokenKind, msg: &str) {
        let mut colors = ariadne::ColorGenerator::new();
        let a = colors.next();
        Report::build(
            ReportKind::Error,
            (self.file_name.clone(), self.span.clone()),
        )
        .with_code("Lexer Error")
        .with_note(format!("Error parsing White Space, Found: {:?}", token))
        .with_label(
            Label::new((self.file_name.clone(), self.span.clone()))
                .with_message(msg)
                .with_color(a),
        )
        .finish()
        .print((
            self.file_name.clone(),
            Source::from(self.source.clone().as_str()),
        ))
        .unwrap();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test() {
        let input = "
let x =
	1

{
	TestValue = \"Hello World\"
}
";
        let expected_output = vec![
            TokenKind::Let,
            TokenKind::Identifier("x".to_string()),
            TokenKind::Assign,
            TokenKind::NewLine,
            TokenKind::Indent,
            TokenKind::NumberLiteral("1".to_string()),
            TokenKind::NewLine,
            TokenKind::Dedent,
            TokenKind::LBrace,
            TokenKind::NewLine,
            TokenKind::Indent,
            TokenKind::ident("TestValue"),
            TokenKind::Assign,
            TokenKind::str("Hello World"),
            TokenKind::NewLine,
            TokenKind::Dedent,
            TokenKind::RBrace,
            TokenKind::NewLine,
        ];
        let output = lex_raw(input);
        assert!(output.is_ok());
        let kinds: Vec<TokenKind> = output
            .unwrap()
            .into_iter()
            .map(|Token { kind, .. }| kind)
            .collect();
        assert_eq!(kinds, expected_output);
    }
}
