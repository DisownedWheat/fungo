use ariadne::{self, Label, Report, ReportKind, Source};
use logos::Logos;
use serde::Serialize;
use std::{ops::Range, rc::Rc};

fn clean_string<'s>(lexer: &mut logos::Lexer<'s, TokenKind>) -> String {
    let value = lexer.slice();
    let mut new_str = value.chars().skip(1).collect::<String>();
    new_str.pop();
    new_str
}

#[derive(Debug, Logos, PartialEq, Eq, Clone, Hash, Serialize)]
// #[logos(skip r"[ \t\n\f]+")]
pub enum TokenKind {
    #[regex(r"[-]?[0-9]+[.0-9]*", |lex| (lex.slice().to_string()))]
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
    #[token("then")]
    Then,
    #[token("else")]
    Else,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("match")]
    Match,
    #[token("function")]
    FunctionMatch,
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
    #[token("fun")]
    Lambda,
    #[token("namespace")]
    NameSpace,

    #[token("for")]
    For,
    #[token("in")]
    In,
    #[token("while")]
    While,
    #[token("do")]
    Do,

    #[regex(r"[\+\-\|\/\%\^<>=.&\*:]+", |lex| lex.slice().to_string())]
    Operator(String),

    #[regex(r"[a-zA-Z_$@][a-zA-Z0-9_$@]*", |lex| lex.slice().to_string())]
    Identifier(String),

    Assign,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    Colon,
    #[token(";")]
    SemiColon,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token(",")]
    Comma,
    Dot,
    Pointer,
    Deref,
    #[token("<-")]
    ChannelAssign,
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

pub type Span = Range<usize>;

#[derive(Debug)]
pub enum LexerError {
    InvalidParseStep,
    FileNotFound(String),
}

pub type Token = (TokenKind, TokenState);
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TokenState {
    pub span: Span,
    pub source: Rc<String>,
    pub file: Rc<String>,
}

#[derive(Default)]
struct OpFolder {
    prev: Option<TokenState>,
    tokens: Vec<Token>,
}

impl OpFolder {
    fn push_token(mut self, (kind, state): Token) -> Self {
        match &kind {
            TokenKind::Operator(s) => match s.as_str() {
                "=" => self.tokens.push((TokenKind::Assign, state)),
                "." => self.tokens.push((TokenKind::Dot, state)),
                ":" => self.tokens.push((TokenKind::Colon, state)),
                "&" => self.tokens.push((TokenKind::Pointer, state)),
                "*" => self.prev = Some(state),
                _ => self.tokens.push((kind, state)),
            },
            TokenKind::Space | TokenKind::Tab if self.prev.is_some() => {
                let prev = self.prev.unwrap();
                self.tokens
                    .push((TokenKind::Operator("*".to_string()), prev));
                self.prev = None;
                self.tokens.push((kind, state));
            }
            _ => {
                if let Some(tok) = self.prev {
                    self.tokens.push((TokenKind::Deref, tok));
                    self.prev = None;
                }
                self.tokens.push((kind, state));
            }
        };
        self
    }
}

pub fn lex_raw(input: &str) -> Result<(Vec<Token>, Rc<String>), LexerError> {
    let path = Rc::new("stdin".to_string());
    let input_rc = Rc::new(input.to_string());

    let mut lexer = TokenKind::lexer(input);
    let mut tokens = Vec::with_capacity(input.len() / 10);
    while let Some(token) = lexer.next() {
        let span = lexer.span();
        match token {
            Ok(x) => tokens.push(Ok((
                x,
                TokenState {
                    span,
                    source: input_rc.clone(),
                    file: path.clone(),
                },
            ))),
            Err(_) => tokens.push(Err(())),
        }
    }
    let mut range = 0..0;
    let mut context = Rc::new("".to_string());
    let error_src = input_rc.clone();
    let filtered_tokens = tokens
        .into_iter()
        .filter(move |token| match token {
            Ok((_, x)) => {
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
        .fold(OpFolder::default(), |acc, token| acc.push_token(token))
        .tokens;

    let mut whitespace_parser =
        WhiteSpaceParser::new(filtered_tokens, input_rc.clone(), path.clone());
    whitespace_parser.parse();
    let parsed_output = whitespace_parser.get_output();
    return Ok((parsed_output, input_rc));
}

pub fn lex(file_path: &str) -> Result<(Vec<Token>, Rc<String>), LexerError> {
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
            Ok(x) => tokens.push(Ok((
                x,
                TokenState {
                    span,
                    source: input.clone(),
                    file: refcounted_file_path.clone(),
                },
            ))),
            Err(_) => tokens.push(Err(())),
        }
    }

    // Filter out the errors
    // TODO: Actually do something with the errors
    let mut range = 0..0;
    let mut context = Rc::new("".to_string());
    let error_src = input.clone();
    let filtered_tokens = tokens
        .into_iter()
        .filter(move |token| match token {
            Ok((_, x)) => {
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
        .fold(OpFolder::default(), |acc, token| acc.push_token(token))
        .tokens;

    let mut whitespace_parser =
        WhiteSpaceParser::new(filtered_tokens, input.clone(), refcounted_file_path.clone());
    whitespace_parser.parse();
    let parsed_output = whitespace_parser.get_output();
    return Ok((parsed_output, input));
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
        let token = (
            TokenKind::Indent,
            TokenState {
                span: self.span.clone(),
                source: self.source.clone(),
                file: self.file_name.clone(),
            },
        );
        self.output.push(token);
    }

    fn push_dedent(&mut self) {
        let token = (
            TokenKind::Dedent,
            TokenState {
                span: self.span.clone(),
                source: self.source.clone(),
                file: self.file_name.clone(),
            },
        );
        self.output.push(token);
    }

    fn pop_to_root(&mut self) {
        while self.current_indent > 0 {
            self.push_dedent();
            self.current_indent -= 1;
        }
    }

    fn pop_token(&mut self) -> Option<Token> {
        if let Some((kind, state)) = self.input.pop() {
            let span = state.span.clone();
            self.span = span;
            self.current_token = Some(kind.clone());
            Some((kind, state))
        } else {
            None
        }
    }

    pub fn parse(&mut self) {
        while let Some(current) = self.pop_token() {
            let (kind, _) = &current;
            match kind {
                &TokenKind::NewLine => {
                    if self.output.len() == 0 {
                        continue;
                    }
                    self.output.push(current);
                    while let Some((kind, state)) = self.pop_token() {
                        let tok = &kind;
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
                                self.output.push((kind, state));
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
            let (kind, _) = &current;
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
            let (kind, _) = &current;
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

    fn setup() {
        let _ = env_logger::builder()
            .filter_level(log::LevelFilter::Info)
            .try_init();
    }
    #[test]
    fn test() {
        setup();
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
            .0
            .into_iter()
            .map(|(kind, _)| kind)
            .collect();
        assert_eq!(kinds, expected_output);
    }

    #[test]
    fn lexer_operators() {
        setup();
        let input = "
let x = 1
x * 2.051
*a ++ 5
b && c
5 * x
";

        let expected_output = vec![
            TokenKind::Let,
            TokenKind::ident("x"),
            TokenKind::Assign,
            TokenKind::num("1"),
            TokenKind::NewLine,
            TokenKind::ident("x"),
            TokenKind::op("*"),
            TokenKind::num("2.051"),
            TokenKind::NewLine,
            TokenKind::Deref,
            TokenKind::ident("a"),
            TokenKind::op("++"),
            TokenKind::num("5"),
            TokenKind::NewLine,
            TokenKind::ident("b"),
            TokenKind::op("&&"),
            TokenKind::ident("c"),
            TokenKind::NewLine,
            TokenKind::num("5"),
            TokenKind::op("*"),
            TokenKind::ident("x"),
            TokenKind::NewLine,
        ];
        let output = lex_raw(input);
        assert!(output.is_ok());
        let kinds: Vec<TokenKind> = output
            .unwrap()
            .0
            .into_iter()
            .map(|(kind, _)| kind)
            .collect();
        assert_eq!(kinds, expected_output);
    }
}
