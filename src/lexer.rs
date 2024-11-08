use ariadne::{self, Label, Report, ReportKind, Source};
use logos::Logos;
use std::{ops::Range, rc::Rc};

#[derive(Debug, Logos, PartialEq, Eq, Clone, Hash)]
// #[logos(skip r"[ \t\n\f]+")]
pub enum TokenKind {
    #[regex(r"[0-9]+", |lex| (lex.slice().to_string()))]
    NumberLiteral(String),
    #[token("let")]
    Let,
    #[token("open")]
    Import,
    #[regex(r#""[^"]*""#, |lex| (lex.slice().to_string()))]
    StringLiteral(String),

    #[token("of")]
    Of,
    #[token("go")]
    Go,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("match")]
    Match,

    #[regex(r"\+|-|\/|%|\^|<<|>>")]
    Operator,

    #[regex(r"[a-zA-Z_$@][a-zA-Z0-9_$@]*", |lex| (lex.slice().to_string()))]
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
    Channel,
    #[token("::")]
    Append,
    #[token("type")]
    TypeKeyword,
    #[token("private")]
    Private,
    #[token("mutable")]
    Mut,
    #[token("==")]
    Equality,
    #[token(">")]
    GT,
    #[token("<")]
    LT,
    #[token(">=")]
    GTE,
    #[token("<=")]
    LTE,
    #[token("and")]
    And,
    #[token("or")]
    Or,
    #[token("|>")]
    PipeRight,
    #[token("|")]
    Pipe,
    #[token("..")]
    Range,
    #[regex(r"//.*")]
    Comment,
    #[token("->")]
    ReturnType,
    #[token("interface")]
    Interface,

    EOF,
    #[regex(r"\n")]
    NewLine,
    #[regex(r"\t")]
    Tab,
    #[regex(" ")]
    Space,

    Indent,
    Dedent,
}

type Span = Range<usize>;

#[derive(Debug)]
pub enum LexerError {
    InvalidParseStep,
    FileNotFound,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub source: Rc<String>,
    pub file: Rc<String>,
}

pub fn lex(file_path: &str) -> Result<Vec<Token>, LexerError> {
    // Get the source code as a Rc<String>
    let input = match std::fs::read_to_string(file_path) {
        Ok(i) => i,
        Err(_) => return Err(LexerError::FileNotFound),
    };
    let refcounted_file_path = Rc::new(file_path.to_string());
    let refcounted_input = Rc::new(input);

    // Lex the input
    let mut lexer = TokenKind::lexer(&refcounted_input);
    let mut tokens = Vec::with_capacity(refcounted_input.len() / 10);
    while let Some(token) = lexer.next() {
        let span = lexer.span();
        match token {
            Ok(x) => tokens.push(Ok(Token {
                kind: x,
                span,
                source: refcounted_input.clone(),
                file: refcounted_file_path.clone(),
            })),
            Err(_) => tokens.push(Err(())),
        }
    }
    tokens.push(Ok(Token {
        kind: TokenKind::EOF,
        span: (0 as usize)..(0 as usize),
        source: refcounted_input.clone(),
        file: refcounted_file_path.clone(),
    }));

    // Filter out the errors
    // TODO: Actually do something with the errors
    let mut range = 0..0;
    let mut context = Rc::new("".to_string());
    let error_src = refcounted_input.clone();
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

    let mut whitespace_parser = WhiteSpaceParser::new(
        filtered_tokens,
        refcounted_input.clone(),
        refcounted_file_path.clone(),
    );
    whitespace_parser.parse();
    let parsed_output = whitespace_parser.get_output();
    return Ok(parsed_output);
}

struct WhiteSpaceParser {
    input: Vec<Token>,
    output: Vec<Token>,
    current_token: Option<TokenKind>,
    current_indent: usize,
    source: Rc<String>,
    file_name: Rc<String>,
    span: Range<usize>,
    end_token_for_block: TokenKind,
    end_token_stack: Vec<TokenKind>,
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
            end_token_for_block: TokenKind::EOF,
            end_token_stack: Vec::new(),
        }
    }

    fn get_output(self) -> Vec<Token> {
        self.output
    }

    fn push_end_token(&mut self, token: TokenKind) {
        self.end_token_stack.push(self.end_token_for_block.clone());
        self.end_token_for_block = token;
    }

    fn pop_end_token(&mut self) {
        if let Some(token) = self.end_token_stack.pop() {
            self.end_token_for_block = token;
        } else {
            panic!("Popped empty end token stack");
        }
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
            if kind == &self.end_token_for_block {
                self.output.push(current);
                break;
            }

            match (kind, self.end_token_for_block == TokenKind::EOF) {
                (&TokenKind::NewLine, true) => {
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

                    if let Some(x) = self.output.last() {
                        if x.kind == self.end_token_for_block {
                            break;
                        }
                    }
                }
                (&TokenKind::NewLine, false) => continue,

                (&TokenKind::LParen, _) => {
                    self.output.push(current);
                    self.push_end_token(TokenKind::RParen);
                    self.parse();
                    self.pop_end_token();
                    continue;
                }

                (&TokenKind::LBrace, _) => {
                    self.output.push(current);
                    self.push_end_token(TokenKind::RBrace);
                    self.parse();
                    self.pop_end_token();
                    continue;
                }

                (&TokenKind::LBracket, _) => {
                    self.output.push(current);
                    self.push_end_token(TokenKind::RBracket);
                    self.parse();
                    self.pop_end_token();
                    continue;
                }

                (&TokenKind::Space, _) | (&TokenKind::Tab, _) => continue,

                _ => self.output.push(current),
            };
        }
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
