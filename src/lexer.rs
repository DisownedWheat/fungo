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

    // #[regex(r"\r\n")]
    // #[regex(r"\n")]
    // NewLine,
    //
    // #[regex(r"[ \t\f]")]
    // Whitespace,
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
    #[token(r"\n")]
    NewLine,
    #[token(r"\t")]
    Tab,
    #[token(" ")]
    Space,

    Block(Vec<Token>),
}

type Span = Range<usize>;

#[derive(Debug)]
pub enum LexerError {
    InvalidParseStep,
    FileNotFound,
}

pub type Token = (TokenKind, Span, Rc<String>, Rc<String>);

pub fn lex(file_path: &str) -> Result<Vec<Token>, LexerError> {
    let input = match std::fs::read_to_string(file_path) {
        Ok(i) => i,
        Err(_) => return Err(LexerError::FileNotFound),
    };
    let counted_file_path = Rc::new(file_path.to_string());
    let counted_input = Rc::new(input);
    let mut lexer = TokenKind::lexer(&counted_input);
    let mut tokens = Vec::with_capacity(counted_input.len() / 10);
    while let Some(token) = lexer.next() {
        let span = lexer.span();
        match token {
            Ok(x) => tokens.push(Ok((
                x,
                span,
                counted_input.clone(),
                counted_file_path.clone(),
            ))),
            Err(_) => tokens.push(Err(())),
        }
    }
    tokens.push(Ok((
        TokenKind::EOF,
        (0 as usize)..(0 as usize),
        counted_input.clone(),
        counted_file_path,
    )));
    let filtered_tokens = tokens
        .into_iter()
        .map(|token| token)
        .filter(|token| token.is_ok())
        .map(|token| token.unwrap())
        .collect::<Vec<Token>>();
    let mut whitespace_parser = WhiteSpaceParser::new(filtered_tokens, counted_input.clone());
    whitespace_parser.parse(TokenKind::EOF);
    let parsed_output = whitespace_parser.get_output();
    if parsed_output.len() > 1 {
        panic!("Output has more than one token");
    }
    parsed_output
        .first()
        .map(|(mut kind, _, _, _)| match kind {
            TokenKind::Block(mut toks) => std::mem::take(&mut toks),
            _ => unreachable!(),
        })
        .ok_or(LexerError::InvalidParseStep)
}

struct WhiteSpaceParser {
    input: Vec<Token>,
    output: Vec<Token>,
    stack: Vec<Vec<Token>>,
    current: Vec<Token>,
    current_indent: usize,
    source: Rc<String>,
}

impl WhiteSpaceParser {
    pub fn new(mut input: Vec<Token>, source: Rc<String>) -> Self {
        input.reverse();
        Self {
            output: Vec::with_capacity(input.len() * 2),
            stack: Vec::with_capacity(10),
            current: Vec::with_capacity(20),
            input,
            current_indent: 0,
            source,
        }
    }

    fn get_output(mut self) -> Vec<Token> {
        self.pop_to_root();
        let token = self.build_token();
        self.output.push(token);
        self.output
    }

    fn push_stack(&mut self) {
        let token = self.build_token();
        self.current.push(token);
    }

    fn pop_stack(&mut self) {
        if let Some(mut block) = self.stack.pop() {
            block.push(self.build_token());
            self.current = block;
        } else {
            panic!("Popped empty token stack");
        }
    }

    fn pop_to_root(&mut self) {
        while let Some(mut block) = self.stack.pop() {
            block.push(self.build_token());
            self.current = block;
        }
    }

    fn build_token(&mut self) -> Token {
        let tmp = std::mem::take(&mut self.current);
        self.current = vec![];
        (
            TokenKind::Block(tmp),
            0..0,
            Rc::new("".to_string()),
            self.source.clone(),
        )
    }

    pub fn parse(&mut self, end: TokenKind) {
        while let Some(current) = self.input.pop() {
            let (kind, _, _, _) = &current;
            log::info!("Parsing {:?}", kind);
            if kind == &end {
                self.current.push(current);
                break;
            }

            if kind == &TokenKind::NewLine {
                while let Some(current) = self.input.pop() {
                    let tok = &current.0;
                    match tok {
                        &TokenKind::NewLine => continue,
                        &TokenKind::Space => {
                            let new_indent = self.parse_spaces();
                            if new_indent > self.current_indent {
                                self.push_stack();
                                self.current_indent = new_indent;
                            } else if new_indent < self.current_indent {
                                self.pop_stack();
                                self.current_indent = new_indent;
                            } else {
                                break;
                            }
                        }
                        &TokenKind::Tab => {
                            let new_indent = self.parse_tabs();
                            if new_indent > self.current_indent {
                                self.push_stack();
                                self.current_indent = new_indent;
                            } else if new_indent < self.current_indent {
                                self.pop_stack();
                                self.current_indent = new_indent;
                            } else {
                                break;
                            }
                        }
                        _ => {
                            self.current_indent = 0;
                            self.pop_to_root();
                            break;
                        }
                    }
                }
            }
            if kind == &TokenKind::LParen {
                self.current.push(current);
                self.parse(TokenKind::RParen);
                continue;
            }
            if kind == &TokenKind::LBrace {
                self.current.push(current);
                self.parse(TokenKind::RBrace);
                continue;
            }
            if kind == &TokenKind::LBracket {
                self.current.push(current);
                self.parse(TokenKind::RBracket);
                continue;
            }
            self.current.push(current);
        }
    }

    fn parse_spaces(&mut self) -> usize {
        let mut indent = 1;
        while let Some(current) = self.input.pop() {
            let tok = &current.0;
            match tok {
                &TokenKind::Tab => {
                    self.build_error_report(current, "Found tab when parsing spaces");
                    panic!();
                }
                &TokenKind::Space => {
                    indent += 1;
                    continue;
                }
                _ => break,
            };
        }
        indent
    }

    fn parse_tabs(&mut self) -> usize {
        let mut indent = 1;
        while let Some(current) = self.input.pop() {
            let tok = &current.0;
            match tok {
                TokenKind::Space => {
                    self.build_error_report(current, "Found space when parsing tabs");
                    panic!();
                }
                TokenKind::Tab => {
                    indent += 1;
                    continue;
                }
                _ => break,
            };
        }
        indent
    }

    fn build_error_report(&self, token: Token, msg: &str) {
        let (_, range, source, context) = token;
        let mut colors = ariadne::ColorGenerator::new();
        let a = colors.next();
        Report::build(ReportKind::Error, (context.clone(), range.clone()))
            .with_code("Lexer Error")
            .with_note("Error parsing White Space")
            .with_label(
                Label::new((context.clone(), range.clone()))
                    .with_message(msg)
                    .with_color(a),
            )
            .finish()
            .print((context.clone(), Source::from(source.clone().as_str())))
            .unwrap();
    }
}
