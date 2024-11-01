use logos::Logos;
use std::{ops::Range, rc::Rc};

#[derive(Debug, Logos, PartialEq, Eq, Clone, Hash)]
#[logos(skip r"[ \t\n\f]+")]
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
}

type Span = Range<usize>;

#[derive(Debug)]
pub enum LexerError {
    InvalidToken,
    FileNotFound,
}

pub type Token = (TokenKind, Span, Rc<String>, Rc<String>);

pub fn lex(file_path: &str) -> Result<Vec<Result<Token, ()>>, LexerError> {
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
    return Ok(tokens);
}
