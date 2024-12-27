// AST NODES

use crate::lexer::{Span, Token, TokenKind};
use serde::Serialize;

#[derive(Debug, Serialize, Clone)]
pub struct ASTString {
    pub value: String,
    pub span: Span,
}

impl ASTString {
    pub fn default() -> Self {
        ASTString {
            value: String::new(),
            span: 0..0,
        }
    }

    pub fn from_str(value: &str) -> Self {
        ASTString {
            value: value.to_string(),
            span: 0..0,
        }
    }

    pub fn from_str_with_span(value: &str, span: Span) -> Self {
        ASTString {
            value: value.to_string(),
            span,
        }
    }
    pub fn from_token((kind, state): Token) -> Self {
        match kind {
            TokenKind::StringLiteral(value)
            | TokenKind::NumberLiteral(value)
            | TokenKind::Identifier(value)
            | TokenKind::Operator(value) => ASTString {
                value,
                span: state.span,
            },
            _ => panic!(),
        }
    }
}

impl PartialEq for ASTString {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl Eq for ASTString {}

// Imports
#[derive(Debug, Serialize, PartialEq, Eq, Clone)]
pub struct FungoImport {
    pub module: Vec<ASTString>,
}

#[derive(Debug, Serialize, PartialEq, Eq)]
pub struct GoImport {
    pub module: ASTString,
    pub alias: Option<ASTString>,
}

// Identifiers

#[derive(Debug, Serialize, PartialEq, Eq)]
pub struct Identifier {
    pub value: ASTString,
}

#[derive(Debug, Serialize, PartialEq, Eq)]
pub struct Accessor {
    left: Box<IdentifierType>,
    right: Option<ASTString>,
}

#[derive(Debug, Serialize, PartialEq, Eq)]
pub enum IdentifierType {
    Identifier(ASTString, Option<Type>),
    Deref(Box<IdentifierType>),
    Pointer(Box<IdentifierType>),
    ArrayDestructure(Vec<IdentifierType>, Option<Type>),
    RecordDestructure(Vec<IdentifierType>, Option<Type>),
    TupleDestructure(Vec<IdentifierType>, Option<Type>),
    Bucket,
    Unit,
}

impl IdentifierType {
    pub fn get_name(&self) -> Option<&str> {
        match self {
            IdentifierType::Identifier(ref identifier, _) => Some(identifier.value.as_str()),
            _ => None,
        }
    }
}

// Types

#[derive(Debug, Serialize, PartialEq, Eq)]
pub enum Type {
    Unit,
    Type {
        name: ASTString,
        module: Option<ASTString>,
    },
    Pointer(Box<Type>),
    Slice(Box<Type>),
}

#[derive(Debug, Serialize, PartialEq, Eq)]
pub enum TypeDef {
    Type(Type),
    VariantDefinition {
        fields: Vec<(ASTString, Option<TypeDef>)>,
    },
    RecordDefinition(RecordDefinition),
    TupleDefinition {
        length: usize,
        types: Vec<TypeDef>,
    },
}

// Records
#[derive(Debug, Serialize, PartialEq, Eq)]
pub struct RecordDefinitionField {
    pub name: ASTString,
    pub type_: TypeDef,
}

#[derive(Debug, Serialize, PartialEq, Eq)]
pub struct RecordDefinition {
    pub fields: Vec<RecordDefinitionField>,
}

#[derive(Debug, Serialize, PartialEq, Eq)]
pub struct RecordField {
    pub name: ASTString,
    pub value: Expr,
}

#[derive(Debug, Serialize, PartialEq, Eq)]
pub enum Expr {
    Identifier(IdentifierType),
    Block(Vec<Stmt>),
    BoolLiteral(bool),
    StringLiteral(ASTString),
    IntLiteral(ASTString),
    FloatLiteral(ASTString),
    IfExpr {
        condition: Box<Expr>,
        consequent: Box<Expr>,
        alternative: Option<Box<Expr>>,
    },
    RecordLiteral {
        fields: Vec<RecordField>,
    },
    ArrayLiteral(Vec<Expr>),
    TupleLiteral(Vec<Expr>),
    FunctionCall {
        name: ASTString,
        args: Vec<Expr>,
    },
    Accessor {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Lambda {
        args: Vec<IdentifierType>,
        return_type: Option<Type>,
        body: Box<Expr>,
    },
}

#[derive(Debug, Serialize, PartialEq, Eq)]
pub enum Stmt {
    Expr(Expr),
    LetStatement {
        identifier: IdentifierType,
        value: Expr,
        mutable: bool,
    },
    FunctionDefinition {
        name: Option<ASTString>,
        arguments: Vec<IdentifierType>,
        return_type: Option<Type>,
        body: Expr,
    },
    ForInLoop {
        condition_arg: IdentifierType,
        condition_expr: Expr,
        consequent: Expr,
    },
    WhileLoop {
        condition: Expr,
        consequent: Expr,
    },
    TypeDefinition(ASTString, TypeDef),
}

#[derive(Debug, Serialize, PartialEq, Eq)]
pub enum TopLevel {
    NameSpace(Vec<ASTString>),
    GoImport {
        module: ASTString,
        alias: Option<ASTString>,
    },
    FungoImport(FungoImport),
    Stmt(Stmt),
}
