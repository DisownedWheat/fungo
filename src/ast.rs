// AST NODES
use std::rc::Rc;

use serde::Serialize;

pub type ASTString = String;
pub type LogicBlock = Vec<Stmt>;
pub type TokenPosition = (usize, usize);

// Imports
#[derive(Debug, Serialize, PartialEq, Eq)]
pub struct FungoImport {
    pub module: ASTString,
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
    Pointer(Box<IdentifierType>),
    ArrayDestructure(Vec<IdentifierType>, Option<Type>),
    RecordDestructure(Vec<IdentifierType>, Option<Type>),
    TupleDestructure(Vec<IdentifierType>, Option<Type>),
    Bucket,
    Unit,
}

impl IdentifierType {
    pub fn get_name(&self) -> Option<ASTString> {
        match self {
            IdentifierType::Identifier(identifier, _) => Some(identifier.clone()),
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
pub struct LetExpression {
    pub identifier: IdentifierType,
    pub value: Box<Expr>,
    pub mutable: bool,
}

// Functions
#[derive(Debug, Serialize, PartialEq, Eq)]
pub struct FunctionDefinition {
    pub name: Option<ASTString>,
    pub arguments: Vec<IdentifierType>,
    pub return_type: Option<Type>,
    pub body: LogicBlock,
}

#[derive(Debug, Serialize, PartialEq, Eq)]
pub struct Tuple {
    length: usize,
    values: Vec<Type>,
}

#[derive(Debug, Serialize, PartialEq, Eq)]
pub struct EnumDefiniton {
    pub fields: Vec<(ASTString, Option<Type>)>,
}

#[derive(Debug, Serialize, PartialEq, Eq)]
pub struct PipeRight {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Debug, Serialize, PartialEq, Eq)]
pub struct Assign {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Debug, Serialize, PartialEq, Eq)]
pub enum Expr {
    Unit,
    Identifier(IdentifierType),
    Block(Vec<Stmt>),
    BoolLiteral(bool),
    StringLiteral(ASTString),
    IntLiteral(ASTString),
    FloatLiteral(ASTString),
    RecordLiteral {
        fields: Vec<RecordField>,
    },
    ArrayLiteral(Vec<Expr>),
    TupleLiteral(Vec<Expr>),
    BinaryOp {
        left: Box<Expr>,
        right: Box<Expr>,
        op: ASTString,
    },
    FunctionCall {
        name: ASTString,
        args: Vec<Expr>,
    },
    Accessor {
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Index {
        left: Box<Expr>,
        right: Box<Expr>,
    },
}

#[derive(Debug, Serialize, PartialEq, Eq)]
pub enum Stmt {
    Expr(Expr),
    LetStatement(LetExpression),
    FunctionDefinition(FunctionDefinition),
    TypeDefinition(ASTString, TypeDef),
}

#[derive(Debug, Serialize, PartialEq, Eq)]
pub enum TopLevel {
    GoImport(GoImport),
    FungoImport(FungoImport),
    Stmt(Stmt),
}
