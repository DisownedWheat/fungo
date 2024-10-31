// AST NODES

use std::rc::Rc;

pub type ASTString = Rc<String>;
pub type LogicBlock = Vec<ASTNode>;
pub type TokenPosition = (usize, usize);

// Imports
#[derive(Debug)]
pub struct FungoImport {
    pub module: ASTString,
}

#[derive(Debug)]
pub struct GoImport {
    pub module: ASTString,
    pub alias: Option<ASTString>,
}

// Identifiers

#[derive(Debug)]
pub struct Identifier {
    pub value: ASTString,
}

#[derive(Debug)]
pub struct Accessor {
    left: Box<IdentifierType>,
    right: Option<ASTString>,
}

#[derive(Debug)]
pub enum IdentifierType {
    Identifier(ASTString, Option<Type>),
    Pointer(Box<IdentifierType>),
    ArrayDestructure(Vec<IdentifierType>, Option<Type>),
    RecordDestructure(Vec<IdentifierType>, Option<Type>),
    TupleDestructure(Vec<IdentifierType>, Option<Type>),
    Bucket,
    Accessor {
        left: Box<IdentifierType>,
        right: Option<ASTString>,
    },
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

#[derive(Debug)]
pub enum Type {
    Type {
        name: ASTString,
        module: Option<ASTString>,
    },
    Pointer(Box<Type>),
    Slice(Box<Type>),
}

#[derive(Debug)]
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
#[derive(Debug)]
pub struct RecordDefinitionField {
    pub name: ASTString,
    pub type_: TypeDef,
}

#[derive(Debug)]
pub struct RecordDefinition {
    pub fields: Vec<RecordDefinitionField>,
}

#[derive(Debug)]
pub struct RecordField {
    pub name: ASTString,
    pub value: ASTNode,
}

#[derive(Debug)]
pub struct RecordLiteral {
    pub fields: Vec<RecordField>,
}

#[derive(Debug)]
pub struct LetExpression {
    pub identifier: IdentifierType,
    pub value: Box<ASTNode>,
    pub mutable: bool,
}

// Functions
#[derive(Debug)]
pub struct FunctionDefinition {
    pub name: Option<ASTString>,
    pub arguments: Vec<IdentifierType>,
    pub return_type: Option<Type>,
    pub body: LogicBlock,
}

#[derive(Debug)]
pub struct Tuple {
    length: usize,
    values: Vec<Type>,
}

#[derive(Debug)]
pub struct EnumDefiniton {
    pub fields: Vec<(ASTString, Option<Type>)>,
}

#[derive(Debug)]
pub enum TopLevel {
    FunctionDefinition(FunctionDefinition),
    StructMethodDefinition(ASTString, FunctionDefinition),
    TopLevelTypeDef(ASTString, TypeDef),
}

#[derive(Debug)]
pub struct PipeRight {
    pub left: Box<ASTNode>,
    pub right: Box<ASTNode>,
}

#[derive(Debug)]
pub struct Assign {
    pub left: Box<ASTNode>,
    pub right: Box<ASTNode>,
}

#[derive(Debug)]
pub enum ASTNode {
    Bucket,
    Root(Vec<ASTNode>),
    LogicBlock(LogicBlock),
    GoImport(GoImport),
    FungoImport(FungoImport),
    Identifier(IdentifierType),
    RecordDefinition(RecordDefinition),
    RecordLiteral(RecordLiteral),
    LetExpression(LetExpression),
    FunctionDefinition(FunctionDefinition),
    ParenExpression(Option<Box<ASTNode>>),
    ArrayLiteral(Vec<ASTNode>),
    StringLiteral(ASTString),
    NumberLiteral(ASTString),
    BoolLiteral(bool),
    PipeRight(PipeRight),
    Accessor(Accessor),
    Assign(Assign),
    Enum(EnumDefiniton),
    Tuple(Tuple),
    TypeDefinition(ASTString, TypeDef),
    TopLevel(bool, TopLevel),
    NoOp,
    EOF,
}

impl ASTNode {
    pub fn get_ident_name(&self) -> Option<ASTString> {
        match self {
            ASTNode::Identifier(identifier) => identifier.get_name(),
            _ => None,
        }
    }
    // pub fn print(&self) {
    //     match self {
    //         Self::Bucket => {
    //             println!("Bucket");
    //         }
    //         Self::Root(nodes) => {
    //             println!("Root");
    //             nodes.iter().for_each(|node| node.print());
    //         }
    //         Self::LogicBlock(nodes) => {
    //             println!("LogicBlock");
    //             nodes.iter().for_each(|node| node.print());
    //         }
    //         Self::GoImport(go_import) => {
    //             println!("GoImport: {:?}", go_import);
    //         }
    //         Self::FungoImport(fungo_import) => {
    //             println!("FungoImport: {:?}", fungo_import);
    //         }
    //         Self::Identifier(identifier) => {
    //             println!("Identifier: {:?}", identifier);
    //         }
    //         Self::RecordDefinition(record_definition) => {
    //             println!("RecordDefinition: {:?}", record_definition);
    //         }
    //         Self::RecordLiteral(record_literal) => {
    //             println!("RecordLiteral: {:?}", record_literal);
    //         }
    //         Self::LetExpression(let_expression) => {
    //             println!("LetExpression: {:?}", let_expression);
    //         }
    //         Self::FunctionDefinition(function_definition) => {
    //             println!("FunctionDefinition: {:?}", function_definition);
    //         }
    //         Self::ParenExpression(expression) => {
    //             println!("ParenExpression: {:?}", expression);
    //         }
    //         Self::ArrayLiteral(array) => {
    //             println!("ArrayLiteral");
    //             array.iter().for_each(|node| node.print());
    //         }
    //         Self::StringLiteral(string) => {
    //             println!("StringLiteral: {:?}", string);
    //         }
    //         Self::NumberLiteral(number) => {
    //             println!("NumberLiteral: {:?}", number);
    //         }
    //         Self::PipeRight(pipe_right) => {
    //             println!("PipeRight");
    //             println!("Left: {:?}", pipe_right.left);
    //             println!("Right: {:?}", pipe_right.right);
    //         }
    //         Self::Accessor(accessor) => {
    //             println!("Accessor: {:?}", accessor);
    //         }
    //         Self::Enum(enum_) => {
    //             println!("Enum: {:?}", enum_);
    //         }
    //         Self::Tuple(tuple) => {
    //             println!("Tuple: {:?}", tuple);
    //         }
    //         Self::TypeDefinition(name, type_def) => {
    //             println!("TypeDefinition: name: {} {:?}", name, type_def);
    //         }
    //         Self::TopLevel(is_pub, top_level) => {
    //             println!("TopLevel: {:?}", is_pub);
    //             match top_level {
    //                 TopLevel::FunctionDefinition(FunctionDefinition {
    //                     name,
    //                     arguments,
    //                     return_type,
    //                     body,
    //                     pointer,
    //                 }) => {
    //                     println!(
    //                         "TopLevelFunc: Name: {:?}, Arguments: {:?}, Return Type: {:?},  Pointer: {:?}",
    //                         name, arguments, return_type, pointer
    //                     );
    //                     print!("Body: ");
    //                     body.iter().for_each(|node| node.print());
    //                 }
    //                 TopLevel::StructMethodDefinition(
    //                     name,
    //                     FunctionDefinition {
    //                         arguments,
    //                         return_type,
    //                         body,
    //                         pointer,
    //                         ..
    //                     },
    //                 ) => {
    //                     println!("StructMethodDefinition: {:?}", name,);
    //                     println!(
    //                         "Name: {:?}, Arguments: {:?}, Return Type: {:?},  Pointer: {:?}",
    //                         name, arguments, return_type, pointer
    //                     );
    //                     print!("Body: ");
    //                     body.iter().for_each(|node| node.print());
    //                 }
    //                 TopLevel::TopLevelTypeDef(name, type_def) => {
    //                     println!("TopLevelTypeDef: {} {:?}", name, type_def);
    //                 }
    //             }
    //         }
    //         Self::NoOp => {
    //             println!("NoOp");
    //         }
    //         Self::EOF => {
    //             println!("EOF");
    //         }
    //         Self::Assign(assign) => {
    //             println!("Assign");
    //             println!("Left: {:?}", assign.left);
    //             println!("Right: {:?}", assign.right);
    //         }
    //     };
    //     println!("");
    // }
}

impl Default for ASTNode {
    fn default() -> Self {
        ASTNode::NoOp
    }
}
