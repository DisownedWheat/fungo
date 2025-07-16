package ast

import "../lexer"
import "core:log"

Node_Index :: distinct int
Expr_Index :: distinct int
Stmt_Index :: distinct int
Ident_Index :: distinct int

Regular_Index :: union {
	Expr_Index,
	Stmt_Index,
	Ident_Index,
}

Stmt_List :: distinct [dynamic]Statement
TopLevel_List :: distinct [dynamic]TopLevel

Statement :: union {
	LetStatement,
	ForLoop,
	Expression,
}

Expression :: union {
	Identifier,
	IntLiteral,
	FloatLiteral,
	StringLiteral,
	CharLiteral,
	BoolLiteral,
	Block,
	RecordLiteral,
	ArrayLiteral,
	TupleLiteral,
	IfExpression,
	FunctionCall,
	Accessor,
	IdentifierType,
}

BaseNode :: struct {
	token: ^lexer.Token,
}

IntLiteral :: struct {
	using _: BaseNode,
}

FloatLiteral :: struct {
	using _: BaseNode,
}

StringLiteral :: struct {
	using _: BaseNode,
}

CharLiteral :: struct {
	using _: BaseNode,
}

BoolLiteral :: struct {
	using _: BaseNode,
}

RecordField :: struct {
	using _: BaseNode,
	name:    Ident_Index,
	value:   Expr_Index,
}

RecordLiteral :: struct {
	fields: [dynamic]RecordField,
}

ArrayLiteral :: struct {
	values: [dynamic]Expr_Index,
}

TupleLiteral :: struct {
	values: [dynamic]Expr_Index,
}

LetStatement :: struct {
	using _: BaseNode,
	mutable: bool,
	bind:    Ident_Index,
	value:   Expr_Index,
}

Block :: struct {
	statements: [dynamic]Stmt_Index,
	args:       Maybe([dynamic]Ident_Index),
}

IfExpression :: struct {
	condition:   Expr_Index,
	consequent:  Expr_Index,
	alternative: Maybe(Expr_Index),
}

FunctionCall :: struct {
	using _: BaseNode,
	args:    [dynamic]Expr_Index,
	op:      bool,
}

Accessor :: struct {
	left:  Expr_Index,
	right: Expr_Index,
}

ForLoop :: struct {
	using _:   BaseNode,
	condition: Maybe(Expr_Index),
	body:      Expr_Index,
}

ModuleDefinition :: struct {
	using _: BaseNode,
	name:    Maybe(ASTString),
	body:    [dynamic]Node_Index,
}

GoImport :: struct {
	module: ASTString,
	alias:  Maybe(ASTString),
}

FungoImport :: struct {
	modules: [dynamic]ASTString,
}

TopLevel :: union {
	GoImport,
	FungoImport,
	TypeDefinition,
	LetStatement,
}

print_node :: proc(node: $T/BaseNode) {
	token := node.token
	log.infof("%s :: %s", token.kind, token.lexer^.input[token.span[0]:token.span[1]])
}
