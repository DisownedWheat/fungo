package ast2

import "../lexer"
import "core:log"

Statement :: union {
	TypeLiteral,
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
	^IdentifierType,
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
	name:    IdentifierType,
	value:   Expression,
}

RecordLiteral :: struct {
	fields: [dynamic]RecordField,
}

ArrayLiteral :: struct {
	values: [dynamic]Expression,
}

TupleLiteral :: struct {
	values: [dynamic]Expression,
}

LetStatement :: struct {
	using _: BaseNode,
	mutable: bool,
	bind:    IdentifierType,
	value:   ^Expression,
}

Block :: struct {
	using _:    BaseNode,
	statements: [dynamic]Statement,
	args:       Maybe([dynamic]IdentifierType),
}

IfExpression :: struct {
	condition:   ^Expression,
	consequent:  ^Expression,
	alternative: Maybe(^Expression),
}

FunctionCall :: struct {
	using _: BaseNode,
	args:    [dynamic]Expression,
	op:      bool,
}

Accessor :: struct {
	left:  ^Expression,
	right: ^Expression,
}

ForLoop :: struct {
	using _:   BaseNode,
	condition: Maybe(^Expression),
	body:      ^Expression,
}

ModuleDefinition :: struct {
	using _: BaseNode,
	name:    Maybe(ASTString),
	body:    [dynamic]TopLevel,
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
