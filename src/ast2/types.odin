package ast2

UnitType :: struct {
	using _: BaseNode,
}

TypeLiteralType :: struct {
	using _: BaseNode,
	name:    ASTString,
	modules: [dynamic]ASTString,
}

PointerType :: struct {
	using _: BaseNode,
	type: ^TypeLiteral
	}

SliceType :: struct {
	using _: BaseNode,
	type: ^TypeLiteral
}

TypeLiteral :: union {
	UnitType,
	TypeLiteralType,
	PointerType,
	SliceType,
}

RecordTypeField :: struct {
	using _: BaseNode,
	type:    TypeDefinition,
}

RecordTypeDefinition :: struct {
	using _: BaseNode,
	fields:  [dynamic]RecordTypeField,
}

TupleDefinition :: struct {
	using _: BaseNode,
	length:  int,
	types:   [dynamic]TypeDefinition,
}

AbstractType :: struct {
	using _:     BaseNode,
	type_params: [dynamic]ASTString,
}

VariantType :: struct {
	using _: BaseNode,
}

TypeDefinition :: union {
	AbstractType,
	TypeLiteral,
	VariantType,
	RecordTypeDefinition,
	TupleDefinition,
}
