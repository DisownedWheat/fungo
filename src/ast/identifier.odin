package ast

Identifier :: struct {
	using _: BaseNode,
}

Bucket :: struct {
	using _: BaseNode,
}

Unit :: struct {
	using _: BaseNode,
}

ArrayDestructure :: struct {
	using _: BaseNode,
	idents:  [dynamic]IdentifierType,
}

RecordDestructure :: struct {
	using _: BaseNode,
	idents:  [dynamic]IdentifierType,
}

TupleDestructure :: struct {
	using _: BaseNode,
	idents:  [dynamic]IdentifierType,
}

Pointer :: distinct IdentifierType
Deref :: distinct IdentifierType

IdentifierType :: union {
	Identifier,
	^Pointer,
	^Deref,
	ArrayDestructure,
	RecordDestructure,
	TupleDestructure,
}
