package ast2

Identifier :: struct {
	using _: BaseNode,
	type:    Maybe(^Statement),
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
	idents:  [dynamic]Identifier,
}

TupleDestructure :: struct {
	using _: BaseNode,
	idents:  [dynamic]Identifier,
}

Pointer :: struct {
	using _: BaseNode,
	value:   ^Expression,
}
Deref :: struct {
	using _: BaseNode,
	value:   ^Expression,
}

IdentifierType :: union {
	Identifier,
	Pointer,
	Deref,
	ArrayDestructure,
	RecordDestructure,
	TupleDestructure,
}
