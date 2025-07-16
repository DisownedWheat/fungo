package ast

Identifier :: struct {
	using _: BaseNode,
	type:    Maybe(Stmt_Index),
}

Bucket :: struct {
	using _: BaseNode,
}

Unit :: struct {
	using _: BaseNode,
}

ArrayDestructure :: struct {
	using _: BaseNode,
	idents:  [dynamic]Ident_Index,
}

RecordDestructure :: struct {
	using _: BaseNode,
	idents:  [dynamic]Ident_Index,
}

TupleDestructure :: struct {
	using _: BaseNode,
	idents:  [dynamic]Ident_Index,
}

Pointer :: distinct Ident_Index
Deref :: distinct Ident_Index

IdentifierType :: union {
	Identifier,
	Pointer,
	Deref,
	ArrayDestructure,
	RecordDestructure,
	TupleDestructure,
}
