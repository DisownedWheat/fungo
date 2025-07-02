package ast

import "../lexer"

ASTString :: struct {
	using _: BaseNode,
}

ast_string_from_token :: proc(token: ^lexer.Token) -> ASTString {
	return {token = token}
}
