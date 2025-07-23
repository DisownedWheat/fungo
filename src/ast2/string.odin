package ast2

import "../lexer"
import "core:bytes"
import "core:log"

ASTString :: struct {
	using _: BaseNode,
}

ast_string_from_token :: proc(token: ^lexer.Token) -> ASTString {
	return {token = token}
}

ast_string_dummy :: proc(allocator := context.allocator) -> ASTString {
	token := new(lexer.Token, allocator)
	token^ = lexer.Token {
		kind = .ModuleFileName,
		span = {0, 0},
	}
	return {token = token}
}

ast_string_print :: proc(str: ASTString, args: ..any) {
	token := str.token
	if args != nil {
		log.info(args)
	}
	log.infof(
		"AST STRING -- %s :: %s",
		token.kind,
		token.lexer^.input[token.span[0]:token.span[1]],
	)
}
