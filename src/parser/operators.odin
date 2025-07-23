package parser

import "../lexer"
import "core:unicode/utf8"

check_kinds :: proc(tokens: []lexer.Token, expected: []lexer.TokenKind) -> bool {
	if len(tokens) < len(expected) {
		return false
	}

	for kind, i in expected {
		t := tokens[i]
		if t.kind != kind {
			return false
		}
	}

	return true
}

check_dot :: proc(token: ^lexer.Token) -> bool {
	return (token.kind == .Operator) && (token.lexer.input[token.span[0]] == '.')
}

check_comma :: proc(token: ^lexer.Token) -> bool {
	return (token.kind == .Operator) && (token.lexer.input[token.span[0]] == ',')
}

check_colon :: proc(token: ^lexer.Token) -> bool {
	return (token.kind == .Operator) && (token.lexer.input[token.span[0]] == ':')
}

check_pointer :: proc(token: ^lexer.Token) -> bool {
	return (token.kind == .Operator) && (token.lexer.input[token.span[0]] == '&')
}

check_deref :: proc(token: ^lexer.Token) -> bool {
	return (token.kind == .Operator) && (token.lexer.input[token.span[0]] == '*')
}

check_operator :: proc(token: ^lexer.Token, expected_operator: string) -> bool {
	if token.kind != .Operator {
		return false
	}
	i := 0
	for char in expected_operator {
		defer i += utf8.rune_size(char)

		// expected_operator is longer than token span
		if token.span[0] + i > token.span[1] {
			return false
		}

		r := utf8.rune_at(token.lexer.input, token.span[0] + i)
		if r != char {
			return false
		}
	}
	return true
}
