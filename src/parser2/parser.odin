package parser2

import ast "../ast2"
import "../lexer"
import "core:bytes"
import "core:log"
import "core:mem"

Parser_Error :: enum {
	Invalid_Import,
	Invalid_TopLevel,
	Invalid_Identifier,
	Unexpected_EOF,
	Unexpected_Token,
	TODO,
}

Parser_Bool :: proc(p: ^Parser) -> bool
Parser_Func :: proc(p: ^Parser)

Parser :: struct {
	tokens:            lexer.TokenList,
	position:          int,
	rollback_position: int,
	current:           ^lexer.Token,
	peek:              ^lexer.Token,
	module:            ast.ModuleDefinition,
	file_name:         string,
	allocator:         mem.Allocator,
}

@(private)
parser_peek :: proc(p: ^Parser) -> ^lexer.Token {
	if p.position + 1 < len(p.tokens) {
		return &p.tokens[p.position + 1]
	}
	return lexer.eof_token()
}

@(private)
parser_init :: proc(l: ^lexer.Lexer, allocator := context.allocator) -> Parser {
	module := ast.ModuleDefinition {
		body = make([dynamic]ast.TopLevel),
	}
	return Parser {
		tokens = l.tokens[:],
		position = 0,
		allocator = allocator,
		current = &l.tokens[0],
		peek = &l.tokens[1],
		module = module,
		file_name = l.file_name,
	}
}

@(private)
parser_next_one :: proc(p: ^Parser) -> bool {
	defer {
		p.current = &p.tokens[p.position]
		p.peek = parser_peek(p)
	}
	p.position += 1
	return p.peek.kind != .EOF
}

@(private)
parser_next_n :: proc(p: ^Parser, n: int) -> bool {
	defer {
		p.position += n
		p.current = &p.tokens[p.position]
		p.peek = parser_peek(p)
	}
	position := p.position
	for i in position ..= (position + n) {
		if p.tokens[i].kind == .EOF {
			return false
		}
	}
	return true
}

@(private)
parser_next :: proc {
	parser_next_one,
	parser_next_n,
}

@(private)
parser_rollback :: proc(p: ^Parser) {
	defer {
		p.current = &p.tokens[p.position]
		p.peek = parser_peek(p)
	}
	p.position = p.rollback_position
}

parser_check_ahead_func :: proc(
	p: ^Parser,
	delimiter: lexer.TokenKind,
	check: Parser_Bool,
) -> bool {
	p.rollback_position = p.position
	defer parser_rollback(p)
	for p.current.kind != delimiter {
		result := parser_next(p)
		if !result {
			return false
		}
	}
	return check(p)
}

parser_check_ahead_token :: proc(
	p: ^Parser,
	delimiter: lexer.TokenKind,
	to_check: lexer.TokenKind,
) -> bool {
	p.rollback_position = p.position
	defer parser_rollback(p)

	for p.current.kind != delimiter {
		result := parser_next(p)
		if !result {
			return false
		}
	}

	return p.peek.kind == to_check
}

parser_check_ahead_string :: proc(
	p: ^Parser,
	delimiter: lexer.TokenKind,
	to_check: lexer.TokenKind,
	value: string,
) -> bool {
	p.rollback_position = p.position
	defer parser_rollback(p)

	for p.current.kind != delimiter {
		result := parser_next(p)
		if !result {
			return false
		}
	}

	if p.peek.kind != to_check {
		return false
	}

	str := p.peek.lexer.input[p.peek.span[0]:p.peek.span[1]]
	return str == value
}

parser_check_ahead :: proc {
	parser_check_ahead_func,
	parser_check_ahead_token,
	parser_check_ahead_string,
}

@(private)
parse_go_import :: proc(p: ^Parser) -> (ast.GoImport, Parser_Error) {
	imp: ast.GoImport
	#partial switch p.current.kind {
	case .StringLiteral:
		str := ast.ast_string_from_token(p.current)
		imp := ast.GoImport {
			module = str,
		}
		parser_next(p)
	case .Identifier:
		ident := p.current
		if !parser_next(p) || p.current.kind != .StringLiteral {
			return imp, .Invalid_Import
		}
		str := ast.ast_string_from_token(p.current)
		imp := ast.GoImport {
			module = str,
			alias  = ast.ast_string_from_token(ident),
		}
		parser_next(p)
	case:
		return imp, .Invalid_Import
	}
	return imp, nil
}

@(private)
parse_fungo_import :: proc(p: ^Parser) -> (node: ast.FungoImport, err: Parser_Error) {
	modules: [dynamic]ast.ASTString
	defer {
		if err != nil {
			delete(modules)
		}
	}

	node = ast.FungoImport {
		modules = modules,
	}

	#partial switch p.current.kind {
	case .Identifier:
		str := ast.ast_string_from_token(p.current)
		append(&node.modules, str)
		if check_dot(p.peek) {
			for check_dot(p.peek) && parser_next(p, 2) {
				#partial switch p.current.kind {
				case .Identifier:
					str := ast.ast_string_from_token(p.current)
					append(&node.modules, str)
					parser_next(p)
				case:
					err = .Invalid_Import
					return
				}
			}
		} else {
			parser_next(p)
		}
	case:
		err = .Invalid_Import
		return
	}
	return
}

@(private)
parse_imports :: proc(p: ^Parser) -> (node: ast.TopLevel, err: Parser_Error) {
	#partial switch p.current.kind {
	case .StringLiteral:
		node = parse_go_import(p) or_return
	case .Identifier:
		if parser_peek(p).kind == .StringLiteral {
			node = parse_go_import(p) or_return
		} else {
			node = parse_fungo_import(p) or_return
		}
	case:
		err = .Invalid_Import
	}
	return
}

@(private)
parse_top_level :: proc(p: ^Parser) -> (node: ast.TopLevel, err: Parser_Error) {
	#partial switch p.current.kind {
	case .Let:
		// node = parse_let_statement(p) or_return
		// if err != nil {
		// 	return -1, err
		// }
		// return ast.Node_Index(node), nil
		err = .TODO
		return
	case .Type:
		err = .TODO
		return
	case .Module:
		err = .TODO
		return
	case .Import:
		parser_next(p)
		return parse_imports(p)
	case:
		err = .Invalid_TopLevel
		return
	}
}

parse :: proc(
	lexer: ^lexer.Lexer,
	allocator := context.allocator,
) -> (
	parser: Parser,
	err: Parser_Error,
) {
	parser = parser_init(lexer, allocator)
	context.allocator = parser.allocator
	for parser.peek.kind != .EOF {
		node := parse_top_level(&parser) or_return
		append(&parser.module.body, node)
	}
	return
}
