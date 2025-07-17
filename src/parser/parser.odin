package parser

import "../ast"
import "../lexer"
import "core:bytes"
import "core:fmt"
import "core:log"
import "core:mem"

ParserError :: enum {
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
	nodes:             ast.Stmt_List,
	top_level:         ast.TopLevel_List,
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
	top_level := make(ast.TopLevel_List, allocator)
	nodes := make(ast.Stmt_List, allocator)
	module := ast.ModuleDefinition {
		body = make([dynamic]ast.Node_Index),
	}
	return Parser {
		tokens = l.tokens[:],
		position = 0,
		allocator = allocator,
		current = &l.tokens[0],
		peek = &l.tokens[1],
		top_level = top_level,
		module = module,
		file_name = l.file_name,
		nodes = nodes,
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

@(private)
parser_add_statment :: proc(parser: ^Parser, node: $I/ast.Statement) -> ast.Stmt_Index {
	return ast.Stmt_Index(append(&parser.nodes, node))
}

@(private)
parser_add_expr :: proc(parser: ^Parser, node: $I/ast.Expression) -> ast.Expr_Index {
	append(&parser.nodes, node)
	return ast.Expr_Index(len(parser.nodes) - 1)
}

@(private)
parser_add_top_level :: proc(parser: ^Parser, node: ast.TopLevel) -> ast.Node_Index {
	append(&parser.top_level, node)
	return ast.Node_Index(len(parser.top_level) - 1)
}

@(private)
parser_add_identifier :: proc(parser: ^Parser, node: ast.IdentifierType) -> ast.Ident_Index {
	append(&parser.nodes, ast.Expression(node))
	return ast.Ident_Index(len(parser.nodes) - 1)
}

parser_add :: proc {
	parser_add_expr,
	parser_add_statment,
	parser_add_top_level,
	parser_add_identifier,
}

@(private)
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
parse_go_import :: proc(p: ^Parser) -> (ast.Node_Index, ParserError) {
	imp: ast.TopLevel
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
			return -1, .Invalid_Import
		}
		str := ast.ast_string_from_token(p.current)
		imp := ast.GoImport {
			module = str,
			alias  = ast.ast_string_from_token(ident),
		}
		parser_next(p)
	case:
		return -1, .Invalid_Import
	}
	index := parser_add(p, imp)
	return index, nil
}

@(private)
parse_fungo_import :: proc(p: ^Parser) -> (ret: ast.Node_Index, err: ParserError) {
	modules: [dynamic]ast.ASTString
	defer {
		if err != nil {
			delete(modules)
		}
	}

	imp := ast.FungoImport {
		modules = modules,
	}

	#partial switch p.current.kind {
	case .Identifier:
		str := ast.ast_string_from_token(p.current)
		append(&imp.modules, str)
		if check_dot(p.peek) {
			for check_dot(p.peek) && parser_next(p, 2) {
				#partial switch p.current.kind {
				case .Identifier:
					str := ast.ast_string_from_token(p.current)
					append(&imp.modules, str)
					parser_next(p)
				case:
					return -1, .Invalid_Import
				}
			}
		} else {
			parser_next(p)
		}
		ret = parser_add(p, imp)
	case:
		return -1, .Invalid_Import
	}
	return
}

@(private)
parse_imports :: proc(p: ^Parser) -> (node: ast.Node_Index, err: ParserError) {
	node = -1
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
parse_statement :: proc(p: ^Parser) -> (idx: ast.Stmt_Index, err: ParserError) {
	if p.current.kind == .Let {
		parser_next(p)
		idx = parse_let_statement(p) or_return
	} else {
		expr := parse_expression(p) or_return
		idx = ast.Stmt_Index(expr)
	}
	return
}

@(private)
parse_function :: proc(p: ^Parser) -> (idx: ast.Expr_Index, err: ParserError) {

	first_token: ^lexer.Token

	// Parse the args
	args := make([dynamic]ast.Ident_Index, p.allocator)
	defer {
		if err != nil {
			delete(args)
		}
	}

	if p.current.kind == .LParen {
		parser_next(p)
	}
	for {
		ident := parse_identifier(p) or_return
		append(&args, ident)
		if p.current.kind == .RParen {
			parser_next(p)
			break
		}
		if !check_comma(p.current) {
			err = .Unexpected_Token
			return
		}
		parser_next(p)
	}

	if p.current.kind != .LBrace {
		err = .Unexpected_Token
		return
	}
	first_token = p.current
	parser_next(p)

	stmts := make([dynamic]ast.Stmt_Index, p.allocator)
	for {
		if p.current.kind == .RBrace {
			break
		}
		stmt := parse_statement(p) or_return
		append(&stmts, stmt)
	}
	func := ast.Block {
		token      = first_token,
		statements = stmts,
		args       = args,
	}
	idx = parser_add(p, ast.Expression(func))
	return
}

@(private)
parse_expression :: proc(p: ^Parser) -> (idx: ast.Expr_Index, err: ParserError) {
	number := false
	#partial switch p.current.kind {
	case .IntLiteral:
		number = true
		node := ast.IntLiteral {
			token = p.current,
		}
		idx = parser_add(p, ast.Expression(node))
		parser_next(p)

	case .LParen:
		if parser_check_ahead(p, .RParen, lexer.TokenKind.LBrace) {
			return parse_function(p)
		}

	case .LBrace:
		start := p.current
		parser_next(p)
		stmts := make([dynamic]ast.Stmt_Index)
		for p.current.kind != .RBrace {
			if p.current.kind == .EOF {
				err = .Unexpected_EOF
				return
			}
			s := parse_statement(p) or_return
			append(&stmts, s)
		}

		node := ast.Block {
			token      = start,
			statements = stmts,
		}
		idx = parser_add(p, ast.Expression(node))

	case:
		err = .TODO
		return
	}

	// TODO: Handle function calls
	if !number && p.current.kind == .LParen {}

	// TODO: Handle indexing
	if !number && p.current.kind == .LBracket {}

	if p.current.kind == .Operator {
		args := make([dynamic]ast.Expr_Index, p.allocator)
		left := idx
		append(&args, left)
		operator := p.current

		parser_next(p)

		right := parse_expression(p) or_return
		append(&args, right)
		node := ast.FunctionCall {
			token = operator,
			args  = args,
			op    = true,
		}
		idx = parser_add(p, ast.Expression(node))
	}

	return
}

@(private)
parse_let_statement :: proc(p: ^Parser) -> (idx: ast.Stmt_Index, err: ParserError) {
	first_token := p.current

	parser_next(p)
	mutable := false
	if p.current.kind == .Mut {
		mutable = true
		parser_next(p)
	}

	ident := parse_identifier(p) or_return
	if !check_operator(p.current, "=") {
		err = .Unexpected_Token
		return
	}
	parser_next(p)
	expr := parse_expression(p) or_return

	let_stmt := ast.LetStatement {
		token = first_token,
		bind  = ident,
		value = expr,
	}
	x := ast.IntLiteral{}
	idx = parser_add(p, ast.Statement(let_stmt))
	return
}

@(private)
parse_top_level :: proc(p: ^Parser) -> (ast.Node_Index, ParserError) {
	#partial switch p.current.kind {
	case .Let:
		node, err := parse_let_statement(p)
		if err != nil {
			return -1, err
		}
		return ast.Node_Index(node), nil
	case .Type:
		return -1, .TODO
	case .Module:
		return -1, .TODO
	case .Import:
		parser_next(p)
		return parse_imports(p)
	case:
		return -1, .Invalid_TopLevel
	}
}

parse :: proc(
	lexer: ^lexer.Lexer,
	allocator := context.allocator,
) -> (
	parser: Parser,
	err: ParserError,
) {
	parser = parser_init(lexer, allocator)
	context.allocator = parser.allocator
	for parser.peek.kind != .EOF {
		node := parse_top_level(&parser) or_return
		append(&parser.module.body, node)
	}
	return parser, nil
}
