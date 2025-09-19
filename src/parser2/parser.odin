package parser2

import ast "../ast2"
import "../lexer"
import "base:runtime"
import "core:bytes"
import "core:log"
import "core:mem"
import oast "core:odin/ast"


Parser_Error_Struct :: struct {
	kind: Error_Kind,
	ctx:  string,
	loc:  runtime.Source_Code_Location,
}

Parser_Error :: union {
	Parser_Error_Struct,
}

Error_Kind :: enum {
	Invalid_Import,
	Invalid_TopLevel,
	Invalid_Identifier,
	Unexpected_EOF,
	Unexpected_Token,
	TODO,
}

@(private)
Parser_Bool :: proc(p: ^Parser) -> bool
@(private)
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
parser_slice_tokens :: proc(
	p: ^Parser,
	delimiter: lexer.TokenKind,
	loc := #caller_location,
) -> (
	[]lexer.Token,
	Parser_Error,
) {
	initial_position := p.position
	p.rollback_position = initial_position

	found := false
	for parser_next(p) {
		if p.current.kind == delimiter {
			found := true
			break
		}
	}

	if !found {
		return {}, Parser_Error_Struct{kind = .Unexpected_EOF, ctx = "Slice Tokens", loc = loc}
	}

	target_position := p.position
	parser_rollback(p)
	return p.tokens[initial_position:target_position], nil
}

@(private)
parser_slice_tokens_to_parser :: proc(
	p: ^Parser,
	delimiter: lexer.TokenKind,
	loc := #caller_location,
) -> (
	^Parser,
	Parser_Error,
) {
	slice, err := parser_slice_tokens(p, delimiter, loc)
	if err != nil {
		return {}, err
	}

	p2 := new(Parser, p.allocator)
	p2^ = parser_init(slice, p.file_name, p.allocator)
	return p2, nil
}

// Creates a parser struct. Most AST nodes are heap allocated so the allocator passed through will be used
@(private)
parser_init :: proc(
	tokens: []lexer.Token,
	file_name: string,
	allocator := context.allocator,
) -> Parser {
	module := ast.ModuleDefinition {
		body = make([dynamic]ast.TopLevel),
	}
	return Parser {
		tokens = tokens,
		position = 0,
		allocator = allocator,
		current = &tokens[0],
		peek = &tokens[1],
		module = module,
		file_name = file_name,
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

//Proceeds the parser position up until the next instance of the delimiter token and then runs the
//provided procedure. If the delimiter is not found will return false. The parser position will
//roll back to the current position after the provided procedure returns
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

//Proceeds the parser position up until the next instance of the delimiter token and then checks
//whether the next token is the one provided. If the delimiter token is not found will return false.
//The parser position will roll back to the current position after the peek token is checked
@(private)
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

//Proceeds the parser position up until the next instance of the delimiter token and then checks
//whether the next token has a string value. If the delimiter token is not found will return false.
//The parser position will roll back to the current position after the peek token is checked
@(private)
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

@(private)
parser_check_ahead :: proc {
	parser_check_ahead_func,
	parser_check_ahead_token,
	parser_check_ahead_string,
}

@(private)
parse_go_import :: proc(
	p: ^Parser,
	loc := #caller_location,
) -> (
	imp: ast.GoImport,
	err: Parser_Error,
) {
	#partial switch p.current.kind {
	case .StringLiteral:
		str := ast.ast_string_from_token(p.current)
		imp = ast.GoImport {
			module = str,
		}
		parser_next(p)
	case .Identifier:
		ident := p.current
		if !parser_next(p) || p.current.kind != .StringLiteral {
			return imp, Parser_Error_Struct{kind = .Invalid_Import, ctx = "Go Import"}
		}
		str := ast.ast_string_from_token(p.current)
		imp = ast.GoImport {
			module = str,
			alias  = ast.ast_string_from_token(ident),
		}
		parser_next(p)
	case:
		err = Parser_Error_Struct {
			kind = .Invalid_Import,
			ctx  = "GoImport",
			loc  = loc,
		}
		return
	}
	return
}

@(private)
parse_fungo_import :: proc(
	p: ^Parser,
	loc := #caller_location,
) -> (
	node: ast.FungoImport,
	err: Parser_Error,
) {
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
					err = Parser_Error_Struct {
						kind = .Invalid_Import,
						ctx  = "Fungo Import",
						loc  = loc,
					}
					return
				}
			}
		} else {
			parser_next(p)
		}
	case:
		err = Parser_Error_Struct {
			kind = .Invalid_Import,
			ctx  = "Fungo Import",
			loc  = loc,
		}
		return
	}
	return
}

@(private)
parse_imports :: proc(
	p: ^Parser,
	loc := #caller_location,
) -> (
	node: ast.TopLevel,
	err: Parser_Error,
) {
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
		err = Parser_Error_Struct {
			kind = .Invalid_Import,
			ctx  = "Parse Imports",
			loc  = loc,
		}
	}
	return
}

@(private)
parse_top_level :: proc(
	p: ^Parser,
	loc := #caller_location,
) -> (
	node: ast.TopLevel,
	err: Parser_Error,
) {
	#partial switch p.current.kind {
	case .Let:
		node = parse_let_statement(p) or_return
		return
	case .Type:
		err = Parser_Error_Struct{.TODO, "Top Level", loc}
		return
	case .Module:
		err = Parser_Error_Struct{.TODO, "Top Level", loc}
		return
	case .Import:
		parser_next(p)
		return parse_imports(p)
	case:
		err = Parser_Error_Struct{.Invalid_TopLevel, "Top Level", loc}
		return
	}
}

@(private)
parse_function :: proc(
	p: ^Parser,
	loc := #caller_location,
) -> (
	expr: ast.Block,
	err: Parser_Error,
) {
	first_token: ^lexer.Token

	args := make([dynamic]ast.IdentifierType, p.allocator)
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
			err = Parser_Error_Struct{.Unexpected_Token, "Function :: Args", loc}
			return
		}
		parser_next(p)
	}

	if p.current.kind != .LBrace {
		err = Parser_Error_Struct{.Unexpected_Token, "Function :: Block", loc}
		return
	}

	first_token = p.current
	parser_next(p)

	stmts := make([dynamic]ast.Statement, p.allocator)
	for {
		if p.current.kind == .RBrace {
			break
		}
		stmt := parse_statement(p) or_return
		append(&stmts, stmt)
	}

	expr = ast.Block {
		token      = first_token,
		statements = stmts,
		args       = args,
	}

	return
}

@(private)
parse_if_expression :: proc(p: ^Parser) -> (expr: ast.IfExpression, err: Parser_Error) {
	condition := parse_expression(p) or_return

	if p.current.kind != .LBrace {
		log.error(condition)
		err = Parser_Error_Struct {
			kind = .Unexpected_Token,
			ctx  = "If Expression :: LBrace",
			loc  = #location(),
		}
		return
	}

	// This should return a block considering it's an LBrace
	consequent := parse_expression(p) or_return

	if p.current.kind == .Else {
		parser_next(p)
		if p.current.kind != .LBrace {
			err = Parser_Error_Struct{.Unexpected_Token, "If Expression :: Else", #location()}
			return
		}
		alternate := parse_expression(p) or_return
	}

	return
}

@(private)
parse_expression :: proc(p: ^Parser) -> (expr: ast.Expression, err: Parser_Error) {
	number := false
	#partial switch p.current.kind {
	case .IntLiteral:
		number = true
		expr = ast.IntLiteral {
			token = p.current,
		}
		parser_next(p)

	case .FloatLiteral:
		number = true
		expr = ast.FloatLiteral {
			token = p.current,
		}
		parser_next(p)

	case .StringLiteral:
		expr = ast.StringLiteral {
			token = p.current,
		}
		parser_next(p)

	case .CharLiteral:
		expr = ast.CharLiteral {
			token = p.current,
		}
		parser_next(p)

	case .Identifier:
		expr = ast.Identifier {
			token = p.current,
		}
		parser_next(p)

	case .If:
		parser_next(p)
		expr = parse_if_expression(p) or_return

	case .LParen:
		if parser_check_ahead(p, .RParen, lexer.TokenKind.LBrace) {
			return parse_function(p)
		}


	// TODO: Add tuple parsing, and nested expressions

	case .LBrace:
		start := p.current
		parser_next(p)
		stmts := make([dynamic]ast.Statement)
		for p.current.kind != .RBrace {
			if p.current.kind == .EOF {
				err = Parser_Error_Struct{.Unexpected_EOF, "Expression :: LBrace", #location()}
				return
			}
			s := parse_statement(p) or_return
			append(&stmts, s)
		}

		expr = ast.Block {
			token      = start,
			statements = stmts,
		}

	case:
		err = Parser_Error_Struct{.TODO, "Expression", #location()}
		return
	}

	found: bool
	found = parse_post_expression_operators(p, &expr)
	if found {
		return
	}

	if p.current.kind == .Operator {
		args := make([dynamic]ast.Expression, p.allocator)
		append(&args, expr)
		operator := p.current

		parser_next(p)

		right := parse_expression(p) or_return
		append(&args, right)
		expr = ast.FunctionCall {
			token = operator,
			args  = args,
			op    = true,
		}
	}

	return
}

parse_post_expression_operators :: proc(
	p: ^Parser,
	curr_expr: ^ast.Expression,
	loc := #caller_location,
) -> (
	ok: bool,
) {
	#partial switch p.current.kind {
	case .Dot:
		// TODO: Handle accessors
		parser_next(p)
		if p.current.kind != .Identifier {
			ok = false
			return
		}
		right, err := parse_identifier(p)
		if err != nil {
			return
		}

		right_ptr := new(ast.IdentifierType, p.allocator)
		right_ptr^ = right
		left_ptr := new(ast.Expression, p.allocator)
		left_ptr^ = curr_expr^

		expr: ast.Accessor
		expr.left = curr_expr
		expr.right = auto_cast right_ptr
		ok = true
		curr_expr^ = expr
		return

	case .LParen:
		// TODO: Handle function calls
		parser_next(p)
		ok = false
		return
	case .LBracket:
		// TODO: Handle indexing
		parser_next(p)
		ok = false
	case .Operator:
		// TODO: Handle operators
		operator := p.current
		parser_next(p)
		right_expr, err := parse_expression(p)
		if err != nil {
			return
		}
		args := make([dynamic]ast.Expression, 0, 2, p.allocator)
		append(&args, curr_expr^)
		append(&args, right_expr)
		curr_expr^ = ast.FunctionCall {
			token = operator,
			args  = args,
			op    = true,
		}
	}

	return
}

@(private)
parse_let_statement :: proc(
	p: ^Parser,
	loc := #caller_location,
) -> (
	stmt: ast.LetStatement,
	err: Parser_Error,
) {
	first_token := p.current
	mutable := false

	parser_next(p)
	if p.current.kind == .Mut {
		mutable = true
		parser_next(p)
	}

	ident := parse_identifier(p) or_return
	if !check_operator(p.current, "=") {
		err = Parser_Error_Struct {
			kind = .Unexpected_Token,
			loc  = loc,
		}
		return
	}
	parser_next(p)
	expr := new(ast.Expression, p.allocator)
	expr^ = parse_expression(p) or_return

	stmt = ast.LetStatement {
		token = first_token,
		bind  = ident,
		value = expr,
	}

	return
}

@(private)
parse_statement :: proc(
	p: ^Parser,
	loc := #caller_location,
) -> (
	stmt: ast.Statement,
	err: Parser_Error,
) {
	if p.current.kind == .Let {
		stmt = parse_let_statement(p) or_return
		return
	}
	stmt = parse_expression(p) or_return
	return
}

parse :: proc(
	tokens: []lexer.Token,
	file_name: string,
	allocator := context.allocator,
	loc := #caller_location,
) -> (
	parser: Parser,
	err: Parser_Error,
) {
	parser = parser_init(tokens, file_name, allocator)
	context.allocator = parser.allocator
	for parser.peek.kind != .EOF {
		node := parse_top_level(&parser) or_return
		append(&parser.module.body, node)
	}
	return
}
