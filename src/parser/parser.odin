package parser

import "../ast"
import "../lexer"
import "core:fmt"
import "core:mem"

ParserError :: enum {}

ParserState :: enum {
	Module,
	FunctionArgs,
	Type,
	Default,
}

Parser :: struct {
	tokens:         lexer.TokenList,
	lexer_position: int,
	atoms:          [dynamic]ast.Atom,
	state:          ParserState,
	node_stack:     [dynamic]ast.Node,
	next:           proc(p: ^Parser) -> bool,
}

@(private)
parser_init :: proc(
	tokens: lexer.TokenList,
	allocator: mem.Allocator = context.allocator,
) -> Parser {
	atoms := make([dynamic]ast.Atom)
	node_stack := make([dynamic]ast.Node)
	return Parser {
		tokens = tokens,
		lexer_position = -1,
		atoms = atoms,
		state = .Module,
		node_stack = node_stack,
		next = parser_next,
	}
}

@(private)
parser_next :: proc(p: ^Parser) -> bool {
	p.lexer_position += 1
	return p.lexer_position < len(p.tokens)
}

@(private)
parse_top_level :: proc(p: ^Parser) {

}

parse :: proc(lexer: lexer.Lexer) -> (Parser, ParserError) {
	parser := parser_init(lexer.tokens[:])
	for parser->next() {
		token := parser.tokens[parser.lexer_position]
		fmt.printfln("%s :: %s", token.kind, lexer.input[token.span[0]:token.span[1]])
	}
	return parser, nil
}
