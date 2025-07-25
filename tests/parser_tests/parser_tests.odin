package parser_tests

import test "../"
import "../../src/lexer"
import parser "../../src/parser2"
import "core:encoding/json"
import "core:log"
import "core:testing"
import "core:unicode"
import "core:unicode/utf8"

@(test)
test_parser :: proc(t: ^testing.T) {
	defer free_all(context.allocator)
	context.logger.lowest_level = log.Level.Debug
	input := test.load_file()
	l, lexer_err := lexer.lex(input, allocator = context.allocator)
	if lexer_err != nil {
		testing.fail(t)
	}
	p, parser_err := parser.parse(l, context.allocator)
	if parser_err != nil {
		log.info("PARSER ERROR")
		lexer.print_tokens([]lexer.Token{p.current^, p.peek^})
		log.info("NODES")
		log.error(parser_err)
		testing.fail(t)
	}
}

@(test)
test_operator_checks :: proc(t: ^testing.T) {
	defer free_all(context.allocator)
	context.logger.lowest_level = log.Level.Debug
	input := "====="
	l, lexer_err := lexer.lex(input, allocator = context.allocator)
	if lexer_err != nil {
		testing.fail(t)
	}
	result := parser.check_operator(&l.tokens[0], "=====")
	if !result {
		testing.fail(t)
	}
}
