package lexer_tests

import "../../src/lexer"
import "core:log"
import "core:testing"
import "core:unicode"
import "core:unicode/utf8"

@(test)
test_lexer :: proc(t: ^testing.T) {
	context.logger.lowest_level = log.Level.Debug

	input := `
		import "test"
		let x = 1
		let main (arg1: string) = {
			if arg1 == "Hello World" {
				arg1.append('!')
			}
			arg1 + x
		}

		module Testing = (innerModule: A) {
			type t = A.t
		}
	`


	l, lexer_err := lexer.lex(input)
	log.infof("LEXER SRC:\n %s", l.input)
	if lexer_err != nil {
		testing.fail(t)
	}

	lexer.print_tokens(l^)
}
