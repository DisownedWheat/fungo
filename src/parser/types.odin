package parser

import "../ast"

parse_type_literal :: proc(p: ^Parser) -> (idx: ast.Stmt_Index, err: ParserError) {

		#partial switch p.current.kind {
		case .Operator:
			if check_pointer(p.current) {

			} else if check_deref(p.current) {} else {
				err = .Unexpected_Token
				return
			}
		case .Identifier:
		case:
			err = .Unexpected_Token
			return
		}

	return
}
