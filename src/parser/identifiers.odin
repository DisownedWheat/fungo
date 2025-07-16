package parser

import "../ast"
import "../lexer"

@(private)
parse_identifier :: proc(p: ^Parser) -> (node: ast.Ident_Index, err: ParserError) {
	#partial switch p.current.kind {
	case .Identifier:
		identifier := ast.Identifier {
			token = p.current,
		}
		if !parser_next(p) {
			err = .Unexpected_EOF
			return
		}
		if check_colon(p.current) && parser_next(p) {

		}
		node = parser_add(p, identifier)
		return

	case .LParen:
		initial_token := p.current
		p.rollback_position = p.position
		idents := make([dynamic]ast.Ident_Index, p.allocator)
		defer {
			if err != nil {
				delete(idents)
				p.position = p.rollback_position
				parser_next(p)
				node, err = parse_identifier(p)
			}
		}
		parser_next(p)
		for p.current.kind != .RParen {
			node, err = parse_identifier(p)
			if err != nil {
				return
			}
			append(&idents, node)
		}
		node_literal := ast.TupleDestructure {
			token  = initial_token,
			idents = idents,
		}
		node = parser_add(p, node_literal)
		return

	case .LBracket:
		initial_token := p.current
		idents := make([dynamic]ast.Ident_Index, p.allocator)
		defer {
			if err != nil {
				delete(idents)
			}
		}
		parser_next(p)
		for p.current.kind != .RBracket {
			node, err = parse_identifier(p)
			if err != nil {
				return
			}
			append(&idents, node)
			if check_comma(p.current) {
				parser_next(p)
			}
		}
		node_literal := ast.ArrayDestructure {
			token  = initial_token,
			idents = idents,
		}
		node = parser_add(p, node_literal)
		return
	case .LBrace:
		initial_token := p.current
		idents := make([dynamic]ast.Ident_Index, p.allocator)
		defer {
			if err != nil {
				delete(idents)
			}
		}
		parser_next(p)
		for p.current.kind != .RBrace {
			if p.current.kind != .Identifier {
				err = .Unexpected_Token
				return
			}
			n := ast.Identifier {
				token = p.current,
			}
			ni := parser_add(p, n)
			append(&idents, ni)
			parser_next(p)

			if p.current.kind == .RBrace {
				continue
			}

			if check_comma(p.current) {
				parser_next(p)
				continue
			}

			err = .Unexpected_Token
			return
		}
	case:
		err = .Invalid_Identifier
	}
	return
}
