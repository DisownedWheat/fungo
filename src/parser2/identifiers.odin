package parser2

import ast "../ast2"
import "../lexer"

@(private)
parse_identifier :: proc(p: ^Parser) -> (ident: ast.IdentifierType, err: Maybe(Parser_Error)) {
	// TODO: Check type literal after identifier is parsed
	defer {
		if check_colon(p.current) && parser_next(p) {
			parser_next(p)
		}
	}

	#partial switch p.current.kind {
	case .Identifier:
		ident = ast.Identifier {
			token = p.current,
		}
		if !parser_next(p) {
			err = Parser_Error {
				kind = .Unexpected_EOF,
				ctx  = "Identifier :: Identifier",
			}
			return
		}
		return

	case .LParen:
		initial_token := p.current
		p.rollback_position = p.position
		idents := make([dynamic]ast.IdentifierType, p.allocator)
		defer {
			if err != nil {
				delete(idents)
				p.position = p.rollback_position
				parser_next(p)
				ident, err = parse_identifier(p)
			}
		}
		parser_next(p)
		for p.current.kind != .RParen {
			ident, err = parse_identifier(p)
			if err != nil {
				return
			}
			append(&idents, ident)
		}
		ident = ast.TupleDestructure {
			token  = initial_token,
			idents = idents,
		}
		return

	case .LBracket:
		initial_token := p.current
		idents := make([dynamic]ast.IdentifierType, p.allocator)
		defer {
			if err != nil {
				delete(idents)
			}
		}
		parser_next(p)
		for p.current.kind != .RBracket {
			ident, err = parse_identifier(p)
			if err != nil {
				return
			}
			append(&idents, ident)
			if check_comma(p.current) {
				parser_next(p)
			}
		}
		ident = ast.ArrayDestructure {
			token  = initial_token,
			idents = idents,
		}
		return
	case .LBrace:
		initial_token := p.current
		idents := make([dynamic]ast.IdentifierType, p.allocator)
		defer {
			if err != nil {
				delete(idents)
			}
		}
		parser_next(p)
		for p.current.kind != .RBrace {
			if p.current.kind != .Identifier {
				err = Parser_Error {
					kind = .Unexpected_Token,
					ctx  = "Identifier :: LBRACE",
				}
				return
			}
			n := ast.Identifier {
				token = p.current,
			}
			append(&idents, n)
			parser_next(p)

			if p.current.kind == .RBrace {
				continue
			}

			if check_comma(p.current) {
				parser_next(p)
				continue
			}

			err = Parser_Error {
				kind = .Unexpected_Token,
				ctx  = "Identifier :: LBRACE",
			}
			return
		}
		ident = ast.RecordDestructure {
			token  = initial_token,
			idents = idents,
		}
		return
	case:
		err = Parser_Error {
			kind = .Invalid_Identifier,
			ctx  = "Identifier",
		}
	}

	err = Parser_Error {
		kind = .TODO,
		ctx  = "Identifier",
	}
	return
}

@(private)
parse_identifier_expression :: proc(p: ^Parser) -> (ident: ast.IdentifierType, err: Parser_Error) {
	err = Parser_Error {
		kind = .TODO,
		ctx  = "Identifier Expression",
	}
	return
}
