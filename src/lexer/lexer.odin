#+feature dynamic-literals
package lexer

import "core:fmt"
import "core:log"
import "core:mem"
import "core:os"
import "core:strings"
import "core:unicode"
import "core:unicode/utf8"

TokenIndex :: int

TokenList :: []Token

ColumnUpdate :: distinct int
RowUpdate :: distinct int

PositionUpdate :: union {
	ColumnUpdate,
	RowUpdate,
}

get_token :: proc(list: TokenList, index: TokenIndex) -> (Token, bool) {
	token: Token
	if index >= len(list) {
		return token, false
	}
	return list[index], true
}

TokenKind :: enum {
	Identifier,
	Operator,
	IntLiteral,
	FloatLiteral,
	StringLiteral,
	CharLiteral,
	Comment,
	LParen,
	RParen,
	LBracket,
	RBracket,
	LBrace,
	RBrace,
	Let,
	Mut,
	Go,
	If,
	Else,
	For,
	True,
	False,
	Import,
	Module,
	Type,
	With,
	When,
	Interface,
	Rec,
	Match,
	Dot,
	Comma,
	Assign,
}

LexerState :: enum {
	Default,
	String,
	Char,
	Operator,
	Int,
	Float,
}

LexerErr :: enum {
	UnexpectedChar,
}

Token :: struct {
	kind: TokenKind,
	span: [2]int,
}

OPERATORS := [?]rune{'+', '-', '=', '!', '@', '%', '*', '/', '^', '|', '<', '>', '?', ':', ';'}

Keywords: map[string]TokenKind = map[string]TokenKind {
	"let"       = .Let,
	"mut"       = .Mut,
	"go"        = .Go,
	"if"        = .If,
	"else"      = .Else,
	"for"       = .For,
	"true"      = .True,
	"false"     = .False,
	"module"    = .Module,
	"type"      = .Type,
	"with"      = .With,
	"when"      = .When,
	"interface" = .Interface,
	"rec"       = .Rec,
	"match"     = .Match,
}

Lexer :: struct {
	input:        string,
	position:     int,
	line:         int,
	column:       int,
	span_start:   int,
	state:        LexerState,
	change_state: proc(lexer: ^Lexer, state: LexerState),
	tokens:       [dynamic]Token,
}

print_tokens :: proc(l: Lexer) {
	for token in l.tokens {
		fmt.printfln("%s :: %s", token.kind, l.input[token.span[0]:token.span[1]])
	}
}

log_tokens :: proc(l: Lexer) {
	for token in l.tokens {
		log.infof("%s :: '%s'", token.kind, l.input[token.span[0]:token.span[1]])
	}
}

move_info_position :: proc(l: ^Lexer, char: rune) {
	switch char {
	case '\r', '\n':
		l.line += 1
	case:
		l.column += 1
	}
}

change_state :: proc(lexer: ^Lexer, state: LexerState) {

	// If we're switching to the default state and the first char of the buffer is an int
	// then we'll override the state change to be the Int state kind
	if state == .Default {
		if lexer.position - lexer.span_start == 1 {
			char := utf8.rune_at(lexer.input, lexer.span_start)
			switch char {
			case '0' ..= '9':
				lexer.state = .Int
				return
			case:
				for op in OPERATORS {
					if char == op {
						lexer.state = .Operator
						return
					}
				}
			}
		}
		lexer.state = state
	} else {
		lexer.state = state
	}
}

new_lexer :: proc(input: string) -> Lexer {
	lexer := Lexer {
		change_state = change_state,
		state        = .Default,
		input        = input,
		line         = 1,
		column       = 1,
	}
	return lexer
}

add_simple_token :: proc(lexer: ^Lexer, kind: TokenKind, char: rune) {
	log.info(lexer.span_start, lexer.position, lexer.input[lexer.position:lexer.position])
	parse_buffer(lexer)
	size := utf8.rune_size(char)
	log.info(
		lexer.position,
		lexer.position + size,
		lexer.input[lexer.position:lexer.position + size],
	)
	token := Token {
		kind = kind,
		span = {lexer.span_start + size, lexer.position + size},
	}
	append(&lexer.tokens, token)
}

parse_buffer :: proc(lexer: ^Lexer) {
	defer {
		count := 0
		for c in lexer.input[lexer.span_start:lexer.position] {
			count += utf8.rune_size(c)
		}
		lexer.span_start += count
	}

	is_all_whitespace := true
	for char in lexer.input[lexer.span_start:lexer.position] {
		if !unicode.is_white_space(char) {
			is_all_whitespace = false
			break
		}
	}

	if is_all_whitespace {
		return
	}

	token: Token
	switch lexer.state {
	case .Char:
		token = Token {
			kind = .CharLiteral,
			span = {lexer.span_start, lexer.position},
		}
	case .String:
		token = Token {
			kind = .StringLiteral,
			span = {lexer.span_start, lexer.position},
		}
	case .Operator:
		token = Token {
			kind = .Operator,
			span = {lexer.span_start, lexer.position},
		}
	case .Int:
		token = Token {
			kind = .IntLiteral,
			span = {lexer.span_start, lexer.position},
		}
	case .Float:
		token = Token {
			kind = .FloatLiteral,
			span = {lexer.span_start, lexer.position},
		}
	case .Default:
		str := string(lexer.input[lexer.span_start:lexer.position])
		keyword, ok := &Keywords[str]
		if ok {
			token = {
				kind = keyword^,
				span = {lexer.span_start, lexer.position},
			}
		} else {
			token = {
				kind = .Identifier,
				span = {lexer.span_start, lexer.position},
			}
		}
	}
	append(&lexer.tokens, token)
}

default_lexing_case :: proc(lexer: ^Lexer, char: rune) {
	switch char {
	case '.':
		add_simple_token(lexer, .Dot, char)
	case ',':
		add_simple_token(lexer, .Comma, char)
	case '(':
		add_simple_token(lexer, .LParen, char)
	case ')':
		add_simple_token(lexer, .RParen, char)
	case '[':
		add_simple_token(lexer, .LBracket, char)
	case ']':
		add_simple_token(lexer, .RBracket, char)
	case '{':
		add_simple_token(lexer, .LBrace, char)
	case '}':
		add_simple_token(lexer, .RBrace, char)
	case '0' ..= '9':
		if lexer.position - lexer.span_start == 0 {
			lexer->change_state(.Int)
		}
	case:
		for op in OPERATORS {
			if op == char {
				parse_buffer(lexer)
				lexer->change_state(.Operator)
				return
			}
		}
	}
}


lex :: proc(input: string) -> (Lexer, LexerErr) {
	lexer := new_lexer(input)

	for char in lexer.input {
		defer lexer.position += utf8.rune_size(char)

		switch lexer.state {
		case .Default:
			switch char {
			case ' ', '\t', '\n', '\r':
				parse_buffer(&lexer)
				lexer.span_start += utf8.rune_size(char)
			case '\'':
				parse_buffer(&lexer)
				lexer->change_state(.Char)
			case '"':
				parse_buffer(&lexer)
				lexer->change_state(.String)
			case:
				default_lexing_case(&lexer, char)
			}
		case .Char:
			switch char {
			case '\'':
				parse_buffer(&lexer)
				lexer->change_state(.Default)
			case:
			}
		case .String:
			switch char {
			case '\"':
				parse_buffer(&lexer)
				lexer->change_state(.Default)
			case:
			}
		case .Operator:
			op_flag := false
			for op in OPERATORS {
				if op == char {
					op_flag = true
					break
				}
			}
			if op_flag {
				continue
			}
			parse_buffer(&lexer)
			switch char {
			case '\'':
				lexer->change_state(.Char)
			case '"':
				lexer->change_state(.String)
			case '\t', ' ', '\n', '\r':
				lexer.span_start = lexer.position + 1
				lexer->change_state(.Default)
			case:
				lexer->change_state(.Default)
			}
		case .Int:
			switch char {
			case '.':
				lexer->change_state(.Float)
			case '0' ..= '9', '_':
			case '\'':
				parse_buffer(&lexer)
				lexer->change_state(.Char)
			case '"':
				parse_buffer(&lexer)
				lexer->change_state(.String)
			case '\t', ' ', '\n', '\r':
				parse_buffer(&lexer)
				lexer.span_start = lexer.position + 1
				lexer->change_state(.Default)
			case:
				parse_buffer(&lexer)
				lexer->change_state(.Default)
			}
		case .Float:
			switch char {
			case '.':
				return lexer, .UnexpectedChar
			case '0' ..= '9', '_':
			case '\'':
				parse_buffer(&lexer)
				lexer->change_state(.Char)
			case '"':
				parse_buffer(&lexer)
				lexer->change_state(.String)
			case '\t', ' ', '\n', '\r':
				parse_buffer(&lexer)
				lexer.span_start = lexer.position + 1
				lexer->change_state(.Default)
			case:
				parse_buffer(&lexer)
				lexer->change_state(.Default)
			}
		}
	}

	parse_buffer(&lexer)

	return lexer, nil
}
