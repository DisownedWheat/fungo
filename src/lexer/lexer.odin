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
	Fn,
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
	EOF,
	ModuleFileName,
}

LexerState :: enum {
	Default,
	String,
	Char,
	Operator,
	Int,
	Float,
	Comment,
}

LexerErr :: enum {
	UnexpectedChar,
}

Token :: struct {
	kind:  TokenKind,
	span:  [2]int,
	lexer: ^Lexer,
}

OPERATORS := [?]rune {
	'+',
	'-',
	'=',
	'!',
	'@',
	'%',
	'*',
	'/',
	'^',
	'|',
	'<',
	'>',
	'?',
	':',
	';',
	'.',
	',',
}

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
	"import"    = .Import,
	"fn"        = .Fn,
}

Lexer :: struct {
	input:        string,
	file_name:    string,
	position:     int,
	line:         int,
	column:       int,
	span_start:   int,
	state:        LexerState,
	change_state: proc(lexer: ^Lexer, state: LexerState),
	tokens:       [dynamic]Token,
}

eof_token :: proc(allocator := context.allocator) -> ^Token {
	token := new(Token, allocator)
	token^ = {
		kind = .EOF,
		span = {0, 0},
	}
	return token
}

print_tokens_lexer :: proc(l: Lexer) {
	for token in l.tokens {
		log.infof("%s :: '%s'", token.kind, l.input[token.span[0]:token.span[1]])
	}
}

print_tokens_tokens :: proc(ts: []Token) {
	for token in ts {
		log.infof("%s :: '%s'", token.kind, token.lexer^.input[token.span[0]:token.span[1]])
	}
}

print_tokens :: proc {
	print_tokens_lexer,
	print_tokens_tokens,
}

@(private)
lexer_skip_char :: proc(lexer: ^Lexer, char: rune) {
	lexer.span_start += utf8.rune_size(char)
}

@(private)
move_info_position :: proc(l: ^Lexer, char: rune) {
	switch char {
	case '\r', '\n':
		l.line += 1
	case:
		l.column += 1
	}
}

@(private)
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

@(private)
new_lexer :: proc(file_name: string, input: string, allocator := context.allocator) -> ^Lexer {
	lexer := new(Lexer, allocator)
	lexer^ = {
		change_state = change_state,
		state        = .Default,
		input        = input,
		file_name    = file_name,
		line         = 1,
		column       = 1,
	}
	return lexer
}

@(private)
add_simple_token :: proc(lexer: ^Lexer, kind: TokenKind, char: rune) {
	parse_buffer(lexer)
	size := utf8.rune_size(char)
	token := Token {
		kind  = kind,
		span  = {lexer.span_start + size, lexer.position + size},
		lexer = lexer,
	}
	append(&lexer.tokens, token)
}

@(private)
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
			kind  = .CharLiteral,
			span  = {lexer.span_start, lexer.position},
			lexer = lexer,
		}
	case .String:
		token = Token {
			kind  = .StringLiteral,
			span  = {lexer.span_start, lexer.position},
			lexer = lexer,
		}
	case .Operator:
		token = Token {
			kind  = .Operator,
			span  = {lexer.span_start, lexer.position},
			lexer = lexer,
		}
	case .Int:
		token = Token {
			kind  = .IntLiteral,
			span  = {lexer.span_start, lexer.position},
			lexer = lexer,
		}
	case .Float:
		token = Token {
			kind  = .FloatLiteral,
			span  = {lexer.span_start, lexer.position},
			lexer = lexer,
		}
	case .Default:
		str := string(lexer.input[lexer.span_start:lexer.position])
		keyword, ok := &Keywords[str]
		if ok {
			token = {
				kind  = keyword^,
				span  = {lexer.span_start, lexer.position},
				lexer = lexer,
			}
		} else {
			token = {
				kind  = .Identifier,
				span  = {lexer.span_start, lexer.position},
				lexer = lexer,
			}
		}
	case .Comment:
	}
	append(&lexer.tokens, token)
}

@(private)
default_lexing_case :: proc(lexer: ^Lexer, char: rune) {
	switch char {
	// case '.':
	// 	add_simple_token(lexer, .Dot, char)
	// 	lexer.span_start += 1
	// case ',':
	// 	add_simple_token(lexer, .Comma, char)
	// 	lexer.span_start += 1
	case '(':
		add_simple_token(lexer, .LParen, char)
		lexer.span_start += 1
	case ')':
		add_simple_token(lexer, .RParen, char)
		lexer.span_start += 1
	case '[':
		add_simple_token(lexer, .LBracket, char)
		lexer.span_start += 1
	case ']':
		add_simple_token(lexer, .RBracket, char)
		lexer.span_start += 1
	case '{':
		add_simple_token(lexer, .LBrace, char)
		lexer.span_start += 1
	case '}':
		add_simple_token(lexer, .RBrace, char)
		lexer.span_start += 1
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

lex :: proc(
	input: string,
	file_name: string = "",
	allocator := context.allocator,
) -> (
	^Lexer,
	LexerErr,
) {
	lexer := new_lexer(file_name, input)

	for char in lexer.input {
		defer lexer.position += utf8.rune_size(char)

		switch lexer.state {
		case .Default:
			switch char {
			case ' ', '\t', '\n', '\r':
				parse_buffer(lexer)
				lexer.span_start += utf8.rune_size(char)
			case '\'':
				parse_buffer(lexer)
				lexer->change_state(.Char)
			case '"':
				parse_buffer(lexer)
				lexer->change_state(.String)
			case:
				default_lexing_case(lexer, char)
			}
		case .Char:
			switch char {
			case '\'':
				lexer.span_start += 1
				parse_buffer(lexer)
				log.warn(char)
				lexer->change_state(.Default)
			case:
			}
		case .String:
			switch char {
			case '\"':
				lexer.span_start += 1
				parse_buffer(lexer)
				lexer->change_state(.Default)
				lexer_skip_char(lexer, char)
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
			parse_buffer(lexer)
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
				parse_buffer(lexer)
				lexer->change_state(.Char)
			case '"':
				parse_buffer(lexer)
				lexer->change_state(.String)
			case '\t', ' ', '\n', '\r':
				parse_buffer(lexer)
				lexer.span_start = lexer.position + 1
				lexer->change_state(.Default)
			case:
				parse_buffer(lexer)
				lexer->change_state(.Default)
			}
		case .Float:
			switch char {
			case '.':
				return lexer, .UnexpectedChar
			case '0' ..= '9', '_':
			case '\'':
				parse_buffer(lexer)
				lexer->change_state(.Char)
			case '"':
				parse_buffer(lexer)
				lexer->change_state(.String)
			case '\t', ' ', '\n', '\r':
				parse_buffer(lexer)
				lexer.span_start = lexer.position + 1
				lexer->change_state(.Default)
			case:
				parse_buffer(lexer)
				lexer->change_state(.Default)
			}
		case .Comment:
			switch char {
			case '\t', ' ', '\n', '\r':
				lexer.span_start = lexer.position + 1
				lexer->change_state(.Default)
			}
		}
	}

	parse_buffer(lexer)

	return lexer, nil
}
