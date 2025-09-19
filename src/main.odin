package main

import "core:flags"
import "core:fmt"
import "core:log"
import "core:mem"
import vmem "core:mem/virtual"
import "core:os"
import "core:strings"
import "core:testing"
import "core:unicode/utf8"
import "lexer"
import parser "parser2"

Command :: enum {
	build,
	check,
}

Args :: struct {
	command: Command `args:"pos=0,required" usage:"Command to run"`,
	file:    os.Handle `args:"pos=1,required,file=r" usage:"Input file to compile."`,
	outDir:  os.Handle `args:"name=out" usage:"Optional directory to output the build artifacts."`,
	check:   bool `args:"name=check" usage:"Run the compiler without generating build artifacts"`,
}

main :: proc() {
	arena: vmem.Arena
	alloc_err := vmem.arena_init_growing(&arena)
	if alloc_err != nil {
		fmt.panicf("Could not create arena allocator: %v", alloc_err)
	}
	allocator := vmem.arena_allocator(&arena)
	context.allocator = allocator
	defer vmem.arena_destroy(&arena)

	context.logger = log.create_console_logger()

	when ODIN_DEBUG {
		fmt.println("DEBUG")
		track: mem.Tracking_Allocator
		mem.tracking_allocator_init(&track, context.allocator)
		context.allocator = mem.tracking_allocator(&track)

		context.logger.lowest_level = log.Level.Debug

		defer {
			if len(track.allocation_map) > 0 {
				fmt.eprintf("=== %v allocations not freed: ===\n", len(track.allocation_map))
				for _, entry in track.allocation_map {
					fmt.eprintf("- %v bytes @ %v\n", entry.size, entry.location)
				}
			}
			if len(track.bad_free_array) > 0 {
				fmt.eprintf("=== %v incorrect frees: ===\n", len(track.bad_free_array))
				for entry in track.bad_free_array {
					fmt.eprintf("- %p @ %v\n", entry.memory, entry.location)
				}
			}
			mem.tracking_allocator_destroy(&track)
		}
	}


	args: Args
	style: flags.Parsing_Style = .Unix

	flags.parse_or_exit(&args, os.args, style)

	input, ok := os.read_entire_file_from_handle(args.file)
	if !ok {
		fmt.println("Unable to read input file")
		os.exit(1)
	}
	l, lexer_err := lexer.lex(string(input))
	if lexer_err != nil {
		fmt.eprintfln("Error lexing input: %v", lexer_err)
		os.exit(1)
	}
	fmt.println(l.input)

	p, parser_error := parser.parse(l.tokens[:], l.file_name)
	if parser_error != nil {
		fmt.eprintfln("Error parsing input: %v", parser_error)
	}
	fmt.println(p)
}


@(test)
test_compiler :: proc(t: ^testing.T) {

	input := `
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
	fmt.printfln("LEXER SRC: %s", l.input)
	if lexer_err != nil {
		testing.fail(t)
	}

	// p, parser_error := parser.parse(l)
	// if parser_error != nil {
	// 	testing.fail(t)
	// }
	// fmt.println(p)
}
