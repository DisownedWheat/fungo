package tests

import "core:log"
import "core:os"
import "core:strings"

load_file :: proc(allocator := context.allocator) -> string {
	value, ok := os.read_entire_file("./test_files/test.fg", allocator = allocator)
	if !ok {
		log.fatal(ok)
	}
	defer delete(value)
	return strings.clone_from(value)
}
