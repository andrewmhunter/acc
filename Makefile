
# This should be broken into multiple rules but for now it compiles fast enough
# that it doesn't matter.

all:
	gcc -Wno-unused-function -g -o acc main.c scanner.c mem.c parser.c expression.c statement.c type.c identifier.c compiler.c hash.c instruction.c diag.c

