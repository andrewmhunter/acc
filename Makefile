
all:
	gcc -g -o acc main.c scanner.c mem.c parser.c expression.c statement.c type.c identifier.c

