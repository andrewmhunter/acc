
DIR := build

TARGET := acc

SRCS := main.c scanner.c mem.c parser.c expression.c statement.c type.c \
		identifier.c compiler.c hash.c instruction.c diag.c condition.c \
		util.c compile_statement.c compile_expression.c compile_condition.c

OBJS := $(SRCS:%.c=$(DIR)/%.o)

CFLAGS := -Wno-unused-function -g

# This should be broken into multiple rules but for now it compiles fast enough
# that it doesn't matter.

all:
	$(CC) $(CFLAGS) -o $(TARGET) $(SRCS)

