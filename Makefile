
DIR := build
TARGET := acc

SRCS := main.c scanner.c mem.c parser.c expression.c statement.c type.c \
	identifier.c compiler.c hash.c instruction.c diag.c condition.c \
	util.c compile_statement.c compile_expression.c compile_condition.c \
	optimizer.c match.c rule.c

OBJS := $(SRCS:%.c=$(DIR)/%.o)
DEPS := $(OBJS:.o=.d)

CFLAGS := -g -c
CPPFLAGS := -MD -MP
LDFLAGS :=

$(TARGET): $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $(OBJS)

$(DIR)/%.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $@ $<

.PHONY: clean
clean:
	rm $(TARGET)
	rm -r $(DIR)

.PHONY: install
install:
	cp $(TARGET) $(HOME)/.local/bin/$(TARGET)
	cp acc-pipeline $(HOME)/.local/bin/acc-pipeline

-include $(DEPS)
