#ifndef PARSER_H
#define PARSER_H

#include "scanner.h"
#include "expression.h"
#include "statement.h"

typedef struct {
    Scanner scan;
    Token peekToken;
    bool hasPeekToken;
    bool hadError;
    bool recoveringError;
} Parser;

Parser newParser(Scanner scan);
Expression* expression(Parser* parser);
Statement* statement(Parser* parser);

#endif

