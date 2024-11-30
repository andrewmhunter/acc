#ifndef PARSER_H
#define PARSER_H

#include "scanner.h"
#include "expression.h"
#include "statement.h"
#include "mem.h"

typedef struct {
    Scanner scan;
    Token peekToken;
    bool hasPeekToken;
    bool hadError;
    bool recoveringError;
    Arena* staticLifetime;
    //Arena* compileLifetime;
} Parser;

Parser newParser(Arena* staticLifetime, Scanner scan);
const Expression* expression(Parser* parser);
Statement* statement(Parser* parser);

#endif

