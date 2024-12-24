#ifndef PARSER_H
#define PARSER_H

#include "scanner.h"
#include "expression.h"
#include "statement.h"
#include "mem.h"
#include "diag.h"

typedef struct {
    Arena* staticLifetime;
    Diagnostics* diag;
    Scanner scan;
    Token peekToken;
    bool hasPeekToken;
    bool recoveringError;
} Parser;

Parser newParser(Arena* staticLifetime, Diagnostics* diag, Scanner scan);
const Expression* expression(Parser* parser);
Statement* statement(Parser* parser);

Program parseProgram(Parser* parser);

#endif

