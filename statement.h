#ifndef STATEMENT_H
#define STATEMENT_H

#include <stdlib.h>
#include <stdbool.h>
#include "expression.h"
#include "type.h"

typedef enum {
    STATEMENT_EXPRESSION,
    STATEMENT_IF,
    STATEMENT_WHILE,
    STATEMENT_BLOCK,
} StatementType;

struct Statement;

typedef struct {
    struct Statement** data;
    size_t length;
    size_t capacity;
} StatementList;

typedef struct Statement {
    StatementType type;
    union {
        Expression* expression;
        struct {
            Expression* condition;
            struct Statement* inner;
            struct Statement* onElse;
        } conditional;
        struct {
            Expression* condition;
            struct Statement* inner;
            bool isDoWhile;
        } whileLoop;
        StatementList block;
    };
} Statement;


Statement* stmtNew(StatementType type);
void stmtFree(Statement* stmt);
void stmtPrint(Statement* stmt, int depth);

StatementList stmtListNew();
void stmtListFree(StatementList* list);
void stmtListAppend(StatementList* list, Statement* stmt);

#endif

