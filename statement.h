#ifndef STATEMENT_H
#define STATEMENT_H

#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include "expression.h"
#include "type.h"

typedef enum {
    STATEMENT_EXPRESSION,
    STATEMENT_IF,
    STATEMENT_WHILE,
    STATEMENT_BLOCK,
    STATEMENT_VARIABLE,
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
        const Expression* expression;
        struct {
            const Expression* condition;
            struct Statement* inner;
            struct Statement* onElse;
        } conditional;
        struct {
            const Expression* condition;
            struct Statement* inner;
            bool isDoWhile;
        } whileLoop;
        struct {
            const Type* type;
            Identifier id;
            const Expression* expr;
        } variableDeclaration;
        StatementList block;
    };
} Statement;


Statement* stmtNew(StatementType type);
void stmtFree(Statement* stmt);
void stmtPrint(FILE* file, Statement* stmt, int depth, bool firstLine);

StatementList stmtListNew();
void stmtListFree(StatementList* list);
void stmtListAppend(StatementList* list, Statement* stmt);

#endif

