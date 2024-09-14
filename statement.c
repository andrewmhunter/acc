#include <stdlib.h>
#include <stdio.h>
#include "statement.h"
#include "mem.h"

Statement* stmtNew(StatementType type) {
    Statement* stmt = malloc(sizeof(Statement));
    stmt->type = type;
    return stmt;
}

void stmtFree(Statement* stmt) {
    if (stmt == NULL) {
        return;
    }

    switch (stmt->type) {
        case STATEMENT_EXPRESSION:
            exprFree(stmt->expression);
            break;
        case STATEMENT_IF:
            exprFree(stmt->conditional.condition);
            stmtFree(stmt->conditional.inner);
            stmtFree(stmt->conditional.onElse);
            break;
        case STATEMENT_BLOCK:
            stmtListFree(&stmt->block);
            break;
        case STATEMENT_WHILE:
            exprFree(stmt->whileLoop.condition);
            stmtFree(stmt->whileLoop.inner);
            break;
    }
    free(stmt);
}

static void printIndent(int depth) {
    for (int i = 0; i < depth; ++i) {
        printf("    ");
    }
}

void stmtPrint(Statement* stmt, int depth) {
    printIndent(depth);
    depth += 1;
    switch (stmt->type) {
        case STATEMENT_EXPRESSION:
            exprPrint(stmt->expression);
            printf(";");
            break;
        case STATEMENT_IF:
            printf("if (");
            exprPrint(stmt->conditional.condition);
            printf(")\n");
            stmtPrint(stmt->conditional.inner, depth - 1);
            if (stmt->conditional.onElse != NULL) {
                printIndent(depth - 1);
                printf("else\n");
                stmtPrint(stmt->conditional.onElse, depth - 1);
            }
            break;
        case STATEMENT_WHILE:
            printf("while (");
            exprPrint(stmt->whileLoop.condition);
            printf(")\n");
            stmtPrint(stmt->whileLoop.inner, depth - 1);
            break;
        case STATEMENT_BLOCK:
            printf("{\n");
            for (int i = 0; i < stmt->block.length; ++i) {
                stmtPrint(stmt->block.data[i], depth);
            }
            printIndent(depth - 1);
            printf("}");
            break;
    }
    printf("\n");
}

StatementList stmtListNew() {
    StatementList list = {.data = NULL, .length = 0, .capacity = 0};
    return list;
}

void stmtListFree(StatementList* list) {
    for (int i = 0; i < list->length; ++i) {
        stmtFree(list->data[i]);
    }
    free(list->data);
}

void stmtListAppend(StatementList* list, Statement* stmt) {
    list->data = EXTEND_ARRAY(list->data, Statement*, &list->capacity, list->length, 1);
    list->data[list->length++] = stmt;
}

