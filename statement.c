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
            break;
        case STATEMENT_IF:
            stmtFree(stmt->conditional.inner);
            stmtFree(stmt->conditional.onElse);
            break;
        case STATEMENT_BLOCK:
            stmtListFree(&stmt->block);
            break;
        case STATEMENT_WHILE:
            stmtFree(stmt->whileLoop.inner);
            break;
        case STATEMENT_VARIABLE:
            break;
    }
    free(stmt);
}

static void printIndent(FILE* file, int depth) {
    for (int i = 0; i < depth; ++i) {
        printf("    ");
    }
}

void stmtPrint(FILE* file, Statement* stmt, int depth) {
    printIndent(file, depth);
    depth += 1;
    switch (stmt->type) {
        case STATEMENT_EXPRESSION:
            exprPrint(file, stmt->expression);
            fprintf(file, ";");
            break;
        case STATEMENT_IF:
            fprintf(file, "if (");
            exprPrint(file, stmt->conditional.condition);
            fprintf(file, ")\n");
            stmtPrint(file, stmt->conditional.inner, depth - 1);
            if (stmt->conditional.onElse != NULL) {
                printIndent(file, depth - 1);
                fprintf(file, "else\n");
                stmtPrint(file, stmt->conditional.onElse, depth - 1);
            }
            break;
        case STATEMENT_WHILE:
            fprintf(file, "while (");
            exprPrint(file, stmt->whileLoop.condition);
            fprintf(file, ")\n");
            stmtPrint(file, stmt->whileLoop.inner, depth - 1);
            break;
        case STATEMENT_BLOCK:
            fprintf(file, "{\n");
            for (size_t i = 0; i < stmt->block.length; ++i) {
                stmtPrint(file, stmt->block.data[i], depth);
            }
            printIndent(file, depth - 1);
            fprintf(file, "}");
            break;
        case STATEMENT_VARIABLE:
            typePrint(file, stmt->variableDeclaration.type);
            fprintf(file, " ");
            identPrint(file, stmt->variableDeclaration.id);
            if (stmt->variableDeclaration.expr != NULL) {
                fprintf(file, " = ");
                exprPrint(file, stmt->variableDeclaration.expr);
            }
            fprintf(file, ";");
            break;
    }
    fprintf(file, "\n");
}

StatementList stmtListNew() {
    StatementList list = {.data = NULL, .length = 0, .capacity = 0};
    return list;
}

void stmtListFree(StatementList* list) {
    for (size_t i = 0; i < list->length; ++i) {
        stmtFree(list->data[i]);
    }
    free(list->data);
}

void stmtListAppend(StatementList* list, Statement* stmt) {
    APPEND_ARRAY(
            NULL,
            list->data,
            Statement*,
            list->capacity,
            list->length,
            stmt
        );
}

