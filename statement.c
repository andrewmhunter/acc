#include <stdlib.h>
#include <stdio.h>
#include "statement.h"
#include "mem.h"

Statement* stmtNew(StatementType type, Location location) {
    Statement* stmt = malloc(sizeof(Statement));
    stmt->type = type;
    stmt->location = location;
    return stmt;
}

void stmtFree(Statement* stmt) {
    if (stmt == NULL) {
        return;
    }

    switch (stmt->type) {
        case STATEMENT_RETURN:
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

void stmtPrint(FILE* file, const Statement* stmt, int depth, bool firstLine) {
    printIndent(file, depth);
    depth += 1;
    switch (stmt->type) {
        case STATEMENT_EXPRESSION:
            exprPrint(file, stmt->expression);
            fprintf(file, ";");
            break;
        case STATEMENT_RETURN:
            fprintf(file, "return");
            if (stmt->expression != NULL) {
                fprintf(file, " ");
                exprPrint(file, stmt->expression);
            }
            fprintf(file, ";");
            break;
        case STATEMENT_IF:
            fprintf(file, "if (");
            exprPrint(file, stmt->conditional.condition);
            fprintf(file, ")\n");

            if (firstLine) {
                return;
            }

            stmtPrint(file, stmt->conditional.inner, depth - 1, firstLine);
            if (stmt->conditional.onElse != NULL) {
                printIndent(file, depth - 1);
                fprintf(file, "else\n");
                stmtPrint(file, stmt->conditional.onElse, depth - 1, firstLine);
            }
            break;
        case STATEMENT_WHILE:
            fprintf(file, "while (");
            exprPrint(file, stmt->whileLoop.condition);
            fprintf(file, ")\n");

            if (firstLine) {
                return;
            }

            stmtPrint(file, stmt->whileLoop.inner, depth - 1, firstLine);
            break;
        case STATEMENT_BLOCK:
            fprintf(file, "{\n");

            if (firstLine) {
                return;
            }

            for (size_t i = 0; i < stmt->block.length; ++i) {
                stmtPrint(file, stmt->block.data[i], depth, firstLine);
            }
            printIndent(file, depth - 1);
            fprintf(file, "}");
            break;
        case STATEMENT_VARIABLE:
            typePrint(file, stmt->variableDeclaration.type);
            fprintf(file, " ");
            identPrint(file, &stmt->variableDeclaration.id);
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

Location stmtLoc(const Statement* stmt) {
    return stmt->location;
}


Program programNew() {
    return (Program) {
        .declarations = NULL,
        .delarationsCount = 0,
        .delarationsCapacity = 0,
    };
}

void programPushDeclaration(Program* program, Declaration* declaration) {
    APPEND_ARRAY(
            NULL,
            program->declarations,
            Declaration*,
            program->delarationsCapacity,
            program->delarationsCount,
            declaration
        );
}

Declaration* functionDeclarationNew(
        Arena* arena,
        const Type* returnType,
        Identifier name,
        const Statement* body,
        size_t arity,
        Parameter* parameters,
        Location location
) {
    Declaration* declaration = ARENA_ALLOC(arena, Declaration);
    *declaration = (Declaration) {
        .kind = DECL_FUNCTION,
        .function = (FunctionDeclaration) {
            .returnType = returnType,
            .name = name,
            .location = location,
            .body = body,
            .arity = arity,
            .parameters = parameters,
        }
    };
    return declaration;
}

Declaration* variableDeclarationNew(Arena* arena, const Type* type, Identifier name, Location location) {
    Declaration* declaration = ARENA_ALLOC(arena, Declaration);
    *declaration = (Declaration) {
        .kind = DECL_VARIABLE,
        .variable = (VariableDeclaration) {
            .type = type,
            .name = name,
            .location = location,
        }
    };
    return declaration;
}


