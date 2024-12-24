#ifndef STATEMENT_H
#define STATEMENT_H

#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include "expression.h"
#include "type.h"
#include "diag.h"

typedef enum {
    STATEMENT_EXPRESSION,
    STATEMENT_IF,
    STATEMENT_WHILE,
    STATEMENT_BLOCK,
    STATEMENT_VARIABLE,
    STATEMENT_RETURN,
} StatementType;

struct Statement;

typedef struct {
    struct Statement** data;
    size_t length;
    size_t capacity;
} StatementList;

typedef struct Statement {
    StatementType type;
    Location location;
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

typedef struct {
    const Type* type;
    Identifier name;
} Parameter;

typedef struct {
    const Type* returnType;
    Identifier name;
    Location location;
    const Statement* body;
    size_t arity;
    Parameter* parameters;
} FunctionDeclaration;

typedef struct {
    const Type* type;
    Identifier name;
    Location location;
} VariableDeclaration;

typedef enum {
    DECL_FUNCTION,
    DECL_VARIABLE,
} DeclarationKind;

typedef struct Declaration {
    DeclarationKind kind;
    union {
        struct {
            const Type* type;
            Identifier name;
            Location location;
        };
        FunctionDeclaration function;
        VariableDeclaration variable;
    };
} Declaration;

typedef struct {
    Declaration** declarations;
    size_t delarationsCount;
    size_t delarationsCapacity;
} Program;


Statement* stmtNew(StatementType type, Location location);
void stmtFree(Statement* stmt);
void stmtPrint(FILE* file, const Statement* stmt, int depth, bool firstLine);

StatementList stmtListNew();
void stmtListFree(StatementList* list);
void stmtListAppend(StatementList* list, Statement* stmt);

Location stmtLoc(const Statement* stmt);

Program programNew();
void programPushDeclaration(Program* program, Declaration* declaration);

Declaration* functionDeclarationNew(
        Arena* arena,
        const Type* returnType,
        Identifier name,
        const Statement* body,
        size_t arity,
        Parameter* parameters,
        Location location
    );

Declaration* variableDeclarationNew(Arena* arena, const Type* type, Identifier name, Location location);

#endif

