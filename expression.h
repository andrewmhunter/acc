#ifndef EXPRESSION_H
#define EXPRESSION_H

#include "type.h"
#include "identifier.h"
#include "mem.h"

typedef int Label;

typedef enum {
    // Zero is implicitly an invalid expression
    EXPR_BINARY = 1,
    EXPR_UNARY,
    EXPR_LITERAL,
    EXPR_CAST,
    EXPR_VARIABLE,
    EXPR_ASSIGN,
    EXPR_LABEL,
    EXPR_STACKOFFSET,
} ExpressionType;

typedef enum {
    BINARY_ADD,
    BINARY_SUBTRACT,
    BINARY_MULTIPLY,
    BINARY_DIVIDE,
    BINARY_NONE,
    BINARY_SHIFT_RIGHT,
    BINARY_SHIFT_LEFT,
    BINARY_LOGICAL_OR,
    BINARY_LOGICAL_AND,
    BINARY_EQUAL,
    BINARY_NOT_EQUAL,
    BINARY_LESS,
    BINARY_LESS_EQUAL,
    BINARY_GREATER,
    BINARY_GREATER_EQUAL,
} BinaryOperation;

typedef enum {
    UNARY_NEGATE,
    UNARY_NOT,
    UNARY_ADDRESSOF,
    UNARY_DEREFERENCE,
} UnaryOperation;

typedef struct Expression {
    ExpressionType type;
    union {
        struct {
            BinaryOperation operation;
            const struct Expression* left;
            const struct Expression* right;
        } binary;
        struct {
            const struct Expression* inner;
            UnaryOperation operation;
        } unary;
        struct {
            const Type* type;
            const struct Expression* inner;
        } cast;
        Identifier variable;
        int literal;
        int stackOffset;
        Label label;
    };
} Expression;

const Expression* exprBinary(Arena* arena, BinaryOperation operation, const Expression* left, const Expression* right);
const Expression* exprAssign(Arena* arena, BinaryOperation operation, const Expression* left, const Expression* right);
const Expression* exprUnary(Arena* arena, UnaryOperation operation, const Expression* inner);
const Expression* exprLiteral(Arena* arena, int value);
const Expression* exprCast(Arena* arena, const Type* type, const Expression* inner);
const Expression* exprVariable(Arena* arena, Identifier name);
const Expression* exprLabel(Arena* arena, Label value);
const Expression* exprStackOffset(Arena* arena, Label value);

void exprPrint(FILE* file, const Expression* expr);

bool isLiteral(const Expression* expr);
bool isStackOffset(const Expression* expr);
bool exprEquals(const Expression* expr0, const Expression* expr1);

#endif

