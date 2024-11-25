#ifndef EXPRESSION_H
#define EXPRESSION_H

#include "type.h"
#include "identifier.h"
#include "mem.h"

typedef enum {
    EXPR_BINARY,
    EXPR_UNARY,
    EXPR_LITERAL,
    EXPR_CAST,
    EXPR_VARIABLE,
    EXPR_ASSIGN,
} ExpressionType;

typedef enum {
    BINARY_ADD = '+',
    BINARY_SUBTRACT = '-',
    BINARY_MULTIPLY = '*',
    BINARY_DIVIDE = '/',
    BINARY_NONE = '\\',
} BinaryOperation;

typedef enum {
    UNARY_NEGATE = '-',
} UnaryOperation;

typedef struct Expression {
    ExpressionType type;
    union {
        struct {
            BinaryOperation operation;
            struct Expression* left;
            struct Expression* right;
        } binary;
        struct {
            UnaryOperation operation;
            struct Expression* inner;
        } unary;
        struct {
            Type* type;
            struct Expression* inner;
        } cast;
        Identifier variable;
        int literal;
    };
} Expression;

Expression* exprBinary(Arena* arena, BinaryOperation operation, Expression* left, Expression* right);
Expression* exprAssign(Arena* arena, BinaryOperation operation, Expression* left, Expression* right);
Expression* exprUnary(Arena* arena, UnaryOperation operation, Expression* inner);
Expression* exprLiteral(Arena* arena, int value);
Expression* exprCast(Arena* arena, Type* type, Expression* inner);
Expression* exprVariable(Arena* arena, Identifier name);

void exprPrint(FILE* file, Expression* expr);

bool exprEquals(Expression* expr0, Expression* expr1);

#endif

