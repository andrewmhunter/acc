#ifndef EXPRESSION_H
#define EXPRESSION_H

#include "type.h"

typedef enum {
    EXPR_BINARY,
    EXPR_UNARY,
    EXPR_LITERAL,
    EXPR_CAST,
} ExpressionType;

typedef enum {
    BINARY_ADD = '+',
    BINARY_SUBTRACT = '-',
    BINARY_MULTIPLY = '*',
    BINARY_DIVIDE = '/',
    BINARY_ASSIGN = '=',
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
        int literal;
    };
} Expression;

Expression* exprBinary(BinaryOperation operation, Expression* left, Expression* right);
Expression* exprUnary(UnaryOperation operation, Expression* inner);
Expression* exprLiteral(int value);
Expression* exprCast(Type* type, Expression* inner);

void exprFree(Expression* expr);

void exprPrint(Expression* expr);

#endif

