#ifndef EXPRESSION_H
#define EXPRESSION_H

#include "type.h"
#include "identifier.h"
#include "mem.h"
#include "type.h"
#include "diag.h"

typedef int Label;

typedef enum {
    // Zero is implicitly an invalid expression
    EXPR_BINARY = 1,
    EXPR_UNARY,
    EXPR_CALL,
    EXPR_LITERAL,
    EXPR_CAST,
    EXPR_VARIABLE,
    EXPR_ASSIGN,
    EXPR_LABEL,
    EXPR_STACKOFFSET,
} ExpressionType;

typedef enum {
    BINARY_NONE,
    BINARY_ADD,
    BINARY_SUBTRACT,
    BINARY_MULTIPLY,
    BINARY_DIVIDE,
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
            const struct Expression* name;
            const struct Expression** arguments;
            size_t argumentCount;
            Position endPosition;
        } call;

        struct {
            const struct Expression* condition;
            const struct Expression* onTrue;
            const struct Expression* onFalse;
        } ternary;

        Identifier variable;

        struct {
            Location location;
            union {
                struct {
                    UnaryOperation operation;
                    const struct Expression* inner;
                } unary;

                struct {
                    const Type* type;
                    const struct Expression* inner;
                } cast;

                struct {
                    const Identifier* functionName;
                    int offset;
                } stackOffset;

                int literal;
                Label label;
            };
        };
    };
} Expression;

const Expression* exprBinary(Arena* arena, BinaryOperation operation, const Expression* left, const Expression* right);
const Expression* exprAssign(Arena* arena, BinaryOperation operation, const Expression* left, const Expression* right);
const Expression* exprUnary(Arena* arena, UnaryOperation operation, const Expression* inner, Location location);
const Expression* exprLiteral(Arena* arena, int value, Location location);
const Expression* exprCast(Arena* arena, const Type* type, const Expression* inner, Location location);
const Expression* exprVariable(Arena* arena, Identifier name);
const Expression* exprLabel(Arena* arena, Label value, Location location);
const Expression* exprStackOffset(Arena* arena, const Identifier* functionName, int offset, Location location);
const Expression* exprFunctionCall(Arena* arena, const Expression* name, size_t argumentCount, const Expression** arguments, Position endPosition);

void exprPrint(FILE* file, const Expression* expr);

bool isPure(const Expression* expr);
bool isLiteral(const Expression* expr);
bool isStackOffset(const Expression* expr);
bool exprEquals(const Expression* expr0, const Expression* expr1);

Location exprLoc(const Expression* expr);

#endif

