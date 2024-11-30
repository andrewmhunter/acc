#include "expression.h"
#include "mem.h"
#include "diag.h"
#include <stdlib.h>
#include <stdio.h>

static Expression* exprNew(Arena* arena, ExpressionType type) {
    Expression* expr = ARENA_ALLOC(arena, Expression);
    expr->type = type;
    return expr;
}

static const Expression* binaryLiteral(
        Arena* arena,
        BinaryOperation operation,
        const Expression* leftExpr,
        const Expression* rightExpr
) {
    if (((isStackOffset(leftExpr) && isLiteral(rightExpr))
        || (isStackOffset(rightExpr) && isLiteral(leftExpr)))
        && (operation == BINARY_ADD)
    ) {
        return exprStackOffset(arena, leftExpr->literal + rightExpr->literal);
    }

    if (!isLiteral(leftExpr) || !isLiteral(rightExpr)) {
        return NULL;
    }

    int left = leftExpr->literal;
    int right = rightExpr->literal;

    int result = 0;
    switch (operation) {
        case BINARY_ADD:
            result = left + right;
            break;
        case BINARY_SUBTRACT:
            result = left - right;
            break;
        case BINARY_MULTIPLY:
            result = left * right;
            break;
        case BINARY_DIVIDE:
            ASSERT(right != 0, "cannot divide by 0");
            result = left / right;
            break;
        case BINARY_SHIFT_LEFT:
            result = left << right;
            break;
        case BINARY_SHIFT_RIGHT:
            result = left >> right;
            break;
        case BINARY_EQUAL:
            result = left == right;
            break;
        case BINARY_NOT_EQUAL:
        case BINARY_LOGICAL_AND:
        case BINARY_LOGICAL_OR:
        case BINARY_LESS:
        case BINARY_LESS_EQUAL:
        case BINARY_GREATER:
        case BINARY_GREATER_EQUAL:
            return NULL;
        case BINARY_NONE:
            UNREACHABLE();
    }

    return exprLiteral(arena, result);
}

const Expression* exprBinary(
        Arena* arena,
        BinaryOperation operation,
        const Expression* left,
        const Expression* right
) {
    const Expression* exprlit = binaryLiteral(arena, operation, left, right);
    if (exprlit != NULL) {
        return exprlit;
    }

    Expression* expr = exprNew(arena, EXPR_BINARY);
    expr->binary.operation = operation;
    expr->binary.left = left;
    expr->binary.right = right;
    return expr;
}

const Expression* exprAssign(
        Arena* arena,
        BinaryOperation operation,
        const Expression* left,
        const Expression* right
) {
    Expression* expr = exprNew(arena, EXPR_ASSIGN);
    expr->binary.operation = operation;
    expr->binary.left = left;
    expr->binary.right = right;
    return expr;
}

const Expression* unaryLiteral(
        Arena* arena,
        UnaryOperation operation,
        const Expression* innerExpr
) {
    if (!isLiteral(innerExpr)) {
        return NULL;
    }

    int inner = innerExpr->literal;

    int result = 0;
    switch (operation) {
        case UNARY_NEGATE:
            result = -inner;
            break;
        case UNARY_NOT:
        case UNARY_ADDRESSOF:
        case UNARY_DEREFERENCE:
            return NULL;
            break;
    }

    return exprLiteral(arena, result);
}

const Expression* exprUnary(
        Arena* arena,
        UnaryOperation operation,
        const Expression* inner
) {
    const Expression* litexpr = unaryLiteral(arena, operation, inner);
    if (litexpr != NULL) {
        return litexpr;
    }

    Expression* expr = exprNew(arena, EXPR_UNARY);
    expr->unary.operation = operation;
    expr->unary.inner = inner;
    return expr;
}

#define LITERAL_INTERN_MIN -16
#define LITERAL_INTERN_MAX 32

static Expression internedLiterals[LITERAL_INTERN_MAX - LITERAL_INTERN_MIN + 1] = {};

const Expression* exprLiteral(Arena* arena, int value) {
    if (value >= LITERAL_INTERN_MIN && value <= LITERAL_INTERN_MAX) {
        size_t index = value - LITERAL_INTERN_MIN;
        if (internedLiterals[index].type == 0) {
            internedLiterals[index].type = EXPR_LITERAL;
            internedLiterals[index].literal = value;
        }
        //fprintf(stderr, "Got interned literal: %d\n", value);
        return &internedLiterals[index];
    }

    //fprintf(stderr, "Allocated literal: %d\n", value);

    Expression* expr = exprNew(arena, EXPR_LITERAL);
    expr->literal = value;
    return expr;
}

const Expression* exprLabel(Arena* arena, int value) {
    Expression* expr = exprNew(arena, EXPR_LABEL);
    expr->label = value;
    return expr;
}

#define STACKOFFSET_INTERN_MAX 32

static Expression stackOffsets[STACKOFFSET_INTERN_MAX + 1] = {};

const Expression* exprStackOffset(Arena* arena, int value) {
    if (value >= 0 && value <= STACKOFFSET_INTERN_MAX) {
        if (stackOffsets[value].type == 0) {
            stackOffsets[value].type = EXPR_STACKOFFSET;
            stackOffsets[value].stackOffset = value;
        }
        //fprintf(stderr, "Got interned stackOffset: %d\n", value);
        return &stackOffsets[value];
    }

    //fprintf(stderr, "Allocated stackoffset: %d\n", value);

    Expression* expr = exprNew(arena, EXPR_STACKOFFSET);
    expr->stackOffset = value;
    return expr;
}

const Expression* exprCast(Arena* arena, const Type* type, const Expression* inner) {
    Expression* expr = exprNew(arena, EXPR_CAST);
    expr->cast.type = type;
    expr->cast.inner = inner;
    return expr;
}

const Expression* exprVariable(Arena* arena, Identifier name) {
    Expression* expr = exprNew(arena, EXPR_VARIABLE);
    expr->variable = name;
    return expr;
}

static void printBinaryOperator(FILE* file, BinaryOperation op) {
    switch (op) {
        case BINARY_ADD:
            fprintf(file, "+");
            break;
        case BINARY_SUBTRACT:
            fprintf(file, "-");
            break;
        case BINARY_MULTIPLY:
            fprintf(file, "*");
            break;
        case BINARY_DIVIDE:
            fprintf(file, "/");
            break;
        case BINARY_SHIFT_LEFT:
            fprintf(file, "<<");
            break;
        case BINARY_SHIFT_RIGHT:
            fprintf(file, ">>");
            break;
        case BINARY_LOGICAL_OR:
            fprintf(file, "||");
            break;
        case BINARY_LOGICAL_AND:
            fprintf(file, "&&");
            break;
        case BINARY_EQUAL:
            fprintf(file, "==");
            break;
        case BINARY_NOT_EQUAL:
            fprintf(file, "!=");
            break;
        case BINARY_LESS:
            fprintf(file, "<");
            break;
        case BINARY_LESS_EQUAL:
            fprintf(file, "<=");
            break;
        case BINARY_GREATER:
            fprintf(file, ">");
            break;
        case BINARY_GREATER_EQUAL:
            fprintf(file, ">=");
            break;
        case BINARY_NONE:
            fprintf(file, "INVALID_BINARY_OPERATOR");
            break;
    }
}

static void printUnaryOperator(FILE* file, UnaryOperation op) {
    switch (op) {
        case UNARY_NEGATE:
            fprintf(file, "-");
            break;
        case UNARY_NOT:
            fprintf(file, "!");
            break;
        case UNARY_ADDRESSOF:
            fprintf(file, "&");
            break;
        case UNARY_DEREFERENCE:
            fprintf(file, "*");
            break;
    }
}

void exprPrint(FILE* file, const Expression* expr) {
    if (expr == NULL) {
        fprintf(file, "Null");
        return;
    }
    switch (expr->type) {
        case EXPR_LITERAL:
            fprintf(file, "%d", expr->literal);
            break;
        case EXPR_UNARY:
            fprintf(file, "(");
            printUnaryOperator(file, expr->unary.operation);
            fprintf(file, " ");
            exprPrint(file, expr->unary.inner);
            fprintf(file, ")");
            break;
        case EXPR_BINARY:
            fprintf(file, "(");
            exprPrint(file, expr->binary.left);
            fprintf(file, " ");
            printBinaryOperator(file, expr->binary.operation);
            fprintf(file, " ");
            exprPrint(file, expr->binary.right);
            fprintf(file, ")");
            break;
        case EXPR_CAST:
            fprintf(file, "((");
            typePrint(file, expr->cast.type);
            fprintf(file, ")(");
            exprPrint(file, expr->cast.inner);
            fprintf(file, "))");
            break;
        case EXPR_VARIABLE:
            identPrint(file, expr->variable);
            break;
        case EXPR_ASSIGN:
            fprintf(file, "(");
            exprPrint(file, expr->binary.left);
            fprintf(file, " = ");
            exprPrint(file, expr->binary.right);
            fprintf(file, ")");
            break;
        case EXPR_LABEL:
            fprintf(file, "_Label_%d", expr->label);
            break;
        case EXPR_STACKOFFSET:
            fprintf(file, "(_Stack + %d)", expr->stackOffset);
            break;
    }
}

bool isLiteral(const Expression* expr) {
    return expr->type == EXPR_LITERAL;
}

bool isStackOffset(const Expression* expr) {
    return expr->type == EXPR_STACKOFFSET;
}

bool exprEquals(const Expression* expr0, const Expression* expr1) {
    if (expr0->type != expr1->type) {
        return false;
    }
    switch (expr0->type) {
        case EXPR_LITERAL:
            return expr0->literal == expr1->literal;
        case EXPR_UNARY:
            return expr0->unary.operation == expr1->unary.operation
                && exprEquals(expr0->unary.inner, expr1->unary.inner);
        case EXPR_BINARY:
            return expr0->binary.operation == expr1->binary.operation
                && exprEquals(expr0->binary.left, expr1->binary.left)
                && exprEquals(expr0->binary.right, expr1->binary.right);
        case EXPR_CAST:
            return typeEquals(expr0->cast.type, expr1->cast.type)
                && exprEquals(expr0->cast.inner, expr1->cast.inner);
        case EXPR_VARIABLE:
            return identEquals(expr0->variable, expr1->variable);
        case EXPR_ASSIGN:
            return expr0->binary.operation == expr1->binary.operation
                && exprEquals(expr0->binary.left, expr1->binary.left)
                && exprEquals(expr0->binary.right, expr1->binary.right);
        case EXPR_LABEL:
            return expr0->label == expr1->label;
        case EXPR_STACKOFFSET:
            return expr0->stackOffset == expr1->stackOffset;
    }
}

