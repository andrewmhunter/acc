#include "expression.h"
#include <stdlib.h>
#include <stdio.h>

Expression* exprNew(ExpressionType type) {
    Expression* expr = malloc(sizeof(Expression));
    expr->type = type;
    return expr;
}

Expression* exprBinary(BinaryOperation operation, Expression* left, Expression* right) {
    Expression* expr = exprNew(EXPR_BINARY);
    expr->binary.operation = operation;
    expr->binary.left = left;
    expr->binary.right = right;
    return expr;
}

Expression* exprUnary(UnaryOperation operation, Expression* inner) {
    Expression* expr = exprNew(EXPR_UNARY);
    expr->unary.operation = operation;
    expr->unary.inner = inner;
    return expr;
}

Expression* exprLiteral(int value) {
    Expression* expr = exprNew(EXPR_LITERAL);
    expr->literal = value;
    return expr;
}

Expression* exprCast(Type* type, Expression* inner) {
    Expression* expr = exprNew(EXPR_CAST);
    expr->cast.type = type;
    expr->cast.inner = inner;
    return expr;
}


void exprFree(Expression* expr) {
    if (expr == NULL) {
        return;
    }

    switch (expr->type) {
        case EXPR_BINARY:
            exprFree(expr->binary.left);
            exprFree(expr->binary.right);
            break;
        case EXPR_UNARY:
            exprFree(expr->unary.inner);
            break;
        case EXPR_CAST:
            typeFree(expr->cast.type);
            exprFree(expr->cast.inner);
            break;
        default:
            break;
    }

    free(expr);
}

void exprPrint(Expression* expr) {
    if (expr == NULL) {
        printf("Null");
        return;
    }
    switch (expr->type) {
        case EXPR_LITERAL:
            printf("%d", expr->literal);
            break;
        case EXPR_UNARY:
            printf("(%c ", expr->unary.operation);
            exprPrint(expr->unary.inner);
            printf(")");
            break;
        case EXPR_BINARY:
            printf("(");
            exprPrint(expr->binary.left);
            printf(" %c ", expr->binary.operation);
            exprPrint(expr->binary.right);
            printf(")");
            break;
        case EXPR_CAST:
            printf("((");
            typePrint(expr->cast.type);
            printf(")(");
            exprPrint(expr->cast.inner);
            printf("))");
            break;
    }
}

