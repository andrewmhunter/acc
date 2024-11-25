#include "expression.h"
#include "mem.h"
#include <stdlib.h>
#include <stdio.h>

static Expression* exprNew(Arena* arena, ExpressionType type) {
    Expression* expr = ARENA_ALLOC(arena, Expression);
    expr->type = type;
    return expr;
}

Expression* exprBinary(Arena* arena, BinaryOperation operation, Expression* left, Expression* right) {
    Expression* expr = exprNew(arena, EXPR_BINARY);
    expr->binary.operation = operation;
    expr->binary.left = left;
    expr->binary.right = right;
    return expr;
}

Expression* exprAssign(Arena* arena, BinaryOperation operation, Expression* left, Expression* right) {
    Expression* expr = exprNew(arena, EXPR_ASSIGN);
    expr->binary.operation = operation;
    expr->binary.left = left;
    expr->binary.right = right;
    return expr;
}

Expression* exprUnary(Arena* arena, UnaryOperation operation, Expression* inner) {
    Expression* expr = exprNew(arena, EXPR_UNARY);
    expr->unary.operation = operation;
    expr->unary.inner = inner;
    return expr;
}

Expression* exprLiteral(Arena* arena, int value) {
    Expression* expr = exprNew(arena, EXPR_LITERAL);
    expr->literal = value;
    return expr;
}

Expression* exprCast(Arena* arena, Type* type, Expression* inner) {
    Expression* expr = exprNew(arena, EXPR_CAST);
    expr->cast.type = type;
    expr->cast.inner = inner;
    return expr;
}

Expression* exprVariable(Arena* arena, Identifier name) {
    Expression* expr = exprNew(arena, EXPR_VARIABLE);
    expr->variable = name;
    return expr;
}


void exprPrint(FILE* file, Expression* expr) {
    if (expr == NULL) {
        fprintf(file, "Null");
        return;
    }
    switch (expr->type) {
        case EXPR_LITERAL:
            fprintf(file, "%d", expr->literal);
            break;
        case EXPR_UNARY:
            fprintf(file, "(%c ", expr->unary.operation);
            exprPrint(file, expr->unary.inner);
            fprintf(file, ")");
            break;
        case EXPR_BINARY:
            fprintf(file, "(");
            exprPrint(file, expr->binary.left);
            fprintf(file, " %c ", expr->binary.operation);
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
    }
}

bool exprEquals(Expression* expr0, Expression* expr1) {
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
    }
}

