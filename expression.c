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
        const Expression* rightExpr,
        Location location
) {
    if ((isStackOffset(leftExpr) && isLiteral(rightExpr))
        && (operation == BINARY_ADD)
    ) {
        return exprStackOffset(
                arena,
                leftExpr->stackOffset.functionName,
                leftExpr->stackOffset.offset + rightExpr->literal,
                location
            );
    }

    if ((isStackOffset(rightExpr) && isLiteral(leftExpr))
        && (operation == BINARY_ADD)
    ) {
        return exprStackOffset(
                arena,
                rightExpr->stackOffset.functionName,
                rightExpr->stackOffset.offset + leftExpr->literal,
                location
            );
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
            if (right == 0) {
                return NULL;
            }
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
        case BINARY_BITWISE_OR:
            result = left | right;
            break;
        case BINARY_BITWISE_AND:
            result = left & right;
            break;
        case BINARY_BITWISE_XOR:
            result = left ^ right;
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

    return exprLiteral(arena, result, location);
}

const Expression* exprBinary(
        Arena* arena,
        BinaryOperation operation,
        const Expression* left,
        const Expression* right
) {
    const Expression* exprlit = binaryLiteral(arena, operation, left, right, locSpan(exprLoc(left), exprLoc(right)));
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
        const Expression* innerExpr,
        Location location
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

    return exprLiteral(arena, result, location);
}

const Expression* exprUnary(
        Arena* arena,
        UnaryOperation operation,
        const Expression* inner,
        Location location
) {
    const Expression* litexpr = unaryLiteral(arena, operation, inner, location);
    if (litexpr != NULL) {
        return litexpr;
    }

    Expression* expr = exprNew(arena, EXPR_UNARY);
    expr->unary.operation = operation;
    expr->unary.inner = inner;
    expr->location = location;
    return expr;
}

/*#define LITERAL_INTERN_MIN -16
#define LITERAL_INTERN_MAX 32

static Expression internedLiterals[LITERAL_INTERN_MAX - LITERAL_INTERN_MIN + 1] = {};*/

const Expression* exprLiteral(Arena* arena, int value, Location location) {
    /*if (value >= LITERAL_INTERN_MIN && value <= LITERAL_INTERN_MAX) {
        size_t index = value - LITERAL_INTERN_MIN;
        if (internedLiterals[index].type == 0) {
            internedLiterals[index].type = EXPR_LITERAL;
            internedLiterals[index].literal = value;
        }
        //fprintf(stderr, "Got interned literal: %d\n", value);
        return &internedLiterals[index];
    }*/

    //fprintf(stderr, "Allocated literal: %d\n", value);

    Expression* expr = exprNew(arena, EXPR_LITERAL);
    expr->literal = value;
    expr->location = location;
    return expr;
}

const Expression* exprLabel(Arena* arena, int value, Location location) {
    Expression* expr = exprNew(arena, EXPR_LABEL);
    expr->label = value;
    expr->location = location;
    return expr;
}

//#define STACKOFFSET_INTERN_MAX 32
//
//static Expression stackOffsets[STACKOFFSET_INTERN_MAX + 1] = {};

const Expression* exprStackOffset(Arena* arena, const Identifier* functionName, int offset, Location location) {
    //if (value >= 0 && value <= STACKOFFSET_INTERN_MAX) {
    //    if (stackOffsets[value].type == 0) {
    //        stackOffsets[value].type = EXPR_STACKOFFSET;
    //        stackOffsets[value].stackOffset = value;
    //    }
    //    //fprintf(stderr, "Got interned stackOffset: %d\n", value);
    //    return &stackOffsets[value];
    //}

    //fprintf(stderr, "Allocated stackoffset: %d\n", value);

    Expression* expr = exprNew(arena, EXPR_STACKOFFSET);
    expr->stackOffset.functionName = functionName;
    expr->stackOffset.offset = offset;
    expr->location = location;
    return expr;
}

const Expression* exprCast(Arena* arena, const Type* type, const Expression* inner, Location location) {
    Expression* expr = exprNew(arena, EXPR_CAST);
    expr->cast.type = type;
    expr->cast.inner = inner;
    expr->location = location;
    return expr;
}

const Expression* exprVariable(Arena* arena, Identifier name) {
    Expression* expr = exprNew(arena, EXPR_VARIABLE);
    expr->variable = name;
    return expr;
}

const Expression* exprFunctionCall(
        Arena* arena,
        const Expression* name,
        size_t argumentCount,
        const Expression** arguments,
        Position endPosition
) {
    Expression* expr = exprNew(arena, EXPR_CALL);
    expr->call.name = name;
    expr->call.argumentCount = argumentCount;
    expr->call.arguments = arguments;
    expr->call.endPosition = endPosition;
    return expr;
}

const Expression* exprSizeofType(Arena* arena, const Type* type, Location location) {
    Expression* expr = exprNew(arena, EXPR_SIZEOF);
    expr->exprSizeOf.hasType = true;
    expr->exprSizeOf.type = type;
    expr->location = location;
    return expr;
}

const Expression* exprSizeofExpr(Arena* arena, const Expression* inner, Location location) {
    Expression* expr = exprNew(arena, EXPR_SIZEOF);
    expr->exprSizeOf.hasType = false;
    expr->exprSizeOf.expr = inner;
    expr->location = location;
    return expr;
}

const Expression* exprStringLiteral(Arena* arena, const char* data, int length, Location location) {
    Expression* expr = exprNew(arena, EXPR_STRING);
    expr->string.data = data;
    expr->string.length = length;
    expr->location = location;
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
        case BINARY_BITWISE_OR:
            fprintf(file, "|");
            break;
        case BINARY_BITWISE_AND:
            fprintf(file, "&");
            break;
        case BINARY_BITWISE_XOR:
            fprintf(file, "^");
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
            identPrint(file, &expr->variable);
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
            fprintf(file, "(_Stack_%.*s + %d)",
                    expr->stackOffset.functionName->length,
                    expr->stackOffset.functionName->start,
                    expr->stackOffset.offset
                );
            break;
        case EXPR_CALL:
            exprPrint(file, expr->call.name);
            fprintf(file, "(");
            for (int i = 0; i < expr->call.argumentCount; ++i) {
                exprPrint(file, expr->call.arguments[i]);
                if (i != expr->call.argumentCount - 1) {
                    fprintf(file, ", ");
                }
            }
            fprintf(file, ")");
            break;
        case EXPR_SIZEOF:
            fprintf(file, "sizeof(");
            if (expr->exprSizeOf.hasType) {
                typePrint(file, expr->exprSizeOf.type);
            } else {
                exprPrint(file, expr->exprSizeOf.expr);
            }
            fprintf(file, ")");
            break;
        case EXPR_STRING:
            fprintf(file, "\"%.*s\"", expr->string.length, expr->string.data);
    }
}

bool isPure(const Expression* expr) {
    switch (expr->type) {
        case EXPR_LITERAL:
        case EXPR_STACKOFFSET:
        case EXPR_VARIABLE:
        case EXPR_LABEL:
        case EXPR_SIZEOF:
        case EXPR_STRING:
            return true;
        case EXPR_CALL:
        case EXPR_ASSIGN:
            return false;
        case EXPR_BINARY:
            return isPure(expr->binary.left) && isPure(expr->binary.right);
        case EXPR_UNARY:
            return isPure(expr->unary.inner);
        case EXPR_CAST:
            return isPure(expr->cast.inner);
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
            return identEquals(&expr0->variable, &expr1->variable);
        case EXPR_ASSIGN:
            return expr0->binary.operation == expr1->binary.operation
                && exprEquals(expr0->binary.left, expr1->binary.left)
                && exprEquals(expr0->binary.right, expr1->binary.right);
        case EXPR_LABEL:
            return expr0->label == expr1->label;
        case EXPR_STACKOFFSET:
            return expr0->stackOffset.offset == expr1->stackOffset.offset
                && identEquals(expr0->stackOffset.functionName, expr1->stackOffset.functionName);
        case EXPR_SIZEOF:
            if (expr0->exprSizeOf.hasType) {
                if (!expr1->exprSizeOf.hasType) {
                    return false;
                }
                return typeEquals(expr0->exprSizeOf.type, expr1->exprSizeOf.type);
            }
            if (expr1->exprSizeOf.hasType) {
                return false;
            }
            return exprEquals(expr0->exprSizeOf.expr, expr1->exprSizeOf.expr);
        case EXPR_STRING:
        case EXPR_CALL:
            // TODO: implement
            return false;
    }
}

static Location exprUnaryLoc(const Expression* inner, Position outerPosition) {
    return locSpanPosition(exprLoc(inner), outerPosition);
}

bool getLiteral(const Expression* expr, int* val) {
    if (expr->type != EXPR_LITERAL) {
        return false;
    }

    *val = expr->literal;
    return true;
}

Location exprLoc(const Expression* expr) {
    if (expr == NULL) {
        return NO_LOCATION;
    }

    switch (expr->type) {
        case EXPR_VARIABLE:
            return identLoc(&expr->variable);
        case EXPR_ASSIGN:
        case EXPR_BINARY:
            return locSpan(exprLoc(expr->binary.left), exprLoc(expr->binary.right));
        case EXPR_CALL:
            return locSpanPosition(exprLoc(expr->call.name), expr->call.endPosition);
        case EXPR_SIZEOF:
        case EXPR_CAST:
        case EXPR_UNARY:
        case EXPR_LITERAL:
        case EXPR_LABEL:
        case EXPR_STACKOFFSET:
        case EXPR_STRING:
            return expr->location;
    }
    return NO_LOCATION;
}


static bool binaryOperationIsCondition(BinaryOperation operation) {
    switch (operation) {
        case BINARY_ADD:
        case BINARY_SUBTRACT:
        case BINARY_SHIFT_LEFT:
        case BINARY_MULTIPLY: 
        case BINARY_SHIFT_RIGHT:
        case BINARY_DIVIDE:
        case BINARY_BITWISE_OR:
        case BINARY_BITWISE_AND:
        case BINARY_BITWISE_XOR:
            return false;
        case BINARY_LOGICAL_OR:
        case BINARY_LOGICAL_AND:
        case BINARY_EQUAL:
        case BINARY_NOT_EQUAL:
        case BINARY_LESS:
        case BINARY_LESS_EQUAL:
        case BINARY_GREATER:
        case BINARY_GREATER_EQUAL:
            return true;
        case BINARY_NONE:
            UNREACHABLE();
    }
}

bool exprIsCondition(const Expression* expr) {
    switch (expr->type) {
        case EXPR_BINARY:
            return binaryOperationIsCondition(expr->binary.operation);
        case EXPR_UNARY:
            return expr->unary.operation == UNARY_NOT;
        default:
            return false;
    }
}

