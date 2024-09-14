#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <stdarg.h>
#include "parser.h"
#include "expression.h"
#include "type.h"
#include "identifier.h"

#define MIN(A, B) \
    ((A) < (B) ? (A) : (B))


Parser newParser(Scanner scan) {
    Parser parser = {.scan = scan, .hasPeekToken = false};
    return parser;
}

static inline Token next(Parser* parser) {
    Token token;
    if (parser->hasPeekToken) {
        parser->hasPeekToken = false;
        token = parser->peekToken;
    } else {
        token = nextToken(&parser->scan);
    }
    //printf("%2d %.*s\n", token.kind, token.length, token.start);
    return token;
}

static Token* peek(Parser* parser) {
    if (!parser->hasPeekToken) {
        parser->peekToken = nextToken(&parser->scan);
        parser->hasPeekToken = true;
    }
    return &parser->peekToken;
}

static bool nextIs(Parser* parser, TokenType kind) {
    return peek(parser)->kind == kind;
}

static bool match(Parser* parser, TokenType kind, Token* tokenOut) {
    if (nextIs(parser, kind)) {
        Token token = next(parser);
        if (tokenOut) {
            *tokenOut = token;
        }
        return true;
    }
    return false;
}

static void verror(Parser* parser, Token* token, const char* message, va_list args) {
    parser->hadError = true;
    parser->recoveringError = true;

    fprintf(stderr, "error: ");
    if (token != NULL) {
        fprintf(stderr, "at '%.*s': ", token->length, token->start);
    }
    va_list valist;
    vfprintf(stderr, message, valist);
    fprintf(stderr, "\n");
}

static void error(Parser* parser, Token* token, const char* message, ...) {
    va_list valist;
    va_start(valist, message);
    verror(parser, token, message, valist);
    va_end(valist);
}

static void errorAtNext(Parser* parser, const char* message, ...) {
    va_list valist;
    va_start(valist, message);
    verror(parser, peek(parser), message, valist);
    va_end(valist);
}

static void recover(Parser* parser) {
    if (!parser->recoveringError) {
        return;
    }
    parser->recoveringError = false;
    for (;;) {
        switch (peek(parser)->kind) {
            case TOK_SEMICOLON:
            case TOK_PAREN_RIGHT:
            case TOK_BRACE_RIGHT:
                next(parser);
            case TOK_EOF:
                return;
            default:
                next(parser);
                break;
        }
    }
}

static void consume(Parser* parser, TokenType kind, const char* message) {
    if (nextIs(parser, kind)) {
        next(parser);
        return;
    }
    errorAtNext(parser, message);
}

static bool nextIsType(Parser* parser) {
    switch (peek(parser)->kind) {
        case TOK_INT:
        case TOK_CHAR:
        case TOK_UNSIGNED:
        case TOK_VOID:
            return true;
        default:
            break;
    }
    return false;
}


static Type* intType(Parser* parser) {
    Signedness sign = SIGN_SIGNED;
    IntegerSize size = SIZE_INT;
    if (match(parser, TOK_UNSIGNED, NULL)) {
        sign = SIGN_UNSIGNED;
    }

    if (match(parser, TOK_INT, NULL)) {
        size = SIZE_INT;
    } else if (match(parser, TOK_CHAR, NULL)) {
        size = SIZE_BYTE;
    }

    return typeInteger(sign, size);
}

static Type* type(Parser* parser) {
    Type* type = NULL;
    switch (peek(parser)->kind) {
        case TOK_INT:
        case TOK_CHAR:
        case TOK_UNSIGNED:
            type = intType(parser);
            break;
        case TOK_VOID:
            next(parser);
            type = typeNew(TYPE_VOID);
            break;
        default:
            assert(false && "invalid type");
            break;
    }

    while (match(parser, TOK_ASTERIX, NULL)) {
        type = typePointer(type);
    }
    return type;
}

//static Identifier identifier(Parser* parser) {
//}

static int integerLiteral(Parser* parser, Token token) {
    int value = 0;
    int base = 10;
    const char* start = token.start;
    const char* end = start + token.length;

    if (start[0] == '0') {
        base = 8;
        if (token.length > 2) {
            if (start[1] == 'x' || start[1] == 'X') {
                base = 16;
                start += 2;
            } else if (start[1] == 'b' || start[1] == 'B') {
                base = 2;
                start += 2;
            }
        }
    }

    for (const char* at = start; at < end; ++at) {
        value *= base;
        if (*at >= '0' && *at <= '0' + MIN(base, 10)) {
            value += *at - '0';
        } else if (base == 16 && *at >= 'a' && *at <= 'f') {
            value += *at - 'a' + 0xa;
        } else if (base == 16 && *at >= 'A' && *at <= 'F') {
            value += *at - 'A' + 0xa;
        } else {
            error(parser, &token, "invalid integer literal");
        }
    }
    return value;
}

static Expression* primary(Parser* parser, bool parenthesized) {
    if (parenthesized) {
        Expression* expr = expression(parser);
        consume(parser, TOK_PAREN_RIGHT, "expected ')'");
        return expr;
    }

    Token tok;
    if (match(parser, TOK_PAREN_LEFT, NULL)) {
        Expression* expr = expression(parser);
        consume(parser, TOK_PAREN_RIGHT, "expected ')'");
        return expr;
    }
    if (match(parser, TOK_NUMBER, &tok)) {
        return exprLiteral(integerLiteral(parser, tok));
    }
    errorAtNext(parser, "unexpected token");
    return NULL;
}

static Expression* suffix(Parser* parser, bool parenthesized) {
    return primary(parser, parenthesized);
}

static Expression* prefix(Parser* parser) {
    UnaryOperation op;
    if (match(parser, TOK_MINUS, NULL)) {
        op = UNARY_NEGATE;
    } else if (match(parser, TOK_PAREN_LEFT, NULL)) {
        if (nextIsType(parser)) {
            Type* ty = type(parser);
            consume(parser, TOK_PAREN_RIGHT, "expected ')'");
            return exprCast(ty, prefix(parser));
        }
        return suffix(parser, true);
    } else {
        return suffix(parser, false);
    }
    return exprUnary(op, prefix(parser));
}

static Expression* product(Parser* parser) {
    Expression* left = prefix(parser);
    for (;;) {
        BinaryOperation op;
        if (match(parser, TOK_ASTERIX, NULL)) {
            op = BINARY_MULTIPLY;
        } else if (match(parser, TOK_SLASH, NULL)) {
            op = BINARY_DIVIDE;
        } else {
            return left;
        }
        Expression* right = prefix(parser);
        left = exprBinary(op, left, right);
    }
}

static Expression* sum(Parser* parser) {
    Expression* left = product(parser);
    for (;;) {
        BinaryOperation op;
        if (match(parser, TOK_PLUS, NULL)) {
            op = BINARY_ADD;
        } else if (match(parser, TOK_MINUS, NULL)) {
            op = BINARY_SUBTRACT;
        } else {
            return left;
        }
        Expression* right = product(parser);
        left = exprBinary(op, left, right);
    }
}

static Expression* assignment(Parser* parser) {
    Expression* left = sum(parser);
    if (!match(parser, TOK_EQUAL, NULL)) {
        return left;
    }
    Expression* right = assignment(parser);
    return exprBinary(BINARY_ASSIGN, left, right);
}

Expression* expression(Parser* parser) {
    return assignment(parser);
}

static Statement* conditional(Parser* parser) {
    consume(parser, TOK_PAREN_LEFT, "expected '('");
    Expression* condition = expression(parser);
    consume(parser, TOK_PAREN_RIGHT, "expected ')'");
    Statement* inner = statement(parser);

    Statement* onElse = NULL;
    if (match(parser, TOK_ELSE, NULL)) {
        onElse = statement(parser);
    }

    Statement* stmt = stmtNew(STATEMENT_IF);
    stmt->conditional.condition = condition;
    stmt->conditional.inner = inner;
    stmt->conditional.onElse = onElse;
    return stmt;
}

static Statement* whileLoop(Parser* parser) {
    consume(parser, TOK_PAREN_LEFT, "expected '('");
    Expression* condition = expression(parser);
    consume(parser, TOK_PAREN_RIGHT, "expected ')'");
    Statement* inner = statement(parser);

    Statement* stmt = stmtNew(STATEMENT_WHILE);
    stmt->whileLoop.condition = condition;
    stmt->whileLoop.inner = inner;
    stmt->whileLoop.isDoWhile = false;
    return stmt;
}

static Statement* expressionStatement(Parser* parser) {
    Expression* expr = expression(parser);
    Statement* stmt = stmtNew(STATEMENT_EXPRESSION);
    consume(parser, TOK_SEMICOLON, "expected ';'");
    stmt->expression = expr;
    return stmt;
}

static Statement* block(Parser* parser) {
    StatementList list = stmtListNew();
    while (!match(parser, TOK_BRACE_RIGHT, NULL) && !nextIs(parser, TOK_EOF)) {
        Statement* inner = statement(parser);
        stmtListAppend(&list, inner);
        recover(parser);
    }

    Statement* stmt = stmtNew(STATEMENT_BLOCK);
    stmt->block = list;
    return stmt;
}

Statement* statement(Parser* parser) {
    if (nextIsType(parser)) {
        assert(false && "variables not implemented yet");
    } else if (match(parser, TOK_IF, NULL)) {
        return conditional(parser);
    } else if (match(parser, TOK_WHILE, NULL)) {
        return whileLoop(parser);
    } else if (match(parser, TOK_BRACE_LEFT, NULL)) {
        return block(parser);
    }
    return expressionStatement(parser);
}

