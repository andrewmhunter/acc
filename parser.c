#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <stdarg.h>
#include "parser.h"
#include "expression.h"
#include "type.h"
#include "identifier.h"
#include "util.h"

Parser newParser(Arena* staticLifetime, Diagnostics* diag, Scanner scan) {
    Parser parser = {
        .scan = scan,
        .staticLifetime = staticLifetime,
        .diag = diag,
        .hasPeekToken = false,
    };
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

static Location tokenLoc(const Token* token) {
    return locNew(token->position, token->position + token->length - 1);
}

static void verror(Parser* parser, Token* token, const char* message, va_list args) {
    if (parser->recoveringError) {
        return;
    }

    parser->recoveringError = true;

    errorStart(parser->diag, tokenLoc(token));
    if (token != NULL) {
        fprintf(stderr, "at '%.*s': ", token->length, token->start);
    }

    vfprintf(stderr, message, args);
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
                return;
            case TOK_EOF:
                return;
            default:
                next(parser);
                break;
        }
    }
}

static Location consume(Parser* parser, TokenType kind, const char* message) {
    if (nextIs(parser, kind)) {
        Token tok = next(parser);
        return tokenLoc(&tok);
    }
    errorAtNext(parser, message);
    return NO_LOCATION;
}

static Identifier makeIdentifier(Token token) {
    Identifier id = {.start = token.start, .length = token.length, .position = token.position};
    return id;
}

static Identifier identifier(Parser* parser) {
    if (nextIs(parser, TOK_IDENTIFIER)) {
        return makeIdentifier(next(parser));
    }
    errorAtNext(parser, "expected identifier");
    return BLANK_IDENTIFIER;
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


static const Type* intType(Parser* parser) {
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

    return typeInteger(parser->staticLifetime, sign, size);
}

static const Type* type(Parser* parser) {
    const Type* type = NULL;
    switch (peek(parser)->kind) {
        case TOK_INT:
        case TOK_CHAR:
        case TOK_UNSIGNED:
            type = intType(parser);
            break;
        case TOK_VOID:
            next(parser);
            type = typeNew(parser->staticLifetime, TYPE_VOID);
            break;
        default:
            errorAtNext(parser, "expected type");
            break;
    }

    while (match(parser, TOK_ASTERIX, NULL)) {
        type = typePointer(parser->staticLifetime, type);
    }
    return type;
}

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

static const Expression* primary(Parser* parser, bool parenthesized) {
    if (parenthesized) {
        const Expression* expr = expression(parser);
        consume(parser, TOK_PAREN_RIGHT, "expected ')'");
        return expr;
    }

    Token tok;
    if (match(parser, TOK_PAREN_LEFT, NULL)) {
        const Expression* expr = expression(parser);
        consume(parser, TOK_PAREN_RIGHT, "expected ')'");
        return expr;
    }
    if (match(parser, TOK_NUMBER, &tok)) {
        return exprLiteral(parser->staticLifetime, integerLiteral(parser, tok), tokenLoc(&tok));
    }
    if (match(parser, TOK_IDENTIFIER, &tok)) {
        return exprVariable(parser->staticLifetime, makeIdentifier(tok));
    }
    errorAtNext(parser, "unexpected token");
    return NULL;
}

static const Expression* functionCall(Parser* parser, const Expression* functionName) {
    size_t argumentCount = 0;
    size_t argumentCapacity = 0;
    const Expression** arguments = NULL;

    const char* unclosedMessage = "expected ')' or expression";

    while (!nextIs(parser, TOK_PAREN_RIGHT) && !nextIs(parser, TOK_EOF)) {
        const Expression* argument = expression(parser);
        APPEND_ARRAY(
                parser->staticLifetime,
                arguments,
                const Expression*,
                argumentCapacity,
                argumentCount,
                argument
            );

        if (!match(parser, TOK_COMMA, NULL)) {
            unclosedMessage = "expected ')' or ','";
            break;
        }
    }

    Location closing = consume(parser, TOK_PAREN_RIGHT, unclosedMessage);
    return exprFunctionCall(parser->staticLifetime, functionName, argumentCount, arguments, closing.end);
}

static const Expression* suffix(Parser* parser, bool parenthesized) {
    const Expression* inner = primary(parser, parenthesized);
    for (;;) {
        if (match(parser, TOK_PAREN_LEFT, NULL)) {
            inner = functionCall(parser, inner);
        } else if (match(parser, TOK_SQUARE_LEFT, NULL)) {
            const Expression* toAdd = expression(parser);
            Location end = consume(parser, TOK_SQUARE_RIGHT, "expected ']'");
            const Expression* addition = exprBinary(parser->staticLifetime, BINARY_ADD, inner, toAdd);
            inner = exprUnary(parser->staticLifetime, UNARY_DEREFERENCE, addition, locSpan(exprLoc(inner), end));
        } else {
            break;
        }
    }
    return inner;
}

static const Expression* prefix(Parser* parser) {
    UnaryOperation op;
    Token tok;
    if (match(parser, TOK_MINUS, &tok)) {
        op = UNARY_NEGATE;
    } else if (match(parser, TOK_BANG, &tok)) {
        op = UNARY_NOT;
    } else if (match(parser, TOK_BIT_AND, &tok)) {
        op = UNARY_ADDRESSOF;
    } else if (match(parser, TOK_ASTERIX, &tok)) {
        op = UNARY_DEREFERENCE;
    } else if (match(parser, TOK_PAREN_LEFT, &tok)) {
        if (nextIsType(parser)) {
            const Type* ty = type(parser);
            consume(parser, TOK_PAREN_RIGHT, "expected ')'");
            const Expression* inner = prefix(parser);
            return exprCast(
                    parser->staticLifetime,
                    ty,
                    inner,
                    locSpan(tokenLoc(&tok), exprLoc(inner))
                );
        }
        return suffix(parser, true);
    } else {
        return suffix(parser, false);
    }

    const Expression* inner = prefix(parser);
    return exprUnary(
            parser->staticLifetime,
            op,
            inner,
            locSpan(tokenLoc(&tok), exprLoc(inner))
        );
}

static const Expression* product(Parser* parser) {
    const Expression* left = prefix(parser);
    for (;;) {
        BinaryOperation op;
        if (match(parser, TOK_ASTERIX, NULL)) {
            op = BINARY_MULTIPLY;
        } else if (match(parser, TOK_SLASH, NULL)) {
            op = BINARY_DIVIDE;
        } else {
            return left;
        }
        const Expression* right = prefix(parser);
        left = exprBinary(parser->staticLifetime, op, left, right);
    }
}

static const Expression* sum(Parser* parser) {
    const Expression* left = product(parser);
    for (;;) {
        BinaryOperation op;
        if (match(parser, TOK_PLUS, NULL)) {
            op = BINARY_ADD;
        } else if (match(parser, TOK_MINUS, NULL)) {
            op = BINARY_SUBTRACT;
        } else {
            return left;
        }
        const Expression* right = product(parser);
        left = exprBinary(parser->staticLifetime, op, left, right);
    }
}

static const Expression* bitshift(Parser* parser) {
    const Expression* left = sum(parser);
    for (;;) {
        BinaryOperation op;
        if (match(parser, TOK_LEFT_SHIFT, NULL)) {
            op = BINARY_SHIFT_LEFT;
        } else if (match(parser, TOK_RIGHT_SHIFT, NULL)) {
            op = BINARY_SHIFT_RIGHT;
        } else {
            return left;
        }
        const Expression* right = sum(parser);
        left = exprBinary(parser->staticLifetime, op, left, right);
    }
}

static const Expression* ordering(Parser* parser) {
    const Expression* left = bitshift(parser);
    for (;;) {
        BinaryOperation op;
        if (match(parser, TOK_LESS, NULL)) {
            op = BINARY_LESS;
        } else if (match(parser, TOK_LESS_EQUAL, NULL)) {
            op = BINARY_LESS_EQUAL;
        } else if (match(parser, TOK_GREATER, NULL)) {
            op = BINARY_GREATER;
        } else if (match(parser, TOK_GREATER_EQUAL, NULL)) {
            op = BINARY_GREATER_EQUAL;
        } else {
            return left;
        }
        const Expression* right = bitshift(parser);
        left = exprBinary(parser->staticLifetime, op, left, right);
    }
}

static const Expression* equal(Parser* parser) {
    const Expression* left = ordering(parser);
    for (;;) {
        BinaryOperation op;
        if (match(parser, TOK_EQUAL_EQUAL, NULL)) {
            op = BINARY_EQUAL;
        } else if (match(parser, TOK_NOT_EQUAL, NULL)) {
            op = BINARY_NOT_EQUAL;
        } else {
            return left;
        }
        const Expression* right = ordering(parser);
        left = exprBinary(parser->staticLifetime, op, left, right);
    }
}

static const Expression* logicalAnd(Parser* parser) {
    const Expression* left = equal(parser);
    for (;;) {
        BinaryOperation op;
        if (match(parser, TOK_LOG_AND, NULL)) {
            op = BINARY_LOGICAL_AND;
        } else {
            return left;
        }
        const Expression* right = equal(parser);
        left = exprBinary(parser->staticLifetime, op, left, right);
    }
}

static const Expression* logicalOr(Parser* parser) {
    const Expression* left = logicalAnd(parser);
    for (;;) {
        BinaryOperation op;
        if (match(parser, TOK_LOG_OR, NULL)) {
            op = BINARY_LOGICAL_OR;
        } else {
            return left;
        }
        const Expression* right = logicalAnd(parser);
        left = exprBinary(parser->staticLifetime, op, left, right);
    }
}

static const Expression* assignment(Parser* parser) {
    const Expression* left = logicalOr(parser);
    if (!match(parser, TOK_EQUAL, NULL)) {
        return left;
    }
    const Expression* right = assignment(parser);
    return exprAssign(parser->staticLifetime, BINARY_NONE, left, right);
}

const Expression* expression(Parser* parser) {
    return assignment(parser);
}

static Statement* conditional(Parser* parser, Location startLocation) {
    consume(parser, TOK_PAREN_LEFT, "expected '('");
    const Expression* condition = expression(parser);
    Location endLocation = consume(parser, TOK_PAREN_RIGHT, "expected ')'");
    Statement* inner = statement(parser);

    Statement* onElse = NULL;
    if (match(parser, TOK_ELSE, NULL)) {
        onElse = statement(parser);
    }

    Statement* stmt = stmtNew(STATEMENT_IF, locSpan(startLocation, endLocation));
    stmt->conditional.condition = condition;
    stmt->conditional.inner = inner;
    stmt->conditional.onElse = onElse;
    return stmt;
}

static Statement* whileLoop(Parser* parser, Location startLoc) {
    consume(parser, TOK_PAREN_LEFT, "expected '('");
    const Expression* condition = expression(parser);
    Location endLoc = consume(parser, TOK_PAREN_RIGHT, "expected ')'");
    Statement* inner = statement(parser);

    Statement* stmt = stmtNew(STATEMENT_WHILE, locSpan(startLoc, endLoc));
    stmt->whileLoop.condition = condition;
    stmt->whileLoop.inner = inner;
    stmt->whileLoop.isDoWhile = false;
    return stmt;
}

static Statement* variableDeclaration(Parser* parser, Location identLocation) {
    Statement* stmt = stmtNew(STATEMENT_VARIABLE, identLocation);
    stmt->variableDeclaration.type = type(parser);

    if (!nextIs(parser, TOK_IDENTIFIER)) {
        errorAtNext(parser, "expected identifier");
        return NULL;
    }
    stmt->variableDeclaration.id = makeIdentifier(next(parser));
    
    stmt->variableDeclaration.expr = NULL;
    if (match(parser, TOK_EQUAL, NULL)) {
        stmt->variableDeclaration.expr = expression(parser);
    }

    Location semiLocation = consume(parser, TOK_SEMICOLON, "expected ';' after declaration");
    stmt->location = locSpan(stmt->location, semiLocation);
    return stmt;
}

static Statement* expressionStatement(Parser* parser) {
    const Expression* expr = expression(parser);
    Statement* stmt = stmtNew(STATEMENT_EXPRESSION, exprLoc(expr));
    Location semiLocation = consume(parser, TOK_SEMICOLON, "expected ';'");
    stmt->expression = expr;
    stmt->location = locSpan(stmt->location, semiLocation);
    return stmt;
}

static Statement* block(Parser* parser, Location startLocation) {
    Token tok = tokenNull();
    StatementList list = stmtListNew();
    while (!match(parser, TOK_BRACE_RIGHT, &tok) && !nextIs(parser, TOK_EOF)) {
        Statement* inner = statement(parser);
        stmtListAppend(&list, inner);
        recover(parser);
    }

    Statement* stmt = stmtNew(STATEMENT_BLOCK, locSpan(startLocation, tokenLoc(&tok)));
    stmt->block = list;
    return stmt;
}

static Statement* parseReturn(Parser* parser, Location startLocation) {
    const Expression* expr = NULL;
    if (!nextIs(parser, TOK_SEMICOLON)) {
        expr = expression(parser);
    }
    Location endLocation = consume(parser, TOK_SEMICOLON, "expected ';'");

    Statement* stmt = stmtNew(STATEMENT_RETURN, locSpan(startLocation, endLocation));
    stmt->expression = expr;
    return stmt;
}

Statement* statement(Parser* parser) {
    Location startLocation = tokenLoc(peek(parser));

    if (nextIsType(parser)) {
        return variableDeclaration(parser, startLocation);
    } else if (match(parser, TOK_IF, NULL)) {
        return conditional(parser, startLocation);
    } else if (match(parser, TOK_WHILE, NULL)) {
        return whileLoop(parser, startLocation);
    } else if (match(parser, TOK_BRACE_LEFT, NULL)) {
        return block(parser, startLocation);
    } else if (match(parser, TOK_RETURN, NULL)) {
        return parseReturn(parser, startLocation);
    }
    return expressionStatement(parser);
}

Declaration* parseDeclaration(Parser* parser) {
    Location startLoc = tokenLoc(peek(parser));
    const Type* ty = type(parser);
    Identifier name = identifier(parser);

    /*if (match(parser, TOK_SQUARE_LEFT, NULL)) {
        const Expression* length = expression(parser);
        consume(parser, TOK_SQUARE_RIGHT, "expected ']'");

        
    }*/

    if (match(parser, TOK_SEMICOLON, NULL)) {
        return variableDeclarationNew(
                parser->staticLifetime,
                ty,
                name,
                locSpan(startLoc, identLoc(&name))
            );
    }

    consume(parser, TOK_PAREN_LEFT, "expected '(' or ';'");
    const char* unclosedMessage = "expected ')' or type";

    Parameter* parameters = NULL;
    size_t parameterCapacity = 0;
    size_t parameterCount = 0;
    while (!nextIs(parser, TOK_PAREN_RIGHT) && !nextIs(parser, TOK_EOF)) {
        const Type* paramType = type(parser);
        Identifier paramName = identifier(parser);
        Parameter param = (Parameter) {.type = paramType, .name = paramName};

        APPEND_ARRAY(
                parser->staticLifetime,
                parameters,
                Parameter,
                parameterCapacity,
                parameterCount,
                param
            );

        if (!match(parser, TOK_COMMA, NULL)) {
            unclosedMessage = "expected ')' or ','";
            break;
        }
    }
    Location endLoc = consume(parser, TOK_PAREN_RIGHT, unclosedMessage);

    Statement* body = NULL;
    Token tok = tokenNull();
    if (match(parser, TOK_BRACE_LEFT, &tok)) {
        body = block(parser, tokenLoc(&tok));
    } else {
        consume(parser, TOK_SEMICOLON, "expected ';'");
    }

    return functionDeclarationNew(
            parser->staticLifetime,
            ty,
            name,
            body,
            parameterCount,
            parameters,
            locSpan(startLoc, endLoc)
        );
}

Program parseProgram(Parser* parser) {
    Program progam = programNew();

    while (!nextIs(parser, TOK_EOF) && !parser->recoveringError) {
        Declaration* decl = parseDeclaration(parser);
        programPushDeclaration(&progam, decl);
    }

    return progam;
}

