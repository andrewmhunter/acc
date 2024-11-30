#include "scanner.h"
#include <stdbool.h>
#include <string.h>
#include <ctype.h>

Scanner newScanner(const char* text) {
    Scanner scan = {.tokenStart = text, .tokenEnd = text, .line = 1};
    return scan;
}

static Token makeToken(Scanner* scan, TokenType kind) {
    Token token = {
        .kind = kind,
        .start = scan->tokenStart,
        .length = scan->tokenEnd - scan->tokenStart,
        .line = scan->line,
    };
    scan->tokenStart = scan->tokenEnd;
    scan->tokenEnd = scan->tokenStart;
    return token;
}

static Token makeError(Scanner* scan, const char* message) {
    Token token = {
        .kind = TOK_ERROR,
        .start = message,
        .length = strlen(message)
    };
    scan->tokenStart = scan->tokenEnd;
    scan->tokenEnd = scan->tokenStart;
    return token;
}

static char peek(Scanner* scan) {
    return scan->tokenEnd[0];
}

static char peekNext(Scanner* scan) {
    if (peek(scan) == '\0') {
        return '\0';
    }
    return scan->tokenEnd[1];
}

static char nextChar(Scanner* scan) {
    char ch = *scan->tokenEnd++;
    return ch;
}

static bool match(Scanner* scan, char matching) {
    if (peek(scan) == matching) {
        scan->tokenEnd++;
        return true;
    }
    return false;
}

static Token integer(Scanner* scan) {
    /*int base = 10;
    if (match(scan, '0')) {
        char next = peek(scan);
        switch (next) {
            case 'x':
            case 'X':
                base = 16;
                break;
            case 'b':
            case 'B':
                base = 2;
                break;
            default:
                if (isdigit(next)) {
                    base = 8;
                }
                break;
        }
    }*/
    //while (isdigit(peek(scan))) {
    while (isalnum(peek(scan))) {
        nextChar(scan);
    }
    return makeToken(scan, TOK_NUMBER);
}

static TokenType compareKeyword(
        Scanner* scan,
        int start,
        int size,
        const char* keyword,
        TokenType kind
) {
    if ((scan->tokenEnd - scan->tokenStart - start) == size 
            && memcmp(scan->tokenStart + start, keyword, size) == 0
    ) {
        return kind;
    }
    return TOK_IDENTIFIER;
}

static TokenType identifierType(Scanner* scan) {
    switch (scan->tokenStart[0]) {
        case 'c':
            return compareKeyword(scan, 1, 3, "har", TOK_CHAR);
        case 'e':
            return compareKeyword(scan, 1, 3, "lse", TOK_ELSE);
        case 'i':
            switch (scan->tokenStart[1]) {
                case 'f':
                    return compareKeyword(scan, 2, 0, "", TOK_IF);
                case 'n':
                    return compareKeyword(scan, 2, 1, "t", TOK_INT);
            }
            break;
        case 'u':
            return compareKeyword(scan, 1, 7, "nsigned", TOK_UNSIGNED);
        case 'v':
            return compareKeyword(scan, 1, 3, "oid", TOK_VOID);
        case 'w':
            return compareKeyword(scan, 1, 4, "hile", TOK_WHILE);
    }
    return TOK_IDENTIFIER;
}

static Token identifier(Scanner* scan) {
    while (isalnum(peek(scan)) || peek(scan) == '_') {
        nextChar(scan);
    }
    return makeToken(scan, identifierType(scan));
}

static void skipChar(Scanner* scan) {
    scan->tokenStart++;
    scan->tokenEnd++;
}

static void skipWhitespace(Scanner* scan) {
    for (;;) {
        switch (peek(scan)) {
            case ' ':
            case '\n':
            case '\r':
            case '\t':
                skipChar(scan);
                break;
            default:
                return;
        }
    }
}

static Token makeEither(Scanner* scan, TokenType regular, char second, TokenType other) {
    if (match(scan, second)) {
        return makeToken(scan, other);
    }
    return makeToken(scan, regular);
}

static Token makeEither3(
        Scanner* scan,
        TokenType token0,
        char char1,
        TokenType token1,
        char char2,
        TokenType token2
) {
    if (match(scan, char1)) {
        return makeToken(scan, token1);
    } else if (match(scan, char2)) {
        return makeToken(scan, token2);
    }
    return makeToken(scan, token0);
}

Token nextToken(Scanner* scan) {
    skipWhitespace(scan);

    if (match(scan, '\0')) {
        return makeToken(scan, TOK_EOF);
    }

    if (peek(scan) >= '0' && peek(scan) <= '9') {
        return integer(scan);
    }
    if (isalpha(peek(scan)) || peek(scan) == '_') {
        return identifier(scan);
    }

    switch (nextChar(scan)) {
        case ';':
            return makeToken(scan, TOK_SEMICOLON);
        case '+':
            return makeToken(scan, TOK_PLUS);
        case '-':
            return makeToken(scan, TOK_MINUS);
        case '*':
            return makeToken(scan, TOK_ASTERIX);
        case '/':
            return makeToken(scan, TOK_SLASH);
        case '(':
            return makeToken(scan, TOK_PAREN_LEFT);
        case ')':
            return makeToken(scan, TOK_PAREN_RIGHT);
        case '{':
            return makeToken(scan, TOK_BRACE_LEFT);
        case '}':
            return makeToken(scan, TOK_BRACE_RIGHT);
        case '&':
            return makeEither(scan, TOK_BIT_AND, '&', TOK_LOG_AND);
        case '|':
            return makeEither(scan, TOK_BIT_OR, '|', TOK_LOG_OR);
        case '=':
            return makeEither(scan, TOK_EQUAL, '=', TOK_EQUAL_EQUAL);
        case '<':
            return makeEither3(scan, TOK_LESS, '=', TOK_LESS_EQUAL, '<', TOK_LEFT_SHIFT);
        case '>':
            return makeEither3(scan, TOK_GREATER, '=', TOK_GREATER_EQUAL, '>', TOK_RIGHT_SHIFT);
        case '!':
            return makeEither(scan, TOK_BANG, '=', TOK_NOT_EQUAL);
    }

    return makeError(scan, "unexpected character");
}

