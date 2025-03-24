#ifndef SCANNER_H
#define SCANNER_H

#include <stdbool.h>
#include "diag.h"

typedef enum {
    TOK_PRAGMA,
    TOK_INCLUDE_ASM,

    TOK_PAREN_LEFT,
    TOK_PAREN_RIGHT,
    TOK_BRACE_RIGHT,
    TOK_BRACE_LEFT,
    TOK_SQUARE_LEFT,
    TOK_SQUARE_RIGHT,

    TOK_SEMICOLON,
    TOK_COLON,
    TOK_COMMA,
    TOK_EQUAL,
    TOK_EQUAL_EQUAL,
    TOK_NOT_EQUAL,
    TOK_BANG,

    TOK_NUMBER,
    TOK_IDENTIFIER,
    TOK_STRING,
    TOK_CHARLITERAL,

    TOK_LESS,
    TOK_LESS_EQUAL,
    TOK_GREATER,
    TOK_GREATER_EQUAL,

    TOK_IF,
    TOK_ELSE,
    TOK_WHILE,
    TOK_DO,
    TOK_FOR,
    TOK_GOTO,
    TOK_SIZEOF,
    TOK_RETURN,
    TOK_BREAK,
    TOK_CONTINUE,

    TOK_INT,
    TOK_CHAR,
    TOK_INT24_T,
    TOK_LONG,
    TOK_UNSIGNED,
    TOK_VOID,

    TOK_PLUS,
    TOK_MINUS,
    TOK_ASTERIX,
    TOK_SLASH,

    TOK_LEFT_SHIFT,
    TOK_RIGHT_SHIFT,

    TOK_BIT_AND,
    TOK_BIT_OR,
    TOK_BIT_XOR,

    TOK_LOG_AND,
    TOK_LOG_OR,

    TOK_EOF,
    TOK_ERROR,
} TokenType;

typedef struct {
    TokenType kind;
    int length;
    const char* start;
    Position position;
} Token;

typedef struct {
    Diagnostics* diag;
    const char* textStart;
    const char* tokenStart;
    const char* tokenEnd;
    bool hasPeek;
    Token peekValue;
} Scanner;

Scanner newScanner(Diagnostics* diag, const char* text);
Token nextToken(Scanner* scan);
//Token peek(Scanner* scan);
Token tokenNull();

#endif

