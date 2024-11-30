#include <stdio.h>
#include <stdlib.h>
#include "scanner.h"
#include "compiler.h"
#include "parser.h"
#include "mem.h"

int main(int argc, char** argv) {
    if (argc < 2) {
        fprintf(stderr, "missing file name\n");
        return 1;
    }

    FILE* file = fopen(argv[1], "r");
    if (!file) {
        perror("Error");
        exit(1);
    }

    fseek(file, 0, SEEK_END);
    long fileSize = ftell(file);
    rewind(file);
    char* text = malloc(fileSize + 1);
    fread(text, sizeof(char), fileSize, file);
    text[fileSize] = '\0';
    fclose(file);

    Arena staticLifetime = arenaNew();

    Scanner scan = newScanner(text);
    Parser parser = newParser(&staticLifetime, scan);

    Statement* stmt = statement(&parser);

    if (parser.hadError) {
        return 2;
    }

    stmtPrint(stdout, stmt, 0, false);

    Compiler compiler = compilerNew(&staticLifetime);
    compileFunction(&compiler, stmt);

    stmtFree(stmt);

    arenaFree(&staticLifetime);
    free(text);
    return 0;
}

