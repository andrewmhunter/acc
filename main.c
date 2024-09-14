#include <stdio.h>
#include <stdlib.h>
#include "scanner.h"
#include "parser.h"

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

    Scanner scan = newScanner(text);
    Parser parser = newParser(scan);

    Statement* stmt = statement(&parser);
    stmtPrint(stmt, 0);
    stmtFree(stmt);

    free(text);
    return 0;
}

