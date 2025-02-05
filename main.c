#include <stdio.h>
#include <stdlib.h>
#include "scanner.h"
#include "compiler.h"
#include "parser.h"
#include "mem.h"
#include "compile_statement.h"
#include "diag.h"

char* loadStdin() {
    const int chunkSize = 32;
    char* text = NULL;
    size_t bytesRead = 0;
    size_t size = 0;
    do {
        text = realloc(text, size + chunkSize);
        bytesRead = fread(text + size, 1, chunkSize, stdin);
        size += bytesRead;

    } while(bytesRead == chunkSize);

    text[size] = '\0';
    return text;
}

char* loadFile(const char* fileName) {
    FILE* file = fopen(fileName, "r");
    if (!file) {
        perror("Error");
        return NULL;
    }

    fseek(file, 0, SEEK_END);
    long fileSize = ftell(file);
    rewind(file);
    char* text = malloc(fileSize + 1);
    fread(text, sizeof(char), fileSize, file);
    text[fileSize] = '\0';
    fclose(file);

    return text;
}

int main(int argc, char** argv) {
    char* text = NULL;
    switch (argc) {
        case 1:
            text = loadStdin();
            break;
        case 2:
            text = loadFile(argv[1]);
            break;
        default:
            fprintf(stderr, "incorrect command line arguments\n");
            return 1;
    }

    if (text == NULL) {
        return 1;
    }

    Arena staticLifetime = arenaNew();
    Diagnostics diag = newDiagnostics(text);

    Scanner scan = newScanner(text);
    Parser parser = newParser(&staticLifetime, &diag, scan);

    Program program = parseProgram(&parser);

    if (diag.errorCount != 0) {
        return 2;
    }

    Compiler compiler = compilerNew(&staticLifetime, &diag, program.declarations);
    compileProgram(&compiler, &program);

    arenaFree(&staticLifetime);
    free(text);

    if (diag.errorCount != 0) {
        return 3;
    }
    return 0;
}

