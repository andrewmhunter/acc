#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "scanner.h"
#include "compiler.h"
#include "parser.h"
#include "mem.h"
#include "compile_statement.h"
#include "diag.h"
#include "optimizer.h"

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

typedef struct {
    char* const* argv;
    int argc;
    int index;
    bool error;
} ArgumentParser;

static bool checkArg(ArgumentParser* args, const char* argName, bool hasArgument) {
    if (strcmp(args->argv[args->index], argName) == 0) {
        args->index += 1;
        if (hasArgument && args->index >= args->argc) {
            args->error = true;
            fprintf(stderr, "Error: missing parameter to argument '%s'\n", argName);
            return false;
        }
        return true;
    }
    return false;
}

int main(int argc, char** argv) {
    ArgumentParser args = (ArgumentParser) {
        .argv = argv,
        .argc = argc,
        .index = 1,
        .error = false,
    };

    int optimizationLevel = DEFAULT_OPTIMIZATION_LEVEL;
    const char* defaultFilename = "<stdin>";
    char* text = NULL;

    while (args.index < argc) {
        if (checkArg(&args, "-O0", false)) {
            optimizationLevel = 0;
        } else if (checkArg(&args, "-O1", false)) {
            optimizationLevel = 1;
        } else if (checkArg(&args, "-O2", false)) {
            optimizationLevel = 2;
        } else if (checkArg(&args, "-O3", false)) {
            optimizationLevel = 3;
        } else if (checkArg(&args, "--", false)) {
            text = loadStdin();
        } else {
            if (strlen(argv[args.index]) > 0 && argv[args.index][0] == '-') {
                fprintf(stderr, "Error: invalid command line argument '%s'\n", argv[args.index]);
                args.error = true;
            } else if (!args.error) {
                defaultFilename = argv[args.index];
                text = loadFile(argv[args.index]);
            }
            args.index++;
        }
    }

    if (args.error) {
        return 1;
    }

    if (text == NULL) {
        fprintf(stderr, "Error: no input file\n");
        return 1;
    }

    Arena staticLifetime = arenaNew();
    Diagnostics diag = newDiagnostics(defaultFilename, text);

    Scanner scan = newScanner(text);
    Parser parser = newParser(&staticLifetime, &diag, scan);

    Program program = parseProgram(&parser);

    if (diag.errorCount != 0) {
        return 2;
    }

    Compiler compiler = compilerNew(&staticLifetime, &diag, program.declarations, optimizationLevel);
    compileProgram(&compiler, &program);

    arenaFree(&staticLifetime);
    free(text);

    if (diag.errorCount != 0) {
        return 3;
    }
    return 0;
}

