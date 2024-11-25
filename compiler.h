#ifndef COMPILER_H
#define COMPILER_H

#include <stdlib.h>
#include "statement.h"
#include "instruction.h"

typedef struct {
    Identifier ident;
    Type* type;
    int depth;
    size_t offset;
} StackElement;

struct Compiler;

typedef struct {
    struct Compiler* compiler;
    Instruction* instructions;
    size_t instructionsLength;
    size_t instructionsCapacity;
    StackElement* stack;
    size_t stackLength;
    size_t stackCapacity;
    size_t currentStackSize;
    size_t maxStackSize;
    int scopeDepth;
} Function;

typedef struct Compiler {
    int label;
    Arena* staticLifetime;
} Compiler;

Compiler compilerNew(Arena* staticLifetime);

void compileFunction(Compiler* compiler, Statement* stmt);

#endif

