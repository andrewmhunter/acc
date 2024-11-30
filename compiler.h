#ifndef COMPILER_H
#define COMPILER_H

#include <stdlib.h>
#include "statement.h"
#include "instruction.h"

typedef struct {
    Identifier ident;
    const Type* type;
    int depth;
    size_t offset;
} StackElement;

struct Compiler;

typedef struct {
    struct Compiler* compiler;
    Arena* lifetime;
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
    Arena* lifetime;
} Compiler;

Compiler compilerNew(Arena* staticLifetime);

void compileFunction(Compiler* compiler, Statement* stmt);

#endif

