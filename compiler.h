#ifndef COMPILER_H
#define COMPILER_H

#define PRINT_DELETED
#define PRINT_ALLOCATIONS
#define EMIT_COMMENTS_STATEMENT
//#define EMIT_COMMENTS_LOCATION

#include <stdlib.h>
#include "instruction.h"
//#include "hash.h"
#include "statement.h"
#include "diag.h"

typedef struct {
    const Type* type;
    // Value value;
    Identifier ident;
    int depth;
    int offset;
} StackElement;

struct Compiler;

typedef struct Function {
    struct Compiler* compiler;
    Arena* lifetime;
    Diagnostics* diag;
    const FunctionDeclaration* decl;
    Instruction* instructions;
    size_t instructionsLength;
    size_t instructionsCapacity;
    StackElement* stack;
    size_t stackLength;
    size_t stackCapacity;
    size_t currentStackSize;
    size_t maxStackSize;
    int scopeDepth;
    struct Function* nextFunction;
} Function;

typedef struct {
    Identifier name;
    Declaration* decl;
} Global;

typedef struct Compiler {
    Arena* lifetime;
    Diagnostics* diag;
    int label;
    int globalCount;
    Declaration* const* globals;
    int optimizationLevel;
} Compiler;

Compiler compilerNew(Arena* staticLifetime, Diagnostics* diag, Declaration* const* declarations, int optimizationLevel);
Function* functionNew(Arena* arena, Compiler* compiler, const FunctionDeclaration* decl);

void printFunction(FILE* file, const Function* func);

void emitComment(Function* func, const char* comment);
void emitCommentf(Function* func, const char* format, ...);
void emitCommentStatement(Function* func, const Statement* stmt);
void emitCommentLocation(Function* func, Location loc);
void emitCommentAllocation(Function* func, Identifier ident, int offset, int size);

void emitImplied(Function* func, Opcode opcode);
void emitReg(Function* func, Opcode opcode, Reg reg);
void emitValue(Function* func, Opcode opcode, Value value, int index);
void emitRegReg(Function* func, Opcode opcode, Reg dest, Reg src);
void emitRegValue(Function* func, Opcode opcode, Reg dest, Value src, int index);
void emitValueReg(Function* func, Opcode opcode, Value dest, int index, Reg src);
void emitRegIndexed(Function* func, Opcode opcode, Reg dest);
void emitIndexedReg(Function* func, Opcode opcode, Reg src);
void emitLabel(Function* func, Value label);

void load(Function* func, Reg reg, Value src, int index);
void store(Function* func, Value dest, int index, Reg reg);
void transfer(Function* func, Reg dest, Reg src);

Value createLabel(Compiler* compiler);

Value pushStack(Function* func, const Type* type, Identifier ident);
Value pushStackAnon(Function* func, const Type* type, Location location);
void popStack(Function* func);

void emitJump(Function* func, Value value);
void conditionalJump(Function* func, Condition condition, Value value);

void enterScope(Function* func);
void leaveScope(Function* func);

const Declaration* lookupGlobal(Compiler* compiler, Identifier ident);
Value lookupSymbol(Function* func, Identifier ident);

Value getFuncReturnValue(Arena* arena, const FunctionDeclaration* decl, Location location);

#endif

