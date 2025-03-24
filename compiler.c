#include "compiler.h"
#include "mem.h"
#include "util.h"
#include "diag.h"
#include "condition.h"
#include "match.h"
#include <stdio.h>
#include <string.h>

/// ############################################################################
/// Initalizers
/// ############################################################################

static void stringLiteralHash(Hash* hash, const void* stringLiteral) {
    const StringLiteral* sl = stringLiteral;
    hashBytesSized(hash, sl->data, sl->length);
}

static bool stringLiteralEquals(const void* string0, const void* string1) {
    const StringLiteral* sl0 = string0;
    const StringLiteral* sl1 = string1;

    return sl0->length == sl1->length && memcmp(sl0->data, sl1->data, sl0->length) == 0;
}

Compiler compilerNew(Arena* staticLifetime, Diagnostics* diag, Declaration* const* declarations, int optimizationLevel) {
    return (Compiler) {
        .lifetime = staticLifetime,
        .diag = diag,
        .label = 0,
        .globalCount = 0,
        .globals = declarations,
        .optimizationLevel = optimizationLevel,
        .strings = setNew(stringLiteralHash, stringLiteralEquals),
    };
}

Function* functionNew(Arena* arena, Compiler* compiler, const FunctionDeclaration* decl) {
    Function* func = ARENA_ALLOC(arena, Function);
    *func = (Function) {
        .compiler = compiler,
        .lifetime = compiler->lifetime,
        .diag = compiler->diag,
        .decl = decl,
        .instructions = NULL,
        .instructionsLength = 0,
        .instructionsCapacity = 0,
        .stack = NULL,
        .stackLength = 0,
        .stackCapacity = 0,
        .currentStackSize = 0,
        .maxStackSize = 0,
        .breakContinueStack = NULL,
        .breakContinueLength = 0,
        .breakContinueCapacity = 0,
        .scopeDepth = 0,
        .nextFunction = NULL,
    };
    return func;
}

/// ############################################################################
/// Instruction generation
/// ############################################################################

void insertInstruction(Function* func, Instruction instruction, size_t index) {
    func->instructions = EXTEND_ARRAY(NULL, func->instructions, Instruction, &func->instructionsCapacity, func->instructionsLength, 1);
    for (size_t i = func->instructionsLength - 1; i >= index; ++i) {
        func->instructions[i + 1] = func->instructions[i];
    }
    func->instructionsLength++;
    func->instructions[index] = instruction;
}

void appendInstruction(Function* func, Instruction instruction) {
    //printInstruction(stdout, &instruction);
    APPEND_ARRAY(
        NULL,
        func->instructions,
        Instruction,
        func->instructionsCapacity,
        func->instructionsLength,
        instruction
    );
}

void emitComment(Function* func, const char* comment) {
    appendInstruction(func, (Instruction) {.opcode = INS_COMMENT, .comment = {.string = comment}});
}

void emitCommentf(Function* func, const char* format, ...) {
    va_list args;
    va_start(args, format);
    const char* comment = arenaVsprintf(func->lifetime, format, args);
    va_end(args);
    emitComment(func, comment);
}

void emitCommentStatement(Function* func, const Statement* stmt) {
    appendInstruction(func, (Instruction) {.opcode = INS_COMMENT_STATEMENT, .comment = {.statement = stmt}});
}

void emitCommentLocation(Function* func, Location loc) {
    appendInstruction(func, (Instruction) {.opcode = INS_COMMENT_LOCATION, .comment = {.location = loc}});
}

void emitCommentAllocation(Function* func, Identifier ident, int offset, int size) {
    appendInstruction(func, (Instruction) {
        .opcode = INS_COMMENT_ALLOCATION, .comment = {
            .allocation = {
                .ident = ident,
                .offset = offset,
                .size = size
            }
        }
    });
}


void emitImplied(Function* func, Opcode opcode) {
    appendInstruction(func, instruction0(opcode));
}

void emitReg(Function* func, Opcode opcode, Reg reg) {
    appendInstruction(func, instruction1(opcode, addressReg(reg)));
}

void emitValue(Function* func, Opcode opcode, Value value, int index) {
    if (value.kind == VALUE_DISCARD) {
        UNREACHABLE();
    }
    appendInstruction(func, instruction1Value(
                func->lifetime,
                opcode,
                GET_ADDRESS_VALUE(),
                value,
                index
            ));
}

void emitRegReg(Function* func, Opcode opcode, Reg dest, Reg src) {
    appendInstruction(func, instruction2(opcode, addressReg(dest), addressReg(src)));
}

void emitRegValue(Function* func, Opcode opcode, Reg dest, Value src, int index) {
    if (src.kind == VALUE_DISCARD) {
        UNREACHABLE();
    }
    appendInstruction(func, instruction2Value(
                func->lifetime,
                opcode,
                addressReg(dest),
                GET_ADDRESS_VALUE(),
                src,
                index
            ));
}

void emitValueReg(Function* func, Opcode opcode, Value dest, int index, Reg src) {
    if (dest.kind == VALUE_DISCARD) {
        UNREACHABLE();
    }
    appendInstruction(func, instruction2Value(
                func->lifetime,
                opcode,
                GET_ADDRESS_VALUE(),
                addressReg(src),
                dest,
                index
            ));
}

void emitRegIndexed(Function* func, Opcode opcode, Reg dest) {
    appendInstruction(func, instruction2(
                opcode,
                addressReg(dest),
                (Address) {.kind = ADDRESS_INDEXED}
            ));
}

void emitIndexedReg(Function* func, Opcode opcode, Reg src) {
    appendInstruction(func, instruction2(
                opcode,
                (Address) {.kind = ADDRESS_INDEXED},
                addressReg(src)
            ));
}

void emitLabel(Function* func, Value label) {
    emitValue(func, INS_LABEL, label, 0);
}

void load(Function* func, Reg reg, Value src, int index) {
    emitRegValue(func, INS_MOV, reg, src, index);
}

void store(Function* func, Value dest, int index, Reg reg) {
    if (dest.kind == VALUE_DISCARD) {
        return;
    }
    emitValueReg(func, INS_MOV, dest, index, reg);
}

void transfer(Function* func, Reg dest, Reg src) {
    emitRegReg(func, INS_MOV, dest, src);
}

Value createLabel(Compiler* compiler) {
    Label label = compiler->label++;
    return valueImmediateExpr(&typeAnyInt, exprLabel(compiler->lifetime, label, NO_LOCATION));
}

Value pushStackValue(Function* func, Value value, Identifier ident) {
    func->stack = EXTEND_ARRAY(
        func->lifetime,
        func->stack,
        StackElement,
        &func->stackCapacity,
        func->stackLength,
        1
    );
    size_t offset = func->currentStackSize;

    StackElement* element = &func->stack[func->stackLength++];
    *element = (StackElement) {
        .value = value,
        .ident = ident,
        .depth = func->scopeDepth,
        .offset = offset,
    };

    return value;
}

static Value pushStackLocation(Function* func, const Type* type, Identifier ident, Location loc) {
    size_t offset = func->currentStackSize;
    Value value = valueStackOffset(func->lifetime, type, &func->decl->name, offset, loc);
    value = pushStackValue(func, value, ident);
    func->currentStackSize += typeSize(type);
    func->maxStackSize = MAX(func->maxStackSize, func->currentStackSize);

#ifdef PRINT_ALLOCATIONS
    emitCommentAllocation(func, ident, offset, typeSize(type));
#endif

    return value;
}

Value pushStackArray(Function* func, const Type* type, Identifier ident) {
    size_t offset = func->currentStackSize;
    Value value = valueStackOffsetImmediate(func->lifetime, type, &func->decl->name, offset, identLoc(&ident));
    value = pushStackValue(func, value, ident);
    func->currentStackSize += typeSize(type);
    func->maxStackSize = MAX(func->maxStackSize, func->currentStackSize);

#ifdef PRINT_ALLOCATIONS
    emitCommentAllocation(func, ident, offset, typeSize(type));
#endif

    return value;
}

Value pushStack(Function* func, const Type* type, Identifier ident) {
    if (isArray(type)) {
        return pushStackArray(func, type, ident);
    }
    return pushStackLocation(func, type, ident, identLoc(&ident));
}

Value pushStackAnon(Function* func, const Type* type, Location loc) {
    ASSERT(!isArray(type), "anonymous arrays are not allowed");
    Identifier id = {.start = NULL, .length = 0};
    return pushStackLocation(func, type, id, loc);
}

void popStack(Function* func) {
    StackElement* element = &func->stack[func->stackLength - 1];
    func->currentStackSize = element->offset;
    --func->stackLength;

    // TODO: ERROR: Won't work because of unsigned integers.
    ASSERT(func->stackLength >= 0 && func->currentStackSize >= 0, "stack empty");
}

void emitJump(Function* func, Value value) {
    emitValue(func, INS_JMP, value, 0);
}

void conditionalJump(Function* func, Condition condition, Value value) {
    Opcode ins = INS_JMP;

    if (!condition.negate) {
        switch (condition.flag) {
            case FLAG_Z:
                ins = INS_JZ;
                break;
            case FLAG_C:
                ins = INS_JC;
                break;
            case FLAG_N:
                ins = INS_JN;
                break;
            case FLAG_ALWAYS:
                ins = INS_JMP;
                break;
        }
    } else {
        switch (condition.flag) {
            case FLAG_Z:
                ins = INS_JNZ;
                break;
            case FLAG_C:
                ins = INS_JNC;
                break;
            case FLAG_N:
                ins = INS_JP;
                break;
            case FLAG_ALWAYS:
                return;
        }
    }

    emitValue(func, ins, value, 0);
}

void enterScope(Function* func) {
    func->scopeDepth++;
}

void leaveScope(Function* func) {
    ASSERT(func->scopeDepth > 0, "already at outermost scope");
    
    func->scopeDepth--;
    for (int i = func->stackLength - 1; i >= 0; --i) {
        StackElement* element = &func->stack[i];
        if (element->depth <= func->scopeDepth) {
            break;
        }

        popStack(func);
    }
}

void pushBreakContinue(Function* func, Value breakValue, Value continueValue, bool hasContinue) {
    BreakContinue bc = {
        .breakValue = breakValue,
        .continueValue = continueValue,
        .hasContinue = hasContinue
    };

    APPEND_ARRAY(func->lifetime, func->breakContinueStack, BreakContinue, func->breakContinueCapacity, func->breakContinueLength, bc);
}

void popBreakContinue(Function* func) {
    ASSERT(func->breakContinueLength > 0, "pop on empty break-continue stack ");
    --func->breakContinueLength;
}

const Declaration* lookupGlobal(Compiler* compiler, Identifier ident) {
    for (int i = 0; i < compiler->globalCount; ++i) {
        const Declaration* decl = compiler->globals[i];
        if (identEquals(&decl->name, &ident)) {
            return decl;
        }
    }
    return NULL;
}

Value lookupSymbol(Function* func, Identifier ident) {
    for (int i = func->stackLength - 1; i >= 0; --i) {
        if (identEquals(&ident, &func->stack[i].ident)) {
            return func->stack[i].value;
        }
    }

    const Declaration* decl = lookupGlobal(func->compiler, ident);

    if (decl == NULL) {
        printError(func->diag, identLoc(&ident),
                "'%.*s' is not declared",
                ident.length, ident.start);
        return valueError();
    }

    if (decl->kind != DECL_VARIABLE) {
        printError(func->diag, identLoc(&ident),
                "'%.*s' is declared as a function",
                ident.length, ident.start);
        return valueError();
    }

    if (isArray(decl->type)) {
        return valueImmediateExpr(decl->variable.type, exprVariable(func->lifetime, ident));
    }
    return valueDirectExpr(decl->variable.type, exprVariable(func->lifetime, ident));
}

Value getFuncReturnValue(Arena* arena, const FunctionDeclaration* decl, Location location) {
    return valueStackOffset(arena, decl->returnType, &decl->name, 0, location);
}

void printFunction(FILE* file, const Function* func) {
    const FunctionDeclaration* decl = func->decl;

    int insCount = 0;

    fprintf(file, "%.*s:\n", decl->name.length, decl->name.start);
    for (size_t i = 0; i < func->instructionsLength; ++i) {
        const Instruction* ins = &func->instructions[i];

#ifndef PRINT_DELETED
        if (ins->opcode == INS_DELETED) {
            continue;
        }
#endif

        printInstruction(file, ins, func->diag);

        if (!opcodeSkip(ins->opcode) && ins->opcode != INS_LABEL) {
            insCount += 1;
        }
    }

    fprintf(file, "variable _Stack_%.*s %lu\n", decl->name.length, decl->name.start, func->maxStackSize);
    fprintf(file, "\n; Instruction count: %d\n\n", insCount);
}

Value internString(Compiler* compiler, const char* data, int length) {
    StringLiteral string = {
        .data = data,
        .length = length,
    };

    if (setHas(&compiler->strings, &string)) {
        StringLiteral* interned = setGet(&compiler->strings, &string);
        return interned->label;
    }

    StringLiteral* allocated = ARENA_ALLOC(compiler->lifetime, StringLiteral);
    *allocated = string;
    allocated->label = createLabel(compiler);
    allocated->label.type = typePointer(compiler->lifetime, &typeChar);
    setInsert(&compiler->strings, allocated);
    return allocated->label;
}

void emitInternedStrings(Compiler* compiler) {
    Entry* entry = NULL;
    while ((entry = setIterate(&compiler->strings, entry))) {
        StringLiteral* string = entry->data;
        printValue(stdout, &string->label);
        fprintf(stdout, ":\n    data \"%.*s\\0\"\n", string->length, string->data);
    }
}

