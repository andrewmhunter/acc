#include "compiler.h"
#include "mem.h"
#include "util.h"
#include "diag.h"
#include "condition.h"
#include <stdio.h>

/// ############################################################################
/// Initalizers
/// ############################################################################

//static void globalHash(Hash* hash, const void* global) {
//    Global* g = (Global*)global;
//    hashBytesSized(hash, g->name.start, g->name.length);
//}
//
//static bool globalEquals(const void* left, const void* right) {
//    return identEquals(((Global*)left)->name, ((Global*)right)->name);
//}

Compiler compilerNew(Arena* staticLifetime, Diagnostics* diag, Declaration* const* declarations) {
    return (Compiler) {
        .lifetime = staticLifetime,
        .diag = diag,
        .label = 0,
        //.globals = setNew(globalHash, globalEquals),
        .globalCount = 0,
        .globals = declarations,
    };
}

Function functionNew(Compiler* compiler, const FunctionDeclaration* decl) {
    return (Function) {
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
        .scopeDepth = 0,
    };
}

/// ############################################################################
/// Instruction generation
/// ############################################################################

static void appendInstruction(Function* func, Instruction instruction) {
    printInstruction(stdout, &instruction);
    APPEND_ARRAY(
        NULL,
        func->instructions,
        Instruction,
        func->instructionsCapacity,
        func->instructionsLength,
        instruction
    );
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
    return valueImmediateExpr(typeAnyInteger(), exprLabel(compiler->lifetime, label, NO_LOCATION));
}

static Value pushStackLocation(Function* func, const Type* type, Identifier ident, Location loc) {
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
    element->type = type;
    element->ident = ident;
    element->depth = func->scopeDepth;
    element->offset = offset;

    func->currentStackSize += typeSize(type);
    func->maxStackSize = MAX(func->maxStackSize, func->currentStackSize);

    if (typeSize(type) > 1) {
        fprintf(stdout, ";  Alloc: %lu -> %lu: ", offset, offset + typeSize(type) - 1);
    } else {
        fprintf(stdout, ";  Alloc: %lu: ", offset);
    }

    if (ident.start != NULL) {
        identPrint(stdout, &ident);
    } else {
        fprintf(stdout, "?");
    }
    fprintf(stdout, "\n");


    return valueStackOffset(func->lifetime, type, &func->decl->name, offset, loc);
}

Value pushStack(Function* func, const Type* type, Identifier ident) {
    return pushStackLocation(func, type, ident, identLoc(&ident));
}

Value pushStackAnon(Function* func, const Type* type, Location loc) {
    Identifier id = {.start = NULL, .length = 0};
    return pushStackLocation(func, type, id, loc);
}

void popStack(Function* func) {
    StackElement* element = &func->stack[func->stackLength - 1];
    func->currentStackSize -= typeSize(element->type);
    --func->stackLength;

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
            return valueStackOffset(
                    func->lifetime,
                    func->stack[i].type,
                    &func->decl->name,
                    func->stack[i].offset,
                    identLoc(&ident)
                );
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

    return valueDirectExpr(decl->variable.type, exprVariable(func->lifetime, ident));
}

Value getFuncReturnValue(Arena* arena, const FunctionDeclaration* decl, Location location) {
    return valueStackOffset(arena, decl->returnType, &decl->name, 0, location);
}

