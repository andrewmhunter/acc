#include "diag.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>

void panic(const char* file, int line, const char* function, const char* format, ...) {
    va_list args;
    va_start(args, format);
    vpanic(file, line, function, format, args);
    va_end(args);
}

void vpanic(const char* file, int line, const char* function, const char* format, va_list args) {
    fprintf(stderr, "panic at: %s:%d: %s: ", file, line, function);
    vfprintf(stderr, format, args);
    fprintf(stderr, "\n");

#ifdef BREAK_ON_PANIC
    raise(SIGTRAP);
#endif

    exit(4);
}

