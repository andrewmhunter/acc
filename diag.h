#ifndef DIAG_H
#define DIAG_H

#include <stdbool.h>
#include <stdarg.h>

#define BREAK_ON_PANIC

#define TRY(EXPR) \
    do { \
        if (!(EXPR)) { \
            return false; \
        } \
    } while (false)


[[noreturn]]
void panic(const char* file, int line, const char* function, const char* format, ...);
[[noreturn]]
void vpanic(const char* file, int line, const char* function, const char* format, va_list args);

#define PANIC(FORMAT, ...) \
    panic(__FILE__, __LINE__, __PRETTY_FUNCTION__, (FORMAT) __VA_OPT__(,) __VA_ARGS__)

#define UNREACHABLE() \
    PANIC("unreachable code")

#define UNIMPLEMENTED() \
    PANIC("not yet implemented")

#define ASSERT(CONDITION, FORMAT, ...) \
    do { \
        if (!(CONDITION)) { \
            PANIC((FORMAT) __VA_OPT__(,) __VA_ARGS__); \
        } \
    } while (0)


#endif

