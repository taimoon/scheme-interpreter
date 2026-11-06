#pragma once
#include <stdint.h>
#include <stddef.h>
#include <stdnoreturn.h>

#ifndef assert
#define assert(expr) ((expr) ? (void)(0) : panic(__func__))
#endif

typedef struct s_heap_config_t {
    size_t sz;
    void* fromspace_start;
    void* fromspace_end;
    void* tospace_start;
    void* tospace_end;
    void* gc_markers;
} s_heap_config_t;

noreturn void panic(const char *msg, ...);

s_heap_config_t sys_init();

size_t strlen(const char* s);

void _s_print_int(intptr_t d);

void _s_print_uint(uintptr_t u);

void _s_putc(int c);

void _s_puts(const char* s);

int getchar(void);

int putchar(int);

void exit(int);