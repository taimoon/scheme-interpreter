#pragma once
#include <stdint.h>
#include <stddef.h>

#ifndef assert
#define assert(expr) ((expr) ? (void)(0) : panic(__FUNCTION__))
#endif

void panic(const char *msg, ...);

void sys_init(uint8_t **heap_start, uint8_t **heap_end);

size_t strlen(const char* s);

void _s_print_int(intptr_t d);

void _s_print_uint(uintptr_t u);

void _s_putc(int c);

void _s_puts(const char* s);

int getchar(void);

int putchar(int);

void exit(int);