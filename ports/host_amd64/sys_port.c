#include "sys_port.h"
#include <stdio.h>
#include <stdlib.h>

void _s_print_int(intptr_t d) {
    printf("%d", d);
}

void _s_putc(int c) {
    putchar(c);
}

void _s_puts(const char* s) {
    printf("%s", s);
}

void sys_init(uint8_t **heap_start, uint8_t **heap_end) {
    uintptr_t KB = 1 << 10;
    uintptr_t MB = 1 << 20;
    const char *_heap_size = getenv("HEAP_SIZE");
    intptr_t sz = _heap_size == NULL ? 0 : atoll(_heap_size);
    sz = sz <= 0 ? 128 * KB : sz;
    *heap_start = aligned_alloc(8, sz);
    *heap_end = *heap_start + sz;
}

size_t strlen(const char* s) {
    const char* p = s;
    while (*p) {
        p++;
    }
    return (size_t)(p - s);
}


void panic(const char * msg, ...) {
    fprintf(stderr, "*** panic ***\r\n");
    fprintf(stderr, msg);
    abort();
}
