#include "sys_port.h"
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <stdnoreturn.h>

void _s_print_int(intptr_t d) {
    printf("%"PRIdPTR"", d);
}

void _s_print_uint(uintptr_t u) {
    printf("%"PRIuPTR"", u);
}

void _s_putc(int c) {
    putchar(c);
}

void _s_puts(const char* s) {
    printf("%s", s);
}

s_heap_config_t sys_init() {
    const char *_heap_size = getenv("HEAP_SIZE");
    intptr_t sz = _heap_size == NULL ? 0 : atoll(_heap_size);
    sz = sz <= 0 ? 256 << 10 : sz;
    s_heap_config_t cfg;
    cfg.sz = sz;
    cfg.fromspace_start = aligned_alloc(8, sz);
    cfg.fromspace_end = (uint8_t*)cfg.fromspace_start + sz;
    cfg.tospace_start = aligned_alloc(8, sz);
    cfg.tospace_end = (uint8_t*)cfg.tospace_start + sz;
    cfg.gc_markers = aligned_alloc(8, sz / (sizeof(intptr_t) * 8));
    if(cfg.fromspace_start == NULL) panic(__FUNCTION__);
    if(cfg.tospace_start == NULL) panic(__FUNCTION__);    
    return cfg;
}

size_t strlen(const char* s) {
    const char* p = s;
    while (*p) {
        p++;
    }
    return (size_t)(p - s);
}


noreturn void panic(const char * msg, ...) {
    fprintf(stderr, "*** panic ***\r\n");
    fprintf(stderr, msg);
    abort();
}
