#include "sys_port.h"
#include "pico/stdlib.h"
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
    stdio_init_all();
    while (!stdio_usb_connected()) {
        sleep_ms(10);
    }
    while(getchar() < 0);
    while(getchar() < 0);
    uintptr_t KB = 1 << 10;
    uintptr_t sz = 64 * KB;
    *heap_start = aligned_alloc(8, sz);
    *heap_end = *heap_start + sz;
    if(heap_start == NULL) panic(__FUNCTION__);
}

size_t strlen(const char* s) {
    const char* p = s;
    while (*p) {
        p++;
    }
    return (size_t)(p - s);
}
