#include "sys_port.h"
#include "pico/stdlib.h"
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include "hardware/uart.h"

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

void sys_init(uint8_t **heap_start, uint8_t **heap_end) {
    stdio_init_all();
    while (!stdio_usb_connected()) {
        sleep_ms(10);
    }
    while(getchar() < 0);
    while(getchar() < 0);
    gpio_set_function(0, GPIO_FUNC_UART);
    gpio_set_function(1, GPIO_FUNC_UART);
    gpio_set_function(4, GPIO_FUNC_UART);
    gpio_set_function(5, GPIO_FUNC_UART);
    uart_init(uart0, 115200);
    uart_init(uart1, 115200);
    uart_puts(uart0, "test test uart0!\r\n");
    uart_puts(uart1, "test test uart1!\r\n");
    uintptr_t KB = 1 << 10;
    uintptr_t sz = (64 * 3) * KB;
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
