#include "sys_port.h"
#include "scheme.c"
#ifdef __STDC_HOSTED__
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

int file_getc(void* p) {
    return fgetc((FILE*)p);
}

#endif

int repl_getc(void*) {
    for(;;) {
        int c = getchar();
        while(c < 0) c = getchar();
        // Accept printable ASCII + newline/carriage return
        if ((c >= 0x20 && c <= 0x7E) || c == '\r' || c == '\n') {
            putchar(c == '\r' ? '\n' : c);
            return c;
        }
    }
}

int main(int argc, char **argv) {
    uint8_t *heap_start = NULL;
    uint8_t *heap_end = NULL;
    sys_init(&heap_start, &heap_end);
#ifdef __STDC_HOSTED__
    s_rt_init(heap_start, heap_end, argc, argv);
#else
    s_rt_init(heap_start, heap_end, 0, NULL);
#endif
    ENV = s_rt_init_env();
    bool REPL_MODE = true;
#ifdef __STDC_HOSTED__
    REPL_MODE = getenv("BATCH_MODE") == NULL;
    bool LOUD_MODE = getenv("LOUD_MODE") != NULL;
    if(!REPL_MODE) {
        const char* boot_file = getenv("SCM_BOOT");
        int argi = argc;
        if(boot_file == NULL) {
            if (argc < 2) panic("expect at least one arguments!");
            argi = 1;
            boot_file = argv[argi];
        }
        FILE *fptr = fopen(boot_file, "r");
        if(fptr == NULL) {
            printf("file not found! %s\n", boot_file);
        }
        Lexer_Stream f = {.getc = file_getc, .ch = PEEKED, .data = fptr};
        for(;;) {
            EXP = s_parse(&f);
            if(LOUD_MODE) {
                printf("> ");
                s_rt_writeln(EXP);
            }
            if(EXP == EOF_TAG) {
                ++argi;
                fclose(fptr);
                if(argi >= argc) break;
                fptr = fopen(argv[argi], "r");
                if(fptr == NULL) {
                    printf("file not found! %s\n", argv[argi]);
                    return -1;
                }
                f.data = (void*)fptr;
            }
            s_eval_entry();
            if(VAL != VOID_TAG && LOUD_MODE) s_rt_writeln(VAL);
            s_gc(-1);
        }
    }
    else
#endif
    {
        _s_puts("Scheme REPL\n");
        Lexer_Stream f = {.getc = repl_getc, .ch = PEEKED, .data = NULL};
        for(;;) {
            _s_puts("> ");
            s_gc(-1);
            EXP = s_parse(&f);
            if(f.ch != '\r') { putchar('\n'); };
            s_eval_entry();
            if(VAL != VOID_TAG) s_rt_writeln(VAL);
        }
    }
    return 0;
}
