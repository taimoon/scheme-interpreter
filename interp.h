#ifndef INTERP_H
#define INTERP_H
#include<stdio.h>
#include<stdint.h>
#include<stdlib.h>
#include<assert.h>
#include<stdbool.h>
#include<string.h>
// at least this amount to bootstrap compiler
#define HEAP_SIZE 0x4fff8

typedef enum TAG {
    FX_TAG,
    CHAR_TAG,
    EOF_TAG,
    BOOL_TAG,
    NIL_TAG,
    
    PAIR_TAG,
    VEC_TAG,
    STR_TAG,
    SYM_TAG,

    FN_TAG,
    FILE_TAG,
    TOK_TAG,
} TAG;

typedef struct Object {
    TAG tag;
    int len;
    union {
        int fx;
        bool b;
        char ch;
        char tk;
        struct Object *ptr;
        char *s;
        void (*cont)();
        FILE *fp;
    };
} Object;

void load_n_run(int argc, char *argv[]);


#endif