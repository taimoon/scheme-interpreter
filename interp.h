#ifndef INTERP_H
#define INTERP_H
#include <stdint.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <dlfcn.h>

typedef enum TAG {
    FX_TAG,
    CHAR_TAG,
    EOF_TAG,
    BOOL_TAG,
    NIL_TAG,
    VOID_TAG,
    PAIR_TAG,
    VEC_TAG,
    CLOS_TAG,
    BYTEVEC_TAG,
    STR_TAG,
    SYM_TAG,
    
    FILE_TAG,
    TOK_TAG,
    CONT_TAG,
    UNBOUND_TAG,
} TAG;

typedef struct Object {
    TAG tag;
    int32_t len;
    union {
        intptr_t fx;
        uint32_t ch;
        bool b;
        char tk;
        uint8_t *bv;
        uint32_t* str;
        FILE *fp;
        void (*cont)();
        struct Object *ptr;
    };
} Object;

#define CAR(x) ((Object)x).ptr[0]
#define CDR(x) ((Object)x).ptr[1]
#define CAAR(x) CAR(CAR(x))
#define CAAAR(x) CAR(CAR(CAR(x)))
#define CAAAAR(x) CAR(CAR(CAR(CAR(x))))
#define CDAAAR(x) CDR(CAR(CAR(CAR(x))))
#define CDAAR(x) CDR(CAR(CAR(x)))
#define CADAAR(x) CAR(CDR(CAR(CAR(x))))
#define CDDAAR(x) CDR(CDR(CAR(CAR(x))))
#define CDAR(x) CDR(CAR(x))
#define CADAR(x) CAR(CDR(CAR(x)))
#define CAADAR(x) CAR(CAR(CDR(CAR(x))))
#define CDADAR(x) CDR(CAR(CDR(CAR(x))))
#define CDDAR(x) CDR(CDR(CAR(x)))
#define CADDAR(x) CAR(CDR(CDR(CAR(x))))
#define CDDDAR(x) CDR(CDR(CDR(CAR(x))))
#define CADR(x) CAR(CDR(x))
#define CAADR(x) CAR(CAR(CDR(x)))
#define CAAADR(x) CAR(CAR(CAR(CDR(x))))
#define CDAADR(x) CDR(CAR(CAR(CDR(x))))
#define CDADR(x) CDR(CAR(CDR(x)))
#define CADADR(x) CAR(CDR(CAR(CDR(x))))
#define CDDADR(x) CDR(CDR(CAR(CDR(x))))
#define CDDR(x) CDR(CDR(x))
#define CADDR(x) CAR(CDR(CDR(x)))
#define CAADDR(x) CAR(CAR(CDR(CDR(x))))
#define CDADDR(x) CDR(CAR(CDR(CDR(x))))
#define CDDDR(x) CDR(CDR(CDR(x)))
#define CADDDR(x) CAR(CDR(CDR(CDR(x))))
#define CDDDDR(x) CDR(CDR(CDR(CDR(x))))

#define is_list_one(x) \
    ((x).tag == PAIR_TAG && (CDR(x).tag == NIL_TAG))
#define is_list_two(x) \
    ((x).tag == PAIR_TAG && (CDR(x).tag == PAIR_TAG) \
     && (CDDR(x).tag == NIL_TAG))
#define is_list_three(x) \
    ((x).tag == PAIR_TAG && (CDR(x).tag == PAIR_TAG) \
     && (CDDR(x).tag == PAIR_TAG) && (CDDDR(x).tag == NIL_TAG))

void eval_entry(int argc, char **argv);
Object parse(FILE *fptr);

void gc_flip(int64_t byte_size);
void eval();

void writeln_obj(Object v, FILE *fptr);

void s_apply_2();
void s_call_with_values();
void s_callcc();
void s_eval();
void s_foreign_call();
void s_collect();
void s_exit();
void s_read();
void s_write();
void s_newline();
void s_eq();
void s_void();

void s_proc_pred();
void s_sym_pred();
void s_sym_hash();

void s_pair_pred();
void s_cons();
void s_car();
void s_cdr();
void s_set_car();
void s_set_cdr();

void s_int_pred();
void s_add();
void s_mul();
void s_sub();
void s_div();
void s_mod();
void s_ash();
void s_ior();
void s_iand();
void s_lt();
void s_le();
void s_eqn();
void s_gt();
void s_ge();
void s_bool_pred();
void s_int2char();
void s_char2int();
void s_char_pred();

void s_str_pred();
void s_make_str();
void s_str_len();
void s_str_ref();
void s_str_set();

void s_str2sym();
void s_sym2str();

void s_vec_pred();
void s_make_vec();
void s_vec_len();
void s_vec_ref();
void s_vec_set();

void s_bytevec_pred();
void s_make_bytevec();
void s_bytevec_len();
void s_bytevec_ref();
void s_bytevec_set();

#endif