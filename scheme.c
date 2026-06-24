#pragma once
#include "config.h"
#include "sys_port.h"
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#if SCM_HOSTED
#include <stdio.h>
#endif

#ifndef EOF
#define EOF -1
#endif

#if defined(__x86_64__) || defined(__aarch64__) || (defined(__riscv) && (__riscv_xlen == 64))
#define FIXNUM_MASK     0b00000111
#define FIXNUM_SHIFT    3
#else
#define FIXNUM_MASK     0b00000011
#define FIXNUM_SHIFT    2
#endif

typedef intptr_t word_t;
#define PTR_MASK        0b111

#define SCM_FIXNUM_WIDTH    (SCM_NATIVE_WIDTH - FIXNUM_SHIFT)
#define SCM_INT_MAX       (((word_t)1 << (SCM_NATIVE_WIDTH - FIXNUM_SHIFT - 1)) - 1)
#define SCM_INT_MIN       (-((word_t)1 << (SCM_NATIVE_WIDTH - FIXNUM_SHIFT - 1)))

#define FIXNUM_TAG      0b000
#define PAIR_TAG        0b001
#define FLO_TAG         0b010
#define SYM_TAG         0b011
// OCCUPIED             0b100 // OCCUPIED by fixnums on 32-bits machines
#define CLOS_TAG        0b101
#define IMM_TAG         0b110
#define OBJ_TAG         0b111
// IMM_TAG              0bxxxxx110
#define BOOL_TAG        0b00000110
#define CHAR_TAG        0b00001110
#define NIL_TAG         0b00010110
#define EOF_TAG         0b00011110
#define VOID_TAG        0b00100110
#define UNBOUND_TAG     0b01000110
#define TOK_TAG         0b10000110

#define IMM_SHIFT       8
#define IMM_MASK        0b11111111
#define FALSE_IMM       BOOL_TAG
#define TRUE_IMM        ((1 << IMM_SHIFT) | BOOL_TAG)

#define CLOS_MASK           PTR_MASK
#define PAIR_MASK           PTR_MASK
#define OBJ_MASK                 0b111
#define VEC_TAG                  0b000
#define BYTEVEC_TAG              0b001
#define STR_TAG                  0b010
#define OTH_TAG                  0b011
#define OTH_MASK            0b11111111
#define BIGNUM_TAG               0b011
#define BIGNUM_POS_TAG      0b00000011
#define BIGNUM_NEG_TAG      0b00001011
#define BIGNUM_SIGN_SHIFT   3
#define EPHEMERON_TAG       0b00000111
#define STR_MASK            FIXNUM_MASK
#define VEC_MASK            FIXNUM_MASK
#define BYTEVEC_MASK        FIXNUM_MASK
#define BIGNUM_MASK         0b111


intptr_t HEAP_WORDS;
word_t *free_ptr, *scan_ptr;
word_t *fromspace_start, *fromspace_end;
word_t *tospace_start, *tospace_end;
static bool *gc_markers;

typedef void (*s_funptr_t)();
typedef struct S_ffi_str_t { size_t sz; char *s; } S_ffi_str_t;
typedef struct S_ffi_bytevec_t { size_t sz; uint8_t *bv; } S_ffi_bytevec_t;
typedef struct S_ffi_vec_t { size_t sz; word_t *ptr; } S_ffi_vec_t;
typedef struct S_ffi_sym_t { word_t *val; word_t hash; word_t name; } S_ffi_sym_t;
// 0 - non-negative, 1 - negative
typedef struct S_ffi_sword_t { uintptr_t x; int sign; } S_ffi_sword_t;

#define make_nil() NIL_TAG
#define make_bool(v) ((v) ? TRUE_IMM : FALSE_IMM)
intptr_t SYMBOLS_CAPS = 16;
intptr_t SYMBOLS_SIZE;
s_funptr_t NEXT;
word_t EXP;
word_t PROC;
word_t VAL;
word_t VALS;
word_t CONT;
word_t ENV;
word_t SYMBOLS;
word_t TOK;
word_t QUOTE;
word_t UNQUOTE_SPLICING;
word_t UNQUOTE;
word_t QUASIQUOTE;
word_t BEGIN;
word_t LAMBDA;
word_t IF;
word_t SET_BANG;
word_t DEFINE;
word_t DEFMACRO;
word_t TEMPS[8];
word_t *TEMP_SP;
word_t EPHEMERON_LIST;
word_t GUARDIAN_LIST;
intptr_t _RETC;

#define TEMP_PUSH(v) do { *TEMP_SP++ = v; } while(0)
#define TEMP_POP() *--TEMP_SP

static inline word_t align_to_multiple(word_t alignment, word_t offset) {
    return (offset + (alignment - 1)) & -alignment;
}

S_ffi_bytevec_t s_ffi_to_bytevec(word_t v);
S_ffi_vec_t s_ffi_to_vec(word_t v);
S_ffi_str_t s_ffi_to_str(word_t v);
S_ffi_sym_t s_ffi_to_sym(word_t v);
intptr_t s_ffi_to_fixnum(word_t v);
S_ffi_sword_t s_ffi_to_sword(word_t v);
char s_ffi_to_char(word_t v);
int s_ffi_bignum_sign(word_t v);
word_t s_ffi_bignum_limb(word_t v);

bool s_obj_is_fixnum(word_t v);
bool s_obj_is_pair(word_t v);
bool s_obj_is_sym(word_t v);
bool s_obj_is_vec(word_t v);
bool s_obj_is_bytevec(word_t v);
bool s_obj_is_str(word_t v);
bool s_obj_is_bignum(word_t v);
bool s_obj_is_ephemeron(word_t v);

word_t s_obj_int(intptr_t v);
word_t s_obj_tok(char c);
word_t s_obj_char(char c);
word_t s_obj_str_alloc(intptr_t len, word_t ch);
word_t s_obj_vec(intptr_t fx, word_t x);
word_t s_obj_vec_alloc(intptr_t len, word_t x);
word_t s_obj_bytevec(intptr_t len, uint8_t b);
word_t s_obj_bytevec_alloc(intptr_t len, word_t b);
word_t s_obj_bytevec_from_buf(const char *buf);
word_t s_obj_bignum(word_t limb, word_t sign);
word_t s_obj_bignum_alloc(word_t limb, word_t sign);
word_t s_obj_num(uintptr_t mag, int sign);

word_t s_obj_cons(word_t car, word_t cdr);
word_t s_obj_cons_alloc(word_t car, word_t cdr);
word_t* s_pair_car_ref(word_t v);
word_t* s_pair_cdr_ref(word_t v);

word_t* s_ephemeron_key_ref(word_t v);
word_t* s_ephemeron_val_ref(word_t v);
word_t s_ephemeron_key(word_t v);
word_t s_ephemeron_val(word_t v);

word_t s_sym_name(word_t sym);
word_t s_sym_hash(word_t sym);
word_t s_sym_val(word_t sym);
word_t s_sym_val_set(word_t sym, word_t val);

word_t s_gc(intptr_t sz);
word_t s_rt_add_sym_cstr(const char *buf);
word_t s_rt_add_keyword_cstr(const char *buf);
void s_rt_add_prim(const char *name, s_funptr_t prim);
word_t s_rt_writeln(word_t);

#define CAR(x) *s_pair_car_ref(x)
#define CDR(x) *s_pair_cdr_ref(x)
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

void s_pr_eval();
void s_pr_apply_2();
void s_pr_call_with_values();
void s_pr_callcc();
#if SCM_RAW_FFI
void s_pr_foreign_call();
#endif
void s_pr_exit();
void s_pr_newline();
void s_pr_write();
void s_pr_writeln();

void s_pr_write_mem_32();
void s_pr_read_mem_32();

void s_pr_clos_pred();
void s_pr_null_pred();
void s_pr_bool_pred();

void s_pr_cons();
void s_pr_pair_pred();
void s_pr_car();
void s_pr_cdr();
void s_pr_car_set();
void s_pr_cdr_set();

void s_pr_collect();
void s_pr_install_guardian();
void s_pr_ephemeron();
void s_pr_ephemeron_pred();
void s_pr_ephemeron_key();
void s_pr_ephemeron_val();

void s_pr_sym_pred();
void s_pr_sym_hash();
void s_pr_sym2str();
void s_pr_str2sym();

void s_pr_fx_pred();
void s_pr_int2char();
void s_pr_eq();
void s_pr_eqn();
void s_pr_lt();
void s_pr_le();
void s_pr_gt();
void s_pr_ge();
void s_pr_add();
void s_pr_sub();
void s_pr_mul();
void s_pr_mod();
void s_pr_div();
void s_pr_ash();
void s_pr_not();
void s_pr_ior();
void s_pr_and();

void s_pr_char_pred();
void s_pr_char2int();

void s_pr_str_pred();
void s_pr_make_str();
void s_pr_str_len();
void s_pr_str_ref();
void s_pr_str_set();

void s_pr_vec_pred();
void s_pr_make_vec();
void s_pr_vec_len();
void s_pr_vec_ref();
void s_pr_vec_set();

void s_pr_bytevec_pred();
void s_pr_make_bytevec();
void s_pr_bytevec_len();
void s_pr_bytevec_ref();
void s_pr_bytevec_set();

void s_pr_bignum_pred();
void s_pr_make_bignum();
void s_pr_bignum_limb();
void s_pr_bignum_sign();

void s_rt_init(void* heap_start, void* heap_end, int argc, char ** argv) {
    // 2 * w_sz * n + n * b_sz = (2 * w_sz + b_sz) * n
    if(!((uintptr_t)heap_start % 8 == 0 && heap_end > heap_start)) panic(__func__);
    uintptr_t div_size = 2 * sizeof(word_t) + sizeof(bool);
    uintptr_t heap_size = (uintptr_t)heap_end - (uintptr_t)heap_start;
    heap_size = heap_size - heap_size % (8 * div_size);
    HEAP_WORDS = heap_size / div_size;
    fromspace_start = (word_t*)heap_start;
    fromspace_end = fromspace_start + HEAP_WORDS;
    tospace_start = fromspace_end;
    tospace_end = tospace_start + HEAP_WORDS;
    gc_markers = (bool*)tospace_end;
    if(!(((uintptr_t)fromspace_start % 8) == 0 && ((uintptr_t)tospace_start % 8) == 0)) panic(__func__);
    for (intptr_t i = 0; i < HEAP_WORDS; ++i) { gc_markers[i] = false; }
    free_ptr = fromspace_start;
    /* INTEPRETER */
    SYMBOLS = s_obj_vec(SYMBOLS_CAPS, make_nil());
    SYMBOLS_SIZE = 0;
    GUARDIAN_LIST = NIL_TAG;
    TOK = make_nil();
    QUOTE = s_rt_add_keyword_cstr("quote");
    QUASIQUOTE = s_rt_add_keyword_cstr("quasiquote");
    UNQUOTE = s_rt_add_keyword_cstr("unquote");
    UNQUOTE_SPLICING = s_rt_add_keyword_cstr("unquote-splicing");
    BEGIN = s_rt_add_keyword_cstr("begin");
    LAMBDA = s_rt_add_keyword_cstr("lambda");
    IF = s_rt_add_keyword_cstr("if");
    SET_BANG = s_rt_add_keyword_cstr("set!");
    DEFINE = s_rt_add_keyword_cstr("define");
    DEFMACRO = s_rt_add_keyword_cstr("defmacro");
    TEMP_SP = TEMPS;
    #if SCM_HOSTED
    s_sym_val_set(s_rt_add_sym_cstr("free-standing?"), FALSE_IMM);
    s_sym_val_set(s_rt_add_sym_cstr("hosted?"), TRUE_IMM);
    s_sym_val_set(s_rt_add_sym_cstr("stdin"), (word_t)stdin);
    s_sym_val_set(s_rt_add_sym_cstr("stdout"), (word_t)stdout);
    s_sym_val_set(s_rt_add_sym_cstr("stderr"), (word_t)stderr);
    word_t _args = s_obj_vec(argc, s_obj_int(0));
    S_ffi_vec_t args = s_ffi_to_vec(_args);
    s_sym_val_set(s_rt_add_sym_cstr("ARGS"), _args);
    for(int i = 0; i < argc; ++i) {
        args.ptr[i] = s_obj_bytevec_from_buf(argv[i]);
    }
    #endif
    #if SCM_FREESTANDING
    s_sym_val_set(s_rt_add_sym_cstr("free-standing?"), TRUE_IMM);
    s_sym_val_set(s_rt_add_sym_cstr("hosted?"), FALSE_IMM);
    #endif
    #if SCM_UTF32
    s_sym_val_set(s_rt_add_sym_cstr("unicode-support?"), TRUE_IMM);
    #else
    s_sym_val_set(s_rt_add_sym_cstr("unicode-support?"), FALSE_IMM);
    #endif
    #if SCM_RAW_FFI
    s_rt_add_prim("foreign-call", s_pr_foreign_call);
    #endif
    s_sym_val_set(s_rt_add_sym_cstr("boot?"), TRUE_IMM);
    s_sym_val_set(s_rt_add_sym_cstr("eof"), EOF_TAG);
    s_rt_add_prim("apply", s_pr_apply_2);
    s_rt_add_prim("call-with-values", s_pr_call_with_values);
    s_rt_add_prim("call/cc", s_pr_callcc);
    s_rt_add_prim("eval", s_pr_eval);
    s_rt_add_prim("exit", s_pr_exit);
    s_rt_add_prim("newline", s_pr_newline);
    s_rt_add_prim("write", s_pr_write);
    s_rt_add_prim("writeln", s_pr_writeln);
    s_rt_add_prim("procedure?", s_pr_clos_pred);
    s_rt_add_prim("cons", s_pr_cons);
    s_rt_add_prim("null?", s_pr_null_pred);
    s_rt_add_prim("boolean?", s_pr_bool_pred);
    s_rt_add_prim("pair?", s_pr_pair_pred);
    s_rt_add_prim("car", s_pr_car);
    s_rt_add_prim("cdr", s_pr_cdr);
    s_rt_add_prim("set-car!", s_pr_car_set);
    s_rt_add_prim("set-cdr!", s_pr_cdr_set);
    s_rt_add_prim("collect", s_pr_collect);
    s_rt_add_prim("make-ephemeron", s_pr_ephemeron);
    s_rt_add_prim("install-guardian", s_pr_install_guardian);
    s_rt_add_prim("ephemeron?", s_pr_ephemeron_pred);
    s_rt_add_prim("ephemeron-key", s_pr_ephemeron_key);
    s_rt_add_prim("ephemeron-value", s_pr_ephemeron_val);
    s_rt_add_prim("symbol?", s_pr_sym_pred);
    s_rt_add_prim("symbol-hash", s_pr_sym_hash);
    s_rt_add_prim("symbol->string", s_pr_sym2str);
    s_rt_add_prim("string->symbol", s_pr_str2sym);
    s_rt_add_prim("fixnum?", s_pr_fx_pred);
    s_rt_add_prim("integer->char", s_pr_int2char);
    s_rt_add_prim("eq?", s_pr_eq);
    s_rt_add_prim("=", s_pr_eqn);
    s_rt_add_prim("<", s_pr_lt);
    s_rt_add_prim("<=", s_pr_le);
    s_rt_add_prim(">", s_pr_gt);
    s_rt_add_prim(">=", s_pr_ge);
    s_rt_add_prim("+", s_pr_add);
    s_rt_add_prim("-", s_pr_sub);
    s_rt_add_prim("*", s_pr_mul);
    s_rt_add_prim("mod", s_pr_mod);
    s_rt_add_prim("div", s_pr_div);
    s_rt_add_prim("ash", s_pr_ash);
    s_rt_add_prim("bitwise-not", s_pr_not);
    s_rt_add_prim("bitwise-ior", s_pr_ior);
    s_rt_add_prim("bitwise-and", s_pr_and);
    s_rt_add_prim("char?", s_pr_char_pred);
    s_rt_add_prim("char->integer", s_pr_char2int);
    s_rt_add_prim("string?", s_pr_str_pred);
    s_rt_add_prim("make-string", s_pr_make_str);
    s_rt_add_prim("string-length", s_pr_str_len);
    s_rt_add_prim("string-ref", s_pr_str_ref);
    s_rt_add_prim("string-set!", s_pr_str_set);
    s_rt_add_prim("vector?", s_pr_vec_pred);
    s_rt_add_prim("make-vector", s_pr_make_vec);
    s_rt_add_prim("vector-length", s_pr_vec_len);
    s_rt_add_prim("vector-ref", s_pr_vec_ref);
    s_rt_add_prim("vector-set!", s_pr_vec_set);
    s_rt_add_prim("bytevector?", s_pr_bytevec_pred);
    s_rt_add_prim("make-bytevector", s_pr_make_bytevec);
    s_rt_add_prim("bytevector-length", s_pr_bytevec_len);
    s_rt_add_prim("bytevector-u8-ref", s_pr_bytevec_ref);
    s_rt_add_prim("bytevector-u8-set!", s_pr_bytevec_set);
    s_sym_val_set(s_rt_add_sym_cstr("%fixnum-width"), (word_t)SCM_FIXNUM_WIDTH << FIXNUM_SHIFT);
    s_sym_val_set(s_rt_add_sym_cstr("%native-width"), SCM_NATIVE_WIDTH << FIXNUM_SHIFT);
    s_sym_val_set(s_rt_add_sym_cstr("%greatest-fixnum"), SCM_INT_MAX << FIXNUM_SHIFT);
    s_sym_val_set(s_rt_add_sym_cstr("%least-fixnum"), SCM_INT_MIN << FIXNUM_SHIFT);
    s_rt_add_prim("bignum?", s_pr_bignum_pred);
    s_rt_add_prim("%make-bignum", s_pr_make_bignum);
    s_rt_add_prim("%bignum-limb", s_pr_bignum_limb);
    s_rt_add_prim("%bignum-sign", s_pr_bignum_sign);

    s_rt_add_prim("%read-mem-32", s_pr_read_mem_32);
    s_rt_add_prim("%write-mem-32", s_pr_write_mem_32);
}

word_t s_rt_write(word_t v) {
    if((v & FIXNUM_MASK) == FIXNUM_TAG) {
        _s_print_int(v >> FIXNUM_SHIFT);
    }
    else if(s_obj_is_pair(v)) {
        word_t car = CAR(v);
        word_t cdr = CDR(v);
        
        _s_putc('(');
        s_rt_write(car);
        
        while((cdr & PAIR_MASK) == PAIR_TAG) {
            _s_putc(' ');
            car = CAR(cdr);
            cdr = CDR(cdr);
            s_rt_write(car);
        }
        if((cdr & IMM_MASK) == NIL_TAG) {
            _s_putc(')');
        }
        else {
            _s_puts(" . ");
            s_rt_write(cdr);
            _s_putc(')');
        }
    }
    else if((v & CLOS_MASK) == CLOS_TAG) {
        _s_puts("#<procedure>");
    }
    else if((v & IMM_MASK) == CHAR_TAG) {
        _s_putc('#');
        _s_putc('\\');
        _s_putc(v >> IMM_SHIFT);
    }
    else if((v & IMM_MASK) == TOK_TAG) {
        _s_putc(v >> IMM_SHIFT);
    }
    else if((v & IMM_MASK) == BOOL_TAG) {
        _s_puts(v >> IMM_SHIFT ? "#t" : "#f");
    }
    else if((v & IMM_MASK) == NIL_TAG) {
        _s_puts("()");
    }
    else if((v & IMM_MASK) == EOF_TAG) {
        _s_puts("#!eof");
    }
    else if((v & IMM_MASK) == VOID_TAG) {
        _s_puts("#<void>");
    }
    else if((v & IMM_MASK) == UNBOUND_TAG) {
        _s_puts((v >> IMM_SHIFT) == 1 ? "#<ENTRY-MARK>" : "#<UNBOUND>");
    }
    else if((v & PTR_MASK) == SYM_TAG) {
        v = s_sym_name(v);
        word_t len = *(word_t*)(v - OBJ_TAG) - STR_TAG;
        char *ptr = (char*)((word_t*)(v - OBJ_TAG) + 1);
        len = len >> FIXNUM_SHIFT;
        for(word_t i = 0; i < len; ++i) {
            _s_putc(ptr[i]);
        }
    }
    else if(s_obj_is_str(v)) {
        S_ffi_str_t s = s_ffi_to_str(v);
        _s_putc('"');
        for(uintptr_t i = 0; i < s.sz; ++i) {
            _s_putc(s.s[i]);
        }
        _s_putc('"');
    }
    else if ((v & OBJ_MASK) == OBJ_TAG && (*(word_t*)(v - OBJ_TAG) & VEC_MASK) == VEC_TAG) {
        word_t len = *(word_t*)(v - OBJ_TAG) - VEC_TAG;
        word_t *ptr = ((word_t*)(v - OBJ_TAG) + 1);
        len = len >> FIXNUM_SHIFT;
        _s_puts("#(");
        for(int i = 0; i < len; ++i) {
            s_rt_write(ptr[i]);
            if(i != len - 1)
                _s_putc(' ');
        }
        _s_puts(")");
    }
    else if ((v & OBJ_MASK) == OBJ_TAG && (*(word_t*)(v - OBJ_TAG) & BYTEVEC_MASK) == BYTEVEC_TAG) {
        word_t len = *(word_t*)(v - OBJ_TAG) - BYTEVEC_TAG;
        uint8_t *ptr = (uint8_t*)((word_t*)(v - OBJ_TAG) + 1);
        len = len >> FIXNUM_SHIFT;
        _s_puts("#vu8(");
        for(int i = 0; i < len; ++i) {
            _s_print_int(ptr[i]);
            if(i != len - 1)
                _s_putc(' ');
        }
        _s_puts(")");
    }
    else if (s_obj_is_ephemeron(v)) {
        _s_puts("<ephemeron ");
        s_rt_write(s_ephemeron_key(v));
        _s_putc(' ');
        s_rt_write(s_ephemeron_val(v));
        _s_putc('>');
    }
    else if (s_obj_is_bignum(v)) {
        S_ffi_sword_t sword = s_ffi_to_sword(v);
        if(sword.sign == 1) _s_putc('-');
        _s_print_uint(sword.x);
    }
    else {
        panic(__func__);
    }
    return VOID_TAG;
}

word_t s_rt_writeln(word_t v) {
    s_rt_write(v);
    _s_putc('\n');
    return VOID_TAG;
}

word_t s_obj_int(intptr_t v) {
    if(!(SCM_INT_MIN <= v && v <= SCM_INT_MAX)) panic(__func__);
    return v << FIXNUM_SHIFT;
}

word_t s_obj_tok(char c) {
    return (c << IMM_SHIFT) | TOK_TAG;
}

word_t s_obj_char(char c) {
    return (c << IMM_SHIFT) | CHAR_TAG;
}

word_t s_obj_cons(word_t car, word_t cdr) {
    if(fromspace_end - free_ptr < 2) panic(__func__);
    word_t *_v = free_ptr;
    free_ptr = (word_t*)align_to_multiple(8, (word_t)(free_ptr + 2));
    _v[0] = car;
    _v[1] = cdr;
    word_t v = (word_t)_v | PAIR_TAG;
    return v;
}

word_t s_obj_cons_alloc(word_t car, word_t cdr) {
    TEMP_PUSH(car);
    TEMP_PUSH(cdr);
    s_gc(2 * sizeof(word_t));
    cdr = TEMP_POP();
    car = TEMP_POP();
    return s_obj_cons(car, cdr);
}

word_t s_obj_str_alloc(intptr_t len, word_t ch) {
    ch = s_ffi_to_char(ch);
    word_t n_words = align_to_multiple(8, sizeof(word_t) + sizeof(uint8_t) * len);
    s_gc(n_words);
    word_t *_v = free_ptr;
    free_ptr = (word_t*)((uint8_t*)free_ptr + n_words);
    _v[0] = (len << FIXNUM_SHIFT) | STR_TAG;
    char *s = (char*)(_v + 1);
    for(intptr_t i = 0; i < len; ++i) {
        s[i] = ch;
    }
    word_t v = (word_t)_v | OBJ_TAG;
    return v;
}

word_t s_obj_str_from_buf(const char *buf, size_t len) {
    word_t v = s_obj_str_alloc(len, s_obj_char('\0'));
    char *s = s_ffi_to_str(v).s;
    for(size_t i = 0; i < len; ++i) {
        s[i] = buf[i];
    }
    return v;
}

word_t s_obj_str_from_buf_alloc(const char *buf, size_t len) {
    word_t n_words = align_to_multiple(8, sizeof(word_t) + sizeof(uint8_t) * len);
    s_gc(n_words);
    return s_obj_str_from_buf(buf, len);
}

word_t s_obj_sym(word_t name, word_t hash, word_t val) {
    if(fromspace_end - free_ptr < 3) panic(__func__);
    if(!s_obj_is_str(name)) panic(__func__);
    if(!((hash & FIXNUM_MASK) == FIXNUM_TAG)) panic(__func__);
    word_t *_v = free_ptr;
    free_ptr = (word_t*)align_to_multiple(8, (word_t)(free_ptr + 3));
    _v[0] = val;
    _v[1] = hash;
    _v[2] = name;
    word_t v = (word_t)_v | SYM_TAG;
    return v;
}

word_t s_obj_vec(intptr_t len, word_t x) {
    if(!(len >= 0)) panic(__func__);
    if((fromspace_end - free_ptr < (len + 1))) panic(__func__);
    word_t *_w = free_ptr;
    free_ptr = (word_t*)align_to_multiple(8, (word_t)(free_ptr + len + 1));
    _w[0] = (len << FIXNUM_SHIFT) | VEC_TAG;
    for (intptr_t i = 0; i < len; ++i) {
        _w[i+1] = x;
    }
    word_t v = (word_t)_w | OBJ_TAG;
    return v;
}

word_t s_obj_vec_alloc(intptr_t len, word_t x) {
    if(!(len >= 0)) panic(__func__);
    s_gc((len + 1) * sizeof(word_t));
    return s_obj_vec(len, x);
}

word_t s_obj_bytevec(intptr_t len, uint8_t b) {
    word_t n_words = align_to_multiple(sizeof(word_t), len + sizeof(word_t));
    word_t *_v = free_ptr;
    free_ptr = (word_t*)((uint8_t*)free_ptr + n_words);
    _v[0] = (len << FIXNUM_SHIFT) | BYTEVEC_TAG;
    uint8_t *bv = (uint8_t*)(_v + 1);
    for(intptr_t i = 0; i < len; ++i) {
        bv[i] = b;
    }
    word_t v = (word_t)_v | OBJ_TAG;
    return v;
}

word_t s_obj_bytevec_alloc(intptr_t len, word_t b) {
    b = s_ffi_to_fixnum(b);
    if(!(len >= 0)) panic(__func__);
    if(!((0 <= b && b <= UINT8_MAX) || (INT8_MIN <= b && b <= INT8_MAX))) {
        panic(__func__);
    }
    s_gc(len + sizeof(word_t));
    uint8_t v = b & UINT8_MAX;
    return s_obj_bytevec(len, v);
}

word_t s_obj_bytevec_from_buf(const char *buf) {
    size_t len = strlen(buf);
    word_t v = s_obj_bytevec(len, s_obj_int(0));
    S_ffi_bytevec_t _v = s_ffi_to_bytevec(v);
    for(size_t i = 0; i < len; ++i) {
        _v.bv[i] = buf[i];
    }
    return v;
}

word_t s_obj_clos(s_funptr_t fn, word_t fx) {
    intptr_t len = fx >> FIXNUM_SHIFT;
    if(!(len >= 0)) panic(__func__);
    if((fromspace_end - free_ptr < (len + 2))) panic(__func__);
    word_t *_w = free_ptr;
    free_ptr = (word_t*)align_to_multiple(8, (word_t)(free_ptr + len + 2));
    _w[0] = (word_t)fn;
    _w[1] = fx;
    for(intptr_t i = 0; i < len; ++i) _w[i + 2] = 0;
    word_t v = (word_t)_w | CLOS_TAG;
    return v;
}

word_t s_obj_clos_alloc(s_funptr_t fn, word_t fx) {
    intptr_t len = s_ffi_to_fixnum(fx);
    s_gc((len + 2) * sizeof(word_t));
    return s_obj_clos(fn, fx);
}

word_t s_obj_bignum(word_t limb, word_t sign) {
    if((fromspace_end - free_ptr < 2)) panic(__func__);
    intptr_t s = s_ffi_to_fixnum(sign);
    if(!(s == 0 || s == 1)) panic("s_obj_bignum_alloc sign is either 0 or 1");
    s_ffi_to_vec(limb);
    word_t *_w = free_ptr;
    free_ptr = (word_t*)align_to_multiple(8, (word_t)(free_ptr + 2));
    _w[0] = BIGNUM_TAG | (s << BIGNUM_SIGN_SHIFT);
    _w[1] = limb;
    word_t v = (word_t)_w | OBJ_TAG;
    return v;
}

word_t s_obj_bignum_alloc(word_t limb, word_t sign) {
    TEMP_PUSH(limb);
    TEMP_PUSH(sign);
    s_gc(2 * sizeof(word_t));
    sign = TEMP_POP();
    limb = TEMP_POP();
    return s_obj_bignum(limb, sign);
}

word_t s_obj_num(uintptr_t mag, int sign) {
    if(sign == 0 && mag <= (uintptr_t)SCM_INT_MAX) {
        return s_obj_int((intptr_t)mag);
    }
    if(sign == 1 && mag <= (uintptr_t)SCM_INT_MAX + 1) {
        return s_obj_int(-(intptr_t)(mag - 1) - 1);
    }
    uintptr_t base = ((uintptr_t)1 << (SCM_FIXNUM_WIDTH - 1));
    uintptr_t hi = mag >> (SCM_FIXNUM_WIDTH - 1);
    uintptr_t lo = mag & (base - 1);
    word_t limb = s_obj_vec(2, 0);
    word_t *ptr = s_ffi_to_vec(limb).ptr;
    ptr[0] = s_obj_int(lo);
    ptr[1] = s_obj_int(hi);
    return s_obj_bignum(limb, s_obj_int(sign));
}

word_t s_obj_num_alloc(uintptr_t mag, int sign) {
    s_gc(8 * sizeof(word_t));
    return s_obj_num(mag, sign);
}

word_t s_obj_num_alloc_from_iptr(intptr_t i) {
    return i < 0 ? s_obj_num_alloc(-(uintptr_t)i, 1) : s_obj_num_alloc(i, 0);
}

word_t s_obj_ephemeron(word_t car, word_t cdr) {
    if(fromspace_end - free_ptr < 3) panic(__func__);
    word_t *_v = free_ptr;
    free_ptr = (word_t*)align_to_multiple(8, (word_t)(free_ptr + 4));
    _v[0] = EPHEMERON_TAG;
    _v[1] = car;
    _v[2] = cdr;
    _v[3] = NIL_TAG;
    word_t v = (word_t)_v | OBJ_TAG;
    return v;
}

word_t s_obj_ephemeron_alloc(word_t car, word_t cdr) {
    TEMP_PUSH(car);
    TEMP_PUSH(cdr);
    s_gc(4 * sizeof(word_t));
    cdr = TEMP_POP();
    car = TEMP_POP();
    return s_obj_ephemeron(car, cdr);
}

word_t s_install_guardian(word_t obj, word_t tconc) {
    word_t vec = s_obj_vec(3, 0);
    word_t *ptr = s_ffi_to_vec(vec).ptr;
    ptr[0] = obj;
    ptr[1] = tconc;
    ptr[2] = GUARDIAN_LIST;
    GUARDIAN_LIST = vec;
    return VOID_TAG;
}

/* djb2: hash = hash * 33 + c, init = 5381 */
intptr_t s_rt_hash(const char *buf, size_t len) {
    intptr_t hash = 5381;
    for(size_t i = 0; i < len; ++i) {
        hash = ((hash << 5) + hash) + buf[i];
    }
    hash = hash & SCM_INT_MAX;
    return hash;
}

bool memeq(const char *buf_0, size_t sz_0, const char *buf_1, size_t sz_1) {
    if (sz_0 != sz_1) return false;
    for(size_t i = 0; i < sz_0; ++i) {
        if (buf_0[i] != buf_1[i]) return false;
    }
    return true;
}

// ffi
bool s_obj_is_fixnum(word_t v) {
    return (v & FIXNUM_MASK) == FIXNUM_TAG;
}

bool s_obj_is_pair(word_t v) {
    return (v & PTR_MASK) == PAIR_TAG;
}

bool s_obj_is_sym(word_t v) {
    return (v & PTR_MASK) == SYM_TAG;
}

bool s_obj_is_vec(word_t v) {
    return (v & OBJ_MASK) == OBJ_TAG && (*(word_t*)(v - OBJ_TAG) & VEC_MASK) == VEC_TAG;
}

bool s_obj_is_bytevec(word_t v) {
    return (v & OBJ_MASK) == OBJ_TAG && (*(word_t*)(v - OBJ_TAG) & BYTEVEC_MASK) == BYTEVEC_TAG;
}

bool s_obj_is_str(word_t v) {
    return (v & OBJ_MASK) == OBJ_TAG && (*(word_t*)(v - OBJ_TAG) & STR_MASK) == STR_TAG;
}

bool s_obj_is_bignum(word_t v) {
    return (v & OBJ_MASK) == OBJ_TAG && (*(word_t*)(v - OBJ_TAG) & BIGNUM_MASK) == BIGNUM_TAG;
}

bool s_obj_is_ephemeron(word_t v) {
    return (v & OBJ_MASK) == OBJ_TAG && ((*(word_t*)(v - OBJ_TAG) & OTH_MASK) == EPHEMERON_TAG);
}

// 0 - non-negative, 1 - negative
int s_ffi_bignum_sign(word_t v) {
    if(s_obj_is_bignum(v)) {
        return (((word_t*)(v - OBJ_TAG))[0] >> BIGNUM_SIGN_SHIFT) & 1;
    }
    else {
        panic(__func__);
    }

}

word_t s_ffi_bignum_limb(word_t v) {
    if(s_obj_is_bignum(v)) {
        return ((word_t*)(v - OBJ_TAG))[1];
    }
    else {
        panic(__func__);
    }
}

S_ffi_bytevec_t s_ffi_to_bytevec(word_t v) {
    if (s_obj_is_bytevec(v)) {
        word_t len = *(word_t*)(v - OBJ_TAG) - BYTEVEC_TAG;
        len = s_ffi_to_fixnum(len);
        uint8_t *bv = (uint8_t*)((word_t*)(v - OBJ_TAG) + 1);
        return (S_ffi_bytevec_t){.bv = bv, .sz = len};
    }
    else {
        panic(__func__);
    }
}

S_ffi_vec_t s_ffi_to_vec(word_t v) {
    if (s_obj_is_vec(v)) {
        word_t len = *(word_t*)(v - OBJ_TAG) - VEC_TAG;
        len = s_ffi_to_fixnum(len);
        word_t *ptr = ((word_t*)(v - OBJ_TAG) + 1);
        return (S_ffi_vec_t){.ptr = ptr, .sz = len};
    }
    else {
        panic(__func__);
    }
}

S_ffi_str_t s_ffi_to_str(word_t v) {
    if (s_obj_is_str(v)) {
        word_t len = *(word_t*)(v - OBJ_TAG) - STR_TAG;
        len = s_ffi_to_fixnum(len);
        char *ptr = (char*)((word_t*)(v - OBJ_TAG) + 1);
        return (S_ffi_str_t){.s = ptr, .sz = len};
    }
    else {
        panic(__func__);
    }
}

S_ffi_sym_t s_ffi_to_sym(word_t v) {
    if((v & PTR_MASK) != SYM_TAG) {s_rt_writeln(v); panic(__func__);}
    word_t *ptr = (word_t*)(v - SYM_TAG);
    return (S_ffi_sym_t) {
        .val = ptr,
        .hash = ptr[1],
        .name = ptr[2],
    };
}

intptr_t s_ffi_to_fixnum(word_t v) {
    if((v & FIXNUM_MASK) != FIXNUM_TAG) panic(__func__);
    return v >> FIXNUM_SHIFT;
}

S_ffi_sword_t s_ffi_to_sword(word_t v) {
    if(s_obj_is_fixnum(v)) {
        if(v < 0) {
            return (S_ffi_sword_t) {.x = -(uintptr_t)v >> FIXNUM_SHIFT, .sign = 1};
        }
        else {
            return (S_ffi_sword_t) {.x = v >> FIXNUM_SHIFT, .sign = 0};
        }
    }
    else if(s_obj_is_bignum(v)) {
        S_ffi_vec_t limb = s_ffi_to_vec(s_ffi_bignum_limb(v));
        if(limb.sz != 2) {panic(__func__);}
        uintptr_t lo = (uintptr_t)s_ffi_to_fixnum(limb.ptr[0]);
        uintptr_t hi = (uintptr_t)s_ffi_to_fixnum(limb.ptr[1]) << (SCM_NATIVE_WIDTH - FIXNUM_SHIFT - 1);
        uintptr_t uptr = lo + hi;
        return (S_ffi_sword_t) {.x = uptr, .sign = s_ffi_bignum_sign(v)};
    }
    else {
        panic(__func__);
    }
}

char s_ffi_to_char(word_t v) {
    if((v & IMM_MASK) != CHAR_TAG) {s_rt_writeln(v); panic(__func__);}
    return v >> IMM_SHIFT;
}

#define _s_ffi_from_clos(v) ((word_t*)(v - CLOS_TAG) + 2)

word_t *s_ffi_from_clos(word_t v) {
    if((v & PTR_MASK) == CLOS_TAG) {
        return _s_ffi_from_clos(v);
    }
    else {
        panic(__func__);
    }
}

word_t* s_pair_car_ref(word_t v) {
    if((v & PTR_MASK) == PAIR_TAG) {
        word_t *ptr = (word_t*)(v - PAIR_TAG);
        return ptr;
    }
    else {
        panic(__func__);
    }
}

word_t* s_pair_cdr_ref(word_t v) {
    if((v & PTR_MASK) == PAIR_TAG) {
        word_t *ptr = (word_t*)(v - PAIR_TAG);
        return ptr + 1;
    }
    else {
        panic(__func__);
    }
}

word_t s_pair_car(word_t v) {
    return *s_pair_car_ref(v);
}

word_t s_pair_cdr(word_t v) {
    return *s_pair_cdr_ref(v);
}

word_t* s_ephemeron_key_ref(word_t v) {
    if(s_obj_is_ephemeron(v)) {
        return (word_t*)(v - OBJ_TAG) + 1;
    }
    else {
        panic(__func__);
    }
}

word_t* s_ephemeron_val_ref(word_t v) {
    if(s_obj_is_ephemeron(v)) {
        return (word_t*)(v - OBJ_TAG) + 2;
    }
    else {
        panic(__func__);
    }
}

word_t s_ephemeron_key(word_t v) {
    return *s_ephemeron_key_ref(v);
}

word_t s_ephemeron_val(word_t v) {
    return *s_ephemeron_val_ref(v);
}


word_t s_sym_name(word_t sym) {
    return s_ffi_to_sym(sym).name;
}

word_t s_sym_hash(word_t sym) {
    return s_ffi_to_sym(sym).hash;
}

word_t s_sym_val(word_t sym) {
    return *s_ffi_to_sym(sym).val;
}

word_t s_sym_val_set(word_t sym, word_t val) {
    *(s_ffi_to_sym(sym).val) = val;
    return VOID_TAG;
}

void s_rt_resize_symbol_table();
word_t s_rt_add_sym_buf(const char *buf, size_t len) {
    // NOTE: this function also clean up broken ephemerons.
    s_rt_resize_symbol_table();
    intptr_t hash = s_rt_hash(buf, len);
    word_t *tbl = s_ffi_to_vec(SYMBOLS).ptr;
    word_t bucket = tbl[hash % SYMBOLS_CAPS];
    while(s_obj_is_pair(bucket)) {
        word_t sym = s_ephemeron_key(CAR(bucket));
        S_ffi_str_t name = s_ffi_to_str(s_sym_name(sym));
        if(memeq(name.s, name.sz, buf, len)) {
            return sym;
        }
        bucket = CDR(bucket);
    }
    word_t sym = s_obj_sym(s_obj_str_from_buf(buf, len), s_obj_int(hash), UNBOUND_TAG);
    tbl[hash % SYMBOLS_CAPS] = s_obj_cons(s_obj_ephemeron(sym, sym), tbl[hash % SYMBOLS_CAPS]);
    SYMBOLS_SIZE++;
    return sym;
}

void s_rt_resize_symbol_table() {
    {
        // collecting symbols that are gone
        S_ffi_vec_t table = s_ffi_to_vec(SYMBOLS);
        intptr_t symbol_size = 0;
        for(size_t i = 0; i < table.sz; ++i) {
            word_t bucket = table.ptr[i];
            word_t *prev = table.ptr + i;
            while(s_obj_is_pair(bucket)) {
                word_t sym = s_ephemeron_key(CAR(bucket));
                if(sym == FALSE_IMM) {
                    *prev = CDR(bucket);
                    bucket = CDR(bucket);
                    continue;
                }
                prev = s_pair_cdr_ref(bucket);
                bucket = CDR(bucket);
                symbol_size++;
            }
        }
        SYMBOLS_SIZE = symbol_size;
    }
    intptr_t new_symbol_caps = SYMBOLS_CAPS;
    if(SYMBOLS_SIZE >= SYMBOLS_CAPS * 3 && (fromspace_end - fromspace_start) >= (SYMBOLS_SIZE * 4 + SYMBOLS_CAPS * 2)) {
        new_symbol_caps = SYMBOLS_CAPS * 2;
    }
    else if(SYMBOLS_SIZE * 3 <= SYMBOLS_CAPS * 4 && (fromspace_end - fromspace_start) >= (SYMBOLS_SIZE * 4 + SYMBOLS_CAPS / 2)) {
        new_symbol_caps = SYMBOLS_CAPS / 2;
    }
    else {
        return;
    }
    S_ffi_vec_t old_table = s_ffi_to_vec(SYMBOLS);
    word_t _new_table = s_obj_vec(new_symbol_caps, make_nil());
    S_ffi_vec_t new_table = s_ffi_to_vec(_new_table);
    intptr_t symbol_size = 0;
    for(size_t i = 0; i < old_table.sz; ++i) {
        word_t bucket = old_table.ptr[i];
        while(s_obj_is_pair(bucket)) {
            word_t sym = s_ephemeron_key(CAR(bucket));
            uintptr_t idx = s_ffi_to_fixnum(s_sym_hash(sym)) % new_table.sz;
            new_table.ptr[idx] = s_obj_cons(CAR(bucket), new_table.ptr[idx]);
            bucket = CDR(bucket);
            symbol_size++;
        }
    }
    SYMBOLS = _new_table;
    SYMBOLS_SIZE = symbol_size;
    SYMBOLS_CAPS = new_table.sz;
}

word_t s_rt_add_sym_cstr(const char *buf) {
    return s_rt_add_sym_buf(buf, strlen(buf));
}

word_t s_rt_add_keyword_cstr(const char *name) {
    word_t sym = s_rt_add_sym_buf(name, strlen(name));
    s_sym_val_set(sym, 0);
    return sym;
}

void s_rt_add_prim(const char *name, s_funptr_t prim) {
    s_sym_val_set(s_rt_add_sym_buf(name, strlen(name)), s_obj_clos(prim, 0));
}

/* GARBAGE COLLECTOR */
static inline bool is_nonheap(word_t v) {
    return (v & FIXNUM_MASK) == FIXNUM_TAG || (v & OBJ_MASK) == IMM_TAG;
}

word_t s_copy(const word_t v) {
    if(is_nonheap(v)) {
        return v;
    }
    word_t *ptr = (word_t*)(v & ~PTR_MASK);
    if(free_ptr >= (scan_ptr - 1)) {
        panic("scan_ptr touches free_ptr :(\n");
    }
    assert(tospace_start <= ptr && ptr < tospace_end);
    int idx = ptr - tospace_start;
    if(gc_markers[idx]) {
        return *ptr;
    }
    gc_markers[idx] = true;
    if((v & PTR_MASK) == PAIR_TAG) {
        word_t *ptr = (word_t*)(v - PAIR_TAG);
        word_t *_w = free_ptr;
        free_ptr = (word_t*)align_to_multiple(8, (word_t)(free_ptr + 2));
        _w[0] = ptr[0];
        _w[1] = ptr[1];
        word_t w = (word_t)_w | PAIR_TAG;
        *ptr = w;
        *--scan_ptr = w;
        return w;
    }
    else if((v & PTR_MASK) == CLOS_TAG) {
        word_t *ptr = (word_t*)(v - CLOS_TAG);
        word_t len = s_ffi_to_fixnum(ptr[1]);
        word_t *_w = free_ptr;
        free_ptr = (word_t*)align_to_multiple(8, (word_t)(free_ptr + len + 2));
        _w[0] = ptr[0];
        _w[1] = ptr[1];
        for(int i = 0; i < len; ++i) {
            _w[i + 2] = ptr[i + 2];
        }
        word_t w = (word_t)_w | CLOS_TAG;
        *ptr = w;
        *--scan_ptr = w;
        return w;
    }
    else if((v & PTR_MASK) == SYM_TAG) {
        word_t *ptr = (word_t*)(v - SYM_TAG);
        word_t *_w = free_ptr;
        free_ptr = (word_t*)align_to_multiple(8, (word_t)(free_ptr + 3));
        word_t val = ptr[0];
        word_t hash = ptr[1];
        word_t name = ptr[2];
        s_ffi_to_fixnum(hash);
        _w[0] = val;
        _w[1] = hash;
        _w[2] = name;
        word_t w = (word_t)_w | SYM_TAG;
        *ptr = w;
        *--scan_ptr = w;
        return w;
    }
    else if ((v & OBJ_MASK) == OBJ_TAG && (*(word_t*)(v - OBJ_TAG) & VEC_MASK) == VEC_TAG) {
        word_t len = s_ffi_to_fixnum(*(word_t*)(v - OBJ_TAG) - VEC_TAG);
        assert(len >= 0);
        word_t *_w = free_ptr;
        free_ptr = (word_t*)align_to_multiple(8, (word_t)(free_ptr + len + 1));
        word_t *src = ((word_t*)(v - OBJ_TAG) + 1);
        _w[0] = (len << FIXNUM_SHIFT) | VEC_TAG;
        word_t *dst = _w + 1;
        for(int i = 0; i < len; ++i) {
            dst[i] = src[i];
        }
        word_t w = (word_t)_w | OBJ_TAG;
        word_t *ptr = (word_t*)(v - OBJ_TAG);
        *ptr = w;
        *--scan_ptr = w;
        return w;
    }
    else if((v & OBJ_MASK) == OBJ_TAG && (*(word_t*)(v - OBJ_TAG) & STR_MASK) == STR_TAG) {
        word_t len = s_ffi_to_fixnum(*(word_t*)(v - OBJ_TAG) - STR_TAG);
        uint8_t *src = (uint8_t*)((word_t*)(v - OBJ_TAG) + 1);
        word_t *_w = free_ptr;
        free_ptr = (word_t*)align_to_multiple(8, (word_t)free_ptr + sizeof(word_t) + sizeof(uint8_t) * len);
        uint8_t *dst = (uint8_t*)(_w + 1);
        _w[0] = (len << FIXNUM_SHIFT) | STR_TAG;
        for(intptr_t i = 0; i < len; ++i) {
            dst[i] = src[i];
        }
        word_t w = (word_t)_w | OBJ_TAG;
        word_t *ptr = (word_t*)(v - OBJ_TAG);
        *ptr = w;
        return w;
    }
    else if((v & OBJ_MASK) == OBJ_TAG && (*(word_t*)(v - OBJ_TAG) & BYTEVEC_MASK) == BYTEVEC_TAG) {
        word_t len = s_ffi_to_fixnum(*(word_t*)(v - OBJ_TAG) - BYTEVEC_TAG);
        assert(len >= 0);
        uint8_t *src = (uint8_t*)((word_t*)(v - OBJ_TAG) + 1);
        word_t *_w = free_ptr;
        free_ptr = (word_t*)align_to_multiple(8, (word_t)free_ptr + sizeof(word_t) + len);
        uint8_t *dst = (uint8_t*)(_w + 1);
        _w[0] = (len << FIXNUM_SHIFT) | BYTEVEC_TAG;
        for(intptr_t i = 0; i < len; ++i) {
            dst[i] = src[i];
        }
        word_t w = (word_t)_w | OBJ_TAG;
        word_t *ptr = (word_t*)(v - OBJ_TAG);
        *ptr = w;
        return w;
    }
    else if((v & OBJ_MASK) == OBJ_TAG && (*(word_t*)(v - OBJ_TAG) & BIGNUM_MASK) == BIGNUM_TAG) {
        word_t *_v = (word_t*)(v - OBJ_TAG);
        word_t *_w = free_ptr;
        free_ptr = (word_t*)align_to_multiple(8, (word_t)(free_ptr + 2));
        _w[0] = _v[0];
        _w[1] = _v[1];
        word_t w = (word_t)_w | OBJ_TAG;
        word_t *ptr = (word_t*)(v - OBJ_TAG);
        *ptr = w;
        *--scan_ptr = w;
        return w;
    }
    else if((v & OBJ_MASK) == OBJ_TAG && (*(word_t*)(v - OBJ_TAG) & OTH_MASK) == EPHEMERON_TAG) {
        word_t *_v = (word_t*)(v - OBJ_TAG);
        word_t *_w = free_ptr;
        free_ptr = (word_t*)align_to_multiple(8, (word_t)(free_ptr + 4));
        _w[0] = EPHEMERON_TAG;
        _w[1] = _v[1];
        _w[2] = _v[2];
        _w[3] = EPHEMERON_LIST;
        word_t w = (word_t)_w | OBJ_TAG;
        EPHEMERON_LIST = w;
        word_t *ptr = (word_t*)(v - OBJ_TAG);
        *ptr = w;
        return w;
    }
    else {
        panic(__func__);
    }
}

static inline void _s_collect_scan_ptr() {
    while(scan_ptr < fromspace_end) {
        assert(free_ptr < scan_ptr);
        word_t v = *scan_ptr; ++scan_ptr;
        if((v & PTR_MASK) == PAIR_TAG) {
            word_t *ptr = (word_t*)(v - PAIR_TAG);
            ptr[0] = s_copy(ptr[0]);
            ptr[1] = s_copy(ptr[1]);
        }
        else if((v & PTR_MASK) == CLOS_TAG) {
            word_t *ptr = (word_t*)(v - CLOS_TAG);
            word_t len = s_ffi_to_fixnum(ptr[1]);
            for(int i = 0; i < len; ++i) {
                ptr[i + 2] = s_copy(ptr[i + 2]);
            }
        }
        else if((v & PTR_MASK) == SYM_TAG) {
            word_t *ptr = (word_t*)(v - SYM_TAG);
            ptr[0] = s_copy(ptr[0]);
            ptr[1] = s_copy(ptr[1]);
            ptr[2] = s_copy(ptr[2]);
        }
        else if ((v & OBJ_MASK) == OBJ_TAG && (*(word_t*)(v - OBJ_TAG) & VEC_MASK) == VEC_TAG) {
            word_t len = s_ffi_to_fixnum(*(word_t*)(v - OBJ_TAG) - VEC_TAG);
            word_t *ptr = ((word_t*)(v - OBJ_TAG) + 1);
            for(int i = 0; i < len; ++i) {
                ptr[i] = s_copy(ptr[i]);
            }
        }
        else if((v & OBJ_MASK) == OBJ_TAG && (*(word_t*)(v - OBJ_TAG) & BIGNUM_MASK) == BIGNUM_TAG){
            word_t *ptr = (word_t*)(v - OBJ_TAG);
            ptr[1] = s_copy(ptr[1]);
        }
        else{
            panic(__func__);
        }
    }
}

static inline intptr_t max(intptr_t x, intptr_t y) { return x < y ? y : x; }

static inline bool s_is_forwarded(word_t v) {
    if(is_nonheap(v)) {
        return false;
    }
    word_t *ptr = (word_t*)(v & ~PTR_MASK);
    assert(tospace_start <= ptr && ptr < tospace_end);
    int idx = ptr - tospace_start;
    return gc_markers[idx];
}

static inline void _s_collect_ephemeron() {
    bool is_changed;
    do {
        is_changed = false;
        word_t ephemeron = EPHEMERON_LIST;
        EPHEMERON_LIST = NIL_TAG;
        while(s_obj_is_ephemeron(ephemeron)) {
            word_t *ptr = (word_t*)(ephemeron - OBJ_TAG);
            word_t key = ptr[1];
            word_t val = ptr[2];
            word_t next = ptr[3];
            ptr[3] = NIL_TAG;
            if(is_nonheap(key) || s_is_forwarded(key)) {
                ptr[1] = s_copy(key);
                ptr[2] = s_copy(val);
                is_changed = true;
            }
            else {
                ptr[3] = EPHEMERON_LIST;
                EPHEMERON_LIST = ephemeron;
            }
            ephemeron = next;
        }
        assert(ephemeron == NIL_TAG);
        _s_collect_scan_ptr();
    } while(is_changed);
    while(s_obj_is_ephemeron(EPHEMERON_LIST)) {
        word_t *ptr = (word_t*)(EPHEMERON_LIST - OBJ_TAG);
        ptr[1] = FALSE_IMM;
        ptr[2] = FALSE_IMM;
        EPHEMERON_LIST = ptr[3];
        ptr[3] = NIL_TAG;
    }
    assert(EPHEMERON_LIST == NIL_TAG);
}

static inline void _s_collect_guardian() {
    word_t pending_hold_list = NIL_TAG;
    word_t pending_final_list = NIL_TAG;
    while(s_obj_is_vec(GUARDIAN_LIST)) {
        word_t guardian = GUARDIAN_LIST;
        S_ffi_vec_t vec = s_ffi_to_vec(guardian);
        assert(vec.sz == 3);
        word_t *ptr = vec.ptr;
        word_t obj = ptr[0];
        word_t tconc = ptr[1];
        if(is_nonheap(obj) || is_nonheap(tconc)) panic(__func__);
        GUARDIAN_LIST = ptr[2];
        ptr[2] = NIL_TAG;
        if(s_is_forwarded(obj)) {
            ptr[2] = pending_hold_list;
            pending_hold_list = guardian;
        }
        else {
            ptr[2] = pending_final_list;
            pending_final_list = guardian;
        }
    }
    assert(GUARDIAN_LIST == NIL_TAG);
    do {
        word_t final_list = NIL_TAG;
        word_t *prev_next = &pending_final_list;
        word_t curr = pending_final_list;
        while(s_obj_is_vec(curr)) {
            word_t *ptr = s_ffi_to_vec(curr).ptr;
            word_t tconc = ptr[1];
            word_t next = ptr[2];
            if(s_is_forwarded(tconc)) {
                *prev_next = ptr[2];
                ptr[2] = final_list;
                final_list = curr;
            }
            else {
                prev_next = ptr + 2;
            }
            curr = next;
        }
        if(final_list == NIL_TAG) break;
        while(s_obj_is_vec(final_list)) {
            word_t *ptr = s_ffi_to_vec(final_list).ptr;
            final_list = ptr[2];
            ptr[2] = NIL_TAG;
            word_t obj = s_copy(ptr[0]);
            word_t tconc = s_copy(ptr[1]);
            // add obj to the tconc
            word_t new = s_obj_cons(FALSE_IMM, NIL_TAG);
            { word_t* t = &CAR(CDR(tconc)); assert(fromspace_start <= t && t < fromspace_end); }
            { word_t* t = &CDR(CDR(tconc)); assert(fromspace_start <= t && t < fromspace_end); }
            CADR(tconc) = obj;
            CDDR(tconc) = new;
            CDR(tconc) = new;
        }
        _s_collect_scan_ptr();
    } while(true);
    while(s_obj_is_vec(pending_hold_list)) {
        word_t *ptr = s_ffi_to_vec(pending_hold_list).ptr;
        word_t obj = ptr[0];
        word_t tconc = ptr[1];
        word_t next = ptr[2];
        ptr[2] = NIL_TAG;
        assert(!is_nonheap(obj) && !is_nonheap(tconc));
        if(s_is_forwarded(tconc)) {
            s_install_guardian(s_copy(obj), s_copy(tconc));
        }
        pending_hold_list = next;
    }
}

word_t s_gc(const intptr_t _sz) {
    bool forced_collect = _sz < 0;
    const word_t min_req_sz = 32 * sizeof(word_t);
    const word_t req_sz = forced_collect ? min_req_sz : max(_sz, min_req_sz);
    int free_space_old = fromspace_end - free_ptr;
    if((!forced_collect && req_sz <= (free_space_old * (intptr_t)sizeof(word_t)))){
        return 0;
    }
    for(int i = 0; i < HEAP_WORDS; ++i) {
        gc_markers[i] = false;
    }
    word_t *tmp;
    tmp = fromspace_start;
    fromspace_start = tospace_start;
    tospace_start = tmp;
    tmp = fromspace_end;
    fromspace_end = tospace_end;
    tospace_end = tmp;
    scan_ptr = fromspace_end;
    free_ptr = fromspace_start;
    EPHEMERON_LIST = NIL_TAG;
    // collect those symbol whose value is bound
    {
        S_ffi_vec_t vec = s_ffi_to_vec(SYMBOLS);
        for(size_t i = 0; i < vec.sz; ++i) {
            word_t bucket = vec.ptr[i];
            while(s_obj_is_pair(bucket)) {
                word_t sym = s_ephemeron_key(CAR(bucket));
                if(s_obj_is_sym(sym) && s_sym_val(sym) != UNBOUND_TAG) {
                    s_copy(sym);
                }
                bucket = CDR(bucket);
            }
        }
    }
    EXP = s_copy(EXP);
    PROC = s_copy(PROC);
    VAL = s_copy(VAL);
    VALS = s_copy(VALS);
    CONT = s_copy(CONT);
    ENV = s_copy(ENV);
    SYMBOLS = s_copy(SYMBOLS);
    TOK = s_copy(TOK);
    QUOTE = s_copy(QUOTE);
    UNQUOTE_SPLICING = s_copy(UNQUOTE_SPLICING);
    UNQUOTE = s_copy(UNQUOTE);
    QUASIQUOTE = s_copy(QUASIQUOTE);
    BEGIN = s_copy(BEGIN);
    LAMBDA = s_copy(LAMBDA);
    IF = s_copy(IF);
    SET_BANG = s_copy(SET_BANG);
    DEFINE = s_copy(DEFINE);
    DEFMACRO = s_copy(DEFMACRO);
    for(intptr_t i = 0, n = TEMP_SP - TEMPS; i < n; ++i) {
        TEMPS[i] = s_copy(TEMPS[i]);
    }
    _s_collect_scan_ptr();
    _s_collect_ephemeron();
    _s_collect_guardian();
    s_rt_resize_symbol_table();
    int free_space_new = fromspace_end - free_ptr;
    if(req_sz <= free_space_new * (intptr_t)sizeof(word_t)) {
        return free_space_new << FIXNUM_SHIFT;
    }
    else {
        panic("insufficient memory");
    }
}
/* INTERPRETER */

// lexer
#define PEEKED -2

// NOTE: the Lexer Stream doesn't own data
typedef struct {
    int (*getc)(void*);
    int ch;
    void* data;
} Lexer_Stream;

int s_lex_getc(Lexer_Stream *f) {
    if(f->ch != PEEKED) {
        int ch = f->ch;
        f->ch = PEEKED;
        return ch;
    }
    else {
        return f->getc(f->data);
    }
}

int s_lex_ungetc(int ch, Lexer_Stream *f) {
    if(f->ch != PEEKED) panic(__func__);
    f->ch = ch;
    return 0;
}

int s_lex_peekc(Lexer_Stream *f) {
    if(f->ch == PEEKED) {
        f->ch = s_lex_getc(f);
    }
    return f->ch;
}

bool is_id_char(char c) {
    const char extended_set[] = "!$%&*+-./:<=>?@^_~";
    if((('a' <= c) && (c <= 'z'))
       || (('A' <= c) && (c <= 'Z'))
       || (('0' <= c) && (c <= '9'))){
        return true;
    }
    for(int i = 0; i < (int)sizeof(extended_set)/(int)sizeof(extended_set[0]); ++i) {
        if(extended_set[i] == c)
            return true;
    }
    return false;
}

bool is_num_char(char c) {
    return ('0' <= c && c <= '9') || ('A' <= c && c <= 'F') || ('a' <= c && c <= 'f');
}

uintptr_t lex_num(Lexer_Stream *fptr, int base) {
    uintptr_t x = 0;
    int c = s_lex_getc(fptr);
    while(is_num_char(c)) {
        if('0' <= c && c <= '9'){
            c = c - '0';
        }
        else if ('A' <= c && c <= 'F'){
            c = c - 'A' + 10;
        }
        else if ('a' <= c && c <= 'f'){
            c = c - 'a' + 10;
        }
        x = x * base + c;
        c = s_lex_getc(fptr);
    }
    s_lex_ungetc(c, fptr);
    return x;
}

word_t s_parse_next_token(Lexer_Stream *fptr) {
    char TOKEN_BUF[255];
    if((TOK & IMM_MASK) != NIL_TAG) {
        word_t t = TOK;
        TOK = NIL_TAG;
        return t;
    }
    for(;;) {
        int c = s_lex_getc(fptr);
        if(c < 0 || c == EOF) {
            return EOF_TAG;
        }
        else if(c == ';') {
            while(c != EOF && c != '\n') {
                c = s_lex_getc(fptr);
            }
        }
        else if(c == ' ' || c == '\n' || c == '\t' || c == '\r') {
            while(c >= 0 && (c == ' ' || c == '\n' || c == '\t' || c == '\r')) {
                c = s_lex_getc(fptr);
            }
            s_lex_ungetc(c, fptr);
        }
        else if(c == '\'' || c == '`' || c == '.') {
            return s_obj_tok(c);
        }
        else if(c == '(' || c == '[') {
            return s_obj_tok('(');
        }
        else if(c == ')' || c == ']') {
            return s_obj_tok(')');
        }
        else if(c == ',' && s_lex_peekc(fptr) == '@') {
            s_lex_getc(fptr);
            return s_obj_tok('@');
        }
        else if(c == ',') {
            return s_obj_tok(',');
        }
        else if(c == '#' && s_lex_peekc(fptr) == '\\') {
            s_lex_getc(fptr);
            return s_obj_char(s_lex_getc(fptr));
        }
        else if(c == '#' && s_lex_peekc(fptr) == 'x') {
            s_lex_getc(fptr);
            return s_obj_num(lex_num(fptr, 16), 0);
        }
        else if(c == '#' && s_lex_peekc(fptr) == 'b') {
            s_lex_getc(fptr);
            return s_obj_num(lex_num(fptr, 2), 0);
        }
        else if(c == '#' && s_lex_peekc(fptr) == 't') {
            s_lex_getc(fptr);
            return TRUE_IMM;
        }
        else if(c == '#' && s_lex_peekc(fptr) == 'f') {
            s_lex_getc(fptr);
            return FALSE_IMM;
        }
        else if(c == '-' && '0' <= s_lex_peekc(fptr) && s_lex_peekc(fptr) <= '9') {
            return s_obj_num(lex_num(fptr, 10), 1);
        }
        else if('0' <= c && c <= '9') {
            s_lex_ungetc(c, fptr);
            return s_obj_num(lex_num(fptr, 10), 0);
        }
        else if(c == '"') {
            int i = 0;
            c = s_lex_getc(fptr);
            while(c != '"'){
                if(c == '\\'){
                    c = s_lex_getc(fptr);
                    if(c == 'n'){
                        c = '\n';
                    }
                    else if(c == 'r'){
                        c = '\r';
                    }
                    else if(c == 't'){
                        c = '\t';
                    }
                    else if(c == '\\'){
                        c = '\\';
                    }
                    else if(c == '"'){
                        c = '"';
                    }
                    else {
                        panic(__func__);
                    }
                }
                TOKEN_BUF[i++] = c;
                c = s_lex_getc(fptr);
                if(i + 1 == sizeof(TOKEN_BUF)/sizeof(*TOKEN_BUF)) break;
            }
            TOKEN_BUF[i] = '\0';
            return s_obj_str_from_buf(TOKEN_BUF, i);
        }
        else if(is_id_char(c)) {
            int i = 0;
            TOKEN_BUF[i++] = c;
            c = s_lex_getc(fptr);
            while(is_id_char(c)){
                TOKEN_BUF[i++] = c;
                if(i + 1 == sizeof(TOKEN_BUF)/sizeof(*TOKEN_BUF)) break;
                c = s_lex_getc(fptr);
            }
            TOKEN_BUF[i] = '\0';
            s_lex_ungetc(c, fptr);
            return s_rt_add_sym_buf(TOKEN_BUF, i);
        }
        else {
            panic(__func__);
        }
    }
}

// reader
word_t s_parse(Lexer_Stream *fptr);

word_t s_parse_peek_token(Lexer_Stream *fptr) {
    if(TOK == NIL_TAG) {
        TOK = s_parse_next_token(fptr);
    }
    return TOK;
}

word_t s_parse_list(Lexer_Stream *fptr) {
    word_t tk = s_parse_peek_token(fptr);
    
    if(((tk & IMM_MASK) == TOK_TAG) && (tk >> IMM_SHIFT) == ')') {
        s_parse_next_token(fptr);
        return make_nil();
    }
    else if(((tk & IMM_MASK) == TOK_TAG) && (tk >> IMM_SHIFT) == '.') {
        s_parse_next_token(fptr);
        word_t v = s_parse(fptr);
        s_parse_next_token(fptr);
        return v;
    }
    else {
        word_t car = s_parse(fptr);
        word_t cdr = s_parse_list(fptr);
        return s_obj_cons(car, cdr);
    }
}

word_t s_parse_all(Lexer_Stream *fptr) {
    word_t v = s_parse(fptr);
    if(v == EOF_TAG) {
        return NIL_TAG;
    }
    else {
        return s_obj_cons(v, s_parse_all(fptr));
    }
}

word_t s_parse(Lexer_Stream *fptr) {
    word_t tk = s_parse_next_token(fptr);
    if(((tk & PTR_MASK) != IMM_TAG) || ((tk & PTR_MASK) == IMM_TAG && (tk & IMM_MASK) != TOK_TAG)) {
        return tk;
    }
    else if(tk == s_obj_tok('\'')) {
        return s_obj_cons(QUOTE, s_obj_cons(s_parse(fptr), make_nil()));
    }
    else if(tk == s_obj_tok('@')) {
        return s_obj_cons(UNQUOTE_SPLICING, s_obj_cons(s_parse(fptr), make_nil()));
    }
    else if(tk == s_obj_tok(',')) {
        return s_obj_cons(UNQUOTE, s_obj_cons(s_parse(fptr), make_nil()));
    }
    else if(tk == s_obj_tok('`')) {
        return s_obj_cons(QUASIQUOTE, s_obj_cons(s_parse(fptr), make_nil()));
    }
    else if(tk == s_obj_tok('(')) {
        return s_parse_list(fptr);
    }
    else {
        panic(__func__);
    }
}

// evaluator
word_t s_rt_assq(word_t x, word_t xs) {
    while(s_obj_is_pair(xs)) {
        if(CAAR(xs) == x) {
            return CAR(xs);
        }
        xs = CDR(xs);
    }
    return FALSE_IMM;
}

word_t s_rt_apply_env_maybe(word_t sym, word_t env) {
    while(s_obj_is_pair(env)) {
        word_t val = s_rt_assq(sym, CAR(env));
        if(val != FALSE_IMM) {
            return val;
        }
        env = CDR(env);
    }
    return FALSE_IMM;
}


void s_eval();
void s_apply_clos();
#define _s_apply_cont ((s_funptr_t)s_ffi_from_clos(CONT)[-2])

void s_cont_macro_val() {
    if(_RETC != 1) panic(__func__);
    NEXT = s_apply_clos;
}

void s_cont_after_macro() {
    if(_RETC != 1) panic(__func__);
    EXP = VAL;
    ENV = _s_ffi_from_clos(CONT)[1];
    CONT = _s_ffi_from_clos(CONT)[0];
    NEXT = s_eval;
}

static inline void s_cont_app_nxt() {
    if((_s_ffi_from_clos(CONT)[3] & PTR_MASK) == PAIR_TAG) {
        EXP = CAR(_s_ffi_from_clos(CONT)[3]);
        _s_ffi_from_clos(CONT)[3] = CDR(_s_ffi_from_clos(CONT)[3]);
        ENV = _s_ffi_from_clos(CONT)[4];
        NEXT = s_eval;
    }
    else {
        PROC = CAR(_s_ffi_from_clos(CONT)[1]);
        VAL = CDR(_s_ffi_from_clos(CONT)[1]);
        CONT = _s_ffi_from_clos(CONT)[0];
        NEXT = (s_funptr_t)s_ffi_from_clos(PROC)[-2];
    }
}

void s_cont_app_1() {
    if(_RETC != 1) panic(__func__);
    EXP = s_obj_cons_alloc(VAL, make_nil());
    CDR(_s_ffi_from_clos(CONT)[2]) = EXP;
    _s_ffi_from_clos(CONT)[2] = EXP;
    s_cont_app_nxt();
}

void s_cont_app_0() {
    if(_RETC != 1) panic(__func__);
    if((VAL & PTR_MASK) == CLOS_TAG && _s_ffi_from_clos(VAL)[-2] == (word_t)s_cont_macro_val) {
        PROC = VAL;
        VAL = _s_ffi_from_clos(CONT)[3]; // EXPS
        CONT = _s_ffi_from_clos(CONT)[0];
        EXP = s_obj_clos(s_cont_after_macro, s_obj_int(2));
        _s_ffi_from_clos(EXP)[0] = CONT;
        _s_ffi_from_clos(EXP)[1] = ENV;
        CONT = EXP;
        NEXT = s_apply_clos;
    }
    else {
        EXP = s_obj_cons_alloc(VAL, make_nil());
        _s_ffi_from_clos(CONT)[1] = EXP;
        _s_ffi_from_clos(CONT)[2] = EXP;
        _s_ffi_from_clos(CONT)[-2] = (word_t)s_cont_app_1;
        s_cont_app_nxt();
    }
}

void s_cont_if() {
    if(_RETC != 1) panic(__func__);
    EXP = _s_ffi_from_clos(CONT)[(VAL != FALSE_IMM ? 2 : 3)];
    ENV = _s_ffi_from_clos(CONT)[1];
    CONT = _s_ffi_from_clos(CONT)[0];
    NEXT = s_eval;
}

void s_cont_set() {
    if(_RETC != 1) panic(__func__);
    ENV = _s_ffi_from_clos(CONT)[1];
    word_t FORM = _s_ffi_from_clos(CONT)[3];
    PROC = _s_ffi_from_clos(CONT)[2]; // name
    if(FORM == DEFMACRO) {
        s_ffi_from_clos(VAL)[-2] = (word_t)s_cont_macro_val;
        s_sym_val_set(PROC, VAL);
    }
    else if(FORM == SET_BANG) {
        EXP = s_rt_apply_env_maybe(PROC, ENV);
        if(EXP == FALSE_IMM) {
            s_sym_val_set(PROC, VAL);
        }
        else{
            CDR(EXP) = VAL;
        }
    }
    else if(FORM == DEFINE) {
        EXP = ENV == make_nil() ? ENV : s_rt_assq(PROC, CAR(ENV));
        if(ENV == make_nil()) {
            s_sym_val_set(PROC, VAL);
        }
        else if(EXP != FALSE_IMM) {
            panic("rebound?");
        }
        else {
            EXP = s_obj_cons_alloc(PROC, VAL);
            EXP = s_obj_cons_alloc(EXP, CAR(ENV));
            CAR(ENV) = EXP;
        }
    }
    VAL = VOID_TAG;
    CONT = _s_ffi_from_clos(CONT)[0];
    NEXT = _s_apply_cont;
}

void s_cont_seq() {
    ENV = _s_ffi_from_clos(CONT)[2];
    EXP = _s_ffi_from_clos(CONT)[1];
    if(CDR(EXP) == make_nil()) {
        CONT = _s_ffi_from_clos(CONT)[0];
    }
    else {
        _s_ffi_from_clos(CONT)[1] = CDR(EXP);
    }
    EXP = CAR(EXP);
    NEXT = s_eval;
}

word_t s_rt_init_env() {
    return NIL_TAG;
}

void s_apply_clos() {
    #define LCL_ENV     ENV
    #define LCL_PARAMS  EXP
    #define LCL_ARGV    VALS
    LCL_ENV = make_nil();
    LCL_PARAMS = _s_ffi_from_clos(PROC)[0];
    VALS = VAL;
    while(s_obj_is_pair(LCL_PARAMS)) {
        if((LCL_ARGV & PTR_MASK) != PAIR_TAG) panic("BAD ARGUMENTS!");
        VAL = s_obj_cons_alloc(CAR(LCL_PARAMS), CAR(LCL_ARGV));
        LCL_ENV = s_obj_cons_alloc(VAL, LCL_ENV);
        LCL_PARAMS = CDR(LCL_PARAMS);
        LCL_ARGV = CDR(LCL_ARGV);
    }
    if (LCL_PARAMS != NIL_TAG) {
        VAL = s_obj_cons_alloc(LCL_PARAMS, LCL_ARGV);
        LCL_ENV = s_obj_cons_alloc(VAL, LCL_ENV);
    }
    ENV = s_obj_cons_alloc(LCL_ENV, _s_ffi_from_clos(PROC)[2]);
    EXP = _s_ffi_from_clos(PROC)[1]; // body
    #undef LCL_ENV
    #undef LCL_PARAMS
    #undef LCL_ARGV
    NEXT = s_eval;
}

void s_eval() {
    _RETC = 1;
    VALS = make_nil();
    if((EXP & FIXNUM_MASK) == FIXNUM_TAG
        || (EXP & PTR_MASK) == FLO_TAG
        || (EXP & PTR_MASK) == IMM_TAG
        || s_obj_is_bignum(EXP)
        || s_obj_is_str(EXP)
        || s_obj_is_vec(EXP)
        || s_obj_is_bytevec(EXP)) {
        VAL = EXP;
        NEXT = _s_apply_cont;
    }
    else if((EXP & PTR_MASK) == SYM_TAG) {
        VAL = s_rt_apply_env_maybe(EXP, ENV);
        VAL = VAL == FALSE_IMM ? s_sym_val(EXP) : CDR(VAL);
        if (VAL == UNBOUND_TAG) { s_rt_writeln(EXP); panic("s_eval UNBOUND!");}
        NEXT = _s_apply_cont;
    }
    else if(!s_obj_is_pair(EXP)) {
        s_rt_writeln(EXP);
        panic("unknown form!");
    }
    else if(CAR(EXP) == DEFINE || CAR(EXP) == SET_BANG || CAR(EXP) == DEFMACRO) {
        VAL = s_obj_clos_alloc(s_cont_set, s_obj_int(4));
        // (def var val)
        if((CADR(EXP) & PTR_MASK) == SYM_TAG) {
            _s_ffi_from_clos(VAL)[0] = CONT;
            _s_ffi_from_clos(VAL)[1] = ENV;
            _s_ffi_from_clos(VAL)[2] = CADR(EXP);
            _s_ffi_from_clos(VAL)[3] = CAR(EXP);
            CONT = VAL;
            EXP = CADDR(EXP);
        }
        // (def (kw . params) . es)
        else if((CADR(EXP) & PTR_MASK) == PAIR_TAG &&
                (CAADR(EXP) & PTR_MASK) == SYM_TAG) {
            _s_ffi_from_clos(VAL)[0] = CONT;
            _s_ffi_from_clos(VAL)[1] = ENV;
            _s_ffi_from_clos(VAL)[2] = CAADR(EXP);
            _s_ffi_from_clos(VAL)[3] = CAR(EXP);
            CONT = VAL;
            EXP = s_obj_cons_alloc(CDADR(EXP), CDDR(EXP));
            EXP = s_obj_cons_alloc(LAMBDA, EXP);
        }
        else {
            panic("bad definition form!");
        }
    }
    else if(CAR(EXP) == IF) {
        VAL = s_obj_clos_alloc(s_cont_if, s_obj_int(4));
        _s_ffi_from_clos(VAL)[0] = CONT;
        _s_ffi_from_clos(VAL)[1] = ENV;
        _s_ffi_from_clos(VAL)[2] = CADDR(EXP);
        _s_ffi_from_clos(VAL)[3] = CDDDR(EXP) == NIL_TAG ? VOID_TAG : CADDDR(EXP);
        CONT = VAL;
        EXP = CAR(CDR(EXP));
    }
    else if(CAR(EXP) == LAMBDA) {
        VAL = s_obj_clos_alloc(s_apply_clos, s_obj_int(3));
        _s_ffi_from_clos(VAL)[0] = CADR(EXP); // params
        EXP = CDDR(EXP) == make_nil() ? CADR(EXP) : s_obj_cons_alloc(BEGIN, CDDR(EXP));
        _s_ffi_from_clos(VAL)[1] = EXP; // body
        _s_ffi_from_clos(VAL)[2] = ENV;
        NEXT = _s_apply_cont;
    }
    else if(CAR(EXP) == QUOTE) {
        VAL = CADR(EXP);
        NEXT = _s_apply_cont;
    }
    else if(CAR(EXP) == BEGIN) {
        if(CDDR(EXP) != make_nil()) {
            PROC = s_obj_clos_alloc(s_cont_seq, s_obj_int(3));
            _s_ffi_from_clos(PROC)[0] = CONT;
            _s_ffi_from_clos(PROC)[1] = CDDR(EXP);
            _s_ffi_from_clos(PROC)[2] = ENV;
            CONT = PROC;
        }
        EXP = CADR(EXP);
    }
    else {
        PROC = s_obj_clos_alloc(s_cont_app_0, s_obj_int(5));
        _s_ffi_from_clos(PROC)[0] = CONT;
        _s_ffi_from_clos(PROC)[1] = make_nil();
        _s_ffi_from_clos(PROC)[2] = make_nil();
        _s_ffi_from_clos(PROC)[3] = CDR(EXP);
        _s_ffi_from_clos(PROC)[4] = ENV;
        CONT = PROC;
        EXP = CAR(EXP);
    }
}

void s_cont_end() { panic(__func__); }

word_t s_eval_entry() {
    CONT = s_obj_clos_alloc(s_cont_end, s_obj_int(0));
    // TODO: this is a temporary workaround so that the repl can use the new eval once the kernel intialize it.
    word_t clos = s_sym_val(s_rt_add_sym_cstr("eval"));
    if((s_funptr_t)s_ffi_from_clos(clos)[-2] != s_pr_eval) {
        PROC = clos;
        VAL = s_obj_cons_alloc(EXP,make_nil());
        NEXT = (s_funptr_t)s_ffi_from_clos(clos)[-2];
    }
    else {
        NEXT = s_eval;
    }
    for(;NEXT != s_cont_end;) NEXT();
    if(_RETC != 1) panic(__func__);
    ENV = make_nil();
    return VAL;
}

word_t s_collect() {
    // TODO: this is a temporary workaround so that the repl can use the new eval once the kernel intialize it.
    word_t clos = s_sym_val(s_rt_add_sym_cstr("collect"));
    if((s_funptr_t)s_ffi_from_clos(clos)[-2] != s_pr_collect) {
        PROC = clos;
        VAL = s_obj_cons_alloc(EXP,make_nil());
        NEXT = (s_funptr_t)s_ffi_from_clos(clos)[-2];
        for(;NEXT != s_cont_end;) NEXT();
        if(_RETC != 1) panic(__func__);
        ENV = make_nil();
    }
    else {
        s_gc(-1);
        VAL = VOID_TAG;
    }
    return VAL;
}

// FFI
void s_pr_eval() {
    EXP = CAR(VAL);
    ENV = s_rt_init_env();
    NEXT = s_eval;
}

void s_pr_apply_2() {
    PROC = CAR(VAL);
    VAL = CADR(VAL);
    NEXT = (s_funptr_t)_s_ffi_from_clos(PROC)[-2];
}

void implicit_ret_mv() {
    PROC = _s_ffi_from_clos(CONT)[1];
    NEXT = (s_funptr_t)s_ffi_from_clos(PROC)[-2];
    if(_RETC == 1 && VALS != make_nil()) panic("bad multi values return\r\n");
    if(_RETC > 1 || (_RETC >= 1 && _s_ffi_from_clos(PROC)[-2] == (word_t)s_apply_clos)) {
        VAL = s_obj_cons_alloc(VAL, VALS);
    }
    CONT = _s_ffi_from_clos(CONT)[0];
}

void explicit_ret_cont() {
    if(VAL == make_nil()) {
        _RETC = 0;
    }
    else if(s_obj_is_pair(VAL) && CDR(VAL) == make_nil()) {
        _RETC = 1;
        VALS = make_nil();
        VAL = CAR(VAL);
    }
    else {
        _RETC = 2;
        VALS = CDR(VAL);
        VAL = CAR(VAL);
    }
    CONT = s_ffi_from_clos(PROC)[0];
    NEXT = _s_apply_cont;
}

void s_pr_call_with_values() {
    EXP = s_obj_clos_alloc(implicit_ret_mv, s_obj_int(2));
    _s_ffi_from_clos(EXP)[0] = CONT;
    _s_ffi_from_clos(EXP)[1] = CADR(VAL);
    CONT = EXP;
    PROC = CAR(VAL);
    VAL = make_nil();
    NEXT = (s_funptr_t)s_ffi_from_clos(PROC)[-2];
}

void s_pr_callcc() {
    PROC = CAR(VAL);
    VAL = s_obj_clos_alloc(explicit_ret_cont, s_obj_int(1));
    _s_ffi_from_clos(VAL)[0] = CONT;
    VAL = s_obj_cons_alloc(VAL, make_nil());
    NEXT = (s_funptr_t)s_ffi_from_clos(PROC)[-2];
}

void s_pr_exit() {
    _RETC = 1;
    if(VAL == NIL_TAG) {
        exit(0);
    }
    else {
        exit(CAR(VAL) >> FIXNUM_SHIFT);
    }
}

void s_pr_newline() {
    _RETC = 1;
    _s_putc('\n');
    VAL = VOID_TAG;
    NEXT = _s_apply_cont;

}

void s_pr_write_mem_32() {
    _RETC = 1;
    S_ffi_sword_t addr = s_ffi_to_sword(CAR(VAL));
    S_ffi_sword_t val = s_ffi_to_sword(CADR(VAL));
    if(addr.sign != 0) panic(__func__);
    *(volatile uint32_t*)addr.x = (uint32_t)val.x;
    VAL = VOID_TAG;
    NEXT = _s_apply_cont;
}

void s_pr_read_mem_32() {
    _RETC = 1;
    S_ffi_sword_t addr = s_ffi_to_sword(CAR(VAL));
    uint32_t val = *(volatile uint32_t*)addr.x;
    VAL = s_obj_num_alloc(val, 0);
    NEXT = _s_apply_cont;
}

void s_pr_write() {
    _RETC = 1;
    s_rt_write(CAR(VAL));
    VAL = VOID_TAG;
    NEXT = _s_apply_cont;
}

void s_pr_writeln() {
    _RETC = 1;
    s_rt_writeln(CAR(VAL));
    VAL = VOID_TAG;
    NEXT = _s_apply_cont;
}

void s_pr_clos_pred() {
    _RETC = 1;
    VAL = make_bool((CAR(VAL) & PTR_MASK) == CLOS_TAG);
    NEXT = _s_apply_cont;   
}

void s_pr_null_pred() {
    _RETC = 1;
    VAL = make_bool(CAR(VAL) == NIL_TAG);
    NEXT = _s_apply_cont;
}

void s_pr_bool_pred() {
    _RETC = 1;
    VAL = make_bool((CAR(VAL) & IMM_MASK) == BOOL_TAG);
    NEXT = _s_apply_cont;
}

void s_pr_cons() {
    _RETC = 1;
    VAL = s_obj_cons_alloc(CAR(VAL), CADR(VAL));
    NEXT = _s_apply_cont;
}

void s_pr_pair_pred() {
    _RETC = 1;
    VAL = make_bool(s_obj_is_pair(CAR(VAL)));
    NEXT = _s_apply_cont;
}

void s_pr_car() {
    _RETC = 1;
    VAL = CAAR(VAL);
    NEXT = _s_apply_cont;
}

void s_pr_cdr() {
    _RETC = 1;
    VAL = CDAR(VAL);
    NEXT = _s_apply_cont;
}

void s_pr_car_set() {
    _RETC = 1;
    CAR(CAR(VAL)) = CADR(VAL);
    VAL = VOID_TAG;
    NEXT = _s_apply_cont;
}

void s_pr_cdr_set() {
    _RETC = 1;
    CDR(CAR(VAL)) = CADR(VAL);
    VAL = VOID_TAG;
    NEXT = _s_apply_cont;
}

void s_pr_collect() {
    _RETC = 1;
    if(VAL == NIL_TAG) {
        s_gc(-1);
    }
    else {
        s_gc(s_ffi_to_fixnum(CAR(VAL)));
    }
    VAL = VOID_TAG;
    NEXT = _s_apply_cont;
}

void s_pr_install_guardian() {
    _RETC = 1;
    VAL = s_install_guardian(CAR(VAL), CADR(VAL));
    NEXT = _s_apply_cont;
}

void s_pr_ephemeron() {
    _RETC = 1;
    VAL = s_obj_ephemeron_alloc(CAR(VAL), CADR(VAL));
    NEXT = _s_apply_cont;
}

void s_pr_ephemeron_pred() {
    _RETC = 1;
    VAL = make_bool(s_obj_is_ephemeron(CAR(VAL)));
    NEXT = _s_apply_cont;
}

void s_pr_ephemeron_key() {
    _RETC = 1;
    VAL = s_ephemeron_key(CAR(VAL));
    NEXT = _s_apply_cont;
}

void s_pr_ephemeron_val() {
    _RETC = 1;
    VAL = s_ephemeron_val(CAR(VAL));
    NEXT = _s_apply_cont;
}

void s_pr_sym_pred() {
    _RETC = 1;
    VAL = make_bool((CAR(VAL) & PTR_MASK) == SYM_TAG);
    NEXT = _s_apply_cont;
}

void s_pr_sym_hash() {
    _RETC = 1;
    VAL = s_ffi_to_sym(CAR(VAL)).hash;
    NEXT = _s_apply_cont;
}

void s_pr_sym2str() {
    _RETC = 1;
    S_ffi_str_t name = s_ffi_to_str(s_ffi_to_sym(CAR(VAL)).name);
    VAL = s_obj_str_from_buf_alloc(name.s, name.sz);
    NEXT = _s_apply_cont;
}

void s_pr_str2sym() {
    _RETC = 1;
    S_ffi_str_t name = s_ffi_to_str(CAR(VAL));
    VAL = s_rt_add_sym_buf(name.s, name.sz);
    NEXT = _s_apply_cont;
}

void s_pr_fx_pred() {
    _RETC = 1;
    VAL = make_bool((CAR(VAL) & FIXNUM_MASK) == FIXNUM_TAG);
    NEXT = _s_apply_cont;
}

void s_pr_int2char() {
    _RETC = 1;
    s_ffi_to_fixnum(CAR(VAL));
    VAL = (CAR(VAL) << (IMM_SHIFT - FIXNUM_SHIFT)) | CHAR_TAG;
    NEXT = _s_apply_cont;
}

void s_pr_eq() {
    _RETC = 1;
    VAL = make_bool(CAR(VAL) == CADR(VAL));
    NEXT = _s_apply_cont;
}

void s_pr_eqn() {
    _RETC = 1;
    S_ffi_sword_t v = s_ffi_to_sword(CAR(VAL));
    S_ffi_sword_t w = s_ffi_to_sword(CADR(VAL));
    VAL = make_bool(v.sign == w.sign && v.x == w.x);
    NEXT = _s_apply_cont;
}

void s_pr_lt() {
    _RETC = 1;
    S_ffi_sword_t v = s_ffi_to_sword(CAR(VAL));
    S_ffi_sword_t w = s_ffi_to_sword(CADR(VAL));
    bool b = (v.sign != w.sign)
             ? (v.sign > w.sign) // neg,pos to be true ==> (neg == 1) && (pos == 0)
             : (v.sign == 0 ? v.x < w.x : w.x < v.x);
    VAL = make_bool(b);
    NEXT = _s_apply_cont;
}

void s_pr_le() {
    _RETC = 1;
    S_ffi_sword_t v = s_ffi_to_sword(CAR(VAL));
    S_ffi_sword_t w = s_ffi_to_sword(CADR(VAL));
    bool b = (v.sign != w.sign)
             ? (v.sign > w.sign) // neg,pos to be true ==> (neg == 1) && (pos == 0)
             : (v.sign == 0 ? v.x <= w.x : w.x <= v.x);
    VAL = make_bool(b);
    NEXT = _s_apply_cont;
}

void s_pr_gt() {
    _RETC = 1;
    S_ffi_sword_t v = s_ffi_to_sword(CAR(VAL));
    S_ffi_sword_t w = s_ffi_to_sword(CADR(VAL));
    bool b = (v.sign != w.sign)
             ? (v.sign < w.sign) // pos,neg to be true ==> (pos == 0) && (neg == 1)
             : (v.sign == 0 ? v.x > w.x : w.x > v.x);
    VAL = make_bool(b);
    NEXT = _s_apply_cont;
}

void s_pr_ge() {
    _RETC = 1;
    S_ffi_sword_t v = s_ffi_to_sword(CAR(VAL));
    S_ffi_sword_t w = s_ffi_to_sword(CADR(VAL));
    bool b = (v.sign != w.sign)
             ? (v.sign < w.sign) // pos,neg to be true ==> (pos == 0) && (neg == 1)
             : (v.sign == 0 ? v.x >= w.x : w.x >= v.x);
    VAL = make_bool(b);
    NEXT = _s_apply_cont;
}

S_ffi_sword_t s_sword_add(S_ffi_sword_t v, S_ffi_sword_t w);
S_ffi_sword_t s_sword_sub(S_ffi_sword_t v, S_ffi_sword_t w);

S_ffi_sword_t s_sword_complementize(S_ffi_sword_t v) {
    if(v.sign == 0) {
        return v;
    }
    else if(v.x <= (uintptr_t)INTPTR_MIN) {
        return (S_ffi_sword_t){.x = -v.x, .sign = 1};
    }
    else {
        panic(__func__);
    }
}

S_ffi_sword_t s_sword_abs(S_ffi_sword_t v) {
    return (S_ffi_sword_t){.x = v.x, .sign = 0};
}

S_ffi_sword_t s_sword_add(S_ffi_sword_t v, S_ffi_sword_t w) {
    if(v.sign == w.sign && UINTPTR_MAX - v.x > w.x) {
        return (S_ffi_sword_t){.x = v.x + w.x, .sign = 0};
    }
    else if(w.sign == 1) {
        return s_sword_sub(v, s_sword_abs(w));
    }
    else if(v.sign == 1) {
        return s_sword_sub(w, s_sword_abs(v));
    }
    else {
        return (S_ffi_sword_t){.x = 0, .sign = -1};
    }
}

S_ffi_sword_t s_sword_sub(S_ffi_sword_t v, S_ffi_sword_t w) {
    if(v.sign == 0 && w.sign == 0) {
        if(v.x >= w.x) {
            return (S_ffi_sword_t){.x = v.x - w.x, .sign = 0};
        }
        else {
            return (S_ffi_sword_t){.x = w.x - v.x, .sign = 1};    
        }
    }
    else if(v.sign == 0 && w.sign == 1) {
        return s_sword_add(v, s_sword_abs(w));
    }
    else if(v.sign == 1 && w.sign == 0) {
        S_ffi_sword_t r = s_sword_add(s_sword_abs(v), s_sword_abs(w));
        return (S_ffi_sword_t){.x = r.x, .sign = 1};
    }
    else if(v.sign == 1 && w.sign == 1) {
        return s_sword_sub(s_sword_abs(w), s_sword_abs(v));
    }
    else {
        return (S_ffi_sword_t) {.x = 0, .sign = -1};
    }
}

S_ffi_sword_t s_sword_mul(S_ffi_sword_t v, S_ffi_sword_t w) {
    return (S_ffi_sword_t) {.x = v.x * w.x, .sign = v.sign == w.sign ? 0 : 1};
}

void s_pr_add() {
    _RETC = 1;
    S_ffi_sword_t r = s_sword_add(s_ffi_to_sword(CAR(VAL)), s_ffi_to_sword(CADR(VAL)));
    VAL = r.sign == -1 ? FALSE_IMM : s_obj_num_alloc(r.x, r.sign);
    NEXT = _s_apply_cont;
}

void s_pr_sub() {
    _RETC = 1;
    if(CDR(VAL) == NIL_TAG) {
        S_ffi_sword_t r = s_ffi_to_sword(CAR(VAL));
        VAL = s_obj_num_alloc(r.x, r.sign ? 0 : 1);
    }
    else {
        S_ffi_sword_t r = s_sword_sub(s_ffi_to_sword(CAR(VAL)), s_ffi_to_sword(CADR(VAL)));
        VAL = r.sign == -1 ? FALSE_IMM : s_obj_num_alloc(r.x, r.sign);
    }
    NEXT = _s_apply_cont;
}

void s_pr_mul() {
    _RETC = 1;
    S_ffi_sword_t r = s_sword_mul(s_ffi_to_sword(CAR(VAL)), s_ffi_to_sword(CADR(VAL)));
    VAL = s_obj_num_alloc(r.x, r.sign);
    NEXT = _s_apply_cont;
}

void s_pr_mod() {
    _RETC = 1;
    S_ffi_sword_t v = s_ffi_to_sword(CAR(VAL));
    S_ffi_sword_t w = s_ffi_to_sword(CADR(VAL));
    VAL = s_obj_num_alloc(v.x % w.x, v.sign == w.sign ? 0 : 1);
    NEXT = _s_apply_cont;
}

void s_pr_div() {
    _RETC = 1;
    S_ffi_sword_t v = s_ffi_to_sword(CAR(VAL));
    S_ffi_sword_t w = s_ffi_to_sword(CADR(VAL));
    VAL = s_obj_num_alloc(v.x / w.x, v.sign == w.sign ? 0 : 1);
    NEXT = _s_apply_cont;
}

void s_pr_ash() {
    _RETC = 1;
    S_ffi_sword_t sh = s_ffi_to_sword(CADR(VAL));
    S_ffi_sword_t v = s_ffi_to_sword(CAR(VAL));
    VAL = s_obj_num_alloc((uintptr_t)(sh.sign == 0 ? (v.x << sh.x) : (v.x >> sh.x)), v.sign);
    NEXT = _s_apply_cont;
}

void s_pr_ior() {
    _RETC = 1;
    S_ffi_sword_t v = s_sword_complementize(s_ffi_to_sword(CAR(VAL)));
    S_ffi_sword_t w = s_sword_complementize(s_ffi_to_sword(CADR(VAL)));
    intptr_t r = (intptr_t)(v.x | w.x);
    VAL = s_obj_num_alloc_from_iptr(r);
    NEXT = _s_apply_cont;
}

void s_pr_and() {
    _RETC = 1;
    S_ffi_sword_t v = s_sword_complementize(s_ffi_to_sword(CAR(VAL)));
    S_ffi_sword_t w = s_sword_complementize(s_ffi_to_sword(CADR(VAL)));
    intptr_t r = (intptr_t)(v.x & w.x);
    VAL = s_obj_num_alloc_from_iptr(r);
    NEXT = _s_apply_cont;
}

void s_pr_not() {
    _RETC = 1;
    S_ffi_sword_t v = s_sword_complementize(s_ffi_to_sword(CAR(VAL)));
    intptr_t r = (intptr_t)(~v.x);
    VAL = s_obj_num_alloc_from_iptr(r);
    NEXT = _s_apply_cont;
}

void s_pr_char_pred() {
    _RETC = 1;
    VAL = make_bool((CAR(VAL) & IMM_MASK) == CHAR_TAG);
    NEXT = _s_apply_cont;
}

void s_pr_char2int() {
    _RETC = 1;
    if((CAR(VAL) & IMM_MASK) != CHAR_TAG) panic(__func__);
    VAL = (CAR(VAL) >> (IMM_SHIFT - FIXNUM_SHIFT));
    NEXT = _s_apply_cont;
}

void s_pr_vec_pred() {
    _RETC = 1;
    VAL = make_bool(s_obj_is_vec(CAR(VAL)));
    NEXT = _s_apply_cont;
}

void s_pr_make_vec() {
    _RETC = 1;
    intptr_t len = s_ffi_to_fixnum(CAR(VAL));
    word_t ch = s_obj_is_pair(CDR(VAL)) ? CADR(VAL) : s_obj_int(0);
    VAL = s_obj_vec_alloc(len, ch);
    NEXT = _s_apply_cont;
}

void s_pr_vec_len() {
    _RETC = 1;
    VAL = s_obj_int(s_ffi_to_vec(CAR(VAL)).sz);
    NEXT = _s_apply_cont;
}

void s_pr_vec_ref() {
    _RETC = 1;
    S_ffi_vec_t t = s_ffi_to_vec(CAR(VAL));
    VAL = t.ptr[s_ffi_to_fixnum(CADR(VAL))];
    NEXT = _s_apply_cont;
}

void s_pr_vec_set() {
    _RETC = 1;
    S_ffi_vec_t t = s_ffi_to_vec(CAR(VAL));
    t.ptr[s_ffi_to_fixnum(CADR(VAL))] = CADDR(VAL);
    VAL = VOID_TAG;
    NEXT = _s_apply_cont;
}

void s_pr_str_pred() {
    _RETC = 1;
    VAL = make_bool(s_obj_is_str(CAR(VAL)));
    NEXT = _s_apply_cont;
}

void s_pr_make_str() {
    _RETC = 1;
    intptr_t len = s_ffi_to_fixnum(CAR(VAL));
    word_t ch = s_obj_is_pair(CDR(VAL)) ? CADR(VAL) : s_obj_char(0);
    VAL = s_obj_str_alloc(len, ch);
    NEXT = _s_apply_cont;
}

void s_pr_str_len() {
    _RETC = 1;
    VAL = s_obj_int(s_ffi_to_str(CAR(VAL)).sz);
    NEXT = _s_apply_cont;
}

void s_pr_str_ref() {
    _RETC = 1;
    S_ffi_str_t s = s_ffi_to_str(CAR(VAL));
    VAL = s_obj_char(s.s[s_ffi_to_fixnum(CADR(VAL))]);
    NEXT = _s_apply_cont;
}

void s_pr_str_set() {
    _RETC = 1;
    S_ffi_str_t s = s_ffi_to_str(CAR(VAL));
    s.s[s_ffi_to_fixnum(CADR(VAL))] = s_ffi_to_char(CADDR(VAL));
    VAL = VOID_TAG;
    NEXT = _s_apply_cont;
}

void s_pr_bytevec_pred() {
    _RETC = 1;
    VAL = make_bool(s_obj_is_bytevec(CAR(VAL)));
    NEXT = _s_apply_cont;
}

void s_pr_make_bytevec() {
    _RETC = 1;
    intptr_t len = s_ffi_to_fixnum(CAR(VAL));
    word_t ch = s_obj_is_pair(CDR(VAL)) ? CADR(VAL) : s_obj_int(0);
    VAL = s_obj_bytevec_alloc(len, ch);
    NEXT = _s_apply_cont;
}

void s_pr_bytevec_len() {
    _RETC = 1;
    VAL = s_obj_int(s_ffi_to_bytevec(CAR(VAL)).sz);
    NEXT = _s_apply_cont;
}

void s_pr_bytevec_ref() {
    _RETC = 1;
    S_ffi_bytevec_t t = s_ffi_to_bytevec(CAR(VAL));
    VAL = s_obj_int(t.bv[s_ffi_to_fixnum(CADR(VAL))]);
    NEXT = _s_apply_cont;
}

void s_pr_bytevec_set() {
    _RETC = 1;
    S_ffi_bytevec_t t = s_ffi_to_bytevec(CAR(VAL));
    t.bv[s_ffi_to_fixnum(CADR(VAL))] = (s_ffi_to_fixnum(CADDR(VAL)) & UINT8_MAX);
    VAL = VOID_TAG;
    NEXT = _s_apply_cont;
}

void s_pr_bignum_pred() {
    _RETC = 1;
    VAL = make_bool(s_obj_is_bignum(CAR(VAL)));
    NEXT = _s_apply_cont;
}

void s_pr_make_bignum() {
    _RETC = 1;
    VAL = s_obj_bignum_alloc(CAR(VAL), CADR(VAL));
    NEXT = _s_apply_cont;
}

void s_pr_bignum_limb() {
    _RETC = 1;
    VAL = s_ffi_bignum_limb(CAR(VAL));
    NEXT = _s_apply_cont;
}

void s_pr_bignum_sign() {
    _RETC = 1;
    VAL = s_obj_int(s_ffi_bignum_sign(CAR(VAL)));
    NEXT = _s_apply_cont;
}

#if SCM_RAW_FFI && __STDC_HOSTED__
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <dlfcn.h>
#include <unistd.h>

void *s_ffi_dlopen(const char* inp, const char* entry) {
    const char *_inp = inp;
    const char *_ent = entry;
    void *handle = dlopen(_inp, RTLD_NOW);
    if (!handle) {
        fprintf(stderr, "\n[%s:%d] %s: dlopen failed: %s %s\n", __FILE__, __LINE__, __func__, _inp, dlerror());
        abort();
    }
    void *sym = dlsym(handle, _ent);
    if (!sym) {
        fprintf(stderr, "\n[%s:%d] %s: dlsym failed: %s %s\n", __FILE__, __LINE__, __func__, _ent, dlerror());
        dlclose(handle);
        abort();
    }
    return sym;
}

void s_pr_foreign_call() {
    S_ffi_bytevec_t func_name = s_ffi_to_bytevec(CAR(VAL));
    S_ffi_vec_t args = s_ffi_to_vec(CADR(VAL));
    assert(args.sz <= 6);
    assert(func_name.bv[func_name.sz - 1] == '\0');
    union {
        word_t (*f0)(void);
        word_t (*f1)(word_t);
        word_t (*f2)(word_t, word_t);
        word_t (*f3)(word_t, word_t, word_t);
        word_t (*f4)(word_t, word_t, word_t, word_t);
        word_t (*f5)(word_t, word_t, word_t, word_t, word_t);
        word_t (*f6)(word_t, word_t, word_t, word_t, word_t, word_t);
    } fn;
    fn.f0 = (word_t (*) (void)) s_ffi_dlopen(NULL, (char*)func_name.bv);
    switch (args.sz) {
    case 0:
        VAL = fn.f0();
        break;
    case 1:
        VAL = fn.f1(args.ptr[0]);
        break;
    case 2:
        VAL = fn.f2(args.ptr[0], args.ptr[1]);
        break;
    case 3:
        VAL = fn.f3(args.ptr[0], args.ptr[1], args.ptr[2]);
        break;
    case 4:
        VAL = fn.f4(args.ptr[0], args.ptr[1], args.ptr[2], args.ptr[3]);
        break;
    case 5:
        VAL = fn.f5(args.ptr[0], args.ptr[1], args.ptr[2], args.ptr[3], args.ptr[4]);
        break;
    case 6:
        VAL = fn.f6(args.ptr[0], args.ptr[1], args.ptr[2], args.ptr[3], args.ptr[4], args.ptr[5]);
        break;
    default:
        assert(!"invalid arity for foreign call");
    }
    _RETC = 1;
    NEXT = _s_apply_cont;
}

word_t s_sys_fwrite(word_t bv, word_t _start, word_t _end, word_t _fptr) {
    S_ffi_bytevec_t buf = s_ffi_to_bytevec(bv);
    S_ffi_sword_t start = s_ffi_to_sword(_start);
    S_ffi_sword_t end = s_ffi_to_sword(_end);
    assert(s_obj_is_fixnum(_fptr));
    FILE* fptr = (FILE*)_fptr;
    assert(end.sign == 0 && start.sign == 0 && end.x - start.x <= buf.sz);
    fwrite(buf.bv + start.x, 1, end.x - start.x, fptr);
    return VOID_TAG;
}

word_t s_sys_fread(word_t bv, word_t _start, word_t _end, word_t _fptr) {
    S_ffi_bytevec_t buf = s_ffi_to_bytevec(bv);
    S_ffi_sword_t start = s_ffi_to_sword(_start);
    S_ffi_sword_t end = s_ffi_to_sword(_end);
    assert(s_obj_is_fixnum(_fptr));
    assert(end.sign == 0 && start.sign == 0 && end.x - start.x <= buf.sz);
    FILE* fptr = (FILE*)_fptr;
    size_t sz = fread(buf.bv + start.x, 1, end.x - start.x, (FILE*)fptr);
    if(sz == end.x - start.x) {
        return s_obj_int(sz);
    }
    else if(feof(fptr)) {
        return EOF_TAG;
    }
    else {
        return VOID_TAG;
    }
}

word_t s_sys_fopen(word_t _path, word_t _mode) {
    S_ffi_bytevec_t path = s_ffi_to_bytevec(_path);
    S_ffi_bytevec_t mode = s_ffi_to_bytevec(_mode);
    assert(path.bv[path.sz - 1] == '\0');
    assert(mode.bv[mode.sz - 1] == '\0');
    FILE *fp = fopen((char*)path.bv, (char*)mode.bv);
    // TODO: workaround
    assert((uintptr_t)fp % 8 == 0);
    return fp == NULL ? 0 : (word_t)fp;
}

word_t s_sys_fclose(word_t _fptr) {
    assert(s_obj_is_fixnum(_fptr));
    FILE* fptr = (FILE*)_fptr;
    fclose(fptr);
    return VOID_TAG;
}

word_t s_sys_system(word_t _cmd) {
    S_ffi_bytevec_t cmd = s_ffi_to_bytevec(_cmd);
    assert(cmd.bv[cmd.sz - 1] == '\0');
    return s_obj_int(system((char*)cmd.bv));
}

word_t s_sys_getenv(word_t _name) {
    S_ffi_bytevec_t name = s_ffi_to_bytevec(_name);
    assert(name.bv[name.sz - 1] == '\0');
    char *r = getenv((char*)name.bv);
    if(r == NULL) {
        return make_bool(false);
    }
    else {
        return s_obj_bytevec_from_buf(r);
    }
}

word_t s_sys_setenv(word_t _name, word_t _val) {
    S_ffi_bytevec_t name = s_ffi_to_bytevec(_name);
    S_ffi_bytevec_t val = s_ffi_to_bytevec(_val);
    assert(name.bv[name.sz - 1] == '\0');
    assert(val.bv[val.sz - 1] == '\0');
    setenv((char*)name.bv, (char*)val.bv, 1);
    return VOID_TAG;
}

word_t s_sys_getpid() {
    return s_obj_int(getpid());
}

static inline int _file_getc(void* p) {
    return fgetc((FILE*)p);
}

word_t _parse_file(word_t _path) {
    TEMP_PUSH(_path);
    s_gc(-1);
    _path = TEMP_POP();
    S_ffi_bytevec_t path = s_ffi_to_bytevec(_path);
    assert(path.bv[path.sz - 1] == '\0');
    FILE *fp = fopen((char*)path.bv, "r");
    if(fp == NULL) panic(__func__);
    Lexer_Stream f = {.getc = _file_getc, .ch = PEEKED, .data = fp};
    word_t exps = s_parse_all(&f);
    fclose(fp);
    return exps;
}

#endif