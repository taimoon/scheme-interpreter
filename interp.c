#include "interp.h"
#define TRACE_ERR() fprintf(stderr, "[%s:%d:%s] ", __FILE__, __LINE__, __func__)

intptr_t MAX_HEAP_SIZE = 32 << 20;
intptr_t HEAP_SIZE = 1 << 20;

void (*NEXT)();
uint32_t TOKEN_BUF[512];
Object *free_ptr, *scan_ptr;
Object *fromspace_start, *fromspace_end, *tospace_start, *tospace_end;
static bool *gc_markers;
intptr_t SYMBOLS_CAPS = 1024;
intptr_t SYMBOLS_SIZE;

Object EXP;
Object PROC;
Object VAL;
Object CONT;
Object ENV;
Object SYMBOLS;
Object TOK;
Object QUOTE;
Object UNQUOTE_SPLICING;
Object UNQUOTE;
Object QUASIQUOTE;
Object BEGIN;
Object LAMBDA;
Object IF;
Object SET_BANG;
Object DEFINE;
Object DEFMACRO;
Object *CONST_TABLE[] = {&EXP, &PROC, &VAL, &CONT, &ENV, &SYMBOLS, &TOK, &QUOTE, &UNQUOTE_SPLICING, &UNQUOTE, &QUASIQUOTE, &BEGIN, &LAMBDA, &IF, &SET_BANG, &DEFINE, &DEFMACRO,};

static inline intptr_t align_to_multiple(intptr_t alignment, intptr_t offset){
    return (offset + (alignment - 1)) & -alignment;
}

#define make_nil() ((Object){.tag = NIL_TAG})
#define make_void() ((Object){.tag = VOID_TAG})
#define make_num(n) ((Object){.tag = FX_TAG, .fx = (n)})
#define make_char(c) ((Object){.tag = CHAR_TAG, .ch = (c)})
#define make_cont(f) ((Object){.tag = CONT_TAG, .cont = (f)})
#define make_bool(_b) ((Object){.tag = BOOL_TAG, .b = _b ? true : false})

Object make_pair(Object car, Object cdr) {
    assert(fromspace_end - free_ptr > 2);
    Object v = {.tag = PAIR_TAG, .len = 2, .ptr = free_ptr};
    CAR(v) = car;
    CDR(v) = cdr;
    free_ptr = free_ptr + 2;
    return v;
}

Object alloc_pair() {
    gc_flip(2 * sizeof(Object));
    return make_pair(make_nil(), make_nil());
}

Object make_vec(int32_t len, Object k) {
    assert(fromspace_end - free_ptr > len);
    Object v = {.tag = VEC_TAG, .len = len, .ptr = free_ptr};
    for(int i = 0; i < len; ++i) {
        v.ptr[i] = k;
    }
    free_ptr = free_ptr + len;
    return v;
}

Object make_clos(void (*cont)(), int32_t len) {
    assert(len >= 1);
    Object v = make_vec(len, make_num(0));
    v.tag = CLOS_TAG;
    v.ptr[0] = make_cont(cont);
    return v;
}

Object alloc_clos(void (*cont)(), int32_t len) {
    gc_flip(len * sizeof(Object));
    return make_clos(cont, len);
}

Object make_sym(Object str, Object hash) {
    assert(fromspace_end - free_ptr > 3);
    assert(str.tag == STR_TAG && hash.tag == FX_TAG);
    Object v = {.tag = SYM_TAG, .len = 3, .ptr = free_ptr};
    free_ptr[0] = (Object){.tag = UNBOUND_TAG};
    free_ptr[1] = hash;
    free_ptr[2] = str;
    free_ptr = free_ptr + 3;
    return v;
}

Object sym_name(Object sym) {
    assert(sym.tag == SYM_TAG && sym.ptr[2].tag == STR_TAG);
    return sym.ptr[2];
}

Object sym_hash(Object sym) {
    assert(sym.tag == SYM_TAG && sym.ptr[1].tag == FX_TAG);
    return sym.ptr[1];
}

Object sym_val(Object sym, Object val) {
    assert(sym.tag == SYM_TAG);
    if(val.tag != UNBOUND_TAG) {
        sym.ptr[0] = val;
    }
    return sym.ptr[0];
}

Object make_str(const uint32_t *str, int32_t len) {
    assert((fromspace_end - free_ptr) * sizeof(Object) > len * sizeof(uint32_t));
    Object v = {.tag = STR_TAG, .len = len, .str = (uint32_t*)free_ptr};
    free_ptr = (Object*)align_to_multiple(sizeof(Object), (intptr_t)(v.str + len));
    if(str != NULL) {
        for(int i = 0; i < len; ++i) {
            v.str[i] = str[i];
        }
    }
    return v;
}

Object make_bytevec(const uint8_t *s, int32_t len) {
    assert((fromspace_end - free_ptr) * sizeof(Object) > len);
    Object v = {.tag = BYTEVEC_TAG, .len = len, .bv = (uint8_t*)free_ptr};
    free_ptr = (Object*)align_to_multiple(sizeof(Object), (intptr_t)(v.bv + len));
    if(s != NULL) {
        for(int i = 0; i < len; ++i) {
            v.bv[i] = s[i];
        }
    }
    return v;
}

/* djb2: hash = hash * 33 + c, init = 5381 */
intptr_t utf32_hash(const uint32_t *buf, int32_t len) {
    intptr_t hash = 5381;
    for(int i = 0; i < len; ++i) {
        hash = ((hash << 5) + hash) + buf[i];
    }
    return llabs(hash);
}

bool utf32_cmp(const uint32_t *s0, const uint32_t *s1, int32_t len) {
    for(int i = 0; i < len; ++i) {
        if(s0[i] != s1[i]) {
            return false;
        }
    }
    return true;
}

Object add_symbol(const uint32_t *buf, int32_t len) {
    intptr_t hash = utf32_hash(buf, len);
    intptr_t idx = hash % SYMBOLS.len;
    Object bucket = SYMBOLS.ptr[idx];
    while(bucket.tag == PAIR_TAG) {
        assert(CAR(bucket).tag == SYM_TAG);
        Object sym = CAR(bucket);
        Object str = sym_name(sym);
        if(str.len == len && utf32_cmp(str.str, buf, len)) {
            return sym;
        }
        bucket = CDR(bucket);
        assert(bucket.tag == NIL_TAG || bucket.tag == PAIR_TAG);
    }
    Object sym = make_sym(make_str(buf, len), make_num(hash));
    SYMBOLS.ptr[idx] = make_pair(sym, SYMBOLS.ptr[idx]);
    SYMBOLS_SIZE++;
    return sym;
}

void resize_symbol_table() {
    if(SYMBOLS_SIZE < SYMBOLS_CAPS) {
        return;
    }
    Object new_table = make_vec(SYMBOLS_CAPS * 2, make_nil());
    intptr_t symbol_size = 0;
    for(int i = 0; i < SYMBOLS.len; ++i) {
        Object bucket = SYMBOLS.ptr[i];
        while(bucket.tag == PAIR_TAG) {
            assert(CAR(bucket).tag == SYM_TAG);
            Object sym = CAR(bucket);
            uintptr_t hash = sym_hash(sym).fx;
            uintptr_t idx = hash % new_table.len;
            new_table.ptr[idx] = make_pair(sym, new_table.ptr[idx]);
            bucket = CDR(bucket);
            symbol_size++;
            assert(bucket.tag == NIL_TAG || bucket.tag == PAIR_TAG);
        }
    }
    SYMBOLS = new_table;
    SYMBOLS_SIZE = symbol_size;
    SYMBOLS_CAPS = new_table.len;
}

Object add_symbol_cstr(const char *str) {
    int i;
    for(i = 0; str[i] != '\0'; ++i) {
        TOKEN_BUF[i] = (uint32_t)str[i];
    }
    assert(i < sizeof(TOKEN_BUF)/sizeof(*TOKEN_BUF));
    TOKEN_BUF[i] = '\0';
    return add_symbol(TOKEN_BUF, i);
}

Object add_prim(const char *str, void (*prim)()) {
    return sym_val(add_symbol_cstr(str), alloc_clos(prim, 1));
}

// lexer
int fpeekc(FILE *fptr) {
    int c = fgetc(fptr);
    if(c != EOF){
        ungetc(c, fptr);
        return c;
    }
    else {
        return EOF;
    }
}

bool is_id_char(char c) {
    const char extended_set[] = "!$%&*+-./:<=>?@^_~";
    if((('a' <= c) && (c <= 'z'))
       || (('A' <= c) && (c <= 'Z'))
       || (('0' <= c) && (c <= '9'))){
        return true;
    }
    for(int i = 0; i < sizeof(extended_set)/sizeof(extended_set[0]); ++i) {
        if(extended_set[i] == c)
            return true;
    }
    return false;
}

bool is_num_char(char c){
    return ('0' <= c && c <= '9') || ('A' <= c && c <= 'F') || ('a' <= c && c <= 'f');
}

intptr_t lex_num(FILE *fptr, int base) {
    assert(base == 2 || base == 8 || base == 10 || base == 16);
    int x = 0;
    char c = getc(fptr);
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
        c = getc(fptr);
    }
    ungetc(c, fptr);
    return x;
}

Object next_token(FILE *fptr) {
    if(TOK.tag != NIL_TAG) {
        Object t = TOK;
        TOK = make_nil();
        return t;
    }
    for(;;) {
        int c = getc(fptr);
        if(c == EOF) {
            return (Object){.tag = EOF_TAG};
        }
        else if(c == ';') {
            while(c != EOF && c != '\n') {
                c = getc(fptr);
            }
        }
        else if(c == ' ' || c == '\n' || c == '\t') {
            while(c == ' ' || c == '\n' || c == '\t') {
                c = getc(fptr);
            }
            ungetc(c, fptr);
        }
        else if(c == '\'' || c == '`' || c == '.') {
            return (Object){.tag = TOK_TAG, .tk = c};
        }
        else if(c == '(' || c == '[') {
            return (Object){.tag = TOK_TAG, .tk = '('};
        }
        else if(c == ')' || c == ']') {
            return (Object){.tag = TOK_TAG, .tk = ')'};
        }
        else if(c == ',' && fpeekc(fptr) == '@') {
            getc(fptr);
            return (Object){.tag = TOK_TAG, .tk = '@'};
        }
        else if(c == ',') {
            return (Object){.tag = TOK_TAG, .tk = c};
        }
        else if(c == '"') {
            int i = 0;
            c = getc(fptr);
            while(c != '"'){
                if(c == '\\'){
                    c = getc(fptr);
                    if(c == 'n'){
                        c = '\n';
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
                }
                TOKEN_BUF[i++] = c;
                c = getc(fptr);
            }
            TOKEN_BUF[i] = '\0';
            assert(i < sizeof(TOKEN_BUF)/sizeof(*TOKEN_BUF));
            return make_str(TOKEN_BUF, i);
        }
        else if(c == '#' && fpeekc(fptr) == '\\') {
            getc(fptr);
            return (Object){.tag = CHAR_TAG, .ch = getc(fptr)};
        }
        else if(c == '#' && fpeekc(fptr) == 'x') {
            getc(fptr);
            return make_num(lex_num(fptr, 16));
        }
        else if(c == '#' && fpeekc(fptr) == 'b') {
            getc(fptr);
            return make_num(lex_num(fptr, 2));
        }
        else if(c == '#' && fpeekc(fptr) == 't') {
            getc(fptr);
            return (Object){.tag = BOOL_TAG, .b = true};
        }
        else if(c == '#' && fpeekc(fptr) == 'f') {
            getc(fptr);
            return (Object){.tag = BOOL_TAG, .b = false};
        }
        else if(c == '-' && '0' <= fpeekc(fptr) && fpeekc(fptr) <= '9') {
            return make_num(-lex_num(fptr, 10));
        }
        else if('0' <= c && c <= '9') {
            ungetc(c, fptr);
            return make_num(lex_num(fptr, 10));
        }
        else if(is_id_char(c)) {
            int i = 0;
            TOKEN_BUF[i++] = c;
            c = getc(fptr);
            while(is_id_char(c)){
                TOKEN_BUF[i++] = c;
                c = getc(fptr);
            }
            TOKEN_BUF[i] = '\0';
            assert(i < sizeof(TOKEN_BUF)/sizeof(*TOKEN_BUF));
            ungetc(c, fptr);
            return add_symbol(TOKEN_BUF, i);
        }
        else {
            TRACE_ERR();
            fprintf(stderr, "unknown lexeme: %c %c\n", c, fpeekc(fptr));
            abort();
        }
    }
}

// reader
Object peek_token(FILE *fptr) {
    if(TOK.tag == NIL_TAG) {
        TOK = next_token(fptr);
    }
    return TOK;
}

Object parse_list(FILE *fptr) {
    Object tk = peek_token(fptr);
    if(tk.tag == TOK_TAG && tk.tk == ')') {
        next_token(fptr);
        return make_nil();
    }
    else if(tk.tag == TOK_TAG && tk.tk == '.') {
        next_token(fptr);
        Object v = parse(fptr);
        next_token(fptr);
        return v;
    }
    else {
        Object car = parse(fptr);
        Object cdr = parse_list(fptr);
        return make_pair(car, cdr);
    }
}

Object parse(FILE *fptr) {
    Object tk = next_token(fptr);
    if(tk.tag == STR_TAG || tk.tag == CHAR_TAG || tk.tag == FX_TAG || tk.tag == SYM_TAG
        || tk.tag == EOF_TAG || tk.tag == BOOL_TAG) {
        return tk;
    }
    else if(tk.tag == TOK_TAG && tk.tk == '\'') {
        return make_pair(QUOTE, make_pair(parse(fptr), make_nil()));
    }
    else if(tk.tag == TOK_TAG && tk.tk == '@') {
        return make_pair(UNQUOTE_SPLICING, make_pair(parse(fptr), make_nil()));
    }
    else if(tk.tag == TOK_TAG && tk.tk == ',') {
        return make_pair(UNQUOTE, make_pair(parse(fptr), make_nil()));
    }
    else if(tk.tag == TOK_TAG && tk.tk == '`') {
        return make_pair(QUASIQUOTE, make_pair(parse(fptr), make_nil()));
    }
    else if(tk.tag == TOK_TAG && tk.tk == '(') {
        return parse_list(fptr);
    }
    else {
        TRACE_ERR();
        fprintf(stderr, "unknown token: %d\n", tk.tag);
        abort();
    }
}

Object parse_all(FILE *fptr) {
    Object v = parse(fptr);
    if(v.tag == EOF_TAG) {
        return make_nil();
    }
    else {
        return make_pair(v, parse_all(fptr));
    }
}

// writer
void write_obj(Object v, FILE *fptr) {
    if(v.tag == FX_TAG) {
        fprintf(fptr, "%"PRIdPTR, v.fx);
    }
    else if(v.tag == BOOL_TAG) {
        fprintf(fptr, v.b ? "#t" : "#f");
    }
    else if(v.tag == CHAR_TAG) {
        if(v.ch < 128) {
        fprintf(fptr, "#\\%c", (char)v.ch);
        }
        else {
            fprintf(fptr, "#\\x%x", v.ch);
        }
    }
    else if(v.tag == NIL_TAG) {
        fprintf(fptr, "()");
    }
    else if(v.tag == VOID_TAG) {
        fprintf(fptr, "#<void>");
    }
    else if(v.tag == PAIR_TAG) {
        Object car = v.ptr[0];
        Object cdr = v.ptr[1];
        
        fprintf(fptr, "(");
        write_obj(car, fptr);
        
        while(cdr.tag == PAIR_TAG) {
            fprintf(fptr, " ");
            v = cdr;
            car = v.ptr[0];
            cdr = v.ptr[1];
            write_obj(car, fptr);
        }
        if(cdr.tag == NIL_TAG) {
            fprintf(fptr, ")");
        }
        else {
            fprintf(fptr, " . ");
            write_obj(cdr, fptr);
            fprintf(fptr, ")");
        }
    }
    else if(v.tag == SYM_TAG) {
        v = sym_name(v);
        for(int i = 0; i < v.len; ++i)
            fprintf(fptr, "%c", (char)v.str[i]);
    }
    else if(v.tag == STR_TAG) {
        fprintf(fptr, "\"");
        for(int i = 0; i < v.len; ++i)
            fprintf(fptr, "%c", (char)v.str[i]);
        fprintf(fptr, "\"");
    }
    else if(v.tag == BYTEVEC_TAG) {
        fprintf(fptr, "#vu8(");
        for(int i = 0; i < v.len; ++i) {
            fprintf(fptr, "%d", v.bv[i]);
            if(i != v.len - 1) {
                fprintf(fptr, " ");
            }
        }
        fprintf(fptr, ")");
    }
    else if(v.tag == VEC_TAG) {
        fprintf(fptr, "#(");
        for(int i = 0; i < v.len; ++i) {
            write_obj(v.ptr[i], fptr);
            if(i != v.len - 1) {
                fprintf(fptr, " ");
            }
        }
        fprintf(fptr, ")");
    }
    else if(v.tag == CLOS_TAG) {
        fprintf(fptr, "#<procedure>");
    }
    else if(v.tag == TOK_TAG) {
        fprintf(fptr, "#<TOK %c>", v.tk);
    }
    else if(v.tag == FILE_TAG) {
        fprintf(fptr, "#<FILE %p>\n", v.fp);
    }
    else if(v.tag == CONT_TAG) {
        fprintf(fptr, "#<CONT>");
    }
    else {
        TRACE_ERR();
        fprintf(stderr, "unknown object: %d\n", v.tag);
        abort();
    }
}

void writeln_obj(Object v, FILE *fptr) {
    write_obj(v, fptr);
    putc('\n', fptr);
}

// eval helpers
bool obj_eq(Object x, Object y){
    if(x.tag != y.tag){
        return false;
    }
    else if(x.tag == EOF_TAG || x.tag == NIL_TAG || x.tag == VOID_TAG) {
        return true;
    }
    else if(x.tag == SYM_TAG || x.tag == VEC_TAG
            || x.tag == PAIR_TAG || x.tag == CLOS_TAG
            || x.tag == BYTEVEC_TAG){
        return x.ptr == y.ptr;
    }
    else if(x.tag == FX_TAG){
        return x.fx == y.fx;
    }
    else if(x.tag == CHAR_TAG){
        return x.ch == y.ch;
    }
    else if(x.tag == BOOL_TAG){
        return x.b == y.b;
    }
    else if(x.tag == STR_TAG){
        return x.str == y.str;
    }
    else if(x.tag == FILE_TAG){
        return x.fp == y.fp;
    }
    else{
        TRACE_ERR();
        fprintf(stderr, "unknown tag: %d\n", x.tag);
        abort();
    }
}

Object s_assq(Object sym, Object env) {
    while(env.tag == PAIR_TAG) {
        assert(CAR(env).tag == PAIR_TAG);
        if(obj_eq(sym, CAAR(env))) {
            return CAR(env);
            }
        env = CDR(env);
    }
    return make_bool(false);
}

Object maybe_apply_env(Object sym, Object env) {
    while(env.tag == PAIR_TAG) {
        Object val = s_assq(sym, CAR(env));
        if(val.tag == PAIR_TAG) {
            return val;
        }
        env = CDR(env);
    }
    return make_bool(false);
}

// eval loop
void apply_cont() {
    assert(CONT.tag == CLOS_TAG && CONT.len >= 1);
    assert(CONT.ptr[0].tag == CONT_TAG);
    NEXT = CONT.ptr[0].cont;
}

void implicit_ret_cont() {
    assert(CONT.tag == CLOS_TAG && CONT.len == 3 && CONT.ptr[0].cont == implicit_ret_cont);
    CONT = CONT.ptr[2];
    NEXT = apply_cont;
}

void explicit_ret_cont() {
    assert(is_list_one(VAL));
    assert(PROC.tag == CLOS_TAG && PROC.len == 2 && PROC.ptr[0].cont == explicit_ret_cont);
    VAL = CAR(VAL);
    CONT = PROC.ptr[1];
    NEXT = apply_cont;
}

// #(_ env cont)
void after_macro_cont() {
    assert(CONT.tag == CLOS_TAG && CONT.len >= 3 && CONT.ptr[0].tag == CONT_TAG && CONT.ptr[0].cont == after_macro_cont);
    ENV = CONT.ptr[CONT.len - 2];
    CONT = CONT.ptr[CONT.len - 1];
    EXP = VAL;
    NEXT = eval;
}

// #(_ params body env)
void apply_clos() {
    assert(PROC.len >= 4);
    EXP = alloc_pair();
    EXP.ptr[0] = PROC.ptr[1];
    EXP.ptr[1] = VAL;
    ENV = make_nil();
    ENV = alloc_pair();
    VAL = alloc_pair();
    CAR(VAL) = add_symbol_cstr("ENV");
    CDR(VAL) = make_bool(false);
    CAR(ENV) = VAL;
    while(EXP.ptr[0].tag == PAIR_TAG) {
        assert(EXP.ptr[1].tag == PAIR_TAG);
        VAL = alloc_pair();
        CDR(VAL) = ENV;
        ENV = VAL;
        VAL = alloc_pair();
        CAR(VAL) = CAR(EXP.ptr[0]);
        CDR(VAL) = CAR(EXP.ptr[1]);
        CAR(ENV) = VAL;
        EXP.ptr[0] = CDR(EXP.ptr[0]);
        EXP.ptr[1] = CDR(EXP.ptr[1]);
    }
    if(EXP.ptr[0].tag == SYM_TAG) {
        VAL = alloc_pair();
        CDR(VAL) = ENV;
        ENV = VAL;
        VAL = alloc_pair();
        CAR(VAL) = EXP.ptr[0];
        CDR(VAL) = EXP.ptr[1];
        CAR(ENV) = VAL;
    }
    else {
        assert(EXP.ptr[0].tag == EXP.ptr[1].tag && EXP.ptr[0].tag == NIL_TAG);
    }
    VAL = alloc_pair();
    CAR(VAL) = ENV;
    CDR(VAL) = PROC.ptr[3];
    ENV = VAL;
    EXP = PROC.ptr[2];
    NEXT = eval;
}

void explicit_multi_values() {
    assert(CONT.tag == CLOS_TAG && CONT.len == 3 && CONT.ptr[0].cont == explicit_multi_values);
    PROC = CONT.ptr[1];
    CONT = CONT.ptr[CONT.len - 1];
    NEXT = apply_clos;
}

// #(_ exps ENV CONT)
void if_cont() {
    assert(CONT.len >= 4);
    ENV = CONT.ptr[CONT.len - 2];
    if(!(VAL.tag == BOOL_TAG && VAL.b == false)) {
        EXP = CAR(CONT.ptr[1]);
        NEXT = eval;
    }
    else if(CDR(CONT.ptr[1]).tag == PAIR_TAG) {
        EXP = CADR(CONT.ptr[1]);
        NEXT = eval;
    }
    else {
        VAL = make_void();
        NEXT = apply_cont;
    }
    CONT = CONT.ptr[CONT.len - 1];
}

// #(_ name ty ENV CONT)
void assign_cont() {
    assert(CONT.len >= 5);
    assert(obj_eq(CONT.ptr[2], SET_BANG)
            || obj_eq(CONT.ptr[2], DEFINE)
            || obj_eq(CONT.ptr[2], DEFMACRO));
    ENV = CONT.ptr[CONT.len - 2];
    if(obj_eq(CONT.ptr[2], SET_BANG)) {
        EXP = maybe_apply_env(CONT.ptr[1], ENV);
        if(EXP.tag == PAIR_TAG) {
            CDR(EXP) = VAL;
        }
        else {
            sym_val(CONT.ptr[1], VAL);
        }
    }
    else if(obj_eq(CONT.ptr[2], DEFMACRO)) {
        assert(VAL.tag == CLOS_TAG && VAL.ptr[0].tag == CONT_TAG);
        assert(VAL.ptr[0].cont == after_macro_cont || VAL.ptr[0].cont == apply_clos);
        if(VAL.ptr[0].cont == apply_clos) {
            EXP = make_clos(after_macro_cont, VAL.len);
            for(int i = 1; i < VAL.len; ++i) {
                EXP.ptr[i] = VAL.ptr[i];
            }
            VAL = EXP;
        }
        sym_val(CONT.ptr[1], VAL);
    }
    else { // DEFINE
        EXP = ENV.tag == PAIR_TAG ? s_assq(CONT.ptr[1], CAR(ENV)) : make_nil();
        if(EXP.tag == NIL_TAG) {
            sym_val(CONT.ptr[1], VAL);
        }
        else if(EXP.tag == PAIR_TAG) {
            TRACE_ERR();
            fprintf(stderr, "rebound?: ");
            writeln_obj(CONT.ptr[1], stderr);
            abort();
        }
        else {
            assert(ENV.tag == PAIR_TAG);
            ENV = CAR(ENV);
            assert(ENV.tag == PAIR_TAG);
            PROC = alloc_pair();
            CAR(PROC) = CONT.ptr[1];
            CDR(PROC) = VAL;
            EXP = alloc_pair();
            CAR(EXP) = PROC;
            CDR(EXP) = CDR(ENV);
            CDR(ENV) = EXP;
        }
    }
    CONT = CONT.ptr[CONT.len - 1];
    NEXT = apply_cont;
}

// #(_ vs vs* exps ENV CONT)
void app_cont() {
    assert(CONT.len >= 6);
    EXP = alloc_pair();
    CAR(EXP) = VAL;
    if(CONT.ptr[1].tag == NIL_TAG) {
        CONT.ptr[1] = EXP;
        CONT.ptr[2] = EXP;
    }
    else {
        CDR(CONT.ptr[2]) = EXP;
        CONT.ptr[2] = EXP;
    }
    if((VAL.tag == CLOS_TAG && VAL.len >= 4
        && VAL.ptr[0].tag == CONT_TAG
        && VAL.ptr[0].cont == after_macro_cont)) {
        PROC = VAL;
        EXP = alloc_clos(after_macro_cont, 3);
        EXP.ptr[EXP.len - 2] = CONT.ptr[CONT.len - 2];
        EXP.ptr[EXP.len - 1] = CONT.ptr[CONT.len - 1];
        VAL = CONT.ptr[3];
        CONT = EXP;
        NEXT = apply_clos;
    }
    else if(CONT.ptr[3].tag == PAIR_TAG) {
        EXP = CAR(CONT.ptr[3]);
        CONT.ptr[3] = CDR(CONT.ptr[3]);
        ENV = CONT.ptr[CONT.len - 2];
        NEXT = eval;
    }
    else {
        PROC = CAR(CONT.ptr[1]);
        VAL = CDR(CONT.ptr[1]);
        assert(PROC.tag == CLOS_TAG && PROC.len >= 1 && PROC.ptr[0].tag == CONT_TAG);
        CONT = CONT.ptr[CONT.len - 1];
        NEXT = PROC.ptr[0].cont;
    }
}

// #(_ exps ENV CONT)
void seq_cont() {
    assert(CONT.len >= 4);
    ENV = CONT.ptr[CONT.len - 2];
    EXP = CAR(CONT.ptr[1]);
    if(CDR(CONT.ptr[1]).tag == NIL_TAG) {
        CONT = CONT.ptr[CONT.len - 1];
    }
    else {
        CONT.ptr[1] = CDR(CONT.ptr[1]);
    }
    NEXT = eval;
}

void eval() {
    if(EXP.tag == FX_TAG
        || EXP.tag == BOOL_TAG
        || EXP.tag == CHAR_TAG
        || EXP.tag == VOID_TAG
        || EXP.tag == FILE_TAG
        || EXP.tag == VEC_TAG
        || EXP.tag == STR_TAG
        || EXP.tag == BYTEVEC_TAG) {
        VAL = EXP;
        NEXT = apply_cont;
        return;
    }
    else if(EXP.tag == SYM_TAG) {
        NEXT = apply_cont;
        VAL = maybe_apply_env(EXP, ENV);
        if(VAL.tag == PAIR_TAG) {
            VAL = CDR(VAL);
            return;
        }
        else if(EXP.ptr[0].tag != UNBOUND_TAG) {
            VAL = EXP.ptr[0];
            return;
        }
        else {
            TRACE_ERR();
            fprintf(stderr, "unbound: ");
            writeln_obj(EXP, stderr);
            abort();
        }
    }
    else if(!(EXP.tag == PAIR_TAG)) {
        TRACE_ERR();
        fprintf(stderr, "unknown expression: ");
        writeln_obj(EXP, stderr);
        abort();
    }
    else if(obj_eq(CAR(EXP), QUOTE)) {
        VAL = CADR(EXP);
        NEXT = apply_cont;
    }
    else if(obj_eq(CAR(EXP), LAMBDA)) {
        gc_flip(2 * sizeof(Object));
        PROC = is_list_one(CDDR(EXP)) ? CADDR(EXP) : make_pair(BEGIN, CDDR(EXP));
        VAL = alloc_clos(apply_clos, 4);
        VAL.ptr[1] = CADR(EXP);
        VAL.ptr[2] = PROC;
        VAL.ptr[3] = ENV;
        NEXT = apply_cont;
    }
    else if(obj_eq(CAR(EXP), IF)) {
        VAL = alloc_clos(if_cont, 4);
        VAL.ptr[1] = CDDR(EXP);
        VAL.ptr[2] = ENV;
        VAL.ptr[3] = CONT;
        CONT = VAL;
        EXP = CADR(EXP);
    }
    else if(obj_eq(CAR(EXP), BEGIN)) {
        if(is_list_one(CDR(EXP))) {
            EXP = CADR(EXP);
        }
        else {
            VAL = alloc_clos(seq_cont, 4);
            VAL.ptr[1] = CDDR(EXP);
            VAL.ptr[VAL.len - 2] = ENV;
            VAL.ptr[VAL.len - 1] = CONT;
            CONT = VAL;
            EXP = CADR(EXP);
        }
    }
    else if(obj_eq(CAR(EXP), DEFINE)
            || obj_eq(CAR(EXP), DEFMACRO)
            || obj_eq(CAR(EXP), SET_BANG)) {
        // (def var val)
        if(CADR(EXP).tag == SYM_TAG) {
            VAL = alloc_clos(assign_cont, 5);
            VAL.ptr[1] = CADR(EXP);
            VAL.ptr[2] = CAR(EXP);
            VAL.ptr[VAL.len - 2] = ENV;
            VAL.ptr[VAL.len - 1] = CONT;
            CONT = VAL;
            EXP = CADDR(EXP);
        }
        // (def (kw . params) . es)
        else if(CADR(EXP).tag == PAIR_TAG && CAADR(EXP).tag == SYM_TAG) {
            VAL = alloc_clos(assign_cont, 5);
            VAL.ptr[1] = CAADR(EXP);
            VAL.ptr[2] = CAR(EXP);
            VAL.ptr[VAL.len - 2] = ENV;
            VAL.ptr[VAL.len - 1] = CONT;
            CONT = VAL;
            gc_flip(2 * sizeof(Object));
            PROC = is_list_one(CDDR(EXP)) ? CADDR(EXP) : make_pair(BEGIN, CDDR(EXP));
            VAL = alloc_clos(obj_eq(CAR(EXP), DEFMACRO) ? after_macro_cont : apply_clos, 4);
            VAL.ptr[1] = CDADR(EXP);
            VAL.ptr[2] = PROC;
            VAL.ptr[3] = ENV;
            NEXT = apply_cont;
        }
        else {
            TRACE_ERR();
            fprintf(stderr, "bad form: ");
            writeln_obj(EXP, stderr);
            abort();
        }
    }
    else {
        VAL = alloc_clos(app_cont, 6);
        VAL.ptr[1] = make_nil();
        VAL.ptr[2] = make_nil();
        VAL.ptr[3] = CDR(EXP);
        VAL.ptr[VAL.len - 2] = ENV;
        VAL.ptr[VAL.len - 1] = CONT;
        CONT = VAL;
        EXP = CAR(EXP);
    }
}

// entry
static inline void *ccalloc(size_t __nmemb, size_t __size) {
    void *ptr = calloc(__nmemb, __size);
    assert(ptr != NULL);
    return ptr;
}

void eval_entry(int argc, char **argv) {
    fromspace_start = ccalloc(HEAP_SIZE, sizeof(Object));
    fromspace_end = fromspace_start + HEAP_SIZE;
    tospace_start = ccalloc(HEAP_SIZE, sizeof(Object));
    tospace_end = tospace_start + HEAP_SIZE;
    gc_markers = ccalloc(HEAP_SIZE, sizeof(*gc_markers));
    free_ptr = fromspace_start;
    SYMBOLS = make_vec(SYMBOLS_CAPS, make_nil());
    
    TOK = make_nil();
    QUOTE = add_symbol_cstr("quote");
    QUASIQUOTE = add_symbol_cstr("quasiquote");
    UNQUOTE = add_symbol_cstr("unquote");
    UNQUOTE_SPLICING = add_symbol_cstr("unquote-splicing");
    BEGIN = add_symbol_cstr("begin");
    LAMBDA = add_symbol_cstr("lambda");
    IF = add_symbol_cstr("if");
    SET_BANG = add_symbol_cstr("set!");
    DEFINE = add_symbol_cstr("define");
    DEFMACRO = add_symbol_cstr("defmacro");

    Object args = make_vec(argc, make_num(0));
    for(int i = 0; i < argc; ++i) {
        args.ptr[i] = make_bytevec((uint8_t*)argv[i], strlen(argv[i]));
    }
    sym_val(add_symbol_cstr("ARGS"), args);

    sym_val(add_symbol_cstr("stderr"), (Object){.tag = FILE_TAG, .fp = stderr});
    sym_val(add_symbol_cstr("stdout"), (Object){.tag = FILE_TAG, .fp = stdout});
    sym_val(add_symbol_cstr("stdin"), (Object){.tag = FILE_TAG, .fp = stdin});
    sym_val(add_symbol_cstr("eof"), (Object){.tag = EOF_TAG});

    add_prim("apply", s_apply_2);
    add_prim("call-with-values", s_call_with_values);
    add_prim("call/cc", s_callcc);
    add_prim("eval", s_eval);
    add_prim("foreign-call", s_foreign_call);
    add_prim("collect", s_collect);
    add_prim("exit", s_exit);
    add_prim("write", s_write);
    add_prim("read", s_read);
    add_prim("newline", s_newline);
    add_prim("eq?", s_eq);
    add_prim("void", s_void);
    add_prim("procedure?", s_proc_pred);
    add_prim("symbol?", s_sym_pred);
    add_prim("symbol-hash", s_sym_hash);
    add_prim("pair?", s_pair_pred);
    add_prim("cons", s_cons);
    add_prim("car", s_car);
    add_prim("cdr", s_cdr);
    add_prim("set-car!", s_set_car);
    add_prim("set-cdr!", s_set_cdr);
    add_prim("integer?", s_int_pred);
    add_prim("+", s_add);
    add_prim("-", s_sub);
    add_prim("*", s_mul);
    add_prim("div", s_div);
    add_prim("mod", s_mod);
    add_prim("ash", s_ash);
    add_prim("bitwise-ior", s_ior);
    add_prim("bitwise-and", s_iand);
    add_prim("<=", s_le);
    add_prim("<", s_lt);
    add_prim("=", s_eqn);
    add_prim(">", s_gt);
    add_prim(">=", s_ge);
    add_prim("boolean?", s_bool_pred);
    add_prim("integer->char", s_int2char);
    add_prim("char->integer", s_char2int);
    add_prim("char?", s_char_pred);

    add_prim("string?", s_str_pred);
    add_prim("make-string", s_make_str);
    add_prim("string-length", s_str_len);
    add_prim("string-ref", s_str_ref);
    add_prim("string-set!", s_str_set);

    add_prim("string->symbol", s_str2sym);
    add_prim("symbol->string", s_sym2str);

    add_prim("vector?", s_vec_pred);
    add_prim("make-vector", s_make_vec);
    add_prim("vector-length", s_vec_len);
    add_prim("vector-ref", s_vec_ref);
    add_prim("vector-set!", s_vec_set);

    add_prim("bytevector?", s_bytevec_pred);
    add_prim("make-bytevector", s_make_bytevec);
    add_prim("bytevector-length", s_bytevec_len);
    add_prim("bytevector-u8-ref", s_bytevec_ref);
    add_prim("bytevector-u8-set!", s_bytevec_set);

    char *inp = getenv("SCM_BOOT");
    if(inp == NULL) {
        TRACE_ERR();
        fprintf(stderr, "expect `SCM_BOOT` environment variable\n");
        abort();
    }
    FILE *fptr = fopen(inp, "r");
    if(fptr == NULL) {
        TRACE_ERR();
        fprintf(stderr, "expect %s file\n", inp);
        abort();
    }
    EXP = make_pair(BEGIN, parse_all(fptr));
    fclose(fptr);
    ENV = make_nil();
    CONT = alloc_clos(s_exit, 1);
    NEXT = eval;
    for(;;) {
        NEXT();
    }
    return;
}

bool is_nonheap(Object v) {
    return (v.tag == FX_TAG
            || v.tag == CHAR_TAG
            || v.tag == BOOL_TAG
            || v.tag == FILE_TAG
            || v.tag == VOID_TAG
            || v.tag == EOF_TAG
            || v.tag == NIL_TAG
            || v.tag == CONT_TAG
            || v.tag == UNBOUND_TAG
        );
}

intptr_t gc_calc_off(Object v) {
    if(v.tag == CLOS_TAG || v.tag == PAIR_TAG || v.tag == VEC_TAG || v.tag == SYM_TAG) {
        assert(tospace_start <= v.ptr && v.ptr < tospace_end);
        return v.ptr - tospace_start;
    }
    else if(v.tag == STR_TAG) {
        Object* ptr = (Object*)v.str;
        assert(tospace_start <= ptr && ptr < tospace_end);
        return ptr - tospace_start;
    }
    else if(v.tag == BYTEVEC_TAG) {
        Object* ptr = (Object*)v.bv;
        assert(tospace_start <= ptr && ptr < tospace_end);
        return ptr - tospace_start;
    }
    else {
        TRACE_ERR();
        fprintf(stderr, "unknown object tag %d\n", v.tag);
        abort();
    }
}

Object gc_copy_obj(Object v) {
    if(is_nonheap(v) || v.len == 0) {
        return v;
    }
    assert(free_ptr < scan_ptr);
    intptr_t idx = gc_calc_off(v);
    if(gc_markers[idx] == true) {
        return tospace_start[idx];
    }
    gc_markers[idx] = true;
    if(v.tag == CLOS_TAG || v.tag == VEC_TAG || v.tag == PAIR_TAG || v.tag == SYM_TAG) {
        Object w = make_vec(v.len, make_num(0));
        w.tag = v.tag;
        for(int i = 0; i < v.len; ++i) {
            w.ptr[i] = v.ptr[i];
        }
        tospace_start[idx] = w;
        *--scan_ptr = w;
        return w;
    }
    else if(v.tag == STR_TAG) {
        Object w = make_str(v.str, v.len);
        tospace_start[idx] = w;
        return w;
    }
    else if(v.tag == BYTEVEC_TAG) {
        Object w = make_bytevec(v.bv, v.len);
        tospace_start[idx] = w;
        return w;
    }
    else {
        TRACE_ERR();
        fprintf(stderr, "unknown object tag %d\n", v.tag);
        abort();
    }
}

static inline void gc_scan_pointer() {
    while(scan_ptr < fromspace_end) {
        Object v = *scan_ptr; ++scan_ptr;
        assert(!is_nonheap(v));
        if(v.tag == CLOS_TAG || v.tag == VEC_TAG || v.tag == PAIR_TAG || v.tag == SYM_TAG) {
            for(int i = 0; i < v.len; ++i) {
                v.ptr[i] = gc_copy_obj(v.ptr[i]);
            }
        }
        else {
            TRACE_ERR();
            fprintf(stderr, "unknown object tag %d\n", v.tag);
            abort();
        }
    }
}

static inline void resize_tospace() {
    int tospace_words = (tospace_end - tospace_start);
    if(tospace_words >= MAX_HEAP_SIZE || tospace_words >= MAX_HEAP_SIZE) {
        return;
    }
    Object *tospace_start_new = aligned_alloc(8, MAX_HEAP_SIZE * sizeof(Object));
    bool *gc_markers_new = aligned_alloc(8, MAX_HEAP_SIZE * sizeof(bool));
    if(gc_markers_new != NULL && tospace_start_new != NULL) {
        free(tospace_start);
        free(gc_markers);
        tospace_start = tospace_start_new;
        tospace_end = tospace_start_new + MAX_HEAP_SIZE;
        gc_markers = gc_markers_new;
    }
    else {
        free(gc_markers_new);
        free(tospace_start_new);
        TRACE_ERR();
        fprintf(stderr, "cannot resize\n");
        abort();
    }
    tospace_start_new = NULL;
    gc_markers_new = NULL;
    assert(tospace_start != NULL && tospace_end != NULL && gc_markers != NULL);
}

static inline intptr_t min(intptr_t x, intptr_t y) {
    return x < y ? x : y;
}

static inline intptr_t max(intptr_t x, intptr_t y) {
    return x < y ? y : x;
}

void gc_flip(int64_t byte_size) {
    bool forced_collect = byte_size < 0;
    byte_size = min(max(byte_size, 0),
        (SYMBOLS_CAPS * 4 + SYMBOLS_SIZE * 2) * sizeof(Object)
    );
    if(!forced_collect && (fromspace_end - free_ptr) * sizeof(Object) > byte_size) {
        return;
    }
    for(int i = 0; i < HEAP_SIZE; ++i) {
        gc_markers[i] = false;
    }
    typeof(free_ptr) tmp;
    tmp = fromspace_start;
    fromspace_start = tospace_start;
    tospace_start = tmp;
    tmp = fromspace_end;
    fromspace_end = tospace_end;
    tospace_end = tmp;
    scan_ptr = fromspace_end;
    free_ptr = fromspace_start;
    for(int i = 0; i < sizeof(CONST_TABLE) / sizeof(CONST_TABLE[0]); ++i) {
        *CONST_TABLE[i] = gc_copy_obj(*CONST_TABLE[i]);
    }
    gc_scan_pointer();
    int n = 2;
    int d = 4;
    if((free_ptr - fromspace_start) * d < n * (fromspace_end - fromspace_start) && HEAP_SIZE < MAX_HEAP_SIZE) {
        HEAP_SIZE = min(2 * HEAP_SIZE, MAX_HEAP_SIZE);
        resize_tospace();
        gc_flip(-1);
    }
    assert((free_ptr - fromspace_start) * d < n * (fromspace_end - fromspace_start));
    resize_symbol_table();
    if((fromspace_end - free_ptr) * sizeof(Object) > byte_size) {
        return;
    }
    TRACE_ERR();
    fprintf(stderr, "insufficient memory\n");
    abort();
}

// procedures
void s_apply_2() {
    assert(is_list_two(VAL));
    PROC = CAR(VAL);
    VAL = CADR(VAL);
    assert(PROC.tag == CLOS_TAG && PROC.len >= 1);
    assert(PROC.ptr[0].tag == CONT_TAG);
    NEXT = PROC.ptr[0].cont;
}

void s_call_with_values() {
    assert(is_list_two(VAL));
    EXP = alloc_clos(explicit_multi_values, 3);
    EXP.ptr[1] = CADR(VAL);
    EXP.ptr[EXP.len - 1] = CONT;
    CONT = EXP;
    PROC = CAR(VAL);
    VAL = make_nil();
    NEXT = apply_clos;
}

void s_callcc() {
    assert(is_list_one(VAL));
    PROC = CAR(VAL);
    if(CONT.tag == CLOS_TAG && CONT.len == 3 && CONT.ptr[0].tag == CONT_TAG && (CONT.ptr[0].cont == implicit_ret_cont || CONT.ptr[0].cont == explicit_multi_values)) {
        CAR(VAL) = CONT.ptr[1];
        CONT = CONT.ptr[CONT.len - 1];
        NEXT = apply_clos;
    }
    else {
        EXP = alloc_clos(explicit_ret_cont, 2);
        EXP.ptr[EXP.len - 1] = CONT;
        CAR(VAL) = EXP;
        EXP = alloc_clos(implicit_ret_cont, 3);
        EXP.ptr[1] = CAR(VAL);
        EXP.ptr[EXP.len - 1] = CONT;
        CONT = EXP;
        NEXT = apply_clos;
    }
}

void s_eval() {
    assert(is_list_one(VAL));
    EXP = CAR(VAL);
    ENV = make_nil();
    NEXT = eval;
}

void *s_dlopen(const char* inp, const char* entry) {
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

void s_foreign_call() {
    assert(is_list_two(VAL));
    EXP = CAR(VAL);
    VAL = CADR(VAL);
    assert(VAL.tag == VEC_TAG && VAL.len <= 6);
    assert(EXP.tag == BYTEVEC_TAG && EXP.bv[EXP.len - 1] == '\0');
    union {
        Object (*f0)(void);
        Object (*f1)(Object);
        Object (*f2)(Object, Object);
        Object (*f3)(Object, Object, Object);
        Object (*f4)(Object, Object, Object, Object);
        Object (*f5)(Object, Object, Object, Object, Object);
        Object (*f6)(Object, Object, Object, Object, Object, Object);
    } fn;
    fn.f0 = (Object (*) (void)) s_dlopen(NULL, (char*)EXP.bv);
    int argc = VAL.len;
    switch (argc) {
    case 0:
        VAL = fn.f0();
        break;
    case 1:
        VAL = fn.f1(VAL.ptr[0]);
        break;
    case 2:
        VAL = fn.f2(VAL.ptr[0], VAL.ptr[1]);
        break;
    case 3:
        VAL = fn.f3(VAL.ptr[0], VAL.ptr[1], VAL.ptr[2]);
        break;
    case 4:
        VAL = fn.f4(VAL.ptr[0], VAL.ptr[1], VAL.ptr[2], VAL.ptr[3]);
        break;
    case 5:
        VAL = fn.f5(VAL.ptr[0], VAL.ptr[1], VAL.ptr[2], VAL.ptr[3], VAL.ptr[4]);
        break;
    case 6:
        VAL = fn.f6(VAL.ptr[0], VAL.ptr[1], VAL.ptr[2], VAL.ptr[3], VAL.ptr[4], VAL.ptr[5]);
        break;
    default:
        assert(!"invalid arity for foreign call");
    }
    NEXT = apply_cont;
}

void s_collect() {
    int64_t sz = 128 * sizeof(Object);
    assert((VAL.tag == NIL_TAG) || (is_list_one(VAL) && CAR(VAL).tag == FX_TAG && CAR(VAL).fx > 0));
    sz = is_list_one(VAL) ? (CAR(VAL).fx > sz ? CAR(VAL).fx : sz) : sz;
    VAL = make_void();
    gc_flip(sz);
    NEXT = apply_cont;
}

void s_exit() {
    exit(is_list_one(VAL) && CAR(VAL).tag == FX_TAG ? CAR(VAL).fx : 0);
}

void s_write() {
    if(is_list_two(VAL)) {
        assert(CADR(VAL).tag == FILE_TAG);
        write_obj(CAR(VAL), CADR(VAL).fp);
    }
    else {
        assert(is_list_one(VAL));
        write_obj(CAR(VAL), stdout);
    }
    VAL = make_void();
    NEXT = apply_cont;
}

void s_newline() {
    if(is_list_one(VAL)) {
        assert(CAR(VAL).tag == FILE_TAG);
        putc('\n', CAR(VAL).fp);
    }
    else {
        assert(VAL.tag == NIL_TAG);
        putc('\n', stdout);
    }
    VAL = make_void();
    NEXT = apply_cont;
}

void s_read() {
    gc_flip(1024 * sizeof(Object));
    assert(is_list_one(VAL));
    assert(CAR(VAL).tag == FILE_TAG);
    VAL = parse(CAR(VAL).fp);
    NEXT = apply_cont;
}

void s_eq() {
    assert(is_list_two(VAL));
    VAL = make_bool(obj_eq(CAR(VAL), CADR(VAL)));
    NEXT = apply_cont;
}

void s_void() {
    assert(VAL.tag == NIL_TAG);
    VAL = make_void();
    NEXT = apply_cont;
}

void s_proc_pred() {
    assert(is_list_one(VAL));
    VAL = make_bool(CAR(VAL).tag == CLOS_TAG);
    NEXT = apply_cont;
}

void s_sym_pred() {
    assert(is_list_one(VAL));
    VAL = make_bool(CAR(VAL).tag == SYM_TAG);
    NEXT = apply_cont;
}

void s_sym_hash() {
    assert(is_list_one(VAL));
    VAL = sym_hash(CAR(VAL));
    NEXT = apply_cont;
}

void s_pair_pred() {
    assert(is_list_one(VAL));
    VAL = make_bool(CAR(VAL).tag == PAIR_TAG);
    NEXT = apply_cont;
}

void s_cons() {
    assert(is_list_two(VAL));
    EXP = alloc_pair();
    CAR(EXP) = CAR(VAL);
    CDR(EXP) = CADR(VAL);
    VAL = EXP;
    NEXT = apply_cont;
}

void s_car() {
    assert(is_list_one(VAL));
    assert(CAR(VAL).tag == PAIR_TAG);
    VAL = CAAR(VAL);
    NEXT = apply_cont;
}

void s_cdr() {
    assert(is_list_one(VAL));
    assert(CAR(VAL).tag == PAIR_TAG);
    VAL = CDAR(VAL);
    NEXT = apply_cont;
}

void s_set_car() {
    assert(is_list_two(VAL));
    assert(CAR(VAL).tag == PAIR_TAG);
    CAAR(VAL) = CADR(VAL);
    NEXT = apply_cont;
}

void s_set_cdr() {
    assert(is_list_two(VAL));
    assert(CAR(VAL).tag == PAIR_TAG);
    CDAR(VAL) = CADR(VAL);
    NEXT = apply_cont;
}

void s_int_pred() {
    assert(is_list_one(VAL));
    VAL = make_bool(CAR(VAL).tag == FX_TAG);
    NEXT = apply_cont;
}

void s_add() {
    assert(is_list_two(VAL) && CAR(VAL).tag == FX_TAG && CADR(VAL).tag == FX_TAG);
    VAL = make_num(CAR(VAL).fx + CADR(VAL).fx);
    NEXT = apply_cont;
}

void s_mul() {
    assert(is_list_two(VAL) && CAR(VAL).tag == FX_TAG && CADR(VAL).tag == FX_TAG);
    VAL = make_num(CAR(VAL).fx * CADR(VAL).fx);
    NEXT = apply_cont;
}

void s_sub() {
    if(is_list_two(VAL)) {
        assert(CAR(VAL).tag == FX_TAG && CADR(VAL).tag == FX_TAG);
        VAL = make_num(CAR(VAL).fx - CADR(VAL).fx);
    } else if(is_list_one(VAL)) {
        assert(CAR(VAL).tag == FX_TAG);
        VAL = make_num(-CAR(VAL).fx);
    }
    else {
        assert(false);
    }
    NEXT = apply_cont;
}

void s_div() {
    assert(is_list_two(VAL) && CAR(VAL).tag == FX_TAG && CADR(VAL).tag == FX_TAG);
    VAL = make_num(CAR(VAL).fx / CADR(VAL).fx);
    NEXT = apply_cont;
}

void s_mod() {
    assert(is_list_two(VAL) && CAR(VAL).tag == FX_TAG && CADR(VAL).tag == FX_TAG);
    VAL = make_num(CAR(VAL).fx % CADR(VAL).fx);
    NEXT = apply_cont;
}

void s_ash() {
    assert(is_list_two(VAL) && CAR(VAL).tag == FX_TAG && CADR(VAL).tag == FX_TAG);
    if(CADR(VAL).fx < 0) {
       VAL = make_num(CAR(VAL).fx >> -CADR(VAL).fx);
    }
    else {
        VAL = make_num(CAR(VAL).fx << CADR(VAL).fx);
    }
    NEXT = apply_cont;
}

void s_ior() {
    intptr_t v = 0;
    while(VAL.tag == PAIR_TAG) {
        assert(CAR(VAL).tag == FX_TAG);
        v |= CAR(VAL).fx;
        VAL = CDR(VAL);
    }
    VAL = make_num(v);
    NEXT = apply_cont;
}

void s_iand() {
    intptr_t v = ~0;
    while(VAL.tag == PAIR_TAG) {
        assert(CAR(VAL).tag == FX_TAG);
        v &= CAR(VAL).fx;
        VAL = CDR(VAL);
    }
    VAL = make_num(v);
    NEXT = apply_cont;
}

void s_lt() {
    assert(is_list_two(VAL) && CAR(VAL).tag == FX_TAG && CADR(VAL).tag == FX_TAG);
    VAL = make_bool(CAR(VAL).fx < CADR(VAL).fx);
    NEXT = apply_cont;
}

void s_le() {
    assert(is_list_two(VAL) && CAR(VAL).tag == FX_TAG && CADR(VAL).tag == FX_TAG);
    VAL = make_bool(CAR(VAL).fx <= CADR(VAL).fx);
    NEXT = apply_cont;
}

void s_eqn() {
    assert(is_list_two(VAL) && CAR(VAL).tag == FX_TAG && CADR(VAL).tag == FX_TAG);
    VAL = make_bool(CAR(VAL).fx == CADR(VAL).fx);
    NEXT = apply_cont;
}

void s_gt() {
    assert(is_list_two(VAL) && CAR(VAL).tag == FX_TAG && CADR(VAL).tag == FX_TAG);
    VAL = make_bool(CAR(VAL).fx > CADR(VAL).fx);
    NEXT = apply_cont;
}

void s_ge() {
    assert(is_list_two(VAL) && CAR(VAL).tag == FX_TAG && CADR(VAL).tag == FX_TAG);
    VAL = make_bool(CAR(VAL).fx >= CADR(VAL).fx);
    NEXT = apply_cont;
}

void s_bool_pred() {
    assert(is_list_one(VAL));
    VAL = make_bool(CAR(VAL).tag == BOOL_TAG);
    NEXT = apply_cont;
}

void s_int2char() {
    assert(is_list_one(VAL));
    assert(CAR(VAL).tag == FX_TAG);
    VAL = make_char(CAR(VAL).fx);
    NEXT = apply_cont;
}

void s_char2int() {
    assert(is_list_one(VAL));
    assert(CAR(VAL).tag == CHAR_TAG);
    VAL = make_num(CAR(VAL).ch);
    NEXT = apply_cont;
}

void s_char_pred() {
    assert(is_list_one(VAL));
    VAL = make_bool(CAR(VAL).tag == CHAR_TAG);
    NEXT = apply_cont;
}

void s_vec_pred() {
    assert(is_list_one(VAL));
    VAL = make_bool(CAR(VAL).tag == VEC_TAG);
    NEXT = apply_cont;
}

void s_make_vec() {
    assert(is_list_two(VAL) || is_list_one(VAL));
    assert(CAR(VAL).tag == FX_TAG);
    EXP = is_list_two(VAL) ? CADR(VAL) : make_num(0);
    gc_flip(CAR(VAL).fx * sizeof(Object));
    VAL = make_vec(CAR(VAL).fx, EXP);
    NEXT = apply_cont;
}

void s_vec_len() {
    assert(is_list_one(VAL));
    assert(CAR(VAL).tag == VEC_TAG);
    VAL = make_num(CAR(VAL).len);
    NEXT = apply_cont;
}

void s_vec_ref() {
    assert(is_list_two(VAL));
    assert(CAR(VAL).tag == VEC_TAG && CADR(VAL).tag == FX_TAG);
    assert(CADR(VAL).fx >= 0 && CAR(VAL).len > CADR(VAL).fx);
    VAL = CAR(VAL).ptr[CADR(VAL).fx];
    NEXT = apply_cont;
}

void s_vec_set() {
    assert(is_list_three(VAL));
    assert(CAR(VAL).tag == VEC_TAG && CADR(VAL).tag == FX_TAG);
    assert(CADR(VAL).fx >= 0 && CAR(VAL).len > CADR(VAL).fx);
    CAR(VAL).ptr[CADR(VAL).fx] = CADDR(VAL);
    NEXT = apply_cont;
}

void s_str_pred() {
    assert(is_list_one(VAL));
    VAL = make_bool(CAR(VAL).tag == STR_TAG);
    NEXT = apply_cont;
}

void s_make_str() {
    assert(is_list_two(VAL) || is_list_one(VAL));
    assert(CAR(VAL).tag == FX_TAG);
    uint32_t ch = is_list_two(VAL) ? CADR(VAL).fx : 0;
    gc_flip(CAR(VAL).fx * sizeof(uint32_t));
    VAL = make_str(NULL, CAR(VAL).fx);
    for(int i = 0; i < VAL.len; ++i) {
        VAL.str[i] = ch;
    }
    NEXT = apply_cont;
}

void s_str_len() {
    assert(is_list_one(VAL));
    assert(CAR(VAL).tag == STR_TAG);
    VAL = make_num(CAR(VAL).len);
    NEXT = apply_cont;
}

void s_str_ref() {
    assert(is_list_two(VAL));
    assert(CAR(VAL).tag == STR_TAG && CADR(VAL).tag == FX_TAG);
    assert(CADR(VAL).fx >= 0 && CAR(VAL).len > CADR(VAL).fx);
    VAL = make_char(CAR(VAL).str[CADR(VAL).fx]);
    NEXT = apply_cont;
}

void s_str_set() {
    assert(is_list_three(VAL));
    assert(CAR(VAL).tag == STR_TAG && CADR(VAL).tag == FX_TAG && CADDR(VAL).tag == CHAR_TAG);
    assert(CADR(VAL).fx >= 0 && CAR(VAL).len > CADR(VAL).fx);
    CAR(VAL).str[CADR(VAL).fx] = CADDR(VAL).ch;
    NEXT = apply_cont;
}

void s_str2sym() {
    assert(is_list_one(VAL));
    assert(CAR(VAL).tag == STR_TAG);
    gc_flip(CAR(VAL).len * sizeof(uint32_t) + 3 * sizeof(Object));
    VAL = add_symbol(CAR(VAL).str, CAR(VAL).len);
    NEXT = apply_cont;
}

void s_sym2str() {
    assert(is_list_one(VAL));
    VAL = sym_name(CAR(VAL));
    gc_flip(VAL.len * sizeof(uint32_t));
    VAL = make_str(VAL.str, VAL.len);
    NEXT = apply_cont;
}

void s_bytevec_pred() {
    assert(is_list_one(VAL));
    VAL = make_bool(CAR(VAL).tag == BYTEVEC_TAG);
    NEXT = apply_cont;
}

void s_make_bytevec() {
    assert(is_list_two(VAL) || is_list_one(VAL));
    assert(CAR(VAL).tag == FX_TAG);
    EXP = VAL;
    gc_flip(CAR(VAL).fx);
    VAL = make_bytevec(NULL, CAR(VAL).fx);
    if(is_list_two(EXP)) {
        assert(CADR(EXP).tag == FX_TAG);
        assert(0 <= CADR(EXP).fx && CADR(EXP).fx <= 255);
        uint8_t k = CADR(EXP).fx;
        for(int i = 0; i < VAL.len; ++i) {
            VAL.bv[i] = k;
        }
    }
    NEXT = apply_cont;
}

void s_bytevec_len() {
    assert(is_list_one(VAL));
    assert(CAR(VAL).tag == BYTEVEC_TAG);
    VAL = make_num(CAR(VAL).len);
    NEXT = apply_cont;
}

void s_bytevec_ref() {
    assert(is_list_two(VAL));
    assert(CAR(VAL).tag == BYTEVEC_TAG && CADR(VAL).tag == FX_TAG);
    assert(CADR(VAL).fx >= 0 && CAR(VAL).len > CADR(VAL).fx);
    VAL = make_num(CAR(VAL).bv[CADR(VAL).fx]);
    NEXT = apply_cont;
}

void s_bytevec_set() {
    assert(is_list_three(VAL));
    assert(CAR(VAL).tag == BYTEVEC_TAG && CADR(VAL).tag == FX_TAG && CADDR(VAL).tag == FX_TAG);
    assert(CADR(VAL).fx >= 0 && CAR(VAL).len > CADR(VAL).fx);
    assert(0 <= CADDR(VAL).fx && CADDR(VAL).fx <= 255);
    CAR(VAL).bv[CADR(VAL).fx] = CADDR(VAL).fx;
    NEXT = apply_cont;
}

Object s_fwrite(Object bv, Object start, Object end, Object fptr) {
    assert(bv.tag == BYTEVEC_TAG);
    assert(start.tag == FX_TAG);
    assert(end.tag == FX_TAG);
    assert(end.fx >= 0 && start.fx >= 0 && end.fx - start.fx <= bv.len);
    assert(fptr.tag == FILE_TAG);
    fwrite(bv.bv + start.fx, 1, end.fx - start.fx, fptr.fp);
    return make_void();
}

Object s_fread(Object bv, Object start, Object end, Object fptr) {
    assert(bv.tag == BYTEVEC_TAG);
    assert(start.tag == FX_TAG);
    assert(end.tag == FX_TAG);
    assert(end.fx >= 0 && start.fx >= 0 && end.fx - start.fx <= bv.len);
    assert(fptr.tag == FILE_TAG);
    size_t sz = fread(bv.bv + start.fx, 1, end.fx - start.fx, fptr.fp);
    if(sz == end.fx - start.tag) {
        return make_num(sz);
    }
    else if(feof(fptr.fp)) {
        return (Object){.tag = EOF_TAG};
    }
    else {
        return make_void();
    }
}

Object s_fopen(Object path, Object mode) {
    assert(path.tag == BYTEVEC_TAG && mode.tag == BYTEVEC_TAG);
    assert(path.bv[path.len - 1] == '\0');
    assert(mode.bv[mode.len - 1] == '\0');
    FILE *fp = fopen((char*)path.bv, (char*)mode.bv);
    return fp == NULL ? make_num(0) : (Object){.tag = FILE_TAG, .fp = fp}; 
}

Object s_fclose(Object file) {
    assert(file.tag == FILE_TAG);
    return make_num(fclose(file.fp));
}

Object s_system(Object cmd) {
    assert(cmd.tag == BYTEVEC_TAG);
    assert(cmd.bv[cmd.len - 1] == '\0');
    return make_num(system((char*)cmd.bv));
}

Object s_getenv(Object name) {
    assert(name.tag == BYTEVEC_TAG);
    assert(name.bv[name.len - 1] == '\0');
    char *r = getenv((char*)name.bv);
    if(r == NULL) {
        return make_bool(false);
    }
    else {
        return make_bytevec((uint8_t*)r, strlen(r));
    }
}

Object s_setenv(Object name, Object val) {
    assert(name.tag == BYTEVEC_TAG && val.tag == BYTEVEC_TAG);
    assert(name.bv[name.len - 1] == '\0');
    assert(val.bv[val.len - 1] == '\0');
    setenv((char*)name.bv, (char*)val.bv, 1);
    return make_void();
}

#include <unistd.h>
Object s_getpid() {
    return make_num(getpid());
}
