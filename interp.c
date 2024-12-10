#include"interp.h"

#pragma region
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
#pragma endregion

#define check_list_one(x) ((x).tag == PAIR_TAG)
#define check_list_two(x) check_list_one(x) && (CDR(x).tag == PAIR_TAG)
#define check_list_three(x) check_list_two(x) && (CDDR(x).tag == PAIR_TAG)
#define _make_nil() (Object){.tag=NIL_TAG};

inline static uintptr_t align_to_multiple(uintptr_t alignment, uintptr_t offset);

void load_n_run(int argc, char *argv[]);
void initialize(int argc, char *argv[]);
void end_cont();
void apply_cont();

void _s_write(FILE *fptr, Object obj);
Object make_num(int n);
Object make_pair();
Object alloc_pair();
Object make_vec(int n);
Object alloc_vec(int n);
Object copy_str(const char *s);
Object alloc_str(int sz);
Object make_fn(void (*fn_ptr)());
Object make_char(char c);
Object make_bool(bool b);
Object make_token(char tk);
Object make_fptr(const char *s, const char *mode);
void _strncpy(char *dest, const char *source, int len);

void print_all_token_cont();
void print_all_token();

void s_load_cont();
void s_read_file_cont_1();
void s_read_file_cont();
void s_read_file();

void parse();
void peek_token_cont();
void peek_token();
char peek_char(FILE *fptr);
void next_token();
int lex_num(FILE *fptr, int base);
void lex_comment_block(FILE *fptr);
bool is_num_char(char c);
bool is_id_char(char c);
void parse_cont();
void parse_abrv_cont();
void parse_list();
void parse_dotted_cont();
void parse_list_cont();
void parse_list_cont_1();
void parse_list_cont_2();

Object add_symbol(const char *s);
bool eq(Object x, Object y);
void append_mut(Object xs, Object ys);
Object assq(Object x, Object xs);

void s_load();
void s_eval();
void s_apply();

void s_exit();
void s_cmd_ln();
void s_getpid();
void s_file_exists_pred();
void s_system();
void s_file_pred();
void s_curr_stdout();
void s_curr_stderr();
void s_fopen();
void s_fclose();
void s_eof_obj_pred();
void s_read();
void s_write();
void s_newline();
void s_read_char();
void s_unread_char();
void s_write_char();
void s_writeln();

void s_null_pred();
void s_proc_pred();
void s_boolean_pred();

void s_char_pred();
void s_int_to_char();
void s_char_to_int();

void s_integer_pred();
void s_eq();
void s_add();
void s_sub();
void s_mul();
void s_div();
void s_mod();
void s_le();
void s_lt();
void s_eqn();
void s_ge();
void s_gt();
void s_ash();
void s_band();
void s_ior();

void s_pair_pred();
void s_cons();
void s_car();
void s_cdr();
void s_set_car();
void s_set_cdr();

void s_sym_pred();
void s_sym_to_str();
void s_str_to_sym();

void s_str_pred();
void s_make_str();
void s_str_len();
void s_str_ref();
void s_str_set();

void s_vec_pred();
void s_make_vec();
void s_vec_len();
void s_vec_ref();
void s_vec_set();

void eval();
void if_cont();
void seq_cont();
void assign_cont();
void app_cont();
void apply_macro_clos();
void app_macro_clos_cont_1();
void apply_clos();

void gc_flip(int sz);
void collect_scan_ptr();

Object apply_env(Object x, Object env);

struct {
    bool is_deleted;
    Object obj;
} GC_CELLS[HEAP_SIZE];

Object VAL = _make_nil();
Object EXP = _make_nil();
Object CONT = _make_nil();
Object ENV = _make_nil();
Object PROC = _make_nil();
Object GLOBALS = _make_nil();
Object ARGV = _make_nil();
Object NIL = _make_nil();
Object TOK = _make_nil();
Object SYMBOLS = _make_nil();
Object IF = _make_nil();
Object LAMBDA = _make_nil();
Object BEGIN = _make_nil();
Object SET = _make_nil();
Object DEFMACRO = _make_nil();
Object QUOTE = _make_nil();
Object QUASIQUOTE = _make_nil();
Object UNQUOTE = _make_nil();
Object UNQUOTE_SPLICING = _make_nil();

Object *free_ptr, *scan_ptr;
Object *fromspace_start, *fromspace_end, *tospace_start, *tospace_end;
char TOKEN_BUF[512];
void (*NEXT)();

// implementation start
inline static uintptr_t align_to_multiple(uintptr_t alignment, uintptr_t offset){
    return (offset + (alignment - 1)) & -alignment;
}

void load_n_run(int argc, char *argv[]){
    assert(argc >= 2);
    initialize(argc, argv);
    VAL = copy_str(argv[1]);
    EXP = make_pair();
    CAR(EXP) = VAL;
    VAL = EXP;
    NEXT = s_load;

    for(;;){
        NEXT();
    }
}

void _s_write(FILE *fptr, Object obj){
    switch (obj.tag)
    {
    case FX_TAG:{
        fprintf(fptr, "%d", obj.fx);
        break;
    }
    case BOOL_TAG:{
        if(obj.b){
            fprintf(fptr, "#t");
        }
        else{
            fprintf(fptr, "#f");
        }
        break;
    }
    case CHAR_TAG:{
        fprintf(fptr, "#\\%c", obj.ch);
        break;
    }
    case NIL_TAG:{
        fprintf(fptr, "()");
        break;
    }
    case PAIR_TAG:{
        fprintf(fptr, "(");
        _s_write(fptr, obj.ptr[0]);
        obj = obj.ptr[1];
        while(obj.tag == PAIR_TAG){
            fprintf(fptr, " ");
            _s_write(fptr, obj.ptr[0]);
            obj = obj.ptr[1];
        }
        if(obj.tag == NIL_TAG){
            fprintf(fptr, ")");
        }
        else{
            fprintf(fptr, " . ");
            _s_write(fptr, obj);
            fprintf(fptr, ")");
        }
        break;
    }
    case STR_TAG:{
        fprintf(fptr, "\"");
        for(int i = 0; i < obj.len; ++i){
            if(obj.s[i] == '\n'){
                fprintf(fptr, "\\n");
            }
            else if(obj.s[i] == '\t'){
                fprintf(fptr, "\\t");
            }
            else if(obj.s[i] == '\\'){
                fprintf(fptr, "\\\\");
            }
            else if(obj.s[i] == '"'){
                fprintf(fptr, "\\\"");
            }
            else if(obj.s[i] == '\0'){
                fprintf(fptr, "\\0");
            }
            else{
                fprintf(fptr, "%c", obj.s[i]);
            }
        }
        fprintf(fptr, "\"");
        break;
    }
    case SYM_TAG:{
        fprintf(fptr, "%s", obj.s);
        break;
    }
    case VEC_TAG:{
        fprintf(fptr, "#(");
        for(int i = 0; i < obj.len; ++i){
            _s_write(fptr, obj.ptr[i]);
            if(i != obj.len - 1){
                fprintf(fptr, " ");
            }
        }
        fprintf(fptr, ")");
        break;
    }
    case TOK_TAG:{
        fprintf(fptr, "<TOKEN %c>", obj.tk);
        break;
    }
    case FN_TAG:{
        fprintf(fptr, "<BUILT-IN-FN %p>", obj.cont);
        break;
    }
    case FILE_TAG:{
        fprintf(fptr, "<FILE %p>", obj.fp);
        break;
    }
    case EOF_TAG:{
        fprintf(fptr, "!eof");
        break;
    }
    default:
        fprintf(stderr, "unknown tag: %d\n", obj.tag);
        assert(false);
        break;
    }
}

Object make_num(int n){
    return (Object){.tag = FX_TAG, .fx = n};
}

Object make_fn(void (*fn_ptr)()){
    return (Object){.tag=FN_TAG, .cont = fn_ptr};
}

Object make_char(char c){
    return (Object){.tag=CHAR_TAG, .ch = c};
}

Object make_bool(bool b){
    return (Object){.tag=BOOL_TAG, .b = b};
}

Object make_token(char tk){
    return (Object){.tag=TOK_TAG, .tk = tk};
}

Object make_pair(){
    gc_flip(3);
    return alloc_pair();
}

Object alloc_pair(){
    Object tmp = {.tag = PAIR_TAG, .len = 2, .ptr = free_ptr + 1};
    free_ptr = free_ptr + 3;
    tmp.ptr[0] = NIL;
    tmp.ptr[1] = NIL;
    tmp.ptr[-1] = tmp;
    return tmp;
}

Object make_vec(int n){
    gc_flip(1 + n);
    return alloc_vec(n);
}

Object alloc_vec(int n){
    Object tmp = {.tag = VEC_TAG, .len = n, .ptr = free_ptr + 1};
    free_ptr = free_ptr + n + 1;
    tmp.ptr[-1] = tmp;
    return tmp;
}

Object make_str(char c, int n){
    int sz = 1 + align_to_multiple(sizeof(Object), n + 1) / sizeof(Object);
    gc_flip(sz);
    Object tmp = alloc_str(n);
    for(int i = 0; i < n; ++i){
        tmp.s[i] = c;
    }
    return tmp;
}

Object copy_str(const char *s){
    int n = strlen(s);
    int sz = 1 + align_to_multiple(sizeof(Object),n + 1) / sizeof(Object);
    gc_flip(sz);
    Object tmp = alloc_str(n);
    strcpy(tmp.s, s);
    return tmp;
}

Object alloc_str(int len){
    Object tmp = {.tag = STR_TAG, .len = len, .s = (char*)(free_ptr+1)};
    tmp.s[tmp.len] = '\0';
    *free_ptr = tmp;
    int offset = align_to_multiple(sizeof(Object), tmp.len + 1) / sizeof(Object);
    free_ptr = free_ptr + 1 + offset;
    return tmp;
}

bool eq(Object x, Object y){
    if(x.tag != y.tag){
        return false;
    }
    else if(x.tag == NIL_TAG){
        return true;
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
    else if((x.tag == SYM_TAG) || (x.tag == STR_TAG)){
        return x.s == y.s;
    }
    else if(x.tag == VEC_TAG || x.tag == PAIR_TAG){
        return x.ptr == y.ptr;
    }
    else if(x.tag == FILE_TAG){
        return x.fp == y.fp;
    }
    else{
        fprintf(stderr, "unknown tag: %d\n", x.tag);
        assert(false);
    }
}

Object add_symbol(const char *s){
    Object tmp = SYMBOLS;
    while(tmp.tag == PAIR_TAG){
        if(strcmp(CAR(tmp).s, s) == 0){
            VAL = CAR(tmp);
            return VAL;
        }
        tmp = CDR(tmp);
    }
    PROC = make_pair();
    CDR(PROC) = SYMBOLS;
    SYMBOLS = PROC;
    VAL = copy_str(s);
    VAL.tag = SYM_TAG;
    CAR(SYMBOLS) = VAL;
    return VAL;
}

void append_mut(Object xs, Object ys){
    assert(xs.tag == PAIR_TAG);
    while(CDR(xs).tag == PAIR_TAG){
        xs = CDR(xs);
    }
    assert(xs.tag == PAIR_TAG);
    CDR(xs) = ys;
}

Object assq(Object x, Object xs){
    while (xs.tag == PAIR_TAG){
        if(eq(CAAR(xs), x)){
            return CAR(xs);
        }
        xs = CDR(xs);
    }
    return make_bool(false);
}

void add_prim(const char *s, void (*pr)(void)){
    PROC = make_pair();
    CDR(PROC) = GLOBALS;
    GLOBALS = PROC;
    PROC = make_pair();
    CAR(GLOBALS) = PROC;
    PROC = add_symbol(s);
    CAAR(GLOBALS) = PROC;
    CDAR(GLOBALS) = make_fn(pr);
}

void apply_cont(){
    assert(CONT.tag == VEC_TAG);
    assert(CONT.len >= 1);
    assert(CONT.ptr[0].tag == FN_TAG);
    NEXT = CONT.ptr[0].cont;
}

void end_cont(){
    exit(0);
}

void initialize(int argc, char *argv[]){
    assert(HEAP_SIZE % 2 == 0);
    free_ptr = calloc(HEAP_SIZE * 2, sizeof(Object));
    fromspace_start = free_ptr;
    fromspace_end = fromspace_start + HEAP_SIZE;
    tospace_start = fromspace_end;
    tospace_end = tospace_start + HEAP_SIZE;

    // initialize argv
    for(int i = argc-1; i > 0; --i){
        PROC = make_pair();
        VAL = copy_str(argv[i]);
        CDR(PROC) = ARGV;
        CAR(PROC) = VAL;
        ARGV = PROC;
    }

    CONT = make_vec(1);
    CONT.ptr[0] = make_fn(end_cont);

    IF = add_symbol("if");
    LAMBDA = add_symbol("lambda");
    BEGIN = add_symbol("begin");
    SET = add_symbol("set!");
    DEFMACRO = add_symbol("define-macro");
    QUOTE = add_symbol("quote");
    QUASIQUOTE = add_symbol("quasiquote");
    UNQUOTE = add_symbol("unquote");
    UNQUOTE_SPLICING = add_symbol("unquote-splicing");
    
    add_prim("apply", s_apply);

    add_prim("boolean?",s_boolean_pred);
    add_prim("char?",s_char_pred);
    add_prim("integer->char",s_int_to_char);
    add_prim("char->integer",s_char_to_int);
    add_prim("integer?",s_integer_pred);

    add_prim("eq?", s_eq);
    add_prim("+", s_add);
    add_prim("-",s_sub);
    add_prim("*", s_mul);
    add_prim("div", s_div);
    add_prim("mod", s_mod);
    add_prim("<",s_lt);
    add_prim("<=",s_le);
    add_prim("=",s_eqn);
    add_prim(">=",s_ge);
    add_prim(">",s_gt);
    add_prim("ash", s_ash);
    add_prim("bitwise-and", s_band);
    add_prim("bitwise-ior", s_ior);
    add_prim("writeln", s_writeln);
    add_prim("write", s_write);
    add_prim("read-char", s_read_char);
    add_prim("unread-char", s_unread_char);
    add_prim("write-char", s_write_char);
    add_prim("newline", s_newline);
    add_prim("read", s_read);
    add_prim("eof-object?", s_eof_obj_pred);

    add_prim("port?", s_file_pred);
    add_prim("current-output-port", s_curr_stdout);
    add_prim("current-error-port", s_curr_stderr);
    add_prim("fopen", s_fopen);
    add_prim("fclose", s_fclose);
    add_prim("exit", s_exit);
    add_prim("command-line", s_cmd_ln);
    add_prim("get-process-id", s_getpid);
    add_prim("file-exists?", s_file_exists_pred);
    add_prim("system", s_system);
    add_prim("load", s_load);
    add_prim("eval", s_eval);
    
    add_prim("procedure?", s_proc_pred);
    add_prim("null?", s_null_pred);
    add_prim("pair?", s_pair_pred);
    add_prim("cons", s_cons);
    add_prim("car", s_car);
    add_prim("cdr", s_cdr);
    add_prim("set-car!", s_set_car);
    add_prim("set-cdr!", s_set_cdr);

    add_prim("symbol?", s_sym_pred);
    add_prim("symbol->string", s_sym_to_str);
    add_prim("string->symbol", s_str_to_sym);
    add_prim("string?", s_str_pred);
    add_prim("make-string", s_make_str);
    add_prim("string-length", s_str_len);
    add_prim("string-set!", s_str_set);
    add_prim("string-ref", s_str_ref);

    add_prim("vector?", s_vec_pred);
    add_prim("make-vector", s_make_vec);
    add_prim("vector-length", s_vec_len);
    add_prim("vector-ref", s_vec_ref);
    add_prim("vector-set!", s_vec_set);
}

// #(_ car cont)
void s_read_file_cont_1(){
    PROC = make_pair();
    CAR(PROC) = CONT.ptr[1];
    CDR(PROC) = VAL;
    VAL = PROC;
    CONT = CONT.ptr[CONT.len-1];
    NEXT = apply_cont;
}

// #(_ fp cont)
void s_read_file_cont(){
    if(VAL.tag == EOF_TAG){
        CONT = CONT.ptr[CONT.len-1];
        VAL = NIL;
        NEXT = apply_cont;
    }
    else{
        CONT.ptr[0] = make_fn(s_read_file_cont_1);
        PROC = CONT.ptr[1];
        CONT.ptr[1] = VAL;
        VAL = PROC;
        NEXT = s_read_file;
    }
}

void s_read_file(){
    assert(VAL.tag == FILE_TAG);
    PROC = make_vec(3);
    PROC.ptr[0] = make_fn(s_read_file_cont);
    PROC.ptr[1] = VAL;
    PROC.ptr[PROC.len-1] = CONT;
    CONT = PROC;
    NEXT = parse;
}

void s_load_cont(){
    EXP = make_pair();
    CONT = CONT.ptr[CONT.len - 1];
    CAR(EXP) = BEGIN;
    CDR(EXP) = VAL;
    ENV = GLOBALS;
    NEXT = eval;
}

void s_load(){
    assert(check_list_one(VAL));
    assert(CAR(VAL).tag == STR_TAG);
    VAL = make_fptr(CAR(VAL).s, "r");
    PROC = make_vec(2);
    PROC.ptr[0] = make_fn(s_load_cont);
    PROC.ptr[PROC.len-1] = CONT;
    CONT = PROC;
    NEXT = s_read_file;
}

Object make_fptr(const char *s, const char *mode){
    FILE *fptr = fopen(s, mode);
    if(fptr == NULL){
        fprintf(stderr, "error file: %s\n", s);
        assert(fptr !=  NULL);
    }
    return (Object){.tag = FILE_TAG, .fp = fptr};
}

void _strncpy(char *dest, const char *source, int len){
    for(len = len - 1; len >= 0; --len){
        dest[len] = source[len];
    }
}

void print_all_token_cont(){
    if(VAL.tag == TOK_TAG && VAL.tk == EOF){
        CONT = CONT.ptr[CONT.len-1];
        NEXT = apply_cont;
    }
    else{
        _s_write(stdout, VAL);
        putc('\n', stdout);
        VAL = CONT.ptr[1];
        NEXT = next_token;
    }
}

void print_all_token(){
    assert(VAL.tag == FILE_TAG);
    PROC = make_vec(3);
    PROC.ptr[0] = make_fn(print_all_token);
    PROC.ptr[1] = VAL;
    PROC.ptr[PROC.len-1] = CONT;
    CONT = PROC;
    NEXT = next_token;
}

bool is_id_char(char c){
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

int lex_num(FILE *fptr, int base){
    assert(base == 2 || base == 8 || base == 10 || base == 16);
    int x = 0;
    char c = getc(fptr);
    while(is_num_char(c)){
        x = x * base;
        if('0' <= c && c <= '9'){
            c = c - '0';
        }
        else if ('A' <= c && c <= 'F'){
            c = c - 'A' + 10;
        }
        else if ('a' <= c && c <= 'f'){
            c = c - 'a' + 10;
        }
        x = x + c;
        c = getc(fptr);
    }
    ungetc(c, fptr);
    return x;

}

void lex_comment_block(FILE *fptr){
    while(true){
        char c = getc(fptr);
        assert(c != EOF);
        if(c == '|' && peek_char(fptr) == '#'){
            getc(fptr);
            return;
        }
        else if(c == '#' && peek_char(fptr) == '|'){
            getc(fptr);
            lex_comment_block(fptr);
            return;
        }
    }
}

// #(_ cont)
void peek_token_cont(){
    TOK = VAL;
    CONT = CONT.ptr[CONT.len-1];
    NEXT = apply_cont;
}

// VAL = fptr
void peek_token(){
    assert(VAL.tag == FILE_TAG);
    if(TOK.tag == NIL_TAG){
        PROC = make_vec(2);
        PROC.ptr[0] = make_fn(peek_token_cont);
        PROC.ptr[1] = CONT;
        CONT = PROC;
        NEXT = next_token;
        return;
    }
    else{
        VAL = TOK;
        NEXT = apply_cont;
        return;
    }
}

char peek_char(FILE *fptr){
    char ch = getc(fptr);
    ungetc(ch, fptr);
    return ch;
}

// VAL = fptr
void next_token(){
    if(TOK.tag != NIL_TAG){
        VAL = TOK;
        TOK = NIL;
        NEXT = apply_cont;
        return;
    }
    assert(VAL.tag == FILE_TAG);
    FILE* fptr = VAL.fp;
    int c = getc(fptr); 
    if(c == EOF){
        VAL = (Object){.tag=EOF_TAG};
        NEXT = apply_cont;
    }
    else if(c == ' ' || c == '\n' || c == '\t'){
        return;
    }
    else if(c == ';'){
        while(c != EOF && c != '\n'){
            c = getc(fptr);
        }
    }
    else if(c == '(' || c == '['){
        VAL = make_token('(');
        NEXT = apply_cont;
    }
    else if(c == ')' || c == ']'){
        VAL = make_token(')');
        NEXT = apply_cont;
    }
    else if(c == '.' || c == '\'' || c == '`'){
        VAL = make_token(c);
        NEXT = apply_cont;
    }
    else if(c == ',' && peek_char(fptr) == '@'){
        getc(fptr);
        VAL = make_token('@');
        NEXT = apply_cont;
    }
    else if(c == ','){
        VAL = make_token(',');
        NEXT = apply_cont;
    }
    else if(c == '#' && peek_char(fptr) == 't'){
        getc(fptr);
        VAL = make_bool(true);
        NEXT = apply_cont;
    }
    else if(c == '#' && peek_char(fptr) == 'f'){
        getc(fptr);
        VAL = make_bool(false);
        NEXT = apply_cont;
    }
    else if(c == '#' && peek_char(fptr) == '('){
        getc(fptr);
        VAL = make_token(VEC_TAG);
        NEXT = apply_cont;
    }
    else if(c == '#' && peek_char(fptr) == '\\'){
        getc(fptr);
        c = getc(fptr);
        VAL = make_char(c);
        NEXT = apply_cont;
    }
    else if(c == '#' && peek_char(fptr) == 'b'){
        getc(fptr);
        VAL = make_num(lex_num(fptr, 2));
        NEXT = apply_cont;
    }
    else if(c == '#' && peek_char(fptr) == 'o'){
        getc(fptr);
        VAL = make_num(lex_num(fptr, 8));
        NEXT = apply_cont;
    }
    else if(c == '#' && peek_char(fptr) == 'x'){
        getc(fptr);
        VAL = make_num(lex_num(fptr, 16));
        NEXT = apply_cont;
    }
    else if(c == '#' && peek_char(fptr) == '|'){
        getc(fptr);
        lex_comment_block(fptr);
    }
    else if(c == '-' && '0' <= peek_char(fptr) && peek_char(fptr) <= '9'){
        VAL = make_num(-lex_num(fptr, 10));
        NEXT = apply_cont;
    }
    else if('0' <= c && c <= '9'){
        ungetc(c, fptr);
        VAL = make_num(lex_num(fptr, 10));
        NEXT = apply_cont;
    }
    else if(c == '"'){
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
                else{
                    assert(false);
                }
            }
            TOKEN_BUF[i++] = c;
            c = getc(fptr);
        }
        TOKEN_BUF[i] = '\0';
        VAL = copy_str(TOKEN_BUF);
        NEXT = apply_cont;
    }
    else if(is_id_char(c)){
        int i = 0;
        TOKEN_BUF[i++] = c;
        c = getc(fptr);
        while(is_id_char(c)){
            TOKEN_BUF[i++] = c;
            c = getc(fptr);
        }
        TOKEN_BUF[i] = '\0';
        ungetc(c, fptr);
        VAL = add_symbol(TOKEN_BUF);
        NEXT = apply_cont;
        return;
    }
    else{
        fprintf(stderr, "unknown: %c\n", c);
        assert(false);
    }
}

// #(_ _ cont)
void parse_vector_cont(){
    CONT = CONT.ptr[CONT.len-1];
    int len = 0;
    for(Object tmp = VAL; tmp.tag == PAIR_TAG; tmp = CDR(tmp)){
        ++len;
    }
    PROC = VAL;
    VAL = make_vec(len);
    for(int i = 0; i < len; ++i){
        VAL.ptr[i] = CAR(PROC);
        PROC = CDR(PROC);
    }
    NEXT = apply_cont;
}

// #(_ fp cont)
void parse_dotted_cont(){
    // swap VAL and fp
    PROC = VAL;
    VAL = CONT.ptr[1];
    CONT.ptr[1] = PROC;
    next_token();
    VAL = CONT.ptr[1];
    CONT = CONT.ptr[CONT.len - 1];
    NEXT = apply_cont;
}

// #(_ car cont)
void parse_list_cont_2(){
    PROC = make_pair();
    CAR(PROC) = CONT.ptr[1];
    CDR(PROC) = VAL;
    CONT = CONT.ptr[CONT.len-1];
    VAL = PROC;
    NEXT = apply_cont;
}

// #(_ fp cont)
void parse_list_cont_1(){
    PROC = CONT.ptr[1];
    CONT.ptr[0] = make_fn(parse_list_cont_2);
    CONT.ptr[1] = VAL;
    VAL = PROC;
    NEXT = parse_list;
}

// #(_ fp cont)
void parse_list_cont(){
    if(VAL.tag == TOK_TAG && VAL.tk == ')'){
        TOK = NIL;
        VAL = NIL;
        CONT = CONT.ptr[CONT.len-1];
        NEXT = apply_cont;
    }
    else if(VAL.tag == TOK_TAG && VAL.tk == '.'){
        VAL = CONT.ptr[1];
        TOK = NIL;
        VAL = CONT.ptr[1];
        CONT.ptr[0] = make_fn(parse_dotted_cont);
        NEXT = parse;
    }
    else{
        VAL = CONT.ptr[1];
        CONT.ptr[0] = make_fn(parse_list_cont_1);
        NEXT = parse;
    }
}

void parse_list(){
    assert(VAL.tag == FILE_TAG);
    PROC = make_vec(3);
    PROC.ptr[0] = make_fn(parse_list_cont);
    PROC.ptr[1] = VAL;
    PROC.ptr[PROC.len-1] = CONT;
    CONT = PROC;
    NEXT = peek_token;
}

// #(_ abrv cont)
void parse_abrv_cont(){
    PROC = make_pair();
    CAR(PROC) = VAL;
    VAL = make_pair();
    CDR(VAL) = PROC;
    CAR(VAL) = CONT.ptr[1];
    CONT = CONT.ptr[CONT.len-1];
    NEXT = apply_cont;
}

// #(_ fp cont)
void parse_cont(){
    if(VAL.tag != TOK_TAG){
        CONT = CONT.ptr[CONT.len-1];
        NEXT = apply_cont;
    }
    else if(VAL.tk == '\''){
        VAL = CONT.ptr[1];
        CONT.ptr[0] = make_fn(parse_abrv_cont);
        CONT.ptr[1] = QUOTE;
        NEXT = parse;
    }
    else if(VAL.tk == '`'){
        VAL = CONT.ptr[1];
        CONT.ptr[0] = make_fn(parse_abrv_cont);
        CONT.ptr[1] = QUASIQUOTE;
        NEXT = parse;
    }
    else if(VAL.tk == '@'){
        VAL = CONT.ptr[1];
        CONT.ptr[0] = make_fn(parse_abrv_cont);
        CONT.ptr[1] = UNQUOTE_SPLICING;
        NEXT = parse;
    }
    else if(VAL.tk == ','){
        VAL = CONT.ptr[1];
        CONT.ptr[0] = make_fn(parse_abrv_cont);
        CONT.ptr[1] = UNQUOTE;
        NEXT = parse;
    }
    else if(VAL.tk == VEC_TAG){
        VAL = CONT.ptr[1];
        CONT.ptr[0] = make_fn(parse_vector_cont);
        NEXT = parse_list;
    }
    else if(VAL.tk == '('){
        VAL = CONT.ptr[1];
        CONT = CONT.ptr[CONT.len-1];
        NEXT = parse_list;
    }
    else{
        assert(false);
    }
}

// (file)
void parse(){
    assert(VAL.tag == FILE_TAG);
    PROC = make_vec(3);
    PROC.ptr[0] = make_fn(parse_cont);
    PROC.ptr[1] = VAL;
    PROC.ptr[PROC.len-1] = CONT;
    CONT = PROC;
    NEXT = next_token;
}

Object apply_env(Object x, Object env){
    Object res = assq(x, env);
    if(res.tag != PAIR_TAG){
        fprintf(stderr, "unbound: ");
        _s_write(stderr, x);
        putc('\n', stderr);
        assert(res.tag == PAIR_TAG);
    }
    res = CDR(res);
    return res;
}

// evaluator
// #(_ env cont)
void app_macro_clos_cont_1(){
    assert(CONT.len == 3);
    ENV = CONT.ptr[1];
    CONT = CONT.ptr[CONT.len-1];
    EXP = VAL;
    VAL = NIL;
    NEXT = eval;
}

// #(_ (,subforms . ,body) env)
void apply_macro_clos(){
    NEXT = apply_clos;
    return;
}

// #(_ (,params . ,body) env)
void apply_clos(){
    // will do extend_env over params and VAL with env until params is empty
    // only then eval body
    bool is_macro = PROC.ptr[0].cont == apply_macro_clos;
    if(is_macro){
        EXP = make_vec(3);
        EXP.ptr[0] = make_fn(app_macro_clos_cont_1);
        EXP.ptr[1] = ENV;
        EXP.ptr[PROC.len-1] = CONT;
        CONT = EXP;
    }
    ENV = make_vec(3);
    ENV.ptr[0] = CAR(PROC.ptr[1]);
    ENV.ptr[1] = PROC.ptr[2];
    ENV.ptr[2] = CDR(PROC.ptr[1]);
    while(ENV.ptr[0].tag == PAIR_TAG){
        assert(VAL.tag == PAIR_TAG);
        PROC = make_pair();
        EXP = make_pair();
        CAR(EXP) = CAR(ENV.ptr[0]);
        CDR(EXP) = CAR(VAL);
        CAR(PROC) = EXP;
        CDR(PROC) = ENV.ptr[1];
        ENV.ptr[1] = PROC;
        VAL = CDR(VAL);
        ENV.ptr[0] = CDR(ENV.ptr[0]);
    }
    if(ENV.ptr[0].tag == SYM_TAG){
        PROC = make_pair();
        EXP = make_pair();
        CAR(EXP) = ENV.ptr[0];
        CDR(EXP) = VAL;
        CAR(PROC) = EXP;
        CDR(PROC) = ENV.ptr[1];
        ENV.ptr[1] = PROC;
    }
    VAL = NIL;
    EXP = make_pair();
    CAR(EXP) = BEGIN;
    CDR(EXP) = ENV.ptr[2];
    ENV = ENV.ptr[1];
    NEXT = eval;
}

// #(_ x env cont)
void assign_cont(){
    ENV = assq(CONT.ptr[1],CONT.ptr[2]);
    if(ENV.tag == PAIR_TAG){
        CDR(ENV) = VAL;
    }
    else{
        ENV = make_pair();
        PROC = make_pair();
        CAR(ENV) = CONT.ptr[1];
        CDR(ENV) = VAL;
        CAR(PROC) = ENV;
        append_mut(GLOBALS,PROC);
    }
    VAL = NIL;
    CONT = CONT.ptr[CONT.len-1];
    NEXT = apply_cont;
}

// #(_ (conseq altern) env cont)
void if_cont(){
    if(eq(VAL, make_bool(false))){
        EXP = CADR(CONT.ptr[1]);
        ENV = CONT.ptr[2];
        CONT = CONT.ptr[CONT.len-1];
        NEXT = eval;
    }
    else{
        EXP = CAR(CONT.ptr[1]);
        ENV = CONT.ptr[2];
        CONT = CONT.ptr[CONT.len-1];
        NEXT = eval;
    }
}

// #(_ seq env cont)
void seq_cont(){
    assert(CONT.ptr[1].tag == PAIR_TAG);
    EXP = CAR(CONT.ptr[1]);
    ENV = CONT.ptr[2];
    NEXT = eval;
    if(CDR(CONT.ptr[1]).tag != PAIR_TAG){
        CONT = CONT.ptr[CONT.len - 1];
    }
    else{
        CONT.ptr[1] = CDR(CONT.ptr[1]);
    }
}

// #(_ exps values env cont)
void app_cont(){
    if(VAL.tag == VEC_TAG
        && VAL.len >= 1
        && VAL.ptr[0].tag == FN_TAG
        && VAL.ptr[0].cont == apply_macro_clos){
        // NOTE: env should be current env after expanding the thing
        PROC = VAL;
        VAL = CONT.ptr[1];
        ENV = CONT.ptr[3];
        CONT = CONT.ptr[CONT.len-1];
        NEXT = apply_macro_clos;
        return;
    }
    else if(CONT.ptr[2].tag == NIL_TAG){
        PROC = make_pair();
        CAR(PROC) = VAL;
        CONT.ptr[2] = PROC;
    }
    else{
        PROC = make_pair();
        CAR(PROC) = VAL;
        append_mut(CONT.ptr[2], PROC);
    }

    if(CONT.ptr[1].tag == PAIR_TAG){
        ENV = CONT.ptr[3];
        EXP = CAR(CONT.ptr[1]);
        CONT.ptr[1] = CDR(CONT.ptr[1]);
        NEXT = eval;
    }
    else if(CONT.ptr[2].tag != PAIR_TAG){
        assert(CONT.ptr[2].tag == PAIR_TAG);
    }
    else if(CAR(CONT.ptr[2]).tag == FN_TAG){
        VAL = CONT.ptr[2];
        CONT = CONT.ptr[CONT.len - 1];
        NEXT = CAR(VAL).cont;
        VAL = CDR(VAL);
    }
    else{
        VAL = CONT.ptr[2];
        CONT = CONT.ptr[CONT.len-1];
        PROC = CAR(VAL);
        VAL = CDR(VAL);
        NEXT = apply_clos;
    }
}

void eval(){
    if(EXP.tag == CHAR_TAG || EXP.tag == FX_TAG || EXP.tag == BOOL_TAG
       || EXP.tag == STR_TAG || EXP.tag == VEC_TAG
       || EXP.tag == EOF_TAG){
        VAL = EXP;
        NEXT = apply_cont;
    }
    else if(EXP.tag == SYM_TAG){
        VAL = apply_env(EXP, ENV);
        NEXT = apply_cont;
    }
    else if(EXP.tag != PAIR_TAG){
        _s_write(stderr, EXP);
        putc('\n', stderr);
        assert(EXP.tag == PAIR_TAG);
    }
    else if(eq(CAR(EXP), QUOTE)){
        VAL = CADR(EXP);
        NEXT = apply_cont;
    }
    else if(eq(CAR(EXP), LAMBDA)){
        // (lambda ,params . ,body)
        VAL = make_vec(3);
        VAL.ptr[0] = make_fn(apply_clos);
        VAL.ptr[1] = CDR(EXP);
        VAL.ptr[2] = ENV;
        NEXT = apply_cont;
    }
    else if(eq(CAR(EXP), BEGIN)){
        PROC = make_vec(4);
        PROC.ptr[0] = make_fn(seq_cont);
        PROC.ptr[1] = CDR(EXP);
        PROC.ptr[2] = ENV;
        PROC.ptr[PROC.len - 1] = CONT;
        CONT = PROC;
        NEXT = seq_cont;
    }
    else if(eq(CAR(EXP), IF)){
        PROC = make_vec(4);
        PROC.ptr[0] = make_fn(if_cont);
        PROC.ptr[1] = CDDR(EXP);
        PROC.ptr[2] = ENV;
        PROC.ptr[PROC.len - 1] = CONT;
        CONT = PROC;
        EXP = CADR(EXP);
    }
    else if(eq(CAR(EXP), SET)){
        PROC = make_vec(4);
        PROC.ptr[0] = make_fn(assign_cont);
        PROC.ptr[1] = CADR(EXP);
        PROC.ptr[2] = ENV;
        PROC.ptr[PROC.len - 1] = CONT;
        CONT = PROC;
        EXP = CADDR(EXP);
    }
    else if(eq(CAR(EXP), DEFMACRO)){
        // (define-macro (kw . params) . body)
        VAL = CAADR(EXP); // keyword
        PROC = EXP;
        CADR(PROC) = CDADR(PROC);
        PROC = CDR(PROC);
        EXP = VAL; // keyword

        VAL = make_vec(3);
        VAL.ptr[0] = make_fn(apply_macro_clos);
        VAL.ptr[1] = PROC;
        VAL.ptr[2] = ENV;
        PROC = assq(EXP, ENV);
        if(eq(PROC, make_bool(false))){
            PROC = make_pair();
            CAR(PROC) = EXP;
            CDR(PROC) = VAL;
            VAL = make_pair();
            CAR(VAL) = PROC;
            CDR(VAL) = NIL;
            append_mut(GLOBALS, VAL);
        }
        else{
            CDR(PROC) = VAL;
        }
        VAL = NIL;
        NEXT = apply_cont;
    }
    else{
        PROC = make_vec(5);
        PROC.ptr[0] = make_fn(app_cont);
        PROC.ptr[1] = CDR(EXP);
        PROC.ptr[2] = NIL;
        PROC.ptr[3] = ENV;
        PROC.ptr[PROC.len - 1] = CONT;
        CONT = PROC;
        EXP = CAR(EXP);
    }
}

// garbage collector
int calc_offset(Object obj){
    if(obj.tag == VEC_TAG || obj.tag == PAIR_TAG){
        return (obj.ptr - 1) - tospace_start;
    }
    else if(obj.tag == STR_TAG){
        return ((Object*)obj.s - 1) - tospace_start;
    }
    else{
        _s_write(stderr, obj);
        putc('\n', stderr);
        assert(false);
    }
}

Object copy_obj(Object obj){
    Object tmp;
    if(obj.tag == FX_TAG || obj.tag == CHAR_TAG || obj.tag == BOOL_TAG
        || obj.tag == NIL_TAG || obj.tag == TOK_TAG || obj.tag == FILE_TAG
        || obj.tag == FN_TAG || obj.tag == EOF_TAG){
        return obj;
    }
    else if(obj.tag == SYM_TAG){
        obj.tag = STR_TAG;
        obj = copy_obj(obj);
        obj.tag = SYM_TAG;
        return obj;
    }
    else if(GC_CELLS[calc_offset(obj)].is_deleted){
        return GC_CELLS[calc_offset(obj)].obj;
    }
    else if(obj.tag == PAIR_TAG){
        int offset = calc_offset(obj);
        GC_CELLS[offset].is_deleted = true;
        tmp = alloc_pair();
        tmp.ptr[0] = obj.ptr[0];
        tmp.ptr[1] = obj.ptr[1];
        GC_CELLS[offset].obj = tmp;
        return tmp;
    }
    else if(obj.tag == VEC_TAG){
        int offset = calc_offset(obj);
        GC_CELLS[offset].is_deleted = true;
        tmp = alloc_vec(obj.len);
        for(int i = 0; i < obj.len; ++i){
            tmp.ptr[i] = obj.ptr[i];
        }
        GC_CELLS[offset].obj = tmp;
        return tmp;
    }
    else if(obj.tag == STR_TAG){
        int offset = calc_offset(obj);
        GC_CELLS[offset].is_deleted = true;
        tmp = alloc_str(obj.len);
        _strncpy(tmp.s, obj.s, tmp.len+1);
        GC_CELLS[offset].obj = tmp;
        return tmp;
    }
    else{
        fprintf(stderr, "unknown tag: %d\n", obj.tag);
        assert(false);
    }
}

void collect_scan_ptr(){
    while(scan_ptr < free_ptr){
        Object obj = *scan_ptr++;
        if(obj.tag == VEC_TAG || obj.tag == PAIR_TAG){
            assert(obj.ptr == scan_ptr);
            for(int i = 0; i < obj.len; ++i){
                obj.ptr[i] = copy_obj(obj.ptr[i]);
            }
            scan_ptr = scan_ptr + obj.len;
        }
        else if(obj.tag == STR_TAG){
            assert((Object*)obj.s == scan_ptr);
            int offset = align_to_multiple(sizeof(Object), obj.len + 1) / sizeof(Object);
            scan_ptr = scan_ptr + offset;
        }
        else{
            fprintf(stderr, "unknown tag: %d\n", obj.tag);
            assert(false);
        }
    }
}

void gc_flip(int sz){
    if(fromspace_end - free_ptr > sz){
        return;
    }
    Object *tmp;
    tmp = fromspace_start;
    fromspace_start = tospace_start;
    tospace_start = tmp;

    tmp = fromspace_end;
    fromspace_end = tospace_end;
    tospace_end = tmp;
    
    free_ptr = fromspace_start;
    scan_ptr = free_ptr;

    for(int i = 0; i < HEAP_SIZE; ++i){
        free_ptr[i] = _make_nil();
        GC_CELLS[i].is_deleted = false;
        GC_CELLS[i].obj = NIL;
    }
    GLOBALS = copy_obj(GLOBALS);
    SYMBOLS = copy_obj(SYMBOLS);
    VAL = copy_obj(VAL);
    EXP = copy_obj(EXP);
    CONT = copy_obj(CONT);
    ENV = copy_obj(ENV);
    PROC = copy_obj(PROC);
    ARGV = copy_obj(ARGV);
    NIL = copy_obj(NIL);
    TOK = copy_obj(TOK);
    IF = copy_obj(IF);
    LAMBDA = copy_obj(LAMBDA);
    BEGIN = copy_obj(BEGIN);
    SET = copy_obj(SET);
    DEFMACRO = copy_obj(DEFMACRO);
    QUOTE = copy_obj(QUOTE);
    QUASIQUOTE = copy_obj(QUASIQUOTE);
    UNQUOTE = copy_obj(UNQUOTE);
    UNQUOTE_SPLICING = copy_obj(UNQUOTE_SPLICING);
    collect_scan_ptr();
    if(!(fromspace_end - free_ptr > sz)){
        fprintf(stderr, "free=%ld requested=%d\n", fromspace_end - free_ptr, sz);
        assert(fromspace_end - free_ptr > sz);
    }
}

#pragma region
void s_eval(){
    assert(check_list_one(VAL));
    EXP = CAR(VAL);
    NEXT = eval;
}

// (file)
void s_read(){
    assert(check_list_one(VAL));
    assert(CAR(VAL).tag == FILE_TAG);
    VAL = CAR(VAL);
    NEXT = parse;
}

void s_eof_obj_pred(){
    assert(check_list_one(VAL));
    VAL = make_bool(CAR(VAL).tag == EOF_TAG);
    NEXT = apply_cont;
}

void s_write(){
    if(check_list_two(VAL)){
        assert(CADR(VAL).tag == FILE_TAG);
        _s_write(CADR(VAL).fp, CAR(VAL));
        NEXT = apply_cont;
    }else{
        _s_write(stdout, CAR(VAL));
        NEXT = apply_cont;
    }
}

void s_newline(){
    if(check_list_one(VAL)){
        assert(CAR(VAL).tag == FILE_TAG);
        putc('\n', CAR(VAL).fp);
        NEXT = apply_cont;
    }else{
        putc('\n',stdout);
        NEXT = apply_cont;
    }
}

void s_read_char(){
    assert(check_list_one(VAL));
    assert(CAR(VAL).tag == FILE_TAG);
    char c = getc(CAR(VAL).fp);
    VAL = c == EOF ?  (Object){.tag=EOF_TAG} : make_char(c);
    NEXT = apply_cont;
}

void s_unread_char(){
    assert(check_list_two(VAL));
    assert(CAR(VAL).tag == CHAR_TAG);
    assert(CADR(VAL).tag == FILE_TAG);
    ungetc(CAR(VAL).ch, CADR(VAL).fp);
    NEXT = apply_cont;
}

void s_write_char(){
    assert(check_list_one(VAL));
    assert(CAR(VAL).tag == CHAR_TAG);
    if(check_list_two(VAL)){
        assert(CADR(VAL).tag == FILE_TAG);
        putc(CAR(VAL).ch, CADR(VAL).fp);
        NEXT = apply_cont;
    }else{
        putc(CAR(VAL).ch, stdout);
        NEXT = apply_cont;
    }
}

void s_writeln(){
    if(check_list_two(VAL)){
        assert(CADR(VAL).tag == FILE_TAG);
        _s_write(CADR(VAL).fp, CAR(VAL));
        putc('\n', CADR(VAL).fp);
        NEXT = apply_cont;
    }else{
        _s_write(stdout, CAR(VAL));
        putc('\n', stdout);
        NEXT = apply_cont;
    }
    NEXT = apply_cont;
}

void s_exit(){
    if(!check_list_one(VAL)){
        exit(0);
    }
    else if(CAR(VAL).tag == FX_TAG)
        exit(CAR(VAL).fx);
    else{
        exit(1);
    }
}

void s_cmd_ln(){
    VAL = ARGV;
    NEXT = apply_cont;
}

#include <sys/unistd.h>
void s_getpid(){
    VAL = make_num(getpid());
    NEXT = apply_cont;
}

void s_file_exists_pred(){
    assert(check_list_one(VAL));
    assert(CAR(VAL).tag == STR_TAG);
    VAL = make_bool(access(CAR(VAL).s, F_OK) == 0);
    NEXT = apply_cont;
}

// (cmd)
void s_system(){
    assert(check_list_one(VAL));
    assert(CAR(VAL).tag == STR_TAG);
    VAL = make_num(system(CAR(VAL).s));
    NEXT = apply_cont;
}

void s_file_pred(){
    assert(check_list_one(VAL));
    VAL = make_bool(CAR(VAL).tag == FILE_TAG);
    NEXT = apply_cont;
}

void s_curr_stdout(){
    VAL = (Object){.tag=FILE_TAG, .fp = stdout};
    NEXT = apply_cont;
}

void s_curr_stderr(){
    VAL = (Object){.tag=FILE_TAG, .fp = stderr};
    NEXT = apply_cont;
}

// (fn mode)
void s_fopen(){
    assert(check_list_two(VAL));
    assert(CAR(VAL).tag == STR_TAG);
    assert(CADR(VAL).tag == STR_TAG);
    VAL = make_fptr(CAR(VAL).s, CADR(VAL).s);
    NEXT = apply_cont;
}

// (file)
void s_fclose(){
    assert(check_list_one(VAL));
    assert(CAR(VAL).tag == FILE_TAG);
    fclose(CAR(VAL).fp);
    VAL = NIL;
    NEXT = apply_cont;
}

void s_apply(){
    assert(check_list_two(VAL));
    PROC = CAR(VAL);
    VAL = CADR(VAL);
    if(PROC.tag == FN_TAG){
        NEXT = PROC.cont;
    }
    else{
        NEXT = apply_clos;
    }
}

void s_boolean_pred(){
    assert(check_list_one(VAL));
    VAL = make_bool(CAR(VAL).tag == BOOL_TAG);
    NEXT = apply_cont;
}

void s_char_pred(){
    assert(check_list_one(VAL));
    VAL = make_bool(CAR(VAL).tag == CHAR_TAG);
    NEXT = apply_cont;
}

void s_int_to_char(){
    assert(check_list_one(VAL));
    assert(CAR(VAL).tag == FX_TAG);
    VAL = CAR(VAL);
    VAL.ch = (char) VAL.fx;
    VAL.tag = CHAR_TAG;
    NEXT = apply_cont;
}

void s_char_to_int(){
    assert(check_list_one(VAL));
    assert(CAR(VAL).tag == CHAR_TAG);
    VAL = CAR(VAL);
    VAL.fx = (char) VAL.ch;
    VAL.tag = FX_TAG;
    NEXT = apply_cont;
}

void s_integer_pred(){
    assert(check_list_one(VAL));
    VAL = make_bool(CAR(VAL).tag == FX_TAG);
    NEXT = apply_cont;
}

void s_eq(){
    assert(check_list_two(VAL));
    VAL = make_bool(eq(CAR(VAL), CADR(VAL)));
    NEXT = apply_cont;
}

void s_lt(){
    assert(check_list_two(VAL));
    assert(CAR(VAL).tag == FX_TAG);
    assert(CADR(VAL).tag == FX_TAG);
    VAL = make_bool(CAR(VAL).fx < CADR(VAL).fx);
    NEXT = apply_cont;
}

// (v1 v2)
void s_le(){
    assert(check_list_two(VAL));
    assert(CAR(VAL).tag == FX_TAG);
    assert(CADR(VAL).tag == FX_TAG);
    VAL = make_bool(CAR(VAL).fx <= CADR(VAL).fx);
    NEXT = apply_cont;
}

void s_eqn(){
    assert(check_list_two(VAL));
    assert(CAR(VAL).tag == FX_TAG);
    assert(CADR(VAL).tag == FX_TAG);
    VAL = make_bool(CAR(VAL).fx == CADR(VAL).fx);
    NEXT = apply_cont;
}

void s_ge(){
    assert(check_list_two(VAL));
    assert(CAR(VAL).tag == FX_TAG);
    assert(CADR(VAL).tag == FX_TAG);
    VAL = make_bool(CAR(VAL).fx >= CADR(VAL).fx);
    NEXT = apply_cont;
}

void s_gt(){
    assert(check_list_two(VAL));
    assert(CAR(VAL).tag == FX_TAG);
    assert(CADR(VAL).tag == FX_TAG);
    VAL = make_bool(CAR(VAL).fx > CADR(VAL).fx);
    NEXT = apply_cont;
}

// (v1 v2)
void s_add(){
    assert(check_list_two(VAL));
    assert(CAR(VAL).tag == FX_TAG);
    assert(CADR(VAL).tag == FX_TAG);
    VAL = make_num(CAR(VAL).fx + CADR(VAL).fx);
    NEXT = apply_cont;
}

// (v1 v2)
void s_sub(){
    assert(VAL.tag == PAIR_TAG);
    assert(CAR(VAL).tag == FX_TAG);
    if(CDR(VAL).tag != PAIR_TAG){
        VAL = make_num(-CAR(VAL).fx);
        NEXT = apply_cont;
    }
    else{
        assert(CADR(VAL).tag == FX_TAG);
        VAL = make_num(CAR(VAL).fx - CADR(VAL).fx);
        NEXT = apply_cont;
    }
}

void s_mul(){
    assert(check_list_two(VAL));
    assert(CAR(VAL).tag == FX_TAG);
    assert(CADR(VAL).tag == FX_TAG);
    VAL = make_num(CAR(VAL).fx * CADR(VAL).fx);
    NEXT = apply_cont;
}

void s_div(){
    assert(check_list_two(VAL));
    assert(CAR(VAL).tag == FX_TAG);
    assert(CADR(VAL).tag == FX_TAG);
    assert(CADR(VAL).fx != 0);
    VAL = make_num(CAR(VAL).fx / CADR(VAL).fx);
    NEXT = apply_cont;
}

void s_mod(){
    assert(check_list_two(VAL));
    assert(CAR(VAL).tag == FX_TAG);
    assert(CADR(VAL).tag == FX_TAG);
    assert(CADR(VAL).fx != 0);
    VAL = make_num(CAR(VAL).fx % CADR(VAL).fx);
    NEXT = apply_cont;
}

void s_ash(){
    assert(check_list_two(VAL));
    assert(CAR(VAL).tag == FX_TAG);
    assert(CADR(VAL).tag == FX_TAG);
    if(CADR(VAL).fx < 0){
        VAL = make_num(CAR(VAL).fx >> -CADR(VAL).fx);
    }
    else{
        VAL = make_num(CAR(VAL).fx << CADR(VAL).fx);
    }
    NEXT = apply_cont;
}

void s_band(){
    assert(check_list_two(VAL));
    assert(CAR(VAL).tag == FX_TAG);
    assert(CADR(VAL).tag == FX_TAG);
    VAL = make_num(CAR(VAL).fx & CADR(VAL).fx);
    NEXT = apply_cont;
}

void s_ior(){
    assert(check_list_two(VAL));
    assert(CAR(VAL).tag == FX_TAG);
    assert(CADR(VAL).tag == FX_TAG);
    VAL = make_num(CAR(VAL).fx | CADR(VAL).fx);
    NEXT = apply_cont;
}

void s_proc_pred(){
    assert(check_list_one(VAL));
    VAL = make_bool(CAR(VAL).tag == VEC_TAG
                    && CAR(VAL).len == 3
                    && CAR(VAL).ptr[0].tag == FN_TAG
                    && CAR(VAL).ptr[0].cont == apply_clos);
    NEXT = apply_cont;
}

void s_null_pred(){
    assert(check_list_one(VAL));
    VAL = make_bool(CAR(VAL).tag == NIL_TAG);
    NEXT = apply_cont;
}

void s_pair_pred(){
    assert(check_list_one(VAL));
    VAL = make_bool(CAR(VAL).tag == PAIR_TAG);
    NEXT = apply_cont;
}

void s_cons(){
    assert(check_list_two(VAL));
    PROC = make_pair();
    CAR(PROC) = CAR(VAL);
    CDR(PROC) = CADR(VAL);
    VAL = PROC;
    NEXT = apply_cont;
}

void s_car(){
    assert(check_list_one(VAL));
    assert(CAR(VAL).tag == PAIR_TAG);
    VAL = CAAR(VAL);
    NEXT = apply_cont;
}

void s_cdr(){
    assert(check_list_one(VAL));
    assert(CAR(VAL).tag == PAIR_TAG);
    VAL = CDAR(VAL);
    NEXT = apply_cont;
}

void s_set_car(){
    assert(check_list_two(VAL));
    assert(CAR(VAL).tag == PAIR_TAG);
    CAAR(VAL) = CADR(VAL);
    NEXT = apply_cont;
}

void s_set_cdr(){
    assert(check_list_two(VAL));
    assert(CAR(VAL).tag == PAIR_TAG);
    CDAR(VAL) = CADR(VAL);
    NEXT = apply_cont;
}

void s_sym_pred(){
    assert(check_list_one(VAL));
    VAL = make_bool(CAR(VAL).tag == SYM_TAG);
    NEXT = apply_cont;
}

void s_sym_to_str(){
    assert(check_list_one(VAL));
    assert(CAR(VAL).tag == SYM_TAG);
    PROC = make_str('\0', CAR(VAL).len);
    _strncpy(PROC.s, CAR(VAL).s, CAR(VAL).len+1);
    VAL = PROC;
    NEXT = apply_cont;
}

void s_str_to_sym(){
    assert(check_list_one(VAL));
    assert(CAR(VAL).tag == STR_TAG);
    _strncpy(TOKEN_BUF, CAR(VAL).s, CAR(VAL).len+1);
    TOKEN_BUF[CAR(VAL).len] = '\0';
    add_symbol(TOKEN_BUF);
    NEXT = apply_cont;
}

void s_str_pred(){
    assert(check_list_one(VAL));
    VAL = make_bool(CAR(VAL).tag == STR_TAG);
    NEXT = apply_cont;
}

void s_make_str(){
    assert(check_list_one(VAL));
    assert(CAR(VAL).tag == FX_TAG);
    char c = '\0';
    
    if(check_list_two(VAL)){
        assert(CADR(VAL).tag == CHAR_TAG);
        c = CADR(VAL).ch;
    }

    VAL = make_str(c, CAR(VAL).fx);
    NEXT = apply_cont;
}

void s_str_len(){
    assert(check_list_one(VAL));
    assert(CAR(VAL).tag == STR_TAG);
    VAL = make_num(CAR(VAL).len);
    NEXT = apply_cont;
}

void s_str_set(){
    assert(check_list_three(VAL));
    assert(CAR(VAL).tag == STR_TAG);
    assert(CADR(VAL).tag == FX_TAG);
    assert(CADDR(VAL).tag == CHAR_TAG);
    CAR(VAL).s[CADR(VAL).fx] = CADDR(VAL).ch;
    NEXT = apply_cont;
}

void s_str_ref(){
    assert(check_list_two(VAL));
    assert(CAR(VAL).tag == STR_TAG);
    assert(CADR(VAL).tag == FX_TAG);
    VAL = make_char(CAR(VAL).s[CADR(VAL).fx]);
    NEXT = apply_cont;
}

void s_vec_pred(){
    assert(check_list_one(VAL));
    VAL = make_bool(CAR(VAL).tag == VEC_TAG);
    NEXT = apply_cont;
}

void s_make_vec(){
    assert(check_list_one(VAL));
    assert(CAR(VAL).tag == FX_TAG);
    VAL = make_vec(CAR(VAL).fx);
    for(int i = 0; i < VAL.len; ++i){
        VAL.ptr[i] = make_num(0);
    }
    NEXT = apply_cont;
}

void s_vec_len(){
    assert(check_list_one(VAL));
    assert(CAR(VAL).tag == VEC_TAG);
    VAL = make_num(CAR(VAL).len);
    NEXT = apply_cont;
}

void s_vec_ref(){
    assert(check_list_two(VAL));
    assert(CAR(VAL).tag == VEC_TAG);
    assert(CADR(VAL).tag == FX_TAG);
    VAL = CAR(VAL).ptr[CADR(VAL).fx];
    NEXT = apply_cont;
}

void s_vec_set(){
    assert(check_list_three(VAL));
    assert(CAR(VAL).tag == VEC_TAG);
    assert(CADR(VAL).tag == FX_TAG);
    CAR(VAL).ptr[CADR(VAL).fx] = CADDR(VAL);
    NEXT = apply_cont;
}
#pragma endregion
