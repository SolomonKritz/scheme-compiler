

//#include "gc.h"    // Add back in and change tags if we want to use GC
#include "stdio.h"
#include "stdlib.h"
#include "gc.h"
#include <cstring>


#define CLO_TAG 0
#define CONS_TAG 1
#define INT_TAG 2
#define STR_TAG 3
#define SYM_TAG 4
#define CHAR_TAG 5
#define VEC_TAG 6
#define ENUM_TAG 7


//#define VECTOR_OTHERTAG 1
// Hashes, Sets, gen records, can all be added here


#define V_VOID 39  //32 +7 (+7 is for anything enumerable other than null)
#define V_TRUE 31  //24 +7
#define V_FALSE 15 //8  +7
#define V_NULL 0


// #define MASK64 0xffffffffffffffff // useful for tagging related operations


// #define ASSERT_TAG(v,tag,msg) \
//     if(((v)&7ULL) != (tag)) \
//         fatal_err(msg);

#define ASSERT_VALUE(v,val,msg) \
    if(((u64)(v)) != (val))     \
        fatal_err(msg);


// #define DECODE_CLO(v) ((u64*)((v)&(7ULL^MASK64)))
// #define ENCODE_CLO(v) (((u64)(v)) | CLO_TAG)

// #define DECODE_CONS(v) ((u64*)((v)&(7ULL^MASK64)))
// #define ENCODE_CONS(v) (((u64)(v)) | CONS_TAG)

// #define decode_int(v) ((s32)((u32)(((v)&(7ULL^MASK64)) >> 32)))
// #define encode_int(v) (((u64)(((u32)(v)) << 32)) | INT_TAG)

// #define DECODE_STR(v) ((char*)((v)&(7ULL^MASK64)))
// #define ENCODE_STR(v) (((u64)(v)) | STR_TAG)

// #define DECODE_SYM(v) ((char*)((v)&(7ULL^MASK64)))
// #define ENCODE_SYM(v) (((u64)(v)) | SYM_TAG)

// #define DECODE_OTHER(v) ((u64*)((v)&(7ULL^MASK64)))
// #define ENCODE_OTHER(v) (((u64)(v)) | OTHER_TAG)


// some apply-prim macros for expecting 1 argument or 2 arguments

#define GEN_EXPECT0ARGLIST(f,g) \
    u64 f(u64 lst) \
    { \
        ASSERT_VALUE(lst, V_NULL, "This function takes no arguments.") \
        return g(); \
    }

#define GEN_EXPECT1ARGLIST(f,g) \
    u64 f(u64 lst) \
    { \
        u64 v0 = expect_args1(lst); \
        return g(v0); \
    }

#define GEN_EXPECT2ARGLIST(f,g) \
    u64 f(u64 lst) \
    { \
        u64 rest; \
        u64 v0 = expect_cons(lst, &rest); \
        u64 v1 = expect_cons(rest, &rest); \
        if (rest != V_NULL) \
            fatal_err("prim applied on more than 2 arguments."); \
        return g(v0,v1);                                           \
    }

#define GEN_EXPECT3ARGLIST(f,g) \
    u64 f(u64 lst) \
    { \
        u64 rest; \
        u64 v0 = expect_cons(lst, &rest); \
        u64 v1 = expect_cons(rest, &rest); \
        u64 v2 = expect_cons(rest, &rest); \
        if (rest != V_NULL) \
            fatal_err("prim applied on more than 2 arguments."); \
        return g(v0,v1,v2);                                        \
    }


// No mangled names
extern "C"
{



typedef unsigned long long u64;
typedef signed long long s64;
typedef unsigned long u32;
typedef signed long s32;



// UTILS


u64* alloc(const u64 m)
{
    //return new u64[m];
    return (u64*)GC_MALLOC(m);
}

u64* alloc_atom(const u64 m)
{
    //return new u64[m];
    return (u64*)GC_MALLOC_ATOMIC(m);
}

char* str_alloc(const u64 m)
{
    //return new char[m];
    return (char*)GC_MALLOC_ATOMIC(m);
}

void fatal_err(const char* msg)
{
    printf("\"%s\"", msg);
    printf("\n");
    exit(1);
}

u64* decode_cons(u64 v) {
    u64* p = ((u64*)(v));
    return ((u64*)(p[1]));
}

u64 encode_cons(u64* v) {
    u64* p = alloc(2*sizeof(u64));
    p[0] = CONS_TAG;
    p[1] = ((u64)(v));
    return ((u64)(p));
}

s32 decode_int(u64 v) {
    u64* p = ((u64*)(v));
    return ((s32)((u32)((p[1]) >> 32)));
}

u64 encode_int(s32 v) {
    u64* p = alloc_atom(2*sizeof(u64));
    p[0] = INT_TAG;
    p[1] = ((u64)(((u32)(v)) << 32));
    return ((u64)(p));
}

u64* decode_clo(u64 v) {
    u64* p = ((u64*)(v));
    return ((u64*)(p[1]));
}

u64 encode_clo(u64* v) {
    u64* p = alloc(2*sizeof(u64));
    p[0] = CLO_TAG;
    p[1] = ((u64)(v));
    return ((u64)(p));
}

u64 get_tag(u64 v) {
    u64* p = ((u64*)(v));
    return p[0];
}

const char* decode_str(u64 v) {
    u64* p = ((u64*)(v));
    return ((const char*)(p[1]));
}

u64 encode_str(const char* v) {
    u64* p = alloc_atom(2*sizeof(u64));
    p[0] = STR_TAG;
    p[1] = ((u64)(v));
    return ((u64)(p));
}

const char decode_char(u64 v) {
    u64* p = ((u64*)(v));
    return ((const char)(p[1]));
}

u64 encode_char(const char v) {
    u64* p = alloc_atom(2*sizeof(u64));
    p[0] = CHAR_TAG;
    p[1] = ((u64)(v));
    return ((u64)(p));
}

const char* decode_sym(u64 v) {
    u64* p = ((u64*)(v));
    return ((const char*)(p[1]));
}

u64 encode_sym(const char* v) {
    u64* p = alloc_atom(2*sizeof(u64));
    p[0] = SYM_TAG;
    p[1] = ((u64)(v));
    return ((u64)(p));
}

u64* decode_vec(u64 v) {
    u64* p = ((u64*)(v));
    return ((u64*)(p[1]));
}

u64 encode_vec(u64* v) {
    u64* p = alloc(2*sizeof(u64));
    p[0] = VEC_TAG;
    p[1] = ((u64)(v));
    return ((u64)(p));
}

void assert_tag_i(u64 v, u64 tag, const char* msg) {
    if(v == V_NULL || v == V_TRUE || v == V_FALSE || v == V_VOID || get_tag(v) != tag)
        fatal_err(msg);
}

void print_u64(u64 i)
{
    printf("%llu\n", i);
}

u64 expect_args0(u64 args)
{
    if (args != V_NULL)
        fatal_err("Expected value: null (in expect_args0). Prim cannot take arguments.");
    return V_NULL;
}

u64 expect_args1(u64 args)
{
    assert_tag_i(args, CONS_TAG, "Expected cons value (in expect_args1). Prim applied on an empty argument list.");
    u64* p = decode_cons(args);
    ASSERT_VALUE((p[1]), V_NULL, "Expected null value (in expect_args1). Prim can only take 1 argument.")
    return p[0];
}

u64 expect_cons(u64 p, u64* rest)
{
    // pass a pair value p and a pointer to a word *rest
    // verifiies (cons? p), returns the value (car p) and assigns *rest = (cdr p)
    assert_tag_i(p, CONS_TAG, "Expected a cons value. (expect_cons)");

    u64* pp = decode_cons(p);
    *rest = pp[1];
    return pp[0];
}

u64 expect_clo(u64 p, u64* rest)
{
    // pass a pair value p and a pointer to a word *rest
    // verifiies (clo? p), returns the value (car p) and assigns *rest = (cdr p)
    assert_tag_i(p, CLO_TAG, "RUNTIME ERROR: Cannot apply non-function value");

    u64* pp = decode_clo(p);
    *rest = pp[1];
    return pp[0];
}


// u64 expect_other(u64 v, u64* rest)
// {
//     // returns the runtime tag value
//     // puts the untagged value at *rest
//     ASSERT_TAG(v, VEC_TAG, "Expected a vector or special value. (expect_other)")
//
//     u64* p = DECODE_OTHER(v);
//     *rest = p[1];
//     return p[0];
// }


/////// CONSTANTS


u64 const_init_int(s32 i)
{
    return encode_int(i);
}

u64 const_init_char(const char c)
{
    return encode_char(c);
}

u64 const_init_void()
{
    return V_VOID;
}


u64 const_init_null()
{
    return V_NULL;
}


u64 const_init_true()
{
    return V_TRUE;
}


u64 const_init_false()
{
    return V_FALSE;
}


u64 const_init_string(const char* s)
{
    return encode_str(s);
}

u64 const_init_symbol(const char* s)
{
    return encode_sym(s);
}







/////////// PRIMS


///// effectful prims:


u64 prim_print_aux(u64 v)
{
    if (v == V_NULL)
        printf("()");
    else if (v == V_TRUE)
        printf("#t");
    else if (v == V_FALSE)
        printf("#f");
    else if (v == V_VOID)
        printf("#<void>");
    else if (get_tag(v) == INT_TAG)
    {
        printf("%d", (int)decode_int(v));
    }
    else if (get_tag(v) == CHAR_TAG)
    {
        printf("#\\%c", decode_char(v));
    }
    else if (get_tag(v) == CONS_TAG)
    {
        u64* p = decode_cons(v);
        printf("(");
        prim_print_aux(p[0]);
        printf(" . ");
        prim_print_aux(p[1]);
        printf(")");
    }
    else if (get_tag(v) == CLO_TAG) {
        printf("#<procedure>");
    }
    else if (get_tag(v) == STR_TAG)
    {   // needs to handle escaping to be correct
        printf("\"%s\"", decode_str(v));
    }
    else if (get_tag(v) == SYM_TAG)
    {   // needs to handle escaping to be correct
        printf("%s", decode_sym(v));
    }
    else if (get_tag(v) == VEC_TAG)
    {
        printf("#(");
        u64* vec = decode_vec(v);
        s32 len = decode_int(vec[0]);
        prim_print_aux(vec[1]);
        for (s32 i = 2; i <= len; ++i)
        {
            printf(" ");
            prim_print_aux(vec[i]);
        }
        printf(")");
    }
    else
        printf("(print.. v); unrecognized value %llu", v);
    //...
    return V_VOID;
}

u64 prim_print(u64 v)
{
    if (v == V_NULL)
        printf("'()");
    else if (v == V_TRUE)
        printf("#t");
    else if (v == V_FALSE)
        printf("#f");
    else if (v == V_VOID)
        printf("#<void>");
    else if (get_tag(v) == INT_TAG)
    {
        printf("%ld", decode_int(v));
    }
    else if (get_tag(v) == CHAR_TAG)
    {
        printf("#\\%c", decode_char(v));
    }
    else if (get_tag(v) == CONS_TAG)
    {
        u64* p = decode_cons(v);
        printf("'(");
        prim_print_aux(p[0]);
        printf(" . ");
        prim_print_aux(p[1]);
        printf(")");
    }
    else if (get_tag(v) == CLO_TAG)
        printf("#<procedure>");
    else if (get_tag(v) == STR_TAG)
    {   // needs to handle escaping to be correct
        printf("\"%s\"", decode_str(v));
    }
    else if (get_tag(v) == SYM_TAG)
    {   // needs to handle escaping to be correct
        printf("'%s", decode_sym(v));
    }
    else if (get_tag(v) == VEC_TAG)
    {
        printf("#(");
        u64* vec = decode_vec(v);
        s32 len = decode_int(vec[0]);
        prim_print_aux(vec[1]);
        for (s32 i = 2; i <= len; ++i)
        {
            printf(" ");
            prim_print_aux(vec[i]);
        }
        printf(")");
    }
    else
        printf("(print v); unrecognized value %llu", v);
    //...
    return V_VOID;
}
GEN_EXPECT1ARGLIST(applyprim_print,prim_print)


u64 prim_haltwithtoomanyerr()
{
    printf("\"%s\"", "RUNTIME ERROR: Too many arguments provided to a function");
    printf("\n");
    exit(1);
}
GEN_EXPECT0ARGLIST(applyprim_haltwithtoomanyerr, prim_haltwithtoomanyerr)

u64 prim_haltwithtoofewerr()
{
    printf("\"%s\"", "RUNTIME ERROR: Too few arguments provided to a function");
    printf("\n");
    exit(1);
}
GEN_EXPECT0ARGLIST(applyprim_haltwithtoofewerr, prim_haltwithtoofewerr)

u64 prim_halt(u64 v) // halt
{
    prim_print(v); // display the final value
    printf("\n");
    exit(0);
    return V_NULL;
}
GEN_EXPECT1ARGLIST(applyprim_halt, prim_halt)

u64 prim_string_45ref(u64 s, u64 i) {
    assert_tag_i(s, STR_TAG, "first argument to string-ref must be a string");
    assert_tag_i(i, INT_TAG, "second argument to string-ref must be an int");

    const char* str = decode_str(s);
    s32 idx = decode_int(i);
    const char c = str[idx];
    return encode_char(c);
}
GEN_EXPECT2ARGLIST(applyprim_string_45ref, prim_string_45ref)

u64 applyprim_vector(u64 lst)
{
    // pretty terrible, but works
    u64* buffer = alloc(256 * sizeof(u64));
    s32 i = 0;
    while (lst != V_NULL && lst != V_TRUE && lst != V_FALSE && lst != V_VOID && get_tag(lst) == CONS_TAG && i < 256)
        buffer[i++] = expect_cons(lst, &lst);
    u64* mem = alloc((1 + i) * sizeof(u64));
    mem[0] = encode_int(i);
    for (u64 j = 1; j <= i; ++j)
        mem[j] = buffer[j-1];
    return encode_vec(mem);
}

u64 prim_vector_63(u64 v) // vector?
{
    if (v == V_NULL || v == V_FALSE || v == V_TRUE || v == V_VOID)
        return V_FALSE;
    else if (get_tag(v) == VEC_TAG)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_vector_63, prim_vector_63)

u64 prim_vector_45length(u64 v)
{
    assert_tag_i(v, VEC_TAG, "argument to vector-length must be a vector");

    return decode_vec(v)[0];
}
GEN_EXPECT1ARGLIST(applyprim_vector_45length, prim_vector_45length)

u64 prim_make_45vector(u64 lenv, u64 iv)
{
    assert_tag_i(lenv, INT_TAG, "first argument to make-vector must be an integer");

    const s32 l = decode_int(lenv);
    u64* vec = alloc((1 + l) * sizeof(u64));
    vec[0] = lenv;
    for (u64 i = 1; i <= l; ++i)
        vec[i] = iv;
    return encode_vec(vec);
}
GEN_EXPECT2ARGLIST(applyprim_make_45vector, prim_make_45vector)


u64 prim_vector_45ref(u64 v, u64 i)
{
    assert_tag_i(i, INT_TAG, "second argument to vector-ref must be an integer");
    assert_tag_i(v, VEC_TAG, "first argument to vector-ref must be a vector");

    if ( decode_int(decode_vec(v)[0]) <= decode_int(i))
        fatal_err("RUNTIME ERROR: Attempt to use a vector index that is out of bounds");
        //runtime_err("Attempt to access a vector index that is out of bounds");
    return decode_vec(v)[1+(decode_int(i))];
}
GEN_EXPECT2ARGLIST(applyprim_vector_45ref, prim_vector_45ref)


u64 prim_vector_45set_33(u64 a, u64 i, u64 v)
{
    assert_tag_i(i, INT_TAG, "second argument to vector-set! must be an integer");
    assert_tag_i(a, VEC_TAG, "first argument to vector-set! must be an vector");

    if ( decode_int(decode_vec(a)[0]) <= decode_int(i))
        fatal_err("RUNTIME ERROR: Attempt to use a vector index that is out of bounds");


    decode_vec(a)[1+decode_int(i)] = v;

    return V_VOID;
}
GEN_EXPECT3ARGLIST(applyprim_vector_45set_33, prim_vector_45set_33)


///// void, ...


u64 prim_void()
{
    return V_VOID;
}
GEN_EXPECT0ARGLIST(applyprim_void, prim_void)





///// eq?, eqv?, equal?


u64 prim_eq_63(u64 a, u64 b)
{
    if (a == b)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT2ARGLIST(applyprim_eq_63, prim_eq_63)


u64 prim_eqv_63(u64 a, u64 b)
{
    if (a == b)
        return V_TRUE;
    else if (a != V_NULL && a != V_TRUE && a != V_FALSE && a != V_VOID
        && b != V_NULL && b != V_TRUE && b != V_FALSE && b != V_VOID)
    {
        if (get_tag(a) == STR_TAG && get_tag(b) == STR_TAG) {
            if (strcmp(decode_str(a), decode_str(b)) == 0) return V_TRUE;
            else return V_FALSE;
        }
        else if (get_tag(a) == SYM_TAG && get_tag(b) == SYM_TAG) {
            if (strcmp(decode_sym(a), decode_sym(b)) == 0) return V_TRUE;
            else return V_FALSE;
        }
        else if (get_tag(a) == INT_TAG && get_tag(b) == INT_TAG) {
            if (decode_int(a)==decode_int(b)) return V_TRUE;
            else return V_FALSE;
        }
        else if (get_tag(a) == CHAR_TAG && get_tag(b) == CHAR_TAG) {
            if (decode_char(a)==decode_char(b)) return V_TRUE;
            else return V_FALSE;
        }
        else
            return V_FALSE;
    }
    else
        return V_FALSE;
}
GEN_EXPECT2ARGLIST(applyprim_eqv_63, prim_eqv_63)

/*
u64 prim_equal_63(u64 a, u64 b)
{
    return 0;
}
GEN_EXPECT2ARGLIST(applyprim_equal_63, prim_equal_63)
*/


///// Other predicates


u64 prim_number_63(u64 a)
{
    // We assume that ints are the only number
    if (a == V_NULL || a == V_FALSE || a == V_TRUE || a == V_VOID)
        return V_FALSE;
    else if (get_tag(a) == INT_TAG)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_number_63, prim_number_63)


u64 prim_integer_63(u64 a)
{
    if (a == V_NULL || a == V_FALSE || a == V_TRUE || a == V_VOID)
        return V_FALSE;
    else if (get_tag(a) == INT_TAG)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_integer_63, prim_integer_63)


u64 prim_void_63(u64 a)
{
    if (a == V_VOID)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_void_63, prim_void_63)


u64 prim_procedure_63(u64 a)
{
    if (a == V_NULL || a == V_FALSE || a == V_TRUE || a == V_VOID)
        return V_FALSE;
    else if (get_tag(a) == CLO_TAG)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_procedure_63, prim_procedure_63)


///// null?, cons?, cons, car, cdr


u64 prim_null_63(u64 p) // null?
{
    if (p == V_NULL)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_null_63, prim_null_63)


u64 prim_cons_63(u64 p) // cons?
{
    if (p == V_NULL || p == V_FALSE || p == V_TRUE || p == V_VOID)
        return V_FALSE;
    else if (get_tag(p) == CONS_TAG)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_cons_63, prim_cons_63)


u64 prim_cons(u64 a, u64 b)
{
    u64* p = alloc(2*sizeof(u64));
    p[0] = a;
    p[1] = b;
    return encode_cons(p);
}
GEN_EXPECT2ARGLIST(applyprim_cons, prim_cons)

u64 applyprim_string(u64 lst)
{
    // pretty terrible, but works 2.0
    if (lst == V_NULL) {
        char* newstr = str_alloc(1);
        newstr[0] = '\0';
        return encode_str(newstr);
    }
    assert_tag_i(lst, CONS_TAG, "string takes a LIST of chars");
    char* buffer = str_alloc(2048);
    u64 i = 0;
    u64 codedchar;
    while (lst != V_NULL && lst != V_TRUE && lst != V_FALSE && lst != V_VOID && get_tag(lst) == CONS_TAG && i < 256) {
        codedchar = expect_cons(lst, &lst);
        assert_tag_i(codedchar, CHAR_TAG, "string takes a list of CHARS");
        const char ch = decode_char(codedchar);
        buffer[i++] = (char) ch;
    }
    ASSERT_VALUE(lst, V_NULL,"string takes a LIST of chars")
    char* newstr = str_alloc(i+1);
    for (u64 j = 0; j < i; ++j)
        newstr[j]=buffer[j];
    newstr[i] = '\0';
    return encode_str(newstr);
}

u64 applyprim_string_45append(u64 lst)
{
    // pretty terrible, but works 3.0
    if (lst == V_NULL) {
        char* newstr = str_alloc(1);
        newstr[0] = '\0';
        return encode_str(newstr);
    }
    assert_tag_i(lst, CONS_TAG, "string takes a LIST of strings");
    char** buffer = (char**)GC_MALLOC(256 * sizeof(char*));
    u64 len = 0;
    u64 i = 0;
    u64 codedstr;
    while (lst != V_NULL && lst != V_TRUE && lst != V_FALSE && lst != V_VOID && get_tag(lst) == CONS_TAG && i < 256) {
        codedstr = expect_cons(lst, &lst);
        assert_tag_i(codedstr, STR_TAG, "string-append takes a list of STRINGS");
        const char* str = decode_str(codedstr);
        len = len + strlen(str);
        buffer[i++] = (char*) str;
    }
    ASSERT_VALUE(lst, V_NULL,"string-append takes a LIST of strings")
    char* newstr = str_alloc(len+1);
    strcpy (newstr, buffer[0]);
    for (u64 j = 1; j < i; ++j)
        strcat(newstr, buffer[j]);
    return encode_str(newstr);
}

u64 prim_substring2(u64 s, u64 sidx) {
    assert_tag_i(s, STR_TAG, "first argument to substring must be a string");
    assert_tag_i(sidx, INT_TAG, "second argument to substring must be an int");

    const char* str = decode_str(s);
    s32 start = decode_int(sidx);
    char* substr = str_alloc((strlen(str)+1-start));
    strncpy(substr, str+start, (strlen(str)+1-start));
    return encode_str(substr);
}
GEN_EXPECT2ARGLIST(applyprim_substring2, prim_substring2)

u64 prim_substring3(u64 s, u64 sidx, u64 eidx) {
    assert_tag_i(s, STR_TAG, "first argument to substring must be a string");
    assert_tag_i(sidx, INT_TAG, "second argument to substring must be an int");
    assert_tag_i(eidx, INT_TAG, "third argument to substring must be an int");

    const char* str = decode_str(s);
    s32 start = decode_int(sidx);
    s32 end = decode_int(eidx);
    char* substr = str_alloc((1+end-start));
    strncpy(substr, str+start, (end-start));
    substr[(1+end-start)] = '\0';
    return encode_str(substr);
}
GEN_EXPECT3ARGLIST(applyprim_substring3, prim_substring3)

u64 prim_string_45_62list(u64 s) {
    assert_tag_i(s, STR_TAG, "argument to string->list must be a string");

    const char* str = decode_str(s);
    s32 len = strlen(str);
    const char c0 = str[--len];
    u64 chr = encode_char(c0);
    u64 cell = prim_cons(chr, V_NULL);
    while (len > 0) {
        const char cs = str[--len];
        chr = encode_char(cs);
        cell = prim_cons(chr, cell);
    }
    return cell;
}
GEN_EXPECT1ARGLIST(applyprim_string_45_62list, prim_string_45_62list)

u64 clo_create(u64 a, u64 b)
{
    u64* p = alloc(2*sizeof(u64));
    p[0] = a;
    p[1] = b;
    return encode_clo(p);
}

u64 prim_car(u64 p)
{
    u64 rest;
    u64 v0 = expect_cons(p,&rest);

    return v0;
}
GEN_EXPECT1ARGLIST(applyprim_car, prim_car)

u64 clo_code(u64 p)
{
    u64 rest;
    u64 v0 = expect_clo(p,&rest);

    return v0;
}

u64 prim_cdr(u64 p)
{
    u64 rest;
    u64 v0 = expect_cons(p,&rest);

    return rest;
}
GEN_EXPECT1ARGLIST(applyprim_cdr, prim_cdr)

u64 clo_env(u64 p)
{
    u64 rest;
    u64 v0 = expect_clo(p,&rest);

    return rest;
}

///// s32 prims, +, -, *, =, ...


u64 prim__43(u64 a, u64 b) // +
{
    assert_tag_i(a, INT_TAG, "(prim + a b); a is not an integer");
    assert_tag_i(b, INT_TAG, "(prim + a b); b is not an integer");

        //printf("sum: %ld\n", decode_int(a) + decode_int(b));

    return encode_int(decode_int(a) + decode_int(b));
}

u64 applyprim__43(u64 p)
{
    if (p == V_NULL)
        return encode_int(0);
    else
    {
        assert_tag_i(p, CONS_TAG, "Tried to apply + on non list value.");
        u64* pp = decode_cons(p);
        return encode_int(decode_int(pp[0]) + decode_int(applyprim__43(pp[1])));
    }
}

u64 prim__45(u64 a, u64 b) // -
{
    assert_tag_i(a, INT_TAG, "(prim + a b); a is not an integer");
    assert_tag_i(b, INT_TAG, "(prim - a b); b is not an integer");

    return encode_int(decode_int(a) - decode_int(b));
}

u64 applyprim__45(u64 p)
{
    if (p == V_NULL)
        return encode_int(0);
    else
    {
        assert_tag_i(p, CONS_TAG, "Tried to apply + on non list value.");
        u64* pp = decode_cons(p);
        if (pp[1] == V_NULL)
            return encode_int(0 - decode_int(pp[0]));
        else // ideally would be properly left-to-right
            return encode_int(decode_int(pp[0]) - decode_int(applyprim__43(pp[1])));
    }
}

u64 prim__42(u64 a, u64 b) // *
{
    assert_tag_i(a, INT_TAG, "(prim * a b); a is not an integer");
    assert_tag_i(b, INT_TAG, "(prim * a b); b is not an integer");

    return encode_int(decode_int(a) * decode_int(b));
}

u64 applyprim__42(u64 p)
{
    if (p == V_NULL)
        return encode_int(1);
    else
    {
        assert_tag_i(p, CONS_TAG, "Tried to apply + on non list value.");
        u64* pp = decode_cons(p);
        return encode_int(decode_int(pp[0]) * decode_int(applyprim__42(pp[1])));
    }
}

u64 prim__47(u64 a, u64 b) // /
{
    assert_tag_i(a, INT_TAG, "(prim / a b); a is not an integer");
    assert_tag_i(b, INT_TAG, "(prim / a b); b is not an integer");
    if (decode_int(b) == 0)
        fatal_err("RUNTIME ERROR: Cannot divide by zero");
    return encode_int(decode_int(a) / decode_int(b));
}

u64 prim__61(u64 a, u64 b)  // =
{
    assert_tag_i(a, INT_TAG, "(prim = a b); a is not an integer");
    assert_tag_i(b, INT_TAG, "(prim = a b); b is not an integer");

    if (decode_int(a) == decode_int(b))
        return V_TRUE;
    else
        return V_FALSE;
}

u64 prim__60(u64 a, u64 b) // <
{
    assert_tag_i(a, INT_TAG, "(prim < a b); a is not an integer");
    assert_tag_i(b, INT_TAG, "(prim < a b); b is not an integer");

    if (decode_int(a) < decode_int(b))
        return V_TRUE;
    else
        return V_FALSE;
}

u64 prim__60_61(u64 a, u64 b) // <=
{
    assert_tag_i(a, INT_TAG, "(prim <= a b); a is not an integer");
    assert_tag_i(b, INT_TAG, "(prim <= a b); b is not an integer");

    if (decode_int(a) <= decode_int(b))
        return V_TRUE;
    else
        return V_FALSE;
}

u64 prim_not(u64 a)
{
    if (a == V_FALSE)
        return V_TRUE;
    else
        return V_FALSE;
}
GEN_EXPECT1ARGLIST(applyprim_not, prim_not)



}
