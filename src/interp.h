#ifndef BAMBOO_LISP_INTERP_H_
#define BAMBOO_LISP_INTERP_H_

#include <stdbool.h>

#include <algds/hash_table.h>

#include "algds/vec.h"
#include "sexp.h"

struct parser;
typedef struct parser Parser;

struct interp;
typedef struct interp Interp;

typedef struct {
    SExpRef name;
    SExpRef binding;
} TopBinding;

VECTOR_DEF(TopBinding);

struct interp {
    SExpVector objs;
    TopBindingVector topbindings;
    IntVector empty_space;
    String2IntHashTable symbols;
    SExpRef stack;
    SExpRef t;
    SExpRef f;
    SExpRef reg;
    SExpRef top_level;
    SExpRef nil;
    char *errmsg_buf;
    Parser *parser;
    int gensym_cnt;
    bool alwaysgc;
    int recursion_depth;
};

void Interp_init(Interp *self);
void Interp_free(Interp *self);
SExp* Interp_ref(Interp *self, SExpRef ref);
void Interp_gc(Interp *self, SExpRef tmp_root);
void Interp_add_primitive(Interp *self, const char *name, LispPrimitive fn);
void Interp_add_userfunc(Interp *self, const char *name, LispUserFunc fn);

SExpRef Interp_eval_string(Interp *interp, const char * str);
SExpRef Interp_load_file(Interp *interp, const char *filename);

#define REF(_x) (&(interp->objs.buffer)[(_x).idx])
#define CONS(_x, _y) (lisp_cons(interp, (_x), (_y)))
#define NILP(_x) (lisp_nilp(interp, (_x)))
#define LENGTH(_x) (lisp_length(interp, (_x)))
#define EVAL(_x) (lisp_eval(interp, (_x), false))
#define EVALTAIL(_x) (lisp_eval(interp, (_x), true))
#define TRUEP(_x) (lisp_truep(interp, (_x)))
#define FOREACH(_x, _lst) for (SExpRef _x = _lst; !NILP(_x); _x = CDR(_x))
// control flow
#define CTL_FL(_x) \
    (REF((_x))->type == kErrSignal \
    || REF((_x))->type == kReturnSignal \
    || REF((_x))->type == kBreakSignal \
    || REF((_x))->type == kContinueSignal)
#define VALTYPE(_x) (REF((_x))->type)
#define NIL (interp->nil)
#define CAR(_x) (lisp_car(interp, (_x)))
#define CDR(_x) (lisp_cdr(interp, (_x)))
#define CADR(_x) CAR(CDR(_x))
#define CDDR(_x) CDR(CDR(_x))
#define CADDR(_x) CAR(CDDR(_x))
#define CDDDR(_x) CDR(CDDR(_x))
#define CADDDR(_x) CAR(CDDDR(_x))
#define CDDDDR(_x) CDR(CDDDR(_x))
#define CADDDDR(_x) CAR(CDDDDR(_x))
#define CDDDDDR(_x) CDR(CDDDDR(_x))
#define PUSH_REG(_x) { interp->reg = CONS((_x), interp->reg); }
#define POP_REG() { interp->reg = CDR(interp->reg);  }

const char* lisp_to_string(Interp *interp, SExpRef val);
SExpRef lisp_macroexpand1(Interp *interp, SExpRef macro, SExpRef args);
SExpRef lisp_nreverse(Interp *interp, SExpRef lst);
SExpRef lisp_reverse(Interp *interp, SExpRef lst);
void lisp_defun(Interp *interp, SExpRef name, SExpRef val);
void lisp_defvar(Interp *interp, SExpRef name, SExpRef val);
void lisp_print(Interp *interp, SExpRef obj, FILE *fp);
SExpRef lisp_lookup(Interp *interp, SExpRef name);
SExpRef lisp_lookup_func(Interp *interp, SExpRef name);
SExpRef lisp_apply(Interp *interp, SExpRef fn, SExpRef args, bool istail);
SExpRef lisp_cons(Interp *interp, SExpRef a, SExpRef b);
SExpRef lisp_dup(Interp *interp, SExpRef arg);
bool lisp_nilp(Interp *interp, SExpRef arg);
bool lisp_truep(Interp *interp, SExpRef a);
bool lisp_check_list(Interp *interp, SExpRef lst);
SExpRef lisp_setq(Interp *interp, SExpRef name, SExpRef val);
int lisp_length(Interp *interp, SExpRef lst);
SExpRef lisp_car(Interp *interp, SExpRef arg);
SExpRef lisp_cdr(Interp *interp, SExpRef arg);
SExpRef lisp_eval(Interp *interp, SExpRef arg, bool istail);
SExpRef lisp_eval_args(Interp *interp, SExpRef args);
SExpRef lisp_add(Interp *interp, SExpRef args);
SExpRef lisp_sub(Interp *interp, SExpRef args);
SExpRef lisp_mul(Interp *interp, SExpRef args);
SExpRef lisp_div(Interp *interp, SExpRef args);

SExpRef new_error(Interp *interp, const char *format, ...);
SExpRef new_sexp(Interp *ctx);
SExpRef new_return(Interp *ctx, SExpRef ret);
SExpRef new_break(Interp *ctx);
SExpRef new_continue(Interp *ctx);
SExpRef new_boolean(Interp *ctx, bool val);
SExpRef new_char(Interp *ctx, char val);
SExpRef new_integer(Interp *ctx, int64_t val);
SExpRef new_real(Interp *ctx, double val);
SExpRef new_string(Interp *ctx, const char *val);
SExpRef new_symbol(Interp *ctx, const char *val);
SExpRef new_env(Interp *ctx);
SExpRef new_binding(Interp *ctx, SExpRef name, SExpRef val);
SExpRef new_userfunc(Interp *interp, LispUserFunc val);
SExpRef new_primitive(Interp *interp, LispPrimitive val);
SExpRef new_lambda(Interp *interp, SExpRef param, SExpRef body, SExpRef env);
SExpRef new_macro(Interp *interp, SExpRef param, SExpRef body);
SExpRef new_tailcall(Interp *interp, SExpRef fn, SExpRef args);
SExpRef new_list1(Interp *ctx, SExpRef e1);
SExpRef new_list2(Interp *ctx, SExpRef e1, SExpRef e2);
SExpRef new_list3(Interp *ctx, SExpRef e1, SExpRef e2, SExpRef e3);
SExpRef new_list4(Interp *ctx, SExpRef e1, SExpRef e2, SExpRef e3, SExpRef e4);
SExpRef new_list5(Interp *ctx, SExpRef e1, SExpRef e2, SExpRef e3, SExpRef e4, SExpRef e5);

#endif

