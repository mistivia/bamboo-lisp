#include "interp.h"

#include <stdarg.h>

#include <algds/hash_table.h>

#include "sexp.h"

#define BUFSIZE 1024

#define REF(x) (Interp_ref(interp, (x)))

void PrimitiveEntry_show(PrimitiveEntry self, FILE *fp) { }
VECTOR_IMPL(PrimitiveEntry);

void Interp_init(Interp *self) {
    self->errmsg_buf = malloc(BUFSIZE);
    SExpVector_init(&self->objs);
    IntVector_init(&self->empty_space);
    PrimitiveEntryVector_init(&self->primitives);
    String2IntHashTable_init(&self->symbols);
    self->gc_paused = false;
    SExp sexp;
    sexp.type = kNilSExp;
    SExpVector_push_back(&self->objs, sexp);
    self->nil = (SExpRef){0};

    sexp.type = kEnvSExp;
    sexp.env.parent= self->nil;
    sexp.env.bindings = self->nil;
    SExpVector_push_back(&self->objs, sexp);
    self->top_level = (SExpRef){1};
    sexp.type = kEmptySExp;
    for (int i = 1; i < 1024; i++) {
        SExpVector_push_back(&self->objs, sexp);
        IntVector_push_back(&self->empty_space, i);
    }

    self->evaluating = self->nil;
    self->stack = lisp_cons(self, self->top_level, self->nil);

    Interp_add_primitive(self, "car", primitive_car);
    Interp_add_primitive(self, "cdr", primitive_cdr);
    Interp_add_primitive(self, "cons", primitive_cons);
    Interp_add_primitive(self, "add", primitive_add);
    Interp_add_primitive(self, "sub", primitive_sub);
}

void Interp_free(Interp *self) {
    for (size_t i = 0; i < SExpVector_len(&self->objs); i++) {
        SExp *obj = SExpVector_ref(&self->objs, i);
        if (obj->type == kSymbolSExp || obj->type == kStringSExp) {
            free((void*)obj->str);
        }
    }
    String2IntHashTable_free(&self->symbols);
    SExpVector_free(&self->objs);
    IntVector_free(&self->empty_space);
    PrimitiveEntryVector_free(&self->primitives);
    free(self->errmsg_buf);
}

SExp* Interp_ref(Interp *self, SExpRef ref) {
    if (ref.idx > SExpVector_len(&self->objs)) return NULL;
    SExp *res = SExpVector_ref(&self->objs, ref.idx);
    return res;
}

void Interp_add_primitive(Interp *self, const char *name, LispPrimitive fn) {
    PrimitiveEntryVector_push_back(&self->primitives, (PrimitiveEntry){
        .name = name,
        .fn = fn
    });
}

void Interp_gc(Interp *interp) {
    // TODO
}

SExpRef lisp_cons(Interp *interp, SExpRef car, SExpRef cdr) {
    SExpRef ret = new_sexp(interp);
    SExp *psexp = Interp_ref(interp, ret);
    psexp->type = kPairSExp;
    psexp->pair.car = car;
    psexp->pair.cdr = cdr;
    return ret;
}

SExpRef lisp_dup(Interp *interp, SExpRef val) {
    SExpRef ret = new_sexp(interp);
    *REF(ret) = *REF(val);
    return ret;
}

SExpRef lisp_cadr(Interp *interp, SExpRef val) {
    return lisp_car(interp, lisp_cdr(interp, val));
}
SExpRef lisp_cddr(Interp *interp, SExpRef val) {
    return lisp_cdr(interp, lisp_cdr(interp, val));
}
SExpRef lisp_caddr(Interp *interp, SExpRef val) {
    return lisp_car(interp, lisp_cddr(interp, val));
}
SExpRef lisp_cdddr(Interp *interp, SExpRef val) {
    return lisp_cdr(interp, lisp_cddr(interp, val));
}
SExpRef lisp_cadddr(Interp *interp, SExpRef val) {
    return lisp_car(interp, lisp_cdddr(interp, val));
}
SExpRef lisp_cddddr(Interp *interp, SExpRef val) {
    return lisp_cdr(interp, lisp_cdddr(interp, val));
}

SExpRef lisp_car(Interp *interp, SExpRef val) {
    if (REF(val)->type != kPairSExp) {
        return new_error(interp, "type error: car.\n");
    }
    return REF(val)->pair.car;
}

SExpRef lisp_cdr(Interp *interp, SExpRef val) {
    if (REF(val)->type != kPairSExp) {
        return new_error(interp, "type error: cdr.\n");
    }
    return REF(val)->pair.cdr;
}

bool lisp_check_list(Interp *interp, SExpRef val) {

}

SExpRef lisp_lookup(Interp *interp, const char *name) {
    // TODO
}

SExpRef lisp_check_argnum(Interp *interp, const char *name, int num, SExpRef args) {
    // TODO
    return interp->nil;
}

SExpRef primitive_car(Interp *interp, SExpRef args) {
    SExpRef check = lisp_check_argnum(interp, "car", 1, args);
    if (REF(check)->type == kErrSExp) return args;
    args = lisp_eval_args(interp, args);
    if (REF(args)->type == kErrSExp) return args;
    return lisp_car(interp, lisp_car(interp, args));
}

SExpRef primitive_cdr(Interp *interp, SExpRef args) {
    SExpRef check = lisp_check_argnum(interp, "cdr", 1, args);
    if (REF(check)->type == kErrSExp) return args;
    args = lisp_eval_args(interp, args);
    if (REF(args)->type == kErrSExp) return args;
    return lisp_cdr(interp, lisp_car(interp, args));
}

SExpRef lisp_eval(Interp *interp, SExpRef val) {
    SExpType type;
    type = REF(val)->type;
    if (type == kEnvSExp || type == kEnvSExp || type == kBindingSExp) {
        return new_error(interp, "type error: cannot eval.\n");
    }
    if (type == kIntegerSExp
            || type == kStringSExp
            || type == kBooleanSExp
            || type == kCharSExp
            || type == kErrSExp
            || type == kFuncSExp
            || type == kRealSExp) {
        return val;
    }
    if (type == kSymbolSExp) {
        return lisp_lookup(interp, REF(val)->str);
    }
    if (type == kPairSExp) {
        if (!lisp_check_list(interp, val)) {
            return new_error(interp, "eval: list not proper.\n");
        }
        SExpRef hd = lisp_car(interp, (lisp_car(interp, val)));
        if (REF(hd)->type != kSymbolSExp) {
            return new_error(interp, "eval: first elem must be a symbol.\n");
        }
        const char *symbol = REF(hd)->str;
        for (int i = 0; i < PrimitiveEntryVector_len(&interp->primitives); i++) {
            if (strcmp(symbol, PrimitiveEntryVector_ref(&interp->primitives, i)->name) == 0) {
                LispPrimitive primitive_fn =
                    PrimitiveEntryVector_ref(&interp->primitives, i)->fn;
                return (*primitive_fn)(interp, lisp_cdr(interp, val));
            }
            // TODO: macro / func
        }
    }
    return interp->nil;
}

SExpRef new_sexp(Interp *interp) {
    if (IntVector_len(&interp->empty_space) == 0) {
        if (interp->gc_paused) {
            SExp sexp;
            sexp.type = kEmptySExp;
            SExpVector_push_back(&interp->objs, sexp);
            return (SExpRef){ SExpVector_len(&interp->objs) - 1 };
        } else Interp_gc(interp);
    }
    int idx = *IntVector_ref(&interp->empty_space, IntVector_len(&interp->empty_space) - 1);
    IntVector_pop(&interp->empty_space);
    return (SExpRef){idx};
}

SExpRef new_boolean(Interp *interp, bool val) {
    SExpRef ret = new_sexp(interp);
    REF(ret)->type = kBooleanSExp;
    REF(ret)->boolean = val;
    return ret;
}

SExpRef new_error(Interp *interp, const char *format, ...) {
    va_list args;
    va_start(args, format);
    vsnprintf(interp->errmsg_buf, BUFSIZE, format, args);
    va_end(args);
    SExpRef ret = new_sexp(interp);
    REF(ret)->type = kErrSExp;
    REF(ret)->boolean = interp->errmsg_buf;
    return ret;
}

SExpRef new_char(Interp *interp, char val) {
    SExpRef ret = new_sexp(interp);
    SExp *psexp = Interp_ref(interp, ret);
    psexp->type = kCharSExp;
    psexp->character = val;
    return ret;
}

SExpRef new_integer(Interp *interp, int64_t val) {
    SExpRef ret = new_sexp(interp);
    SExp *psexp = Interp_ref(interp, ret);
    psexp->type = kIntegerSExp;
    psexp->integer = val;
    return ret;
}

SExpRef new_real(Interp *interp, double val) {
    SExpRef ret = new_sexp(interp);
    SExp *psexp = Interp_ref(interp, ret);
    psexp->type = kRealSExp;
    psexp->real = val;
    return ret;
}

SExpRef new_string(Interp *interp, const char *val) {
    char *dup = strdup(val);
    SExpRef ret = new_sexp(interp);
    SExp *psexp = Interp_ref(interp, ret);
    psexp->type = kStringSExp;
    psexp->str = dup;
    return ret;
}

SExpRef new_symbol(Interp *interp, const char *val) {
    String2IntHashTableIter iter = String2IntHashTable_find(&interp->symbols, val);
    if (iter == NULL) {
        char *dup = strdup(val);
        SExpRef ret = new_sexp(interp);
        SExp *psexp = Interp_ref(interp, ret);
        psexp->type = kSymbolSExp;
        psexp->str = dup;
        String2IntHashTable_insert(&interp->symbols, dup, ret.idx);
        return ret;
    } else {
        return (SExpRef){ iter->val };
    }
}

SExpRef new_list1(Interp *interp, SExpRef e1) {
    return lisp_cons(interp, e1, interp->nil);
}

SExpRef new_list2(Interp *interp, SExpRef e1, SExpRef e2) {
    return lisp_cons(interp, e1, new_list1(interp, e2));
}

SExpRef new_list3(Interp *interp, SExpRef e1, SExpRef e2, SExpRef e3) {
    return lisp_cons(interp, e1, new_list2(interp, e2, e3));
}

SExpRef new_list4(Interp *interp, SExpRef e1, SExpRef e2, SExpRef e3, SExpRef e4) {
    return lisp_cons(interp, e1, new_list3(interp, e2, e3, e4));
}

SExpRef new_list5(Interp *interp, SExpRef e1, SExpRef e2, SExpRef e3, SExpRef e4, SExpRef e5) {
    return lisp_cons(interp, e1, new_list4(interp, e2, e3, e4, e5));
}

