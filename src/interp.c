#include "interp.h"
#include "algds/hash_table.h"
#include "sexp.h"

void Interp_init(Interp *self) {
    SExpVector_init(&self->objs);
    IntVector_init(&self->empty_space);
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
    self->stack = cons(self, self->top_level, self->nil);
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
}

SExp* Interp_ref(Interp *self, SExpRef ref) {
    if (ref.idx > SExpVector_len(&self->objs)) return NULL;
    SExp *res = SExpVector_ref(&self->objs, ref.idx);
    return res;
}

void Interp_gc(Interp *interp) {
    // TODO
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
    SExp *psexp = Interp_ref(interp, ret);
    psexp->type = kBooleanSExp;
    psexp->boolean = val;
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

SExpRef cons(Interp *interp, SExpRef car, SExpRef cdr) {
    SExpRef ret = new_sexp(interp);
    SExp *psexp = Interp_ref(interp, ret);
    psexp->type = kPairSExp;
    psexp->pair.car = car;
    psexp->pair.cdr = cdr;
    return ret;
}

SExpRef new_list1(Interp *interp, SExpRef e1) {
    return cons(interp, e1, interp->nil);
}

SExpRef new_list2(Interp *interp, SExpRef e1, SExpRef e2) {
    return cons(interp, e1, new_list1(interp, e2));
}

SExpRef new_list3(Interp *interp, SExpRef e1, SExpRef e2, SExpRef e3) {
    return cons(interp, e1, new_list2(interp, e2, e3));
}

SExpRef new_list4(Interp *interp, SExpRef e1, SExpRef e2, SExpRef e3, SExpRef e4) {
    return cons(interp, e1, new_list3(interp, e2, e3, e4));
}

SExpRef new_list5(Interp *interp, SExpRef e1, SExpRef e2, SExpRef e3, SExpRef e4, SExpRef e5) {
    return cons(interp, e1, new_list4(interp, e2, e3, e4, e5));
}

