#include <bamboo_lisp/interp.h>
#include <bamboo_lisp/sexp.h>

#define VECTOR_TYPEID "ext.core.vector"

LispUserdataMeta bamboo_lisp_array_meta;

static bool is_vector_impl(Interp *interp, SExpRef vec) {
    if (VALTYPE(vec) == kUserDataSExp && strcmp(VECTOR_TYPEID, REF(vec)->userdata_meta->type) == 0) {
        return true;
    }
    return false;
}

static SExpRef is_vector(Interp* interp, SExpRef args) {
    if (LENGTH(args) != 1) return new_error(interp, "vector?: wrongs args num.\n");
    return new_boolean(interp, is_vector_impl(interp, CAR(args)));
}

static SExpRef make_vector(Interp* interp, SExpRef args) {
    SExpRef ret = new_sexp(interp);
    REF(ret)->type = kUserDataSExp;
    REF(ret)->userdata_meta = &bamboo_lisp_array_meta;
    SExpRefVector *data = malloc(sizeof(SExpRefVector));
    SExpRefVector_init(data);
    REF(ret)->userdata = data;
    return ret;
}

static SExpRef vector_ref(Interp* interp, SExpRef args) {
    if (LENGTH(args) != 2) return new_error(interp, "vector-ref: wrong args num.\n");
    if (!is_vector_impl(interp, CAR(args))
            || REF(CADR(args))->type != kIntegerSExp) {
        return new_error(interp, "vector-ref: wrong type.\n");
    }
    int n = REF(CADR(args))->integer;
    SExpRefVector *vec = REF(CAR(args))->userdata;
    if (n >= SExpRefVector_len(vec)) return new_error(interp, "vector-ref: out of bound.\n");
    SExpRef ret = new_sexp(interp);
    return *SExpRefVector_ref(vec, n);
}

static SExpRef vector_append(Interp* interp, SExpRef args) {
    if (LENGTH(args) != 2) return new_error(interp, "vector-append: wrong args num.\n");
    if (!is_vector_impl(interp, CAR(args))) return new_error(interp, "vector-append: first arg not a vector.\n");

    SExpRefVector *vec = REF(CAR(args))->userdata;
    SExpRef elem = CADR(args);
    SExpRefVector_push_back(vec, elem);
    return NIL;
}

static SExpRef vector_insert(Interp* interp, SExpRef args) {
    if (LENGTH(args) != 3) return new_error(interp, "vector-insert: wrong args num.\n");
    if (!is_vector_impl(interp, CAR(args)) || REF(CADR(args))->type != kIntegerSExp)
        return new_error(interp, "vector-insert: wrong types.\n");

    int pos = REF(CADR(args))->integer;
    SExpRefVector *vec = REF(CAR(args))->userdata;
    SExpRef elem = CADDR(args);
    SExpRefVector_insert_before(vec, pos, elem);
    return NIL;
}

static SExpRef vector_delete(Interp* interp, SExpRef args) {
    if (LENGTH(args) != 2) return new_error(interp, "vector-remove: wrong args num.\n");
    if (!is_vector_impl(interp, CAR(args)) || REF(CADR(args))->type != kIntegerSExp)
        return new_error(interp, "vector-remove: wrong types.\n");

    int pos = REF(CADR(args))->integer;
    SExpRefVector *vec = REF(CAR(args))->userdata;
    if (pos >= SExpRefVector_len(vec)) return new_error(interp, "vector-remove: out of bound.\n");
    SExpRefVector_remove(vec, pos);
    return NIL;
}

static SExpRef vector_length(Interp* interp, SExpRef args) {
   if (LENGTH(args) != 1) return new_error(interp, "vector-length: wrong args num.\n");
    if (!is_vector_impl(interp, CAR(args))) return new_error(interp, "vector-length: not a vector.\n");

    SExpRefVector *vec = REF(CAR(args))->userdata;
    return new_integer(interp, SExpRefVector_len(vec));
}

static SExpRef vector_set(Interp* interp, SExpRef args) {
    if (LENGTH(args) != 3) return new_error(interp, "vector-set: wrong args num.\n");
    if (!is_vector_impl(interp, CAR(args)) || REF(CADR(args))->type != kIntegerSExp)
        return new_error(interp, "vector-set: wrong types.\n");

    int pos = REF(CADR(args))->integer;
    SExpRefVector *vec = REF(CAR(args))->userdata;
    if (pos >= SExpRefVector_len(vec)) return new_error(interp, "vector-set: out of bound.\n");

    *SExpRefVector_ref(vec, pos) = CADDR(args);
    return NIL;
}

static void vector_free(void *vself) {
    SExpRefVector *self = vself;
    SExpRefVector_free(self);
    free(self);
}

static void vector_gcmark(Interp *interp, SExpPtrVector *gcstack, void *vself) {
    SExpRefVector *vec = (SExpRefVector *)vself;
    int vecsize = SExpRefVector_len(vec);
    for (int i = 0; i < vecsize; ++i) {
        SExpPtr child = REF(*SExpRefVector_ref(vec, i));
        if (child && !child->marked) {
            SExpPtrVector_push_back(gcstack, child);
        }
    }
}

int bamboo_lisp_ext_init(Interp *interp) {
    bamboo_lisp_array_meta.type = VECTOR_TYPEID;
    bamboo_lisp_array_meta.free = &vector_free;
    bamboo_lisp_array_meta.gcmark = &vector_gcmark;

    Interp_add_userfunc(interp, "vector?", &is_vector);
    Interp_add_userfunc(interp, "make-vector", &make_vector);
    Interp_add_userfunc(interp, "vector-ref", &vector_ref);
    Interp_add_userfunc(interp, "vector-append", &vector_append);
    Interp_add_userfunc(interp, "vector-insert", &vector_insert);
    Interp_add_userfunc(interp, "vector-remove", &vector_delete);
    Interp_add_userfunc(interp, "vector-length", &vector_length);
    Interp_add_userfunc(interp, "vector-set", &vector_set);
    return 1;
}
