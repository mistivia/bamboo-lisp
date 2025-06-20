#include "interp.h"

#include <stdarg.h>
#include <inttypes.h>

#include <algds/hash_table.h>
#include <algds/str.h>

#include "sexp.h"
#include "builtins.h"
#include "primitives.h"

#define BUFSIZE 1024

void PrimitiveEntry_show(PrimitiveEntry self, FILE *fp) { }
VECTOR_IMPL(PrimitiveEntry);

SExpRef unbound = {-1};

void Interp_init(Interp *self) {
    self->errmsg_buf = malloc(BUFSIZE);
    SExpVector_init(&self->objs);
    IntVector_init(&self->empty_space);
    PrimitiveEntryVector_init(&self->primitives);
    String2IntHashTable_init(&self->symbols);
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
    for (int i = 2; i < 1024; i++) {
        SExpVector_push_back(&self->objs, sexp);
        IntVector_push_back(&self->empty_space, i);
    }

    self->stack = lisp_cons(self, self->top_level, self->nil);
    self->reg = self->nil;

    Interp_add_primitive(self, "if", primitive_if);
    Interp_add_primitive(self, "cond", primitive_cond);
    Interp_add_primitive(self, "progn", primitive_progn);
    Interp_add_primitive(self, "setq", primitive_setq);
    Interp_add_primitive(self, "let", primitive_let);
    Interp_add_primitive(self, "while", primitive_while);
    Interp_add_primitive(self, "lambda", primitive_lambda);
    Interp_add_primitive(self, "function", primitive_function);
    Interp_add_primitive(self, "defun", primitive_defun);

    Interp_add_userfunc(self, "car", builtin_car);
    Interp_add_userfunc(self, "list", builtin_list);
    Interp_add_userfunc(self, "cdr", builtin_cdr);
    Interp_add_userfunc(self, "cons", builtin_cons);
    Interp_add_userfunc(self, "+", builtin_add);
    Interp_add_userfunc(self, "-", builtin_sub);
    Interp_add_userfunc(self, "=", builtin_num_equal);
    Interp_add_userfunc(self, "<", builtin_lt);
    Interp_add_userfunc(self, ">", builtin_gt);
    Interp_add_userfunc(self, ">=", builtin_ge);
    Interp_add_userfunc(self, "<=", builtin_le);
}

void Interp_add_userfunc(Interp *interp, const char *name, LispUserFunc fn) {
    SExpRef userfunc = new_userfunc(interp, fn);
    lisp_defun(interp, name, userfunc);
}

void Interp_free(Interp *self) {
    for (size_t i = 0; i < SExpVector_len(&self->objs); i++) {
        SExp *obj = SExpVector_ref(&self->objs, i);
        if (obj->type == kStringSExp) {
            free((void*)obj->str);
        }
    }
    for (String2IntHashTableIter iter = String2IntHashTable_begin(&self->symbols);
            iter != NULL;
            iter = String2IntHashTable_next(&self->symbols, iter)) {
        free((void*)iter->key);
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

void Interp_gc(Interp *interp, SExpRef tmproot) {
    // TODO
}

bool lisp_truep(Interp *interp, SExpRef a) {
    if (REF(a)->type == kNilSExp) return false;
    if (REF(a)->type == kBooleanSExp && !REF(a)->boolean) return false;
    return true;
}

SExpRef lisp_cons(Interp *interp, SExpRef a, SExpRef b) {
    SExpRef obj = new_sexp(interp);
    REF(obj)->type = kPairSExp;
    REF(obj)->pair.car = a;
    REF(obj)->pair.cdr = b;
    return obj;
}

SExpRef lisp_dup(Interp *interp, SExpRef arg) {
    SExpRef obj = new_sexp(interp);
    *REF(obj) = *REF(arg);
    return obj;
}

SExpRef lisp_car(Interp *interp, SExpRef arg) {
    if (REF(arg)->type != kPairSExp) {
        return new_error(interp, "car: wrong argument type.");
    }
    return REF(arg)->pair.car;
}

SExpRef lisp_cdr(Interp *interp, SExpRef arg) {
    if (REF(arg)->type != kPairSExp) {
        return new_error(interp, "cdr: wrong argument type.");
    }
    return REF(arg)->pair.cdr;
}

bool lisp_check_list(Interp *interp, SExpRef lst) {
    while (REF(lst)->type == kPairSExp) {
        lst = CDR(lst);
    }
    return REF(lst)->type == kNilSExp;
}

void lisp_to_string_impl(str_builder_t *sb, Int2IntHashTable *visited, Interp *interp, SExpRef val) {
    SExp *pe = REF(val);
    if (pe->type == kIntegerSExp) {
        str_builder_append(sb, "%"PRId64, pe->integer);
    } else if (pe->type == kRealSExp) {
        str_builder_append(sb, "%lf", pe->real);
    } else if (pe->type == kCharSExp) {
        str_builder_append(sb, "#\%c", pe->character);
    } else if (pe->type == kBooleanSExp) {
        if (pe->boolean) str_builder_append(sb, "#t");
        else str_builder_append(sb, "#f");
    } else if (pe->type == kCharSExp) {
        str_builder_append(sb, "#\%c", pe->character);
    } else if (pe->type == kSymbolSExp) {
        str_builder_append(sb, "%s", pe->str);
    } else if (pe->type == kStringSExp) {
        str_builder_append(sb, "\"%s\"", pe->str);
    } else if (pe->type == kFuncSExp) {
        str_builder_append(sb, "<FUNCTION>");
    } else if (pe->type == kMacroSExp) {
        str_builder_append(sb, "<MACRO>");
    } else if (pe->type == kEnvSExp) {
        str_builder_append(sb, "<ENV>");
    } else if (pe->type == kBindingSExp) {
        str_builder_append(sb, "<BINDING>");
    } else if (pe->type == kNilSExp) {
        str_builder_append(sb, "()");
    } else if (pe->type == kErrSExp) {
        str_builder_append(sb, "<ERROR>");
    } else if (pe->type == kPairSExp) {
        if (Int2IntHashTable_find(visited, val.idx) != NULL) {
            str_builder_append(sb, "<%d>", val.idx);
        } else {
            str_builder_append_char(sb, '(');
            SExpRef cur = val;
            while (REF(cur)->type == kPairSExp
                    && Int2IntHashTable_find(visited, cur.idx) == NULL) {
                Int2IntHashTable_insert(visited, cur.idx, 1);
                lisp_to_string_impl(sb, visited, interp, CAR(cur));
                str_builder_append_char(sb, ' ');
                cur = CDR(cur);
            }
            if (REF(cur)->type == kNilSExp) {
                sb->buf[sb->size - 1] = ')';
                str_builder_append_char(sb, '\0');
            } else if (REF(cur)->type != kPairSExp) {
                str_builder_append(sb, ". ");
                lisp_to_string_impl(sb, visited, interp, cur);
                str_builder_append(sb, ")");
                str_builder_append_char(sb, '\0');
            } else {
                str_builder_append(sb, "<%d>)", cur.idx);
                str_builder_append_char(sb, '\0');
            }
        }
    }
}


const char* lisp_to_string(Interp *interp, SExpRef val) {
    str_builder_t sb;
    Int2IntHashTable visited;
    Int2IntHashTable_init(&visited);
    init_str_builder(&sb);
    lisp_to_string_impl(&sb, &visited, interp, val);    
    Int2IntHashTable_free(&visited);
    return sb.buf;
}

void lisp_defun(Interp *interp, const char *name, SExpRef val) {
    SExpRef binding = REF(interp->top_level)->env.bindings;
    while (REF(binding)->type != kNilSExp) {
        if (strcmp(name, REF(REF(binding)->binding.name)->str) == 0) {
            REF(binding)->binding.func = val;
            return;
        }
        binding = REF(binding)->binding.next;
    }
    binding = REF(interp->top_level)->env.bindings;
    SExpRef newbinding = new_binding(interp, new_symbol(interp, name), NIL);
    REF(newbinding)->binding.func = val;
    REF(newbinding)->binding.next = binding;
    REF(interp->top_level)->env.bindings = newbinding;
}

SExpRef lisp_setq(Interp *interp, const char *name, SExpRef val) {
    SExpRef env = CAR(interp->stack);
    while (REF(env)->type != kNilSExp) {
        SExpRef binding = REF(env)->env.bindings;
        while (REF(binding)->type != kNilSExp) {
            if (strcmp(name, REF(REF(binding)->binding.name)->str) == 0) {
                REF(binding)->binding.value = val;
                return NIL;
            }
            binding = REF(binding)->binding.next;
        }
        env = REF(env)->env.parent;
    }
    return new_error(interp, "Unbound variable: %s.\n", name);
}

SExpRef lisp_lookup(Interp *interp, const char *name) {
    SExpRef env = CAR(interp->stack);
    while (REF(env)->type != kNilSExp) {
        SExpRef binding = REF(env)->env.bindings;
        while (REF(binding)->type != kNilSExp) {
            if (strcmp(name, REF(REF(binding)->binding.name)->str) == 0) {
                SExpRef ret = REF(binding)->binding.value;
                if (ret.idx < 0) goto notfound;
                return ret;
            }
            binding = REF(binding)->binding.next;
        }
        env = REF(env)->env.parent;
    }
notfound:
    return new_error(interp, "Unbound variable: %s.\n", name);
}

void lisp_print(Interp *interp, SExpRef obj, FILE *fp) {
    const char *str = lisp_to_string(interp, obj);
    fprintf(fp, "%s\n", str);
    free((void*)str);
}

SExpRef lisp_lookup_func(Interp *interp, const char *name) {
    SExpRef binding = REF(interp->top_level)->env.bindings;
    while (REF(binding)->type != kNilSExp) {
        if (strcmp(name, REF(REF(binding)->binding.name)->str) == 0) {
            return REF(binding)->binding.func;
        }
        binding = REF(binding)->binding.next;
    }
    return new_error(interp, "Unbound function: %s.\n", name);
}

bool lisp_nilp(Interp *interp, SExpRef obj) {
    return REF(obj)->type == kNilSExp;
}

SExpRef lisp_reverse(Interp *interp, SExpRef lst) {
    SExpRef cur = lst;
    SExpRef ret = NIL;
    while (!NILP(cur)) {
        ret = CONS(CAR(cur), ret);
        cur = CDR(cur);
    }
    return ret;
}

SExpRef lisp_eval_args(Interp *interp, SExpRef args) {
    SExpRef ret = interp->nil;
    SExpRef cur = args;
    while (!NILP(cur)) {
        // save ret in register
        PUSH_REG(ret);
        SExpRef evalres = EVAL(CAR(cur));
        POP_REG();
        if (ERRORP(evalres)) {
            ret = evalres;
            goto end;
        }
        ret = CONS(evalres, ret);
        cur = CDR(cur);
    }
    ret = lisp_reverse(interp, ret);
end:
    Interp_gc(interp, ret);
    return ret;
}

int lisp_length(Interp *interp, SExpRef lst) {
    int cnt = 0;
    while (REF(lst)->type == kPairSExp) {
        cnt++;
        lst = CDR(lst);
    }
    return cnt;
}


SExpRef lisp_eval(Interp *interp, SExpRef sexp) {
    SExpRef ret;
    SExpType type;
    PUSH_REG(sexp);
    type = REF(sexp)->type;
    if (type == kEnvSExp || type == kEnvSExp || type == kBindingSExp) {
        ret = new_error(interp, "type error: cannot eval.\n");
        goto end;
    }
    if (type == kIntegerSExp
            || type == kStringSExp
            || type == kBooleanSExp
            || type == kCharSExp
            || type == kErrSExp
            || type == kFuncSExp
            || type == kRealSExp) {
        ret = sexp;
        goto end;
    }
    if (type == kSymbolSExp) {
        ret = lisp_lookup(interp, REF(sexp)->str);
        goto end;
    }
    if (type == kPairSExp) {
        if (!lisp_check_list(interp, sexp)) {
            ret = new_error(interp, "eval: list not proper.\n");
            goto end;
        }
        if (REF(CAR(sexp))->type != kSymbolSExp) {
            ret = new_error(interp, "eval: first elem must be a symbol.\n");
            goto end;
        }
        const char *symbol = REF(CAR(sexp))->str;
        for (int i = 0; i < PrimitiveEntryVector_len(&interp->primitives); i++) {
            if (strcmp(symbol, PrimitiveEntryVector_ref(&interp->primitives, i)->name) == 0) {
                LispPrimitive primitive_fn =
                    PrimitiveEntryVector_ref(&interp->primitives, i)->fn;
                ret = (*primitive_fn)(interp, CDR(sexp));
                goto end;
            }
        }
        SExpRef fn = lisp_lookup_func(interp, symbol);
        if (REF(fn)->type == kUserFuncSExp) {
            SExpRef args = lisp_eval_args(interp, CDR(sexp));
            if (ERRORP(args)) { ret = args; goto end; }
            PUSH_REG(args);
            ret = (*REF(fn)->userfunc)(interp, args);
            POP_REG();
            goto end;
        }
        // TODO: macro / func
        ret = new_error(interp, "eval: \"%s\" is not a primitive, function, or macro.\n", symbol);
        goto end;
    }
    ret = NIL;
end:
    POP_REG();
    Interp_gc(interp, ret);
    return ret;
}

SExpRef new_sexp(Interp *interp) {
    if (IntVector_len(&interp->empty_space) == 0) {
        SExp sexp;
        sexp.type = kEmptySExp;
        SExpVector_push_back(&interp->objs, sexp);
        return (SExpRef){ SExpVector_len(&interp->objs) - 1 };
    }
    int idx = *IntVector_ref(&interp->empty_space, IntVector_len(&interp->empty_space) - 1);
    IntVector_pop(&interp->empty_space);
    return (SExpRef){idx};
}

SExpRef new_env(Interp *interp) {
    SExpRef ret = new_sexp(interp);
    REF(ret)->type = kEnvSExp;
    REF(ret)->env.parent = NIL;
    REF(ret)->env.bindings = NIL;
    return ret;
}

SExpRef new_lambda(Interp *interp, SExpRef param, SExpRef body, SExpRef env) {
    SExpRef ret = new_sexp(interp);
    REF(ret)->type = kFuncSExp;
    REF(ret)->func.args = param;
    REF(ret)->func.body = body;
    REF(ret)->func.env = env;
    return ret;
}

SExpRef new_binding(Interp *interp, SExpRef sym, SExpRef val) {
    SExpRef ret = new_sexp(interp);
    REF(ret)->type = kBindingSExp;
    REF(ret)->binding.name = sym;
    REF(ret)->binding.value = val;
    return ret;
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
    REF(ret)->str = interp->errmsg_buf;
    return ret;
}

SExpRef new_userfunc(Interp *interp, LispUserFunc val) {
    SExpRef ret = new_sexp(interp);
    REF(ret)->type = kUserFuncSExp;
    REF(ret)->userfunc = val;
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

