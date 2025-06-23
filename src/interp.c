#include "interp.h"

#include <stdarg.h>
#include <inttypes.h>

#include <algds/hash_table.h>
#include <algds/str.h>

#include "sexp.h"
#include "builtins.h"
#include "primitives.h"
#include "parser.h"
#include "prelude.h"

#define BUFSIZE 1024

void TopBinding_show(TopBinding self, FILE *fp) { }
VECTOR_IMPL(TopBinding);

#define UNBOUND ((SExpRef){-1})

void Interp_init(Interp *self) {
    self->gensym_cnt = 42;
    self->parser = malloc(sizeof(Parser));
    Parser_init(self->parser);
    self->parser->ctx = self;
    self->errmsg_buf = malloc(BUFSIZE);
    SExpVector_init(&self->objs);
    IntVector_init(&self->empty_space);
    TopBindingVector_init(&self->topbindings);
    String2IntHashTable_init(&self->symbols);
    int i = 0;
    SExp sexp;
    sexp.marked = false;
    sexp.type = kNilSExp;
    SExpVector_push_back(&self->objs, sexp);
    self->nil = (SExpRef){i}; i++;

    sexp.type = kEnvSExp;
    sexp.env.parent= self->nil;
    sexp.env.bindings = self->nil;
    SExpVector_push_back(&self->objs, sexp);
    self->top_level = (SExpRef){i}; i++;

    sexp.type = kBooleanSExp;
    sexp.boolean = true;
    SExpVector_push_back(&self->objs, sexp);
    self->t= (SExpRef){i}; i++;

    sexp.type = kBooleanSExp;
    sexp.boolean = false;
    SExpVector_push_back(&self->objs, sexp);
    self->f = (SExpRef){i}; i++;

    sexp.type = kEmptySExp;
    for (; i < 1024; i++) {
        SExpVector_push_back(&self->objs, sexp);
        IntVector_push_back(&self->empty_space, i);
    }

    self->stack = lisp_cons(self, self->top_level, self->nil);
    self->reg = self->nil;

    Interp_add_primitive(self, "eval", primitive_eval);
    Interp_add_primitive(self, "apply", primitive_apply);
    Interp_add_primitive(self, "if", primitive_if);
    Interp_add_primitive(self, "cond", primitive_cond);
    Interp_add_primitive(self, "while", primitive_while);
    Interp_add_primitive(self, "progn", primitive_progn);
    Interp_add_primitive(self, "and", primitive_and);
    Interp_add_primitive(self, "or", primitive_or);
    Interp_add_primitive(self, "let", primitive_let);
    Interp_add_primitive(self, "setq", primitive_setq);
    Interp_add_primitive(self, "lambda", primitive_lambda);
    Interp_add_primitive(self, "function", primitive_function);
    Interp_add_primitive(self, "defun", primitive_defun);
    Interp_add_primitive(self, "defvar", primitive_defvar);
    Interp_add_primitive(self, "defmacro", primitive_defmacro);
    Interp_add_primitive(self, "funcall", primitive_funcall);
    Interp_add_primitive(self, "quote", primitive_quote);
    Interp_add_primitive(self, "quasiquote", primitive_quasi);
    Interp_add_primitive(self, "macroexpand-1", primitive_macroexpand1);
    Interp_add_primitive(self, "return", primitive_return);
    Interp_add_primitive(self, "break", primitive_break);
    Interp_add_primitive(self, "continue", primitive_continue);
    Interp_add_primitive(self, "assert", primitive_assert);
    Interp_add_primitive(self, "assert-error", primitive_assert_error);
    Interp_add_primitive(self, "load", primitive_load);

    Interp_add_userfunc(self, "symbol->string", builtin_symbol2string);
    Interp_add_userfunc(self, "intern", builtin_intern);
    Interp_add_userfunc(self, "gensym", builtin_gensym);
    Interp_add_userfunc(self, "float", builtin_float);
    Interp_add_userfunc(self, "tan", builtin_tan);
    Interp_add_userfunc(self, "asin", builtin_asin);
    Interp_add_userfunc(self, "acos", builtin_acos);
    Interp_add_userfunc(self, "log2", builtin_log2);
    Interp_add_userfunc(self, "pow", builtin_pow);
    Interp_add_userfunc(self, "expt", builtin_pow);
    Interp_add_userfunc(self, "exp", builtin_exp);
    Interp_add_userfunc(self, "sqrt", builtin_sqrt);
    Interp_add_userfunc(self, "cbrt", builtin_cbrt);
    Interp_add_userfunc(self, "log10", builtin_log10);
    Interp_add_userfunc(self, "eq", builtin_eq);
    Interp_add_userfunc(self, "ln", builtin_ln);
    Interp_add_userfunc(self, "=", builtin_num_equal);
    Interp_add_userfunc(self, "/=", builtin_num_neq);
    Interp_add_userfunc(self, "concat", builtin_concat);
    Interp_add_userfunc(self, "string", builtin_string);
    Interp_add_userfunc(self, "string=", builtin_string_eq);
    Interp_add_userfunc(self, "string>=", builtin_string_ge);
    Interp_add_userfunc(self, "string<=", builtin_string_le);
    Interp_add_userfunc(self, "string>", builtin_string_gt);
    Interp_add_userfunc(self, "string<", builtin_string_lt);
    Interp_add_userfunc(self, "string/=", builtin_string_neq);
    Interp_add_userfunc(self, "split-string", builtin_split_string);
    Interp_add_userfunc(self, "strip-string", builtin_strip_string);
    Interp_add_userfunc(self, "print", builtin_print);
    Interp_add_userfunc(self, "format", builtin_format);
    Interp_add_userfunc(self, "truncate", builtin_truncate);
    Interp_add_userfunc(self, "mod", builtin_mod);
    Interp_add_userfunc(self, "+", builtin_add);
    Interp_add_userfunc(self, "-", builtin_sub);
    Interp_add_userfunc(self, "*", builtin_mul);
    Interp_add_userfunc(self, "/", builtin_div);
    Interp_add_userfunc(self, "i/", builtin_idiv);
    Interp_add_userfunc(self, ">", builtin_gt);
    Interp_add_userfunc(self, "<", builtin_lt);
    Interp_add_userfunc(self, ">=", builtin_ge);
    Interp_add_userfunc(self, "<=", builtin_le);
    Interp_add_userfunc(self, "abs", builtin_abs);
    Interp_add_userfunc(self, "list", builtin_list);
    Interp_add_userfunc(self, "car", builtin_car);
    Interp_add_userfunc(self, "sin", builtin_sin);
    Interp_add_userfunc(self, "max", builtin_max);
    Interp_add_userfunc(self, "exit", builtin_exit);
    Interp_add_userfunc(self, "not", builtin_not);
    Interp_add_userfunc(self, "cos", builtin_cos);
    Interp_add_userfunc(self, "princ", builtin_princ);
    Interp_add_userfunc(self, "equal", builtin_equal);
    Interp_add_userfunc(self, "atan", builtin_atan);
    Interp_add_userfunc(self, "cons", builtin_cons);
    Interp_add_userfunc(self, "cdr", builtin_cdr);
    Interp_add_userfunc(self, "ceiling", builtin_ceiling);
    Interp_add_userfunc(self, "round", builtin_round);
    Interp_add_userfunc(self, "floor", builtin_floor);
    Interp_add_userfunc(self, "min", builtin_min);
    Interp_add_userfunc(self, "error", builtin_error);


    Interp_add_userfunc(self, "_gcstat", builtin_gcstat);
    Interp_add_userfunc(self, "_alwaysgc", builtin_alwaysgc);

    SExpRef ret = Interp_eval_string(self, bamboo_lisp_prelude);
    Interp *interp = self;
    if (VALTYPE(ret) == kErrSignal) {
        fprintf(stderr, "Failed to load prelude: %s", REF(ret)->str);
    }   
}

SExpRef Interp_eval_string(Interp *interp, const char * str) {
    Parser_set_string(interp->parser, str);
    SExpRef sexp, ret;
    ParseResult parse_result;
    while (1) {
        parse_result = parse_sexp(interp->parser);
        if (parse_result.errmsg != NULL) {
            ret = new_error(interp, "Parsing error: %s", parse_result.errmsg);
            goto end;
        }
        ret = lisp_eval(interp, parse_result.val, false);
        if (Interp_ref(interp, ret)->type == kErrSignal) {
            goto end;
        }
        if (Interp_ref(interp, ret)->type == kBreakSignal
                || Interp_ref(interp, ret)->type == kContinueSignal
                || Interp_ref(interp, ret)->type == kReturnSignal) {
            ret = new_error(interp, "Eval error: unexpected control flow signal.\n");
            goto end;
        }
        if (Parser_is_end(interp->parser)) goto end;
    }
end:
    return ret;
}

SExpRef Interp_load_file(Interp *interp, const char *filename) {
    FILE *fp = NULL;
    fp = fopen(filename, "r");
    if (fp == NULL) {
        return new_error(interp, "Failed to open file: %s\n", filename);
        goto end;
    }
    Parser_set_file(interp->parser, fp);
    SExpRef sexp, ret;
    ParseResult parse_result;
    while (1) {
        parse_result = parse_sexp(interp->parser);
        if (parse_result.errmsg != NULL) {
            ret = new_error(interp, "Parsing error: %s", parse_result.errmsg);
            goto end;
        }
        ret = lisp_eval(interp, parse_result.val, false);
        if (Interp_ref(interp, ret)->type == kErrSignal) {
            goto end;
        }
        if (Interp_ref(interp, ret)->type == kBreakSignal
                || Interp_ref(interp, ret)->type == kContinueSignal
                || Interp_ref(interp, ret)->type == kReturnSignal) {
            ret = new_error(interp, "Eval error: unexpected control flow signal.\n");
            goto end;
        }
        if (Parser_is_end(interp->parser)) goto end;
    }
end:
    fclose(fp);
    return ret;
}

void Interp_add_userfunc(Interp *interp, const char *name, LispUserFunc fn) {
    SExpRef userfunc = new_userfunc(interp, fn);
    SExpRef sym = new_symbol(interp, name);
    lisp_defun(interp, sym, userfunc);
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
    TopBindingVector_free(&self->topbindings);
    free(self->errmsg_buf);
    Parser_free(self->parser);
    free(self->parser);
}

SExp* Interp_ref(Interp *self, SExpRef ref) {
    if (ref.idx > SExpVector_len(&self->objs)) return NULL;
    SExp *res = SExpVector_ref(&self->objs, ref.idx);
    return res;
}

void Interp_add_primitive(Interp *self, const char *name, LispPrimitive fn) {
    SExpRef sym = new_symbol(self, name);
    SExpRef prim = new_primitive(self, fn);
    lisp_defun(self, sym, prim);
}

void Interp_gc(Interp *interp, SExpRef tmproot) {
    int freesize = IntVector_len(&interp->empty_space);
    int heapsize = SExpVector_len(&interp->objs);
    if (freesize > (heapsize >> 4) && !interp->alwaysgc) {
        return;
    }
    SExpRefVector gcstack;
    SExpRefVector_init(&gcstack);
    // add root
    SExpRefVector_push_back(&gcstack, tmproot);
    SExpRefVector_push_back(&gcstack, interp->nil);
    SExpRefVector_push_back(&gcstack, interp->t);
    SExpRefVector_push_back(&gcstack, interp->f);
    SExpRefVector_push_back(&gcstack, interp->stack);
    SExpRefVector_push_back(&gcstack, interp->top_level);
    SExpRefVector_push_back(&gcstack, interp->reg);
    // mark
    while (!SExpRefVector_empty(&gcstack)) {
        SExpRef ref = *SExpRefVector_last(&gcstack);
        SExpRef child;
        SExpRefVector_pop(&gcstack);
        if (ref.idx < 0) continue;
        SExp *obj = REF(ref);
        if (obj->marked) continue;
        obj->marked = true;
        if (obj->type == kPairSExp) {
            child = obj->pair.car;
            if (child.idx >= 0 && !REF(child)->marked) SExpRefVector_push_back(&gcstack, child);
            child = obj->pair.cdr;
            if (child.idx >= 0 && !REF(child)->marked) SExpRefVector_push_back(&gcstack, child);
        } else if (obj->type == kFuncSExp) {
            child = obj->func.args;
            if (child.idx >= 0 && !REF(child)->marked) SExpRefVector_push_back(&gcstack, child);
            child = obj->func.body;
            if (child.idx >= 0 && !REF(child)->marked) SExpRefVector_push_back(&gcstack, child);
            child = obj->func.env;
            if (child.idx >= 0 && !REF(child)->marked) SExpRefVector_push_back(&gcstack, child);
        } else if (obj->type == kEnvSExp) {
            child = obj->env.bindings;
            if (child.idx >= 0 && !REF(child)->marked) SExpRefVector_push_back(&gcstack, child);
            child = obj->env.parent;
            if (child.idx >= 0 && !REF(child)->marked) SExpRefVector_push_back(&gcstack, child);
        } else if (obj->type == kBindingSExp) {
            child = obj->binding.name;
            if (child.idx >= 0 && !REF(child)->marked) SExpRefVector_push_back(&gcstack, child);
            child = obj->binding.value;
            if (child.idx >= 0 && !REF(child)->marked) SExpRefVector_push_back(&gcstack, child);
            child = obj->binding.func;
            if (child.idx >= 0 && !REF(child)->marked) SExpRefVector_push_back(&gcstack, child);
            child = obj->binding.next;
            if (child.idx >= 0 && !REF(child)->marked) SExpRefVector_push_back(&gcstack, child);
        } else if (obj->type == kMacroSExp) {
            child = obj->macro.args;
            if (child.idx >= 0 && !REF(child)->marked) SExpRefVector_push_back(&gcstack, child);
            child = obj->macro.body;
            if (child.idx >= 0 && !REF(child)->marked) SExpRefVector_push_back(&gcstack, child);
        } else if (obj->type == kReturnSignal) {
            child = obj->ret;
            if (child.idx >= 0 && !REF(child)->marked) SExpRefVector_push_back(&gcstack, child);
        } else if (obj->type == kTailcallSExp) {
            child = obj->tailcall.args;
            if (child.idx >= 0 && !REF(child)->marked) SExpRefVector_push_back(&gcstack, child);
            child = obj->tailcall.fn;
            if (child.idx >= 0 && !REF(child)->marked) SExpRefVector_push_back(&gcstack, child);
        }
    }
    SExpRefVector_free(&gcstack);
    // sweep
    for (int i = 0; i < SExpVector_len(&interp->objs); i++) {
        SExp *obj = SExpVector_ref(&interp->objs, i);
        if (obj->marked) {
            obj->marked = false;
            continue;
        }
        if (obj->type == kSymbolSExp) continue;
        if (obj->type == kEmptySExp) continue;
        if (obj->type == kStringSExp) free((void*)obj->str);
        obj->type = kEmptySExp;
        IntVector_push_back(&interp->empty_space, i);
    }
    // enlarge heap
    heapsize = SExpVector_len(&interp->objs);
    int usedsize = heapsize - IntVector_len(&interp->empty_space);
    if (heapsize < usedsize * 4) {
        SExp sexp;
        sexp.marked = false;
        sexp.type = kEmptySExp;
        while (SExpVector_len(&interp->objs) < usedsize * 4) {
            SExpVector_push_back(&interp->objs, sexp);
            IntVector_push_back(&interp->empty_space, SExpVector_len(&interp->objs) - 1);
        }
    }        
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
    } else if (pe->type == kUserFuncSExp) {
        str_builder_append(sb, "<FUNCTION>");
    } else if (pe->type == kMacroSExp) {
        str_builder_append(sb, "<MACRO>");
    } else if (pe->type == kEnvSExp) {
        str_builder_append(sb, "<ENV>");
    } else if (pe->type == kBindingSExp) {
        str_builder_append(sb, "<BINDING>");
    } else if (pe->type == kNilSExp) {
        str_builder_append(sb, "()");
    } else if (pe->type == kErrSignal) {
        str_builder_append(sb, "<ERROR>");
    } else if (pe->type == kReturnSignal) {
        str_builder_append(sb, "<RETURN>");
    } else if (pe->type == kBreakSignal) {
        str_builder_append(sb, "<BREAK>");
    } else if (pe->type == kContinueSignal) {
        str_builder_append(sb, "<CONTINUE>");
    } else if (pe->type == kTailcallSExp) {
        str_builder_append(sb, "<TAILCALL>");
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
            } else if (REF(cur)->type != kPairSExp) {
                str_builder_append(sb, ". ");
                lisp_to_string_impl(sb, visited, interp, cur);
                str_builder_append(sb, ")");
            } else {
                str_builder_append(sb, "<%d>)", cur.idx);
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
    str_builder_append_char(&sb, '\0');
    Int2IntHashTable_free(&visited);
    return sb.buf;
}

SExpRef lisp_macroexpand1(Interp *interp, SExpRef macro, SExpRef args) {
    SExpRef fn = new_lambda(interp, REF(macro)->macro.args, REF(macro)->macro.body, interp->top_level);
    PUSH_REG(fn);
    SExpRef ret = lisp_apply(interp, fn, args, false);
    POP_REG();
    return ret;
error:
    return new_error(interp, "macroexpand: syntax error.\n");
}

void lisp_defun(Interp *interp, SExpRef name, SExpRef val) {
    SExpRef binding = REF(interp->top_level)->env.bindings;
    while (REF(binding)->type != kNilSExp) {
        if (name.idx == REF(binding)->binding.name.idx) {
            REF(binding)->binding.func = val;
            return;
        }
        binding = REF(binding)->binding.next;
    }
    binding = REF(interp->top_level)->env.bindings;
    SExpRef newbinding = new_binding(interp, name, NIL);
    REF(newbinding)->binding.func = val;
    REF(newbinding)->binding.value = UNBOUND;
    REF(newbinding)->binding.next = binding;
    REF(interp->top_level)->env.bindings = newbinding;
    TopBindingVector_push_back(&interp->topbindings, (TopBinding){name, newbinding});
}

void lisp_defvar(Interp *interp, SExpRef name, SExpRef val) {
    SExpRef binding = REF(interp->top_level)->env.bindings;
    while (REF(binding)->type != kNilSExp) {
        if (name.idx == REF(binding)->binding.name.idx) {
            REF(binding)->binding.value = val;
            return;
        }
        binding = REF(binding)->binding.next;
    }
    binding = REF(interp->top_level)->env.bindings;
    SExpRef newbinding = new_binding(interp, name, NIL);
    REF(newbinding)->binding.func = UNBOUND;
    REF(newbinding)->binding.value = val;
    REF(newbinding)->binding.next = binding;
    REF(interp->top_level)->env.bindings = newbinding;
    TopBindingVector_push_back(&interp->topbindings, (TopBinding){name, newbinding});
}

SExpRef lisp_setq(Interp *interp, SExpRef name, SExpRef val) {
    SExpRef env = CAR(interp->stack);
    while (REF(env)->type != kNilSExp) {
        SExpRef binding = REF(env)->env.bindings;
        while (REF(binding)->type != kNilSExp) {
            if (name.idx == REF(binding)->binding.name.idx) {
                REF(binding)->binding.value = val;
                return val;
            }
            binding = REF(binding)->binding.next;
        }
        env = REF(env)->env.parent;
    }
    return new_error(interp, "Unbound variable: %s.\n", REF(name)->str);
}

SExpRef lisp_lookup_topvar(Interp *interp, SExpRef name);

SExpRef lisp_lookup(Interp *interp, SExpRef name) {
    SExpRef env = CAR(interp->stack);
    while (REF(env)->type != kNilSExp) {
        if (env.idx == interp->top_level.idx) {
            return lisp_lookup_topvar(interp, name);
        }
        SExpRef binding = REF(env)->env.bindings;
        while (REF(binding)->type != kNilSExp) {
            if (name.idx == REF(binding)->binding.name.idx) {
                SExpRef ret = REF(binding)->binding.value;
                if (ret.idx < 0) goto notfound;
                return ret;
            }
            binding = REF(binding)->binding.next;
        }
        env = REF(env)->env.parent;
    }
notfound:
    return new_error(interp, "Unbound variable: %s.\n", REF(name)->str);
}

void lisp_print(Interp *interp, SExpRef obj, FILE *fp) {
    const char *str = lisp_to_string(interp, obj);
    fprintf(fp, "%s\n", str);
    free((void*)str);
}

SExpRef lisp_lookup_topvar(Interp *interp, SExpRef name) {
    int topbindings_len = TopBindingVector_len(&interp->topbindings);
    for (int i = 0; i < topbindings_len; i++) {
        TopBinding topbinding = interp->topbindings.buffer[i];
        if (topbinding.name.idx == name.idx) {
            SExpRef ret = REF(topbinding.binding)->binding.value;
            if (ret.idx < 0) goto notfound;
            return ret;
        }
    }
notfound:
    return new_error(interp, "Unbound variable: %s.\n", REF(name)->str);
}

SExpRef lisp_lookup_func(Interp *interp, SExpRef name) {
    int topbindings_len = TopBindingVector_len(&interp->topbindings);
    for (int i = 0; i < topbindings_len; i++) {
        TopBinding topbinding = interp->topbindings.buffer[i];
        if (topbinding.name.idx == name.idx) {
            SExpRef ret = REF(topbinding.binding)->binding.func;
            if (ret.idx < 0) goto notfound;
            return ret;
        }
    }
notfound:
    return new_error(interp, "Unbound function: %s.\n", REF(name)->str);
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

SExpRef lisp_nreverse(Interp *interp, SExpRef lst) {
    SExpRef prev = NIL;
    SExpRef cur = lst;
    SExpRef next_node;

    while (!NILP(cur)) {
        next_node = CDR(cur);
        REF(cur)->pair.cdr = prev;
        prev = cur;
        cur = next_node;
    }
    return prev;
}

SExpRef lisp_eval_args(Interp *interp, SExpRef args) {
    SExpRef ret = interp->nil;
    SExpRef cur = args;
    SExpRef evalres;

    while (!NILP(cur)) {
        // save ret in register
        PUSH_REG(ret);
        evalres = EVAL(CAR(cur));
        POP_REG();
        if (CTL_FL(evalres)) {
            ret = evalres;
            goto end;
        }
        ret = CONS(evalres, ret);
        cur = CDR(cur);
    }
    ret = lisp_nreverse(interp, ret);
end:
    Interp_gc(interp, ret);
    return ret;
}

int lisp_length(Interp *interp, SExpRef lst) {
    int cnt = 0;
    if (VALTYPE(lst) == kNilSExp) {
        return 0;
    } else if (VALTYPE(lst) == kPairSExp) {
        while (REF(lst)->type == kPairSExp) {
            cnt++;
            lst = CDR(lst);
        }
        return cnt;
    } else if (VALTYPE(lst) == kStringSExp) {
        return strlen(REF(lst)->str);
    } else return -1;
}

static SExpRef build_function_env(Interp *interp, SExpRef func, SExpRef args) {
    SExpRef param = REF(func)->func.args;
    SExpRef iparam = param;
    SExpRef iargs = args;
    SExpRef env = new_env(interp);
    REF(env)->env.parent = REF(func)->func.env;
    while (!NILP(iparam)) {
        if (VALTYPE(iparam) == kSymbolSExp) {
            SExpRef binding = new_binding(interp, iparam, iargs);
            REF(binding)->binding.next = REF(env)->env.bindings;
            REF(env)->env.bindings = binding;
            return env;
        }
        SExpRef name = CAR(iparam);
        if (VALTYPE(name) != kSymbolSExp) {
            return new_error(interp, "function syntax error: parameter must be a symbol.\n");
        }
        if (NILP(iargs)) return new_error(interp, "funcall: wrong argument number.\n");
        SExpRef binding = new_binding(interp, name, CAR(iargs));
        REF(binding)->binding.next = REF(env)->env.bindings;
        REF(env)->env.bindings = binding;
        iargs = CDR(iargs);
        iparam = CDR(iparam);
    }
    if (!NILP(iargs)) return new_error(interp, "funcall: wrong argument number.\n");
    return env;
}

SExpRef lisp_apply(Interp *interp, SExpRef fn, SExpRef args, bool istail) {
    SExpRef exp, env, ret, iter;
    if (istail) return new_tailcall(interp, fn, args);
    if (VALTYPE(fn) == kFuncSExp) {
        env = build_function_env(interp, fn, args);
        if (CTL_FL(env)) return env;
        interp->stack = CONS(env, interp->stack);
        iter = REF(fn)->func.body;
        while (!NILP(iter)) {
            exp = CAR(iter);
            if (NILP(CDR(iter))) {
                ret = lisp_eval(interp, exp, true);
                goto end;
            } else {
                ret = EVAL(exp);
            }
            if (CTL_FL(ret)) goto end;
            iter = CDR(iter);
        }
    } else if (VALTYPE(fn) == kUserFuncSExp) {
        LispUserFunc  fnptr = REF(fn)->userfunc;
        PUSH_REG(args);
        ret = (*fnptr)(interp, args);
        POP_REG();
        return ret;
    }
end:
    if (VALTYPE(ret) == kBreakSignal || VALTYPE(ret) == kContinueSignal) {
        ret = new_error(interp, "function call: unexpected control flow signal.\n");
    }
    if (VALTYPE(ret) == kReturnSignal) {
        ret = REF(ret)->ret;
    }
    interp->stack = CDR(interp->stack);
    return ret;
error:
    return new_error(interp, "function call: syntax error.\n");
}

SExpRef lisp_eval(Interp *interp, SExpRef sexp, bool istail) {
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
            || type == kErrSignal
            || type == kBreakSignal
            || type == kContinueSignal
            || type == kReturnSignal
            || type == kTailcallSExp
            || type == kFuncSExp
            || type == kUserFuncSExp
            || type == kRealSExp) {
        ret = sexp;
        goto end;
    }
    if (type == kSymbolSExp) {
        ret = lisp_lookup(interp, sexp);
        goto end;
    }
    SExpRef fn, funcallargs, args;
    if (type == kPairSExp) {
        if (!lisp_check_list(interp, sexp)) {
            ret = new_error(interp, "eval: list not proper.\n");
            goto end;
        }
        if (REF(CAR(sexp))->type != kSymbolSExp) {
            ret = new_error(interp, "eval: first elem must be a symbol.\n");
            goto end;
        }
        SExpRef symbol = CAR(sexp);
        fn = lisp_lookup_func(interp, symbol);
        if (CTL_FL(fn)) {
            ret = new_error(interp, "eval: \"%s\" is not a primitive, function, "
                    "or macro.\n", REF(symbol)->str);
            goto end;
        }
        if (VALTYPE(fn) == kPrimitiveSExp) {
            LispPrimitive primitive_fn = REF(fn)->primitive;
            ret = (*primitive_fn)(interp, CDR(sexp), istail);
            if (VALTYPE(ret) == kTailcallSExp && !istail) {
                    fn = REF(ret)->tailcall.fn;
                    args = REF(ret)->tailcall.args;
                    goto tailcall;
            }
            goto end;
        } else if (VALTYPE(fn) == kFuncSExp || VALTYPE(fn) == kUserFuncSExp) {
            args = CDR(sexp);
            funcallargs = CONS(fn, args);
            PUSH_REG(funcallargs);
            ret = primitive_funcall(interp, funcallargs, istail);
            POP_REG();
            if (VALTYPE(ret) == kTailcallSExp && !istail) {
                fn = REF(ret)->tailcall.fn;
                args = REF(ret)->tailcall.args;
                goto tailcall;
            }
            goto end;
        } else if (VALTYPE(fn) == kMacroSExp) {
            SExpRef args = CDR(sexp);
            SExpRef newast = lisp_macroexpand1(interp, fn, args);
            PUSH_REG(newast);
            ret = lisp_eval(interp, newast, istail);
            POP_REG();
            goto end;
        } else {
            return new_error(interp,
                    "eval: fatal binding eval, %s is not a func, prim "
                    "or macro.\n", REF(symbol)->str);
        }
    }
    ret = new_error(interp, "eval: unknown syntax.\n");
end:
    POP_REG();
    Interp_gc(interp, ret);
    return ret;
tailcall:
    while (1) {
        PUSH_REG(CONS(fn, args));
        ret = lisp_apply(interp, fn, args, false);
        POP_REG();
        if (VALTYPE(ret) != kTailcallSExp) break;
        fn = REF(ret)->tailcall.fn;
        args = REF(ret)->tailcall.args;
    }
    goto end;
}

SExpRef new_sexp(Interp *interp) {
    if (IntVector_len(&interp->empty_space) == 0) {
        SExp sexp;
        sexp.type = kEmptySExp;
        sexp.marked = false;
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

SExpRef new_tailcall(Interp *interp, SExpRef fn, SExpRef args) {
    SExpRef ret = new_sexp(interp);
    REF(ret)->type = kTailcallSExp;
    REF(ret)->tailcall.fn = fn;
    REF(ret)->tailcall.args= args;
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

SExpRef new_macro(Interp *interp, SExpRef param, SExpRef body) {
    SExpRef ret = new_sexp(interp);
    REF(ret)->type = kMacroSExp;
    REF(ret)->macro.args = param;
    REF(ret)->macro.body = body;
    return ret;
}

SExpRef new_binding(Interp *interp, SExpRef sym, SExpRef val) {
    SExpRef ret = new_sexp(interp);
    REF(ret)->type = kBindingSExp;
    REF(ret)->binding.name = sym;
    REF(ret)->binding.value = val;
    REF(ret)->binding.func = UNBOUND;
    REF(ret)->binding.next = NIL;
    return ret;
}

SExpRef new_boolean(Interp *interp, bool val) {
    if (val) return interp->t;
    return interp->f;
}

SExpRef new_error(Interp *interp, const char *format, ...) {
    va_list args;
    va_start(args, format);
    vsnprintf(interp->errmsg_buf, BUFSIZE, format, args);
    va_end(args);
    SExpRef ret = new_sexp(interp);
    REF(ret)->type = kErrSignal;
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

SExpRef new_return(Interp *interp, SExpRef obj) {
    SExpRef ret = new_sexp(interp);
    SExp *psexp = Interp_ref(interp, ret);
    psexp->type = kReturnSignal;
    psexp->ret = obj;
    return ret;
}

SExpRef new_break(Interp *interp) {
    SExpRef ret = new_sexp(interp);
    SExp *psexp = Interp_ref(interp, ret);
    psexp->type = kBreakSignal;
    return ret;
}

SExpRef new_continue(Interp *interp) {
    SExpRef ret = new_sexp(interp);
    SExp *psexp = Interp_ref(interp, ret);
    psexp->type = kContinueSignal;
    return ret;
}

SExpRef new_primitive(Interp *interp, LispPrimitive val) {
    SExpRef ret = new_sexp(interp);
    REF(ret)->type = kPrimitiveSExp;
    REF(ret)->primitive = val;
    return ret;
}

