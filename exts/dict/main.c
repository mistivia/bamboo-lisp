#include <string.h>
#include <stdlib.h>

#include <algds/tree_map.h>
#include <algds/basic_traits.h>
#include <bamboo_lisp/interp.h>
#include <bamboo_lisp/sexp.h>

TREE_MAP_DEF(String, SExpRef);
TREE_MAP_IMPL(String, SExpRef);

typedef String2SExpRefTreeMap DictMap;
typedef String2SExpRefTreeMapIter DictIter;
typedef String2SExpRefTreeMapNode DictNode;

LispUserdataMeta bamboo_lisp_dict_meta;

#define DICT_TYPEID "ext.core.dict"

static bool is_dict_impl(Interp *interp, SExpRef obj) {
    if (VALTYPE(obj) == kUserDataSExp && strcmp(DICT_TYPEID, REF(obj)->userdata_meta->type) == 0) {
        return true;
    }
    return false;
}

static void dict_free(void *vself) {
    DictMap *map = (DictMap *)vself;
    
    DictIter it = String2SExpRefTreeMap_min(map);
    while (it != NULL) {
        if (it->key) {
            free((void*)it->key);
            it->key = NULL;
        }
        it = String2SExpRefTreeMap_next(map, it);
    }

    String2SExpRefTreeMap_free(map);

    free(map);
}


static void dict_gcmark(Interp *interp, SExpPtrVector *gcstack, void *vself) {
    DictMap *map = (DictMap *)vself;
    
    DictIter it = String2SExpRefTreeMap_min(map);
    while (it != NULL) {
        SExpRef val = it->value;
        SExpPtr ptr = REF(val);

        if (ptr && !ptr->marked) {
            SExpPtrVector_push_back(gcstack, ptr);
        }
        
        it = String2SExpRefTreeMap_next(map, it);
    }
}


static SExpRef lisp_is_dict(Interp* interp, SExpRef args) {
    if (LENGTH(args) != 1) return new_error(interp, "dict?: wrongs args num.\n");
    return new_boolean(interp, is_dict_impl(interp, CAR(args)));
}

// (make-dict)
static SExpRef lisp_make_dict(Interp* interp, SExpRef args) {
    if (LENGTH(args) != 0) return new_error(interp, "make-dict: expects no args.\n");

    SExpRef ret = new_sexp(interp);
    REF(ret)->type = kUserDataSExp;
    REF(ret)->userdata_meta = &bamboo_lisp_dict_meta;
    
    DictMap *map = malloc(sizeof(DictMap));
    if (!map) return new_error(interp, "make-dict: out of memory.\n");
    
    String2SExpRefTreeMap_init(map);
    REF(ret)->userdata = map;
    
    return ret;
}

// (dict-set dict key value)
static SExpRef lisp_dict_set(Interp* interp, SExpRef args) {
    if (LENGTH(args) != 3) return new_error(interp, "dict-set: wrong args num.\n");
    
    SExpRef r_dict = CAR(args);
    SExpRef r_key  = CADR(args);
    SExpRef r_val  = CADDR(args);

    if (!is_dict_impl(interp, r_dict)) return new_error(interp, "dict-set: first arg not a dict.\n");
    if (REF(r_key)->type != kStringSExp) return new_error(interp, "dict-set: key must be a string.\n");

    DictMap *map = REF(r_dict)->userdata;
    const char *key_str = REF(r_key)->str;

    DictIter it = String2SExpRefTreeMap_find(map, key_str);
    if (it != NULL) {
        it->value = r_val;
    } else {
        char *key_dup = strdup(key_str);
        String2SExpRefTreeMap_insert(map, key_dup, r_val);
    }

    return NIL;
}

// (dict-get dict key) -> value or nil
static SExpRef lisp_dict_get(Interp* interp, SExpRef args) {
    if (LENGTH(args) != 2) return new_error(interp, "dict-get: wrong args num.\n");

    SExpRef r_dict = CAR(args);
    SExpRef r_key  = CADR(args);

    if (!is_dict_impl(interp, r_dict)) return new_error(interp, "dict-get: first arg not a dict.\n");
    if (REF(r_key)->type != kStringSExp) return new_error(interp, "dict-get: key must be a string.\n");

    DictMap *map = REF(r_dict)->userdata;
    const char *key_str = REF(r_key)->str;

    SExpRef *val_ptr = String2SExpRefTreeMap_get(map, key_str);
    if (val_ptr == NULL) {
        return NIL;
    }
    return *val_ptr;
}

// (dict-remove dict key)
static SExpRef lisp_dict_remove(Interp* interp, SExpRef args) {
    if (LENGTH(args) != 2) return new_error(interp, "dict-remove: wrong args num.\n");

    SExpRef r_dict = CAR(args);
    SExpRef r_key  = CADR(args);

    if (!is_dict_impl(interp, r_dict)) return new_error(interp, "dict-remove: first arg not a dict.\n");
    if (REF(r_key)->type != kStringSExp) return new_error(interp, "dict-remove: key must be a string.\n");

    DictMap *map = REF(r_dict)->userdata;
    const char *key_str = REF(r_key)->str;

    DictIter it = String2SExpRefTreeMap_find(map, key_str);
    if (it != NULL) {
        const char *owned_key = it->key;
        String2SExpRefTreeMap_remove(map, it);
        if (owned_key) free((void*)owned_key);
        free(it);
    }

    return NIL;
}

int bamboo_lisp_dict_ext_init(Interp *interp) {
    bamboo_lisp_dict_meta.type = DICT_TYPEID;
    bamboo_lisp_dict_meta.free = &dict_free;
    bamboo_lisp_dict_meta.gcmark = &dict_gcmark;

    Interp_add_userfunc(interp, "dict?", &lisp_is_dict);
    Interp_add_userfunc(interp, "make-dict", &lisp_make_dict);
    Interp_add_userfunc(interp, "dict-get", &lisp_dict_get);
    Interp_add_userfunc(interp, "dict-set", &lisp_dict_set);
    Interp_add_userfunc(interp, "dict-remove", &lisp_dict_remove);
    // TODO dict-keys
    return 1;
}