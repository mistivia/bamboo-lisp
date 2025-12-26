#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <bamboo_lisp/interp.h>
#include <bamboo_lisp/sexp.h>

#define STREAM_TYPEID "ext.stream"

typedef struct {
    FILE *fp;
    bool should_close;
} LispStream;

LispUserdataMeta bamboo_lisp_stream_meta;

static bool is_stream_impl(Interp *interp, SExpRef obj) {
    if (VALTYPE(obj) == kUserDataSExp && 
        strcmp(STREAM_TYPEID, REF(obj)->userdata_meta->type) == 0) {
        return true;
    }
    return false;
}

static SExpRef make_stream(Interp* interp, FILE *fp, bool should_close) {
    SExpRef ret = new_sexp(interp);
    REF(ret)->type = kUserDataSExp;
    REF(ret)->userdata_meta = &bamboo_lisp_stream_meta;
    
    LispStream *s = malloc(sizeof(LispStream));
    s->fp = fp;
    s->should_close = should_close;
    
    REF(ret)->userdata = s;
    return ret;
}

static FILE* get_input_stream(Interp* interp, SExpRef args, const char* func_name) {
    if (NILP(args)) return stdin;
    
    SExpRef first = CAR(args);
    if (!is_stream_impl(interp, first)) {
        new_error(interp, "%s: argument is not a stream.\n", func_name);
        return NULL;
    }
    LispStream *s = (LispStream*)REF(first)->userdata;
    return s->fp;
}

static FILE* get_output_stream(Interp* interp, SExpRef args, const char* func_name) {
    if (NILP(args)) return stdout;
    
    SExpRef first = CAR(args);
    if (!is_stream_impl(interp, first)) {
        new_error(interp, "%s: argument is not a stream.\n", func_name);
        return NULL;
    }
    LispStream *s = (LispStream*)REF(first)->userdata;
    return s->fp;
}

// (stream? obj)
static SExpRef ext_is_stream(Interp* interp, SExpRef args) {
    if (LENGTH(args) != 1) return new_error(interp, "stream?: wrong args num.\n");
    return new_boolean(interp, is_stream_impl(interp, CAR(args)));
}

// (open-file filename mode) -> stream
static SExpRef ext_open_file(Interp* interp, SExpRef args) {
    if (LENGTH(args) != 2) return new_error(interp, "open-file: wrong args num.\n");
    
    SExpRef arg_fname = CAR(args);
    SExpRef arg_mode = CADR(args);

    if (VALTYPE(arg_fname) != kStringSExp || VALTYPE(arg_mode) != kStringSExp) {
        return new_error(interp, "open-file: filename and mode must be strings.\n");
    }

    const char *fname = REF(arg_fname)->str;
    const char *mode = REF(arg_mode)->str;

    FILE *fp = fopen(fname, mode);
    if (!fp) {
        return new_error(interp, "open-file: failed to open file '%s'.\n", fname);
    }

    return make_stream(interp, fp, true);
}

// (read-char [stream])
static SExpRef ext_read_char(Interp* interp, SExpRef args) {
    if (LENGTH(args) > 1) return new_error(interp, "read-char: too many args.\n");
    
    FILE *fp = get_input_stream(interp, args, "read-char");
    if (!fp) return NIL; // Error handled in get_input_stream but returns NULL

    int c = fgetc(fp);
    if (c == EOF) return NIL; // End of file returns NIL

    return new_char(interp, (char)c);
}

// (read-integer [stream])
static SExpRef ext_read_integer(Interp* interp, SExpRef args) {
    if (LENGTH(args) > 1) return new_error(interp, "read-integer: too many args.\n");

    FILE *fp = get_input_stream(interp, args, "read-integer");
    if (!fp) return NIL;

    long long val;

    int c;
    while(isspace(c = fgetc(fp)));
    ungetc(c, fp);

    if (fscanf(fp, "%lld", &val) == 1) {
        return new_integer(interp, val);
    }
    return NIL;
}

static SExpRef ext_read_number(Interp* interp, SExpRef args) {
    if (LENGTH(args) > 1) return new_error(interp, "read-number: too many args.\n");

    FILE *fp = get_input_stream(interp, args, "read-number");
    if (!fp) return NIL;

    char buffer[64];
    int idx = 0;
    int c;

    while ((c = fgetc(fp)) != EOF && isspace(c));
    
    if (c == EOF) return NIL;

    do {
        if (idx < 63) buffer[idx++] = (char)c;
        c = fgetc(fp);
    } while (c != EOF && !isspace(c) && c != ')' && c != '(');
    
    if (c != EOF) ungetc(c, fp);
    buffer[idx] = '\0';

    if (strchr(buffer, '.') != NULL) {
        char *end;
        double d = strtod(buffer, &end);
        if (end != buffer) return new_real(interp, d);
    } else {
        char *end;
        long long i = strtoll(buffer, &end, 10);
        if (end != buffer) return new_integer(interp, i);
    }

    return NIL;
}

static char* internal_read_line(FILE *fp) {
    size_t cap = 128;
    size_t len = 0;
    char *buf = malloc(cap);
    if (!buf) return NULL;

    int c = fgetc(fp);
    if (c == EOF) {
        free(buf);
        return NULL; 
    }

    while (c != EOF && c != '\n') {
        if (len + 1 >= cap) {
            cap *= 2;
            char *new_buf = realloc(buf, cap);
            if (!new_buf) { free(buf); return NULL; }
            buf = new_buf;
        }
        buf[len++] = (char)c;
        c = fgetc(fp);
    }
    buf[len] = '\0';
    return buf;
}

// (read-line [stream])
static SExpRef ext_read_line(Interp* interp, SExpRef args) {
    if (LENGTH(args) > 1) return new_error(interp, "read-line: too many args.\n");

    FILE *fp = get_input_stream(interp, args, "read-line");
    if (!fp) return NIL;

    char *line = internal_read_line(fp);
    if (line) {
        SExpRef ret = new_string(interp, line);
        free(line);
        return ret;
    }
    return NIL;
}

// (lines stream) -> list of strings
static SExpRef ext_lines(Interp* interp, SExpRef args) {
    if (LENGTH(args) != 1) return new_error(interp, "lines: wrong args num.\n");
    
    FILE *fp = get_input_stream(interp, args, "lines");
    if (!fp) return NIL;

    SExpRef list_head = NIL;

    char *line_str;
    while ((line_str = internal_read_line(fp)) != NULL) {
        SExpRef s = new_string(interp, line_str);
        free(line_str);
        list_head = CONS(s, list_head);
    }

    return lisp_nreverse(interp, list_head);
}

static SExpRef ext_stream_close(Interp *interp, SExpRef args) {
    if (LENGTH(args) != 1) return new_error(interp, "stream-close: wrong args num.\n");
    SExpRef first = CAR(args);
    if (!is_stream_impl(interp, first)) {
        return new_error(interp, "stream-close: argument is not a stream.\n");
    }
    LispStream *s = (LispStream*)REF(first)->userdata;
    if (s->should_close && s->fp != NULL) {
        fclose(s->fp);
        s->fp = NULL;
        s->should_close = false;
    }
    return NIL;
}

// (write-char c [stream])
static SExpRef ext_write_char(Interp* interp, SExpRef args) {
    int len = LENGTH(args);
    if (len < 1 || len > 2) return new_error(interp, "write-char: wrong args num.\n");

    if (VALTYPE(CAR(args)) != kCharSExp) 
        return new_error(interp, "write-char: first arg must be char.\n");
    
    char c = REF(CAR(args))->character;
    
    FILE *fp = get_output_stream(interp, CDR(args), "write-char"); // CDR is the rest of args
    if (!fp) return NIL;

    fputc(c, fp);
    return NIL;
}

// (write-obj obj [stream])
static SExpRef ext_write_obj(Interp* interp, SExpRef args) {
    int len = LENGTH(args);
    if (len < 1 || len > 2) return new_error(interp, "write-obj: wrong args num.\n");

    SExpRef obj = CAR(args);
    FILE *fp = get_output_stream(interp, CDR(args), "write-obj");
    if (!fp) return NIL;

    lisp_print(interp, obj, fp);
    return NIL;
}

static void stream_free(void *vself) {
    LispStream *self = (LispStream*)vself;
    if (self->should_close && self->fp) {
        fclose(self->fp);
    }
    free(self);
}

static void stream_gcmark(Interp *interp, SExpPtrVector *gcstack, void *vself) {
    (void)interp;
    (void)gcstack;
    (void)vself;
}

int bamboo_lisp_ext_io_init(Interp *interp) {
    bamboo_lisp_stream_meta.type = STREAM_TYPEID;
    bamboo_lisp_stream_meta.free = &stream_free;
    bamboo_lisp_stream_meta.gcmark = &stream_gcmark;

    Interp_add_userfunc(interp, "stream?", &ext_is_stream);
    Interp_add_userfunc(interp, "open-file", &ext_open_file);
    Interp_add_userfunc(interp, "stream-close", &ext_stream_close);
    Interp_add_userfunc(interp, "read-char", &ext_read_char);
    Interp_add_userfunc(interp, "read-integer", &ext_read_integer);
    Interp_add_userfunc(interp, "read-number", &ext_read_number);
    Interp_add_userfunc(interp, "read-line", &ext_read_line);
    Interp_add_userfunc(interp, "lines", &ext_lines);
    Interp_add_userfunc(interp, "write-char", &ext_write_char);
    Interp_add_userfunc(interp, "write-obj", &ext_write_obj);
    
    return 1;
}