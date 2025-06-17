#include "parser.h"

#include <ctype.h>
#include <stdlib.h>

static void skip_spaces(Parser *ctx) {
    while (isspace(parser_peek(ctx))) {
        parser_getchar(ctx);
    }
}

ParseResult ParseOk(SExpRef ref) {
    return (ParseResult){ .val = ref, .errmsg = NULL };
}

ParseResult ParseErr(const char *msg) {
    return (ParseResult){ .val = {-1}, .errmsg = msg };
}

ParseResult parse_sexp(Parser *ctx) {
    skip_spaces(ctx);
    int next = parser_peek(ctx);
    if (next == '(') {
        return parse_list(ctx);
    } else if (next == ',') {
        parser_getchar(ctx);
        if (parser_peek(ctx) == '@') {
            return parse_slicing_unquote(ctx);
        }
        return parse_unquote(ctx);
    } else if (next == '`') {
        return parse_quasi(ctx);
    } else if (next == '\'') {
        return parse_quote(ctx);
    }
    return parse_atom(ctx);
}

