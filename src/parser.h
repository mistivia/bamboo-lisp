#ifndef BAMBOO_LISP_PARSER_H_
#define BAMBOO_LISP_PARSER_H_

#include <stdbool.h>

#include "sexp.h"

typedef struct {

} Parser;

typedef struct {
    SExpRef val;
    const char *errmsg;
} ParseResult;

int parser_getchar(Parser *ctx);
int parser_peek(Parser *ctx);

ParseResult parse_sexp(Parser *ctx);
ParseResult parse_list(Parser *ctx);
ParseResult parse_quote(Parser *ctx);
ParseResult parse_unquote(Parser *ctx);
ParseResult parse_slicing_unquote(Parser *ctx);
ParseResult parse_quasi(Parser *ctx);
ParseResult parse_atom(Parser *ctx);
ParseResult parse_number(Parser *ctx);
ParseResult parse_integer(Parser *ctx);
ParseResult parse_real(Parser *ctx);
ParseResult parse_symbol(Parser *ctx);
ParseResult parse_string(Parser *ctx);
ParseResult parse_char(Parser *ctx);

#endif

