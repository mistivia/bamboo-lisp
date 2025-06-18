#ifndef BAMBOO_LISP_PARSER_H_
#define BAMBOO_LISP_PARSER_H_

#include <stdbool.h>

#include "interp.h"
#include "sexp.h"

typedef enum {
    kParseString,
    kParseFile,
} ParseType;

typedef struct {
    Interp *ctx;
    char *errmsg_buf;
    char *token_buf;
    
    ParseType parse_type;
    union {
        struct {
            const char *string;
            const char *str_cursor;
        };
        FILE *fp;
    };
} Parser;

void Parser_init(Parser *self);
void Parser_free(Parser *self);
int Parser_getchar(Parser *self);
int Parser_peek(Parser *self);
void Parser_set_string(Parser *parser, const char *str);
void Parser_set_file(Parser *parser, FILE *fp);

typedef struct {
    SExpRef val;
    const char *errmsg;
} ParseResult;

ParseResult ParseOk(SExpRef ref);
ParseResult ParseErr(Parser *parser, const char *format, ...);
bool ParseResult_is_err(ParseResult res);


ParseResult parse_sexp(Parser *parser);
ParseResult parse_list(Parser *parser);
ParseResult parse_quote(Parser *parser);
ParseResult parse_unquote(Parser *parser);
ParseResult parse_slicing_unquote(Parser *parser);
ParseResult parse_quasi(Parser *parser);
ParseResult parse_atom(Parser *parser);
ParseResult parse_number(Parser *parser);
ParseResult parse_integer(Parser *parser);
ParseResult parse_real(Parser *parser);
ParseResult parse_symbol(Parser *parser);
ParseResult parse_string(Parser *parser);
ParseResult parse_char(Parser *parser);

#endif

