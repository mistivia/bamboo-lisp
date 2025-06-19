#include "parser.h"
#include "sexp.h"

#include <ctype.h>
#include <stdlib.h>
#include <stdarg.h>

#define BUFSIZE 1024

static void skip_spaces(Parser *parser) {
    while (isspace(Parser_peek(parser))) {
        Parser_getchar(parser);
    }
}

ParseResult ParseOk(SExpRef ref) {
    return (ParseResult){ .val = ref, .errmsg = NULL };
}

ParseResult ParseErr(Parser *parser, const char *format, ...) {
    va_list args;
    va_start(args, format);
    vsnprintf(parser->errmsg_buf, BUFSIZE, format, args);
    va_end(args);
    return (ParseResult){ .val = {-1}, .errmsg = parser->errmsg_buf };
}

bool ParseResult_is_err(ParseResult res) {
    if (res.errmsg != NULL) return true;
    return false;
}

void Parser_init(Parser *parser) {
    parser->token_buf = malloc(BUFSIZE);
    parser->errmsg_buf = malloc(BUFSIZE);
}

void Parser_free(Parser *parser) {
    free(parser->token_buf);
    free(parser->errmsg_buf);
}

void Parser_set_string(Parser *parser, const char *str) {
    parser->parse_type = kParseString;
    parser->string = str;
    parser->str_cursor = str;
}

void Parser_set_file(Parser *parser, FILE *fp) {
    parser->parse_type = kParseFile;
    parser->fp = fp;
}

int Parser_getchar(Parser *ctx) {
    if (ctx->parse_type == kParseString) {
        if (*ctx->str_cursor == '\0') return EOF;
        int ret = *ctx->str_cursor;
        ctx->str_cursor++;
        return ret;
    } else if (ctx->parse_type == kParseFile) {
        return fgetc(ctx->fp);
    }
    return EOF;
}

int Parser_peek(Parser *ctx) {
    if (ctx->parse_type == kParseString) {
        if (*ctx->str_cursor == '\0') return EOF;
        int ret = *ctx->str_cursor;
        return ret;
    } else if (ctx->parse_type == kParseFile) {
        int ret = fgetc(ctx->fp);
        if (ret == EOF) return EOF;
        ungetc(ret, ctx->fp);
        return ret;
    }
    return EOF;
}

ParseResult parse_sexp(Parser *parser) {
    skip_spaces(parser);
    if (Parser_peek(parser) == EOF) {
        return ParseErr(parser, "Unexpected EOF.\n");
    }
    int next = Parser_peek(parser);
    if (next == '(') {
        return parse_list(parser);
    } else if (next == ',') {
        Parser_getchar(parser);
        if (Parser_peek(parser) == '@') {
            Parser_getchar(parser);
            return parse_slicing_unquote(parser);
        }
        return parse_unquote(parser);
    } else if (next == '`') {
        Parser_getchar(parser);
        return parse_quasi(parser);
    } else if (next == '\'') {
        Parser_getchar(parser);
        return parse_quote(parser);
    }
    return parse_atom(parser);
}

static ParseResult expect_char(Parser *parser, int chr) {
    if (Parser_peek(parser) == EOF) {
        return ParseErr(parser, "Unexpected EOF.\n");
    }
    if (Parser_peek(parser) == chr) {
        Parser_getchar(parser);
        return ParseOk(parser->ctx->nil);
    }
    return ParseErr(parser, "Unexpected character %c.\n", (char)chr);
}

static ParseResult expect_space(Parser *parser) {
    if (Parser_peek(parser) == EOF) {
        return ParseErr(parser, "Unexpected EOF.\n");
    }
    if (isspace(Parser_peek(parser))) {
        return ParseOk(parser->ctx->nil);
    }
    return ParseErr(parser, "Expect space.\n");
}

static ParseResult expect_space_or_end(Parser *parser) {
    if (Parser_peek(parser) == EOF) {
        return ParseErr(parser, "Unexpected EOF.\n");
    }
    if (isspace(Parser_peek(parser))
            || Parser_peek(parser) == ')') {
        return ParseOk(parser->ctx->nil);
    }
    return ParseErr(parser, "Expect space.\n");
}

static SExpRef build_list_from_vector(Interp *ctx, SExpRefVector elems) {
    int i = SExpRefVector_len(&elems) - 1;
    SExpRef ret = *SExpRefVector_ref(&elems, i);
    i--;
    for (; i >= 0; i--) {
        SExpRef cur = *SExpRefVector_ref(&elems, i);
        ret = lisp_cons(ctx, cur, ret);
    }
    return ret;
}

ParseResult parse_list(Parser *parser) {
    SExpRefVector elems;
    SExpRefVector_init(&elems);
    ParseResult ret;

    ret = expect_char(parser, '(');
    if (ParseResult_is_err(ret)) goto end;
    skip_spaces(parser);
    while (1) {
        if (Parser_peek(parser) == EOF) {
            ret = ParseErr(parser, "Unexpected EOF.\n");
            goto end;
        }
        if (Parser_peek(parser) == ')') {
            Parser_getchar(parser);
            SExpRefVector_push_back(&elems, parser->ctx->nil);
            ret = ParseOk(build_list_from_vector(parser->ctx, elems));
            goto end;
        } else if (Parser_peek(parser) == '.') {
            Parser_getchar(parser);
            break;
        }
        ret = parse_sexp(parser);
        if (ParseResult_is_err(ret)) goto end;
        SExpRefVector_push_back(&elems, ret.val);
        ret = expect_space_or_end(parser);
        if (ParseResult_is_err(ret)) goto end;
        skip_spaces(parser);
    }
    // dot
    ret = expect_space(parser);
    if (ParseResult_is_err(ret)) goto end;
    skip_spaces(parser);
    ret = parse_sexp(parser);
    if (ParseResult_is_err(ret)) goto end;
    SExpRefVector_push_back(&elems, ret.val);
    skip_spaces(parser);
    ret = expect_char(parser, ')');
    if (ParseResult_is_err(ret)) goto end;
    ret = ParseOk(build_list_from_vector(parser->ctx, elems));
end:
    SExpRefVector_free(&elems);
    return ret;
}

static char *read_token(Parser *parser) {
    int i = 0;
    while (!isspace(Parser_peek(parser))
            && Parser_peek(parser) != EOF
            && Parser_peek(parser) != ')'
            && i < BUFSIZE - 1) {
        parser->token_buf[i] = Parser_getchar(parser);
        i++;
    }
    if (i > 1022) return NULL;
    parser->token_buf[i] = '\0';
    return parser->token_buf;
}

static bool is_symbol_init(char c) {
    if (isalpha(c)) return true;
    if (c == '!') return true;
    if (c == '$') return true;
    if (c == '%') return true;
    if (c == '&') return true;
    if (c == '*') return true;
    if (c == '/') return true;
    if (c == ':') return true;
    if (c == '<') return true;
    if (c == '=') return true;
    if (c == '>') return true;
    if (c == '?') return true;
    if (c == '^') return true;
    if (c == '_') return true;
    if (c == '~') return true;
    return false;
}

static bool is_symbol_subsequent(char c) {
    if (is_symbol_init(c)) return true;
    if (isdigit(c)) return true;
    if (c == '+') return true;
    if (c == '-') return true;
    if (c == '.') return true;
    if (c == '@') return true;
    return false;
}

static ParseResult parse_token(Parser *parser, const char *token) {
    int len = strlen(token);    
    if (len == 0) return ParseErr(parser, "Empty token.\n");
    if (len == 1) {
        if (token[0] == '-' || token[0] == '+') {
            return ParseOk(new_symbol(parser->ctx, token));
        }
    }
    if (token[0] == '#') {
        if (len < 2) return ParseErr(parser, "Expect boolean or character.\n");
        if (token[1] == 't') return ParseOk(new_boolean(parser->ctx, true));
        if (token[1] == 'f') return ParseOk(new_boolean(parser->ctx, false));
        if (token[1] == '\\') {
            if (len < 3) return ParseErr(parser, "Expect character.\n");
            if (len == 3) return ParseOk(new_char(parser->ctx, token[2]));
            if (strcmp(token+2, "newline") == 0) return ParseOk(new_char(parser->ctx, '\n'));
            if (strcmp(token+2, "space") == 0) return ParseOk(new_char(parser->ctx, ' '));
            if (strcmp(token+2, "tab") == 0) return ParseOk(new_char(parser->ctx, '\t'));
            if (strcmp(token+2, "return") == 0) return ParseOk(new_char(parser->ctx, '\r'));
            return ParseErr(parser, "Unknown character name: %s.\n", token + 2);
        }
    }
    if (is_symbol_init(token[0])) {
        for (int i = 1; i < len; i++) {
            if (!is_symbol_subsequent(token[i])) {
                return ParseErr(parser, "Not a symbol, containing illegal character: %s\n", token);
            }
        }
        return ParseOk(new_symbol(parser->ctx, token));
    }
    char *endptr;
    int64_t integer = strtoll(token, &endptr, 10);
    if (endptr == token + len) return ParseOk(new_integer(parser->ctx, integer));
    double real = strtod(token, &endptr);
    if (endptr == token + len) return ParseOk(new_real(parser->ctx, real));
    return ParseErr(parser, "Not a number : %s\n.", token);
}

ParseResult parse_string(Parser *parser) {
    ParseResult ret;
    CharVector buf;
    CharVector_init(&buf);
    Parser_getchar(parser);
    while (Parser_peek(parser) != '"') {
        if (Parser_peek(parser) == EOF) {
            return ParseErr(parser, "Unexpected EOF.\n.");
        }
        if (Parser_peek(parser) == '\0') {
            ret = ParseErr(parser, "Unexpected zero terminator.\n");
            goto end;
        }
        if (Parser_peek(parser) != '\\') {
            CharVector_push_back(&buf, Parser_getchar(parser));
        } else {
            Parser_getchar(parser);
            if (Parser_peek(parser) == EOF) {
                return ParseErr(parser, "Unexpected EOF.\n");
            }
            int c = Parser_getchar(parser);
            if (c == EOF) {
                ret = ParseErr(parser, "Unexpected EOF: %c.\n", c);
                goto end;
            } else if (c == '\\')  CharVector_push_back(&buf, '\\');
            else if (c == 't')  CharVector_push_back(&buf, '\t');
            else if (c == 'n')  CharVector_push_back(&buf, '\n');
            else if (c == 'r')  CharVector_push_back(&buf, '\r');
            else if (c == '"')  CharVector_push_back(&buf, '"');
            else {
                ret = ParseErr(parser, "Unexpected escape char: %c.\n", c);
                goto end;
            }
        }
    }
    CharVector_push_back(&buf, '\0');
    ret = ParseOk(new_string(parser->ctx, buf.buffer));
end:
    CharVector_free(&buf);
    return ret;
}

ParseResult parse_atom(Parser *parser) {
    ParseResult ret;
    if (Parser_peek(parser) == EOF) {
        return ParseErr(parser, "Unexpected EOF.\n");
    }
    if (Parser_peek(parser) == '"') return parse_string(parser);
    const char *token = read_token(parser);
    if (token == NULL) return ParseErr(parser, "Token too long.\n");
    return parse_token(parser, token);
}

ParseResult parse_abbrev(Parser *parser, const char *name) {
    if (isspace(Parser_peek(parser))) {
        return ParseErr(parser, "Unexpected space.\n");
    }
    ParseResult ret;
    ret = parse_sexp(parser);
    if (ParseResult_is_err(ret)) return ret;
    SExpRef sym = new_symbol(parser->ctx, name);
    return ParseOk(lisp_cons(parser->ctx, sym, lisp_cons(parser->ctx, ret.val, parser->ctx->nil)));
}

ParseResult parse_quote(Parser *parser) {
    return parse_abbrev(parser, "quote");
}

ParseResult parse_unquote(Parser *parser) {
    return parse_abbrev(parser, "unquote");
}

ParseResult parse_slicing_unquote(Parser *parser) {
    return parse_abbrev(parser, "slicing-unquote");
}

ParseResult parse_quasi(Parser *parser) {
    return parse_abbrev(parser, "quasiquote");
}

