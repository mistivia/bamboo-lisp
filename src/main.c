#include "interp.h"
#include "parser.h"
#include "sexp.h"

int main() {
    int ret = -1;
    Interp interp;
    Parser parser;
    Interp_init(&interp);
    Parser_init(&parser);
    parser.ctx = &interp;

    Parser_set_readline(&parser);
    SExpRef sexp, res;
    ParseResult parse_result;
    while (1) {
        parse_result = parse_sexp(&parser);
        if (parse_result.errmsg != NULL) {
            if (Parser_peek(&parser) == EOF) goto end;
            fprintf(stderr, "Parsing error: %s", parse_result.errmsg);
            continue;
        }

        res = lisp_eval(&interp, parse_result.val);
        if (Interp_ref(&interp, res)->type == kErrSExp) {
            fprintf(stderr, "Eval error: %s", Interp_ref(&interp, res)->str);
            continue;
        }
        lisp_print(&interp, res, stdout);
    }
end:
    Parser_free(&parser);
    Interp_free(&interp);
    return 0;
}
