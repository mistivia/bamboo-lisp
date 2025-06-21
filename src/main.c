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
            free((void*)parser.string);
            Parser_set_readline(&parser);
            continue;
        }

        res = lisp_eval(&interp, parse_result.val, false);
        if (Interp_ref(&interp, res)->type == kErrSignal) {
            fprintf(stderr, "Eval error: %s", Interp_ref(&interp, res)->str);
            continue;
        }
        if (Interp_ref(&interp, res)->type == kBreakSignal
                || Interp_ref(&interp, res)->type == kContinueSignal
                || Interp_ref(&interp, res)->type == kReturnSignal) {
            fprintf(stderr, "Eval error: unexpected control flow signal.\n");
            continue;
        }
        lisp_print(&interp, res, stdout);
    }
end:
    Parser_free(&parser);
    Interp_free(&interp);
    return 0;
}
