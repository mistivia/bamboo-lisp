#include "interp.h"
#include "parser.h"
#include "sexp.h"

int main(int argc, char **argv) {
    int mainret = 0;
    Interp interp;
    Interp_init(&interp);
    if (argc > 2) {
        fprintf(stderr, "Usage: bamboo-lisp [file.lisp]\n");
        return -1;
    }
    if (argc == 2) {
        const char *filename = argv[1];
        SExpRef ret = Interp_load_file(&interp, filename);
        if (Interp_ref(&interp, ret)->type == kErrSignal) {
            fprintf(stderr, "Error: %s", Interp_ref(&interp, ret)->str);
            mainret = -1; goto end;
        }
        if (Interp_ref(&interp, ret)->type == kExceptionSignal) {
            const char *exception_str = lisp_to_string(&interp, Interp_ref(&interp, ret)->ret);
            fprintf(stderr, "Uncatched exception: %s\n", exception_str);
            free((void*)exception_str);
            mainret = -1; goto end;
        }
        if (Interp_ref(&interp, ret)->type == kBreakSignal
                || Interp_ref(&interp, ret)->type == kContinueSignal
                || Interp_ref(&interp, ret)->type == kReturnSignal) {
            fprintf(stderr, "Error: unexpected control flow signal.\n");
            mainret = -1; goto end;
        }
    }
#ifdef WITHREADLINE
    Parser_set_readline(interp.parser);
#else
    Parser_set_file(interp.parser, stdin);
#endif
    SExpRef sexp, res;
    ParseResult parse_result;
    while (1) {
#ifndef WITHREADLINE
        printf(">>> ");
        fflush(stdout);
#endif
        parse_result = parse_sexp(interp.parser);
        if (parse_result.errmsg != NULL) {
            if (Parser_peek(interp.parser) == EOF) goto end;
            fprintf(stderr, "Parsing error: %s", parse_result.errmsg);
#ifdef WITHREADLINE
            free((void*)interp.parser->string);
            Parser_set_readline(interp.parser);
#endif
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
    Interp_free(&interp);
    return mainret;
}
