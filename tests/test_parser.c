#include <assert.h>
#include <stdio.h>

#include "interp.h"
#include "parser.h"
#include "sexp.h"

ParseResult parse_str(Parser *parser, const char* str) {
    Parser_set_string(parser, str);    
    return parse_sexp(parser);
}

#define ATOM_TEST(_str, _type_enum, _field, _expect) \
{ \
    res = parse_str(&parser, (_str)); \
    assert(!ParseResult_is_err(res)); \
    sexp = *Interp_ref(&interp, res.val); \
    assert(sexp.type == (_type_enum)); \
    assert(sexp._field == (_expect)); \
}

#define STRING_TEST(_str, _type_enum, _expect) \
{ \
    res = parse_str(&parser, (_str)); \
    assert(!ParseResult_is_err(res)); \
    sexp = *Interp_ref(&interp, res.val); \
    assert(sexp.type == _type_enum); \
    assert(strcmp(sexp.str, (_expect)) == 0); \
}

#define ERROR_TEST(_str) \
{ \
    res = parse_str(&parser, (_str)); \
    assert(ParseResult_is_err(res)); \
}

int main() {
    printf("[TEST] parser\n");
    Interp interp;
    Parser parser;
    Interp_init(&interp);
    Parser_init(&parser);
    parser.ctx = &interp;

    ParseResult res;
    SExp sexp, a, b, c;

    res = parse_str(&parser, "(+ 2)");
    assert(!ParseResult_is_err(res));
    sexp = *Interp_ref(&interp, res.val);
    assert(sexp.type == kPairSExp);
    a = *Interp_ref(&interp, sexp.pair.car);
    b = *Interp_ref(&interp, sexp.pair.cdr);
    assert(a.type == kSymbolSExp);
    assert(strcmp("+", a.str) == 0);
    a = *Interp_ref(&interp, b.pair.car);
    b = *Interp_ref(&interp, b.pair.cdr);
    assert(a.type == kIntegerSExp);
    assert(a.integer == 2);
    assert(b.type == kNilSExp);


    res = parse_str(&parser, "((1 2)\n . 3)");
    assert(!ParseResult_is_err(res));
    sexp = *Interp_ref(&interp, res.val);
    assert(sexp.type == kPairSExp);
    a = *Interp_ref(&interp, sexp.pair.car);
    b = *Interp_ref(&interp, sexp.pair.cdr);
    assert(a.type == kPairSExp);
    c = *Interp_ref(&interp, a.pair.cdr);
    a = *Interp_ref(&interp, a.pair.car);
    assert(a.type == kIntegerSExp);
    assert(a.integer == 1);
    a = *Interp_ref(&interp, c.pair.car);
    c = *Interp_ref(&interp, c.pair.cdr);
    assert(a.type == kIntegerSExp);
    assert(a.integer == 2);
    assert(c.type == kNilSExp);
    assert(b.type == kIntegerSExp);
    assert(b.integer == 3);

    res = parse_str(&parser, "((1 2) . 3)");
    assert(!ParseResult_is_err(res));
    sexp = *Interp_ref(&interp, res.val);
    assert(sexp.type == kPairSExp);
    a = *Interp_ref(&interp, sexp.pair.car);
    b = *Interp_ref(&interp, sexp.pair.cdr);
    assert(a.type == kPairSExp);
    c = *Interp_ref(&interp, a.pair.cdr);
    a = *Interp_ref(&interp, a.pair.car);
    assert(a.type == kIntegerSExp);
    assert(a.integer == 1);
    a = *Interp_ref(&interp, c.pair.car);
    c = *Interp_ref(&interp, c.pair.cdr);
    assert(a.type == kIntegerSExp);
    assert(a.integer == 2);
    assert(c.type == kNilSExp);
    assert(b.type == kIntegerSExp);
    assert(b.integer == 3);

    res = parse_str(&parser, "((1 2) 3)");
    assert(!ParseResult_is_err(res));
    sexp = *Interp_ref(&interp, res.val);
    assert(sexp.type == kPairSExp);
    a = *Interp_ref(&interp, sexp.pair.car);
    b = *Interp_ref(&interp, sexp.pair.cdr);
    assert(a.type == kPairSExp);
    c = *Interp_ref(&interp, a.pair.cdr);
    a = *Interp_ref(&interp, a.pair.car);
    assert(a.type == kIntegerSExp);
    assert(a.integer == 1);
    a = *Interp_ref(&interp, c.pair.car);
    c = *Interp_ref(&interp, c.pair.cdr);
    assert(a.type == kIntegerSExp);
    assert(a.integer == 2);
    assert(c.type == kNilSExp);
    a = *Interp_ref(&interp, b.pair.car);
    b = *Interp_ref(&interp, b.pair.cdr);
    assert(a.type == kIntegerSExp);
    assert(a.integer == 3);
    assert(b.type == kNilSExp);

    ERROR_TEST("(1 2 . 3 4)");
    ERROR_TEST("(1 2 . )");
    ERROR_TEST("(1 2 .)");

    res = parse_str(&parser, "(1 2 . 3)");
    assert(!ParseResult_is_err(res));
    sexp = *Interp_ref(&interp, res.val);
    assert(sexp.type == kPairSExp);
    a = *Interp_ref(&interp, sexp.pair.car);
    b = *Interp_ref(&interp, sexp.pair.cdr);
    assert(a.type == kIntegerSExp);
    assert(a.integer == 1);
    assert(b.type == kPairSExp);
    a = *Interp_ref(&interp, b.pair.car);
    b = *Interp_ref(&interp, b.pair.cdr);
    assert(a.type == kIntegerSExp);
    assert(a.integer == 2);
    assert(b.type == kIntegerSExp);
    assert(b.integer == 3);

    res = parse_str(&parser, "(1 . 2)");
    assert(!ParseResult_is_err(res));
    sexp = *Interp_ref(&interp, res.val);
    assert(sexp.type == kPairSExp);
    a = *Interp_ref(&interp, sexp.pair.car);
    b = *Interp_ref(&interp, sexp.pair.cdr);
    assert(a.type == kIntegerSExp);
    assert(a.integer == 1);
    assert(b.type == kIntegerSExp);
    assert(b.integer == 2);

    res = parse_str(&parser, "(1 2)");
    assert(!ParseResult_is_err(res));
    sexp = *Interp_ref(&interp, res.val);
    assert(sexp.type == kPairSExp);
    a = *Interp_ref(&interp, sexp.pair.car);
    b = *Interp_ref(&interp, sexp.pair.cdr);
    assert(a.type == kIntegerSExp);
    assert(a.integer == 1);
    a = *Interp_ref(&interp, b.pair.car);
    b = *Interp_ref(&interp, b.pair.cdr);
    assert(a.type == kIntegerSExp);
    assert(a.integer == 2);
    assert(b.type == kNilSExp);


    ATOM_TEST("1.11", kRealSExp, real, 1.11);
    ATOM_TEST("-1.11", kRealSExp, real, -1.11);
    ATOM_TEST("1.11e10", kRealSExp, real, 1.11e10);
    ATOM_TEST("1.11 ", kRealSExp, real, 1.11);
    ATOM_TEST("1.11e10 ", kRealSExp, real, 1.11e10);
    ATOM_TEST(" 1.11 ", kRealSExp, real, 1.11);
    ATOM_TEST(" 1.11e10 ", kRealSExp, real, 1.11e10);
    ERROR_TEST("123.1x");

    ATOM_TEST("42", kIntegerSExp, integer, 42);
    ATOM_TEST("-42", kIntegerSExp, integer, -42);
    ERROR_TEST("123x");

    ATOM_TEST("#t", kBooleanSExp, boolean, true);
    ATOM_TEST("#f", kBooleanSExp, boolean, false);
    ERROR_TEST("#x");

    ATOM_TEST("#\\t", kCharSExp, character, 't');
    ATOM_TEST("#\\newline", kCharSExp, character, '\n');
    ERROR_TEST("#\\uwu");

    STRING_TEST("\"test\"", kStringSExp, "test");
    STRING_TEST("\"t\\nest\"", kStringSExp, "t\nest");
    STRING_TEST("!uwu", kSymbolSExp, "!uwu");
    STRING_TEST("-", kSymbolSExp, "-");
    STRING_TEST("+", kSymbolSExp, "+");
    ERROR_TEST("\"t\\xst\"");
    ERROR_TEST("-abc");
    ERROR_TEST("@1");
    ERROR_TEST("a|");

    res = parse_str(&parser, "()");
    assert(!ParseResult_is_err(res));
    sexp = *Interp_ref(&interp, res.val);
    assert(sexp.type == kNilSExp);

    Interp_free(&interp);
    Parser_free(&parser);
    printf("[PASS] parser\n");
}
