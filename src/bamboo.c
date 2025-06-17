#include "bamboo.h"

SExpRef new_list1(Bamboo *ctx, SExpRef e1) {
    return cons(ctx, e1, nil(ctx));
}

SExpRef new_list2(Bamboo *ctx, SExpRef e1, SExpRef e2) {
    return cons(ctx, e1, new_list1(ctx, e2));
}

SExpRef new_list3(Bamboo *ctx, SExpRef e1, SExpRef e2, SExpRef e3) {
    return cons(ctx, e1, new_list2(ctx, e2, e3));
}

SExpRef new_list4(Bamboo *ctx, SExpRef e1, SExpRef e2, SExpRef e3, SExpRef e4) {
    return cons(ctx, e1, new_list3(ctx, e2, e3, e4));
}

SExpRef new_list5(Bamboo *ctx, SExpRef e1, SExpRef e2, SExpRef e3, SExpRef e4, SExpRef e5) {
    return cons(ctx, e1, new_list4(ctx, e2, e3, e4, e5));
}

