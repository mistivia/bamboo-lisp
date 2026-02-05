#include "str.h"

#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char **str_split(char *str, char delim) {
    char **ret;

    if (str == NULL) return NULL;
    if (*str == '\n') {
        ret = malloc(sizeof(char *));
        *ret = NULL;
        return ret;
    }
    int count = 0;
    char *begin = str;
    for (char *p = str; *p != '\0'; p++) {
        if (*p != delim && !(delim == '\0' && isspace(*p))) {
            continue;
        }
        int size = p - begin;
        if (size > 0) count++;
    }
    count++;
    ret = malloc((count + 1) * sizeof(char *));
    memset(ret, 0, (count + 1) * sizeof(char *));

    begin = str;
    int i = 0;
    bool finished = false;
    for (char *p = str; !finished; p++) {
        if (*p == '\0') finished = true;
        if (*p != delim && *p != '\0' && !(delim == '\0' && isspace(*p))) {
            continue;
        }
        int size = p - begin;
        if (size == 0) {
            begin = p + 1;
            continue;
        }
        char *buf = malloc(sizeof(char) * (size + 1));
        buf[size] = '\0';
        memcpy(buf, begin, size * sizeof(char));
        begin = p + 1;
        ret[i] = buf;
        i++;
    }
    return ret;
}

void destroy_str_list(char **list) {
    char **p = list;
    while (*p != NULL) {
        free(*p);
        p++;
    }
    free(list);
}

char *str_strip(char *str) {
    if (str == NULL) return NULL;
    int len = strlen(str);
    if (len == 0) {
        char* ret = malloc(1);
        ret[0] = '\0';
        return ret;
    }
    char *begin = str;
    char *end = str + len - 1;
    while (isspace(*begin) && begin < end) {
        begin++;
    }
    while (end >= begin && isspace(*end)) {
        end--;
    }
    len = end - begin + 1;
    char *buf = malloc(sizeof(char) * (len) + 1);
    buf[len] = '\0';
    memcpy(buf, begin, len);
    return buf;
}
void StrBuilder_init(StrBuilder* self) {
    init_str_builder(self);
}

void StrBuilder_free(StrBuilder* self) {
    free(self->buf);
}

StrBuilder StrBuilder_move(StrBuilder* self) {
    StrBuilder neo;
    neo.buf = self->buf;
    self->buf = NULL;
    self->cap = 0;
    self->size = 0;
    return neo;
}

void StrBuilder_append_char(StrBuilder *sb, char c) {
    str_builder_append_char(sb, c);
}

void init_str_builder(str_builder_t *sb) {
    *sb = (str_builder_t){.size = 0, .cap = 16};
    sb->buf = malloc(sizeof(char) * 17);
}

static void sb_reserve(str_builder_t *sb, int extra) {
    if (sb->size + extra <= sb->cap) {
        return;
    }
    int new_cap = (sb->size + extra) * 2;
    sb->buf = realloc(sb->buf, new_cap + 1);
    memset(sb->buf + sb->cap, 0, new_cap - sb->cap + 1);
    sb->cap = new_cap;
}

void StrBuilder_append(StrBuilder *sb, char *format, ...) {
    va_list va1;
    va_list va2;
    va_start(va1, format);
    va_copy(va2, va1);
    int size = vsnprintf(NULL, 0, format, va1);
    sb_reserve(sb, size);
    vsnprintf(sb->buf + sb->size, sb->cap - sb->size + 1, format, va2);
    sb->size += size;
}

void str_builder_append(str_builder_t *sb, char *format, ...) {
    va_list va1;
    va_list va2;
    va_start(va1, format);
    va_copy(va2, va1);
    int size = vsnprintf(NULL, 0, format, va1);
    sb_reserve(sb, size);
    vsnprintf(sb->buf + sb->size, sb->cap - sb->size + 1, format, va2);
    sb->size += size;
}

void str_builder_append_char(str_builder_t *sb, char c) {
    sb_reserve(sb, 1);
    sb->buf[sb->size] = c;
    sb->size++;
}

char *fgetline(FILE *fp) {
    str_builder_t sb;
    init_str_builder(&sb);
    while (true) {
        int c = fgetc(fp);
        if (c == EOF && sb.size == 0) return NULL;
        if (c != EOF) str_builder_append_char(&sb, c);
        if (c == EOF || c == '\n') return sb.buf;
    }
    return NULL;
}

int fpeek(FILE *fp) {
    int c = fgetc(fp);
    if (c == EOF) return c;
    ungetc(c, fp);
    return c;
}
