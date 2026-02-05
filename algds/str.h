#ifndef ALGDS_STR_H_
#define ALGDS_STR_H_

#include <stdio.h>

char *str_strip(char *str);
char **str_split(char *str, char delim);
void destroy_str_list(char **list);

struct str_builder {
    char *buf;
    int size;
    int cap;
};
typedef struct str_builder str_builder_t;
typedef struct str_builder StrBuilder;

void StrBuilder_init(StrBuilder* self);
void StrBuilder_append(StrBuilder *self, char *format, ...);
void StrBuilder_append_char(StrBuilder *self, char c);
StrBuilder StrBuilder_move(StrBuilder* self);
void StrBuilder_free(StrBuilder* self);

// legacy
void init_str_builder(str_builder_t *sb);
void str_builder_append(str_builder_t *sb, char *format, ...);
void str_builder_append_char(str_builder_t *sb, char c);

char *fgetline(FILE *fp);
int fpeek(FILE *fp);

#endif
