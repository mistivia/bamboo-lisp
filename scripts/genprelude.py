def escape_c_string(s):
    escape_chars = {
        '\n': '\\n',
        '\t': '\\t',
        '\r': '\\r',
        '\"': '\\"',
        '\'': '\\\'',
        '\\': '\\\\',
        '\b': '\\b',
        '\a': '\\a',
        '\v': '\\v',
        '\f': '\\f',
    }
    result = []
    for c in s:
        if c in escape_chars:
            result.append(escape_chars[c])
        else:
            result.append(c)
    return ''.join(result)

import sys

content = sys.stdin.read()
escaped = escape_c_string(content)
print(f"""
#include "prelude.h"

const char *prelude = "{escaped}";

""")
