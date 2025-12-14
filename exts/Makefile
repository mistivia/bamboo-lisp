mode ?= debug
cc = gcc

cflags = -DWITHREADLINE -fPIC

ldflags = -lm -lalgds -lbamboo-lisp
ifeq ($(mode), debug)
	cflags += -g
else 
	cflags += -O2
endif

objs = \
	vector.so \
	dict.so \
	io.so

all: $(objs)

vector.so: vector.c
	gcc -shared $(cflags) -o $@ $^ $(ldflags)

dict.so: dict.c
	gcc -shared $(cflags) -o $@ $^ $(ldflags)

io.so: io.c
	gcc -shared $(cflags) -o $@ $^ $(ldflags)

install:
	mkdir -p /usr/local/share/bamboo-lisp/exts/
	cp $(objs) /usr/local/share/bamboo-lisp/exts/