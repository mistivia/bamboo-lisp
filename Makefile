mode ?= debug
cc = gcc

includes = -DWITHREADLINE

ldflags = -L./ -lm -lreadline -lalgds
ifeq ($(mode), debug)
	cflags = $(includes) -g
else 
	cflags = $(includes) -O2
endif

curdir = ./
installdir = /usr/local/lib/

src = $(shell find ./ -maxdepth 1 -name '*.c' -not -name 'main.c') $(shell find ./exts/ -maxdepth 1 -name '*.c')
obj = $(src:.c=.o)

tests=$(shell ls tests/*.c)
tests_bin=$(tests:.c=.bin)

all: bamboo-lisp $(tests_bin)

install: bamboo-lisp
	cp bamboo-lisp /usr/local/bin/bamboo-lisp
	mkdir -p /usr/local/include/bamboo_lisp
	cp *.h /usr/local/include/bamboo_lisp/

prelude.c: prelude.lisp
	cat prelude.lisp | python scripts/genprelude.py > prelude.c

bamboo-lisp: main.o $(obj)
	gcc $(cflags) -o $@ $< $(obj) $(ldflags) 

test: bamboo-lisp $(tests_bin)
	@echo
	@echo "Run tests:"
	@scripts/runall.sh $(tests_bin)
	@echo "Run scripts:"
	./bamboo-lisp tests/test.lisp

main.o:main.c
	$(cc) -c $(cflags) $< -MD -MF $@.d -o $@

$(obj):%.o:%.c
	$(cc) -c $(cflags) $< -MD -MF $@.d -o $@

$(tests_bin):%.bin:%.c $(obj)
	$(cc) $(cflags) -I./ $< $(obj) -MD -MF $@.d -o $@ $(ldflags)

clean:
	-rm $(shell find tests/ -name '*.bin')
	-rm $(shell find . -name '*.so')
	-rm $(shell find . -name '*.o')
	-rm $(shell find . -name '*.a')
	-rm $(shell find . -name '*.d')
	-rm bamboo-lisp

DEPS := $(shell find . -name '*.d')
ifneq ($(DEPS),)
include $(DEPS)
endif
