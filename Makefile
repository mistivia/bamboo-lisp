mode ?= debug
cc = gcc

includes = -DWITHREADLINE -fPIC

ldflags = -lm -lreadline -lalgds
ifeq ($(mode), debug)
	cflags = $(includes) -g
else 
	cflags = $(includes) -O2
endif

src = $(shell find ./ -maxdepth 1 -name '*.c' -not -name 'main.c')
obj = $(src:.c=.o)

tests=$(shell ls tests/*.c)
tests_bin=$(tests:.c=.bin)

all: bamboo-lisp ext_example/vector.so $(tests_bin)

install: bamboo-lisp libbamboo-lisp.a
	sudo cp bamboo-lisp /usr/local/bin/bamboo-lisp
	sudo cp libbamboo-lisp.a /usr/local/lib/
	sudo mkdir -p /usr/local/include/bamboo_lisp
	sudo cp *.h /usr/local/include/bamboo_lisp/

prelude.c: prelude.lisp
	cat prelude.lisp | python scripts/genprelude.py > prelude.c

bamboo-lisp:  $(obj) main.o
	gcc $(cflags) -o $@ $^ $(ldflags)

libbamboo-lisp.a: $(obj)
	ar cr $@ $^

ext_example/vector.so: ext_example/vector.c libbamboo-lisp.a
	gcc -shared $(cflags) -I./ -o $@ $^ $(ldflags)

test: bamboo-lisp $(tests_bin) ext_example/vector.so
	@echo
	@echo "Run tests:"
	@scripts/runall.sh $(tests_bin)
	@echo "Run scripts:"
	./bamboo-lisp tests/test.lisp

main.o:main.c
	$(cc) -c $(cflags) $< -MD -MF $@.d -o $@

$(obj):%.o:%.c
	$(cc) -c $(cflags) $< -MD -MF $@.d -o $@

$(obj):%.o:$(libs)

$(tests_bin):%.bin:%.c $(obj) $(libs)
	$(cc) $(cflags) -I./ -Ilibs/ $< $(obj) $(libs) -MD -MF $@.d -o $@ $(ldflags)

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
