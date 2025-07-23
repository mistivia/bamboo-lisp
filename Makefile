mode ?= debug
cc = gcc

includes = -DWITHREADLINE 

ldflags = -lm -lreadline -lalgds
ifeq ($(mode), debug)
	cflags = $(includes) \
		-g \
		-fsanitize=address
else 
	cflags = $(includes) -O2
endif

src = $(shell find ./ -maxdepth 1 -name '*.c' -not -name 'main.c')
obj = $(src:.c=.o)

tests=$(shell ls tests/*.c)
tests_bin=$(tests:.c=.bin)

all: bamboo-lisp

install: bamboo-lisp
	sudo cp bamboo-lisp /usr/local/bin/bamboo

prelude.c: prelude.lisp
	cat prelude.lisp | python scripts/genprelude.py > prelude.c

bamboo-lisp:  $(obj) main.o
	gcc $(cflags) -o $@ $^ $(ldflags)

libbamboo-lisp.a: $(obj)
	ar cr $@ $^

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

$(obj):%.o:$(libs)

$(tests_bin):%.bin:%.c $(obj) $(libs)
	$(cc) $(cflags) -I./ -Ilibs/ $< $(obj) $(libs) -MD -MF $@.d -o $@ $(ldflags)

clean:
	-rm $(shell find tests/ -name '*.bin')
	-rm $(shell find . -name '*.o' -or -name '*.a' -or -name '*.d')
	-rm bamboo-lisp

DEPS := $(shell find . -name '*.d')
ifneq ($(DEPS),)
include $(DEPS)
endif
