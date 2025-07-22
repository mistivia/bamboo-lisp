mode ?= debug
cc = gcc

includes = -DWITHREADLINE -Ilibs/

libs = libs/algds/libalgds.a
ldflags = -lm -lreadline
ifeq ($(mode), debug)
	cflags = $(includes) \
		-g \
		-fsanitize=address
else 
	cflags = $(includes) -flto -O2
endif

src = $(shell find ./ -maxdepth 1 -name '*.c' -not -name 'main.c')
obj = $(src:.c=.o)

tests=$(shell ls tests/*.c)
tests_bin=$(tests:.c=.bin)

all: bamboo-lisp

staticlib: libbamboo-lisp.a

web: web-bamboo-lisp.js

web-bamboo-lisp.js: $(src)
	-rm web-*
	emcc -Ilibs/ $(src) libs/algds/*.c -o web-bamboo-lisp.js \
		-s EXPORTED_FUNCTIONS="['_print_lisp_error', '_malloc', '_free', '_new_interp', '_lisp_to_string', _Interp_eval_string]" -s WASM=1 -s EXPORTED_RUNTIME_METHODS="['stringToUTF8', 'UTF8ToString']"

install: bamboo-lisp
	sudo cp bamboo-lisp /usr/local/bin/bamboo

prelude.c: prelude.lisp
	cat prelude.lisp | python scripts/genprelude.py > prelude.c

bamboo-lisp:  $(obj) main.o libs/algds/libalgds.a
	gcc $(cflags) -o $@ $^ $(ldflags)

libbamboo-lisp.a: $(obj)
	ar cr $@ $^

libs/algds/libalgds.a:
	cd libs/algds && \
		make profile=$(mode)

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
	-rm web-bamboo-lisp*
	-rm bamboo-lisp
	-cd libs/algds && make clean

DEPS := $(shell find . -name '*.d')
ifneq ($(DEPS),)
include $(DEPS)
endif
