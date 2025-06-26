mode ?= debug
cc = gcc

includes = -DWITHREADLINE -I3rdparty/algds/build/include/
#includes = -I3rdparty/algds/build/include/

3rdlibs = 3rdparty/algds/build/lib/libalgds.a
ldflags = -lm -lreadline
ifeq ($(mode), debug)
	cflags = $(includes) \
		-g \
		-fsanitize=address
else ifeq ($(mode), debug)
	cflags = $(includes) -g -flto -O2
endif

src = $(shell find src/ -name '*.c' -not -name 'main.c')
obj = $(src:.c=.o)

tests=$(shell ls tests/*.c)
tests_bin=$(tests:.c=.bin)

all: bamboo-lisp

web: web-bamboo-lisp.js

web-bamboo-lisp.js: $(src)
	-rm web-*
	emcc -I3rdparty/algds/build/include $(src) 3rdparty/algds/src/*.c -o web-bamboo-lisp.js \
		-s EXPORTED_FUNCTIONS="['_print_lisp_error', '_malloc', '_free', '_new_interp', '_lisp_to_string', _Interp_eval_string]" -s WASM=1 -s EXPORTED_RUNTIME_METHODS="['stringToUTF8', 'UTF8ToString']"

install: bamboo-lisp
	sudo cp bamboo-lisp /usr/local/bin/bamboo

src/prelude.c: src/prelude.lisp
	cat src/prelude.lisp | python scripts/genprelude.py > src/prelude.c

bamboo-lisp:  $(obj) src/main.o 3rdparty/algds/build/lib/libalgds.a
	gcc $(cflags) -o $@ $^ $(ldflags)

3rdparty/algds/build/lib/libalgds.a:
	cd 3rdparty/algds && \
		make profile=$(mode)

test: bamboo-lisp $(tests_bin)
	@echo
	@echo "Run tests:"
	@scripts/runall.sh $(tests_bin)
	@echo "Run scripts:"
	./bamboo-lisp tests/test.lisp

src/main.o:src/main.c
	$(cc) -c $(cflags) $< -MD -MF $@.d -o $@

$(obj):%.o:%.c
	$(cc) -c $(cflags) $< -MD -MF $@.d -o $@

$(obj):%.o:$(3rdlibs)

$(tests_bin):%.bin:%.c $(obj) $(3rdlibs)
	$(cc) $(cflags) -Isrc/ $< $(obj) $(3rdlibs) -MD -MF $@.d -o $@ $(ldflags)

clean:
	-rm $(shell find tests/ -name '*.bin')
	-rm $(shell find . -name '*.o' -or -name '*.a' -or -name '*.d')
	-rm bamboo-lisp
	-cd 3rdparty/algds && make clean

DEPS := $(shell find . -name *.d)
ifneq ($(DEPS),)
include $(DEPS)
endif
