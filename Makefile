mode ?= debug
cc = gcc
includes = -I3rdparty/algds/build/include/
3rdlibs = 3rdparty/algds/build/lib/libalgds.a
ldflags = -lreadline
ifeq ($(mode), debug)
	cflags = $(includes) \
		-g \
		-fsanitize=address
else
	cflags = $(includes) -flto -O2
endif

src = $(shell find src/ -name '*.c' -not -name 'main.c')
obj = $(src:.c=.o)

tests=$(shell ls tests/*.c)
tests_bin=$(tests:.c=.bin)

all: bamboo-lisp

src/prelude.c: src/prelude.lisp
	cat src/prelude.lisp | python scripts/genprelude.py > src/prelude.c

bamboo-lisp:  $(obj) src/main.c 3rdparty/algds/build/lib/libalgds.a
	gcc $(cflags) -o $@ $^ $(ldflags)

3rdparty/algds/build/lib/libalgds.a:
	cd 3rdparty/algds && \
		make profile=$(mode)

test: bamboo-lisp $(tests_bin)
	@echo
	@echo "Run tests:"
	@scripts/runall.sh $(tests_bin)
	@echo "Run scripts:"
	cd tests/ && \
		../bamboo-lisp test.lisp


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
