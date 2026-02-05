mode ?= debug

cc = gcc
ifeq ($(mode), debug)
	cflags = \
		-g
else
	cflags = -O2
endif

src = $(shell ls *.c)
obj = $(src:.c=.o)

tests=$(shell ls tests/*.c)
tests_bin=$(tests:.c=.bin)

all: libalgds.a

libalgds.a: $(obj)
	ar cr $@ $^

$(obj):%.o:%.c
	$(cc) -c $(cflags) $< -MD -MF $@.d -o $@

clean:
	-rm -rf build/
	-rm $(shell find tests/ -name '*.bin')
	-rm $(shell find . -name '*.o' -or -name '*.a' -or -name '*.d')

DEPS := $(shell find . -name '*.d')
ifneq ($(DEPS),)
include $(DEPS)
endif
