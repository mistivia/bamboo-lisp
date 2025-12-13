mode ?= debug
cc = gcc

includes = -DWITHREADLINE -fPIC

ldflags = -L./ -lm -lreadline -lalgds 
ifeq ($(mode), debug)
	cflags = $(includes) -g
else 
	cflags = $(includes) -O2
endif

curdir = ./
installdir = /usr/local/lib/

src = $(shell find ./ -maxdepth 1 -name '*.c' -not -name 'main.c')
obj = $(src:.c=.o)

extsrc = $(shell find ./ext_example/ -maxdepth 1 -name '*.c')
extobj = $(extsrc:.c=.so)

tests=$(shell ls tests/*.c)
tests_bin=$(tests:.c=.bin)

all: bamboo-lisp exts $(tests_bin)

exts: $(extobj)

install: bamboo-lisp libbamboo-lisp.so
	sudo cp bamboo-lisp /usr/local/bin/bamboo-lisp
	sudo cp libbamboo-lisp.so $(installdir)
	sudo mkdir -p /usr/local/include/bamboo_lisp
	sudo cp *.h /usr/local/include/bamboo_lisp/

prelude.c: prelude.lisp
	cat prelude.lisp | python scripts/genprelude.py > prelude.c

bamboo-lisp:  libbamboo-lisp.so main.o
	gcc $(cflags) -o $@ $^ $(ldflags) -lbamboo-lisp -Wl,-rpath,$(curdir) -Wl,-rpath,$(installdir)

libbamboo-lisp.so: $(obj)
	gcc -shared -o $@ $^ $(ldflags)

$(extobj):%.so:%.c libbamboo-lisp.so
	gcc -shared $(cflags) -I./ -o $@ $^ $(ldflags) -lbamboo-lisp -Wl,-rpath,$(curdir) -Wl,-rpath,$(installdir)

test: bamboo-lisp $(tests_bin) exts
	@echo
	@echo "Run tests:"
	@scripts/runall.sh $(tests_bin)
	@echo "Run scripts:"
	./bamboo-lisp tests/test.lisp

main.o:main.c
	$(cc) -c $(cflags) $< -MD -MF $@.d -o $@

$(obj):%.o:%.c
	$(cc) -c $(cflags) $< -MD -MF $@.d -o $@

$(tests_bin):%.bin:%.c libbamboo-lisp.so
	$(cc) $(cflags) -I./ $< -MD -MF $@.d -o $@ $(ldflags) -lbamboo-lisp -Wl,-rpath,$(curdir) -Wl,-rpath,$(installdir)

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
