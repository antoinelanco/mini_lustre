compil:
	make -C src

examples:
	make -C examples

all: compil examples

clean:
	make -C src clean
	make -C examples clean

cleanall:
	rm -f *~
	$(MAKE) -C src cleanall
	$(MAKE) -C examples cleanall

.PHONY: all compil cleanall clean examples
