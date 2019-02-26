compil:
	make -C src

clean:
	make -C src clean
	make -C examples clean

cleanall:
	rm -f *~
	$(MAKE) -C src cleanall
	$(MAKE) -C examples cleanall

.PHONY: compil cleanall clean
