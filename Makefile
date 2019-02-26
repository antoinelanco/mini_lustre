
all: src/lmoch

src/lmoch: lib/aez-0.3/aez.cmxa FORCE
	(cd src ; \
	 $(MAKE))

lib/aez-0.3/aez.cmxa:
	(cd lib ; \
	 tar xvfz aez-0.3.tar.gz ; \
	 cd aez-0.3 ; \
	 ./configure ; \
	 $(MAKE))

clean:
	(cd src; $(MAKE) clean)
	(cd examples; $(MAKE) clean)

cleanall:
	rm -f *~
	(cd src; $(MAKE) cleanall)
	(cd examples; $(MAKE) cleanall)

FORCE:

