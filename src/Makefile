all: build

build:
	@dune build

ci:
	$(MAKE) -C .. $@

dist:
	tar zcvf msynth.tar.gz msynth *.ml Makefile OCamlMakefile test/*.ml test/Makefile
