SOURCES = extlib.ml op.ml
RESULT = msynth
THREADS = yes
ANNOTATE = true
PACKS = alsa lo graphics
OCAMLOPTFLAGS = -O3
INCDIRS = ../../src

DOC_FILES = $(SOURCES)
OCAMLDOCFLAGS = -I +threads

all: dncl

test: test.ml dncl
	ocamlopt $< -o $@
	./test

ci:
	$(MAKE) -C .. $@

include OCamlMakefile
