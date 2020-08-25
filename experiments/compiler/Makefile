SOURCES = extlib.ml stream.ml
RESULT = msynth
THREADS = yes
ANNOTATE = true
PACKS = alsa lo graphics
OCAMLOPTFLAGS = -O3

DOC_FILES = $(SOURCES)
OCAMLDOCFLAGS = -I +threads

all: dncl

ci:
	$(MAKE) -C .. $@

include OCamlMakefile
