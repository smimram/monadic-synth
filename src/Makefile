SOURCES = extlib.ml math.ml stream.ml visu.ml note.ml MIDI.ml pattern.ml instrument.ml output.ml op.ml OSC.ml board.ml
RESULT = msynth
THREADS = yes
ANNOTATE = true
PACKS = alsa lo graphics
OCAMLOPTFLAGS = -O3

# the name output.ml apparently messes up ocamldoc
DOC_FILES = $(filter-out output.ml,$(SOURCES))
OCAMLDOCFLAGS = -I +threads


all: dncl

ci:
	$(MAKE) -C .. $@

dist:
	tar zcvf msynth.tar.gz msynth *.ml Makefile OCamlMakefile test/*.ml test/Makefile

include OCamlMakefile
