SOURCES = extlib.ml stream.ml visu.ml note.ml pattern.ml MIDI.ml instrument.ml output.ml OSC.ml
RESULT = msynth
THREADS = yes
ANNOTATE = true
PACKS = alsa lo graphics
OCAMLOPTFLAGS = -inline 10000 -nodynlink #-unsafe

# the name output.ml apparently messes up ocamldoc
DOC_FILES = $(filter-out output.ml,$(SOURCES))
OCAMLDOCFLAGS = -I +threads


all: dncl

test: all
	$(MAKE) -C test

ci:
	git ci . -m "Worked on synth."
	git push

dist:
	tar zcvf msynth.tar.gz msynth *.ml Makefile OCamlMakefile test/*.ml test/Makefile

include OCamlMakefile
