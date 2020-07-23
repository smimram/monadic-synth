SOURCES = extlib.ml stream.ml visu.ml pattern.ml instrument.ml output.ml OSC.ml
RESULT = msynth
THREADS = yes
ANNOTATE = true
INCDIRS = +../pulseaudio +../mm +../lo
LIBS = str bigarray pulseaudio mm lo graphics
OCAMLOPTFLAGS = -inline 10000 -nodynlink #-unsafe

all: dncl

test: all
	$(MAKE) -C test

ci:
	git ci . -m "Worked on synth."
	git push

dist:
	tar zcvf msynth.tar.gz msynth *.ml Makefile OCamlMakefile test/*.ml test/Makefile

include OCamlMakefile
