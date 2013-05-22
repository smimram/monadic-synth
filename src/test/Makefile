ML = $(wildcard *.ml)
PROG = $(ML:.ml=)

all: lib $(PROG)

ci:
	@$(MAKE) -C .. $@

lib:
	@$(MAKE) -C ..

clean:
	rm -f $(wildcard *.cmi *.cmx *.o) $(PROG)

%: %.ml ../msynth.cmxa
	ocamlopt -thread \
	-I +pulseaudio -I +mm -I +lo -I .. \
	unix.cmxa threads.cmxa str.cmxa graphics.cmxa bigarray.cmxa pulseaudio.cmxa mm.cmxa lo.cmxa msynth.cmxa \
	-o $@ $^

.PHONY: lib