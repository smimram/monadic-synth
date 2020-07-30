ML = $(wildcard *.ml)
PROG = $(ML:.ml=)

all: .gitignore lib $(PROG)

ci:
	@$(MAKE) -C .. $@

lib:
	@$(MAKE) -C ..

clean:
	rm -f $(wildcard *.cmi *.cmx *.o) $(PROG)

%: %.ml ../msynth.cmxa
	ocamlopt -thread \
	-I +../alsa -I +../pulseaudio -I +../lo -I +../graphics -I .. \
	unix.cmxa threads.cmxa str.cmxa graphics.cmxa bigarray.cmxa alsa.cmxa pulseaudio.cmxa lo.cmxa msynth.cmxa \
	-o $@ $<

.gitignore: $(PROG)
	@rm -f $@
	@for i in $(PROG); do echo $$i >> $@; done

.PHONY: lib
