ML = $(wildcard *.ml)
PROG = $(ML:.ml=)

all: .gitignore lib $(PROG)

ci:
	@$(MAKE) -C .. $@

lib:
	@$(MAKE) -C ../src

test: lib midi
	./midi

debug: lib bug
	./bug

perf: lib bug
	-perf record --call-graph=dwarf -- ./bug
	pref report

clean:
	rm -f $(wildcard *.cmi *.cmx *.o) $(PROG)

%: %.ml ../src/msynth.cmxa
	ocamlfind ocamlopt -package alsa,lo,graphics,threads -thread -I ../src \
	graphics.cmxa str.cmxa alsa.cmxa unix.cmxa threads.cmxa lo.cmxa msynth.cmxa \
	-o $@ $<

.gitignore: $(PROG)
	@rm -f $@
	@for i in $(PROG); do echo $$i >> $@; done

.PHONY: lib
