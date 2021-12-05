all: .gitignore build

build:
	@dune build

ci:
	@$(MAKE) -C .. $@

test: build
	./midi

debug: bug
	./bug

perf: bug
	-perf record --call-graph=dwarf -- ./bug
	pref report

%: %.ml build
	@dune exec ./$@.exe

.gitignore: $(PROG)
	@rm -f $@
	@for i in $(PROG); do echo $$i >> $@; done
