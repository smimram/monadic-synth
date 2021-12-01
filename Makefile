all: build

build:
	@dune build

doc:
	@dune build @doc

clean:
	@dune clean

install:
	@dune install

ci:
	git ci . -m "Worked on synth."
	git push
