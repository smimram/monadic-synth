all: build

build:
	@dune build

clean:
	@dune clean

install:
	@dune install

ci:
	git ci . -m "Worked on synth."
	git push
