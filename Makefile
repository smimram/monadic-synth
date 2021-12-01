all: site

index.html: ../README.md
	pandoc $< -s -c github.css --toc --metadata title="Monadic synthesizers in OCaml" -o $@

site: index.html
	cd .. && dune build @doc
	rm -rf odoc
	cp -r ../_build/default/_doc/_html odoc

watch:
	while inotifywait ../README.md -e modify; do $(MAKE) index.html; done
