all: index.html ocamldoc

index.html: ../README.md
	pandoc $< -s -c github.css --title "Monadic synthesizers in OCaml" -o $@

ocamldoc:
	@rm -rf ../src/doc
	@$(MAKE) -C ../src htdoc
	@rm -rf ocamldoc
	@cp -r ../src/doc/msynth/html ocamldoc

ci: all
	git add ocamldoc
	git ci ../README.md . -m "Update website."
	git push

.PHONY: ocamldoc
