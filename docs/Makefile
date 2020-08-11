all: index.html ocamldoc

index.html: ../README.md
	pandoc $< -o $@

ocamldoc:
	@rm -rf ../src/doc
	@$(MAKE) -C ../src htdoc
	@rm -rf ocamldoc
	@cp -r ../src/doc/msynth/html ocamldoc

ci: all
	git add ocamldoc
	git ci . -m "Update website."
	git push

.PHONY: ocamldoc
