all clean install:
	$(MAKE) -C src


ci:
	git ci . -m "Worked on synth."
	git push
