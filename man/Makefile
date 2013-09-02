ALL=biblio2yaml.1 pandoc-citeproc.1

.PHONY: all

all: $(ALL)
%.1: %.1.md
	pandoc $< -o $@ -s -t man
