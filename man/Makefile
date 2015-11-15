ALL=man1/pandoc-citeproc.1
DATE=$(shell date "+%Y-%m-%d")
VERSION=$(shell grep version: ../pandoc-citeproc.cabal | head -1 | awk '{print $$2;}')

.PHONY: all

all: $(ALL)
man1/%.1: %.1.md
	pandoc $< -o $@ -s -t man -Vfooter="pandoc-citeproc ${VERSION}" -Vdate="${DATE}"
