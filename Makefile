SOURCEFILES?=$(shell find pandoc-citeproc.hs src tests -name '*.hs')
stack:
	stack install --test --flag 'pandoc:embed_data_files' --flag 'pandoc-citeproc:test_citeproc' --flag 'pandoc-citeproc:embed_data_files' --fast --ghc-options '-Wall' --test-arguments='-j4 --hide-successes $(TESTARGS)' . pandoc

debug:
	stack install --test --flag 'pandoc-citeproc:debug' --flag 'pandoc:embed_data_files' --flag 'pandoc-citeproc:embed_data_files' --fast --ghc-options '-Wall' --test-arguments='-j4 --hide-successes $(TESTARGS)'

quick: deps
	cabal configure --enable-tests -ftest_citeproc -fembed_data_files --disable-optimization --ghc-options "-Wall"
	cabal build

full: deps man
	cabal configure --enable-tests -ftest_citeproc -fembed_data_files --ghc-options "-Wall"
	cabal build

man:
	make -C man -B

deps:
	cabal install --only-dependencies --enable-tests -ftest_citeproc -fembed_data_files

prof: deps
	cabal configure --enable-library-profiling --enable-executable-profiling --enable-optimization --ghc-options "-Wall"
	cabal build

install:
	cabal copy
	cabal register

test:
	cabal test

clean:
	cabal clean

reformat:
	for f in $(SOURCEFILES); do echo $$f; stylish-haskell -i $$f ; done

lint:
	for f in $(SOURCEFILES); do echo $$f; hlint --verbose --cpp-file `stack path --dist-dir`/build/autogen/cabal_macros.h --cpp-file `stack path --dist-dir`/setup/setup_macros.h --refactor --refactor-options='-i -s' $$f; done

update:
	curl 'https://raw.githubusercontent.com/citation-style-language/styles/master/chicago-author-date.csl' > chicago-author-date.csl ; \
	git clone 'https://github.com/citation-style-language/locales' locales-repo ; \
	for x in locales-repo/*.xml; do cp $$x locales/; done ; \
	rm -rf locales-repo; \
	git add locales/*.xml; \
	git add chicago-author-date.csl; \
	git commit -a

.PHONY: quick full prof update clean install test deps man stack
