debug: deps
	cabal configure --enable-tests -ftest_citeproc -fembed_data_files -fdebug --disable-optimization --ghc-options "-pgmPcpphs -optP--cpp"
	cabal build

quick: deps
	cabal configure --enable-tests -ftest_citeproc -fembed_data_files --disable-optimization --ghc-options "-pgmPcpphs -optP--cpp"
	cabal build

full: deps man
	cabal configure --enable-tests -ftest_citeproc -fembed_data_files --ghc-options "-pgmPcpphs -optP--cpp"
	cabal build

man:
	make -C man -B

deps:
	cabal install --only-dependencies --enable-tests -ftest_citeproc -fembed_data_files

prof: deps
	cabal configure --enable-library-profiling --enable-executable-profiling --enable-optimization --ghc-options "-pgmPcpphs -optP--cpp"
	cabal build

install:
	cabal copy
	cabal register

test:
	cabal test

clean:
	cabal clean

update:
	curl 'https://raw.githubusercontent.com/citation-style-language/styles/master/chicago-author-date.csl' > chicago-author-date.csl ; \
	git clone 'https://github.com/citation-style-language/locales' locales-repo ; \
	for x in locales-repo/*.xml; do cp $$x locales/; done ; \
	rm -rf locales-repo; \
	git add locales/*.xml; \
	git add chicago-author-date.csl; \
	git commit -a

.PHONY: quick full prof update clean install test deps man
