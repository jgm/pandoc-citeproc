all:
	cabal install --enable-tests -ftest_citeproc -fembed_data_files --disable-optimization

quick:
	cabal install --disable-optim -ftest_citeproc

prod:
	cabal install --enable-tests -ftest_citeproc -fembed_data_files --enable-optimization

update:
	curl 'https://raw2.github.com/citation-style-language/styles/master/chicago-author-date.csl' > chicago-author-date.csl ; \
	git clone 'https://github.com/citation-style-language/locales' locales-repo ; \
	for x in locales-repo/*.xml; do cp $$x locales/; done ; \
	rm -rf locales-repo; \
	git add locales/*.xml; \
	git add chicago-author-date.csl; \
	git commit -a
