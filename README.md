pandoc-citeproc
===============

This package provides a library and executable to facilitate the use of
citeproc with pandoc 1.12 and greater.  (Earlier versions of pandoc have
integrated citeproc support.)

`pandoc-citeproc`
-----------------

The `pandoc-citeproc` executable can be used as a filter with pandoc to
resolve and format citations using a bibliography file and a CSL
stylesheet.  It can also be used (with `--bib2yaml` or `--bib2json`
options) to convert a bibliography to a YAML format that can be put
directly into a pandoc markdown document or to CSL JSON.  Bibliographies
can be in any of several formats, but bibtex and biblatex are the best
supported.

For usage and further details, see the [pandoc-citeproc man
page](https://github.com/jgm/pandoc-citeproc/blob/master/man/pandoc-citeproc.1.md).

The current version of the package includes code from citeproc-hs,
which has not been updated for some time.  When citeproc-hs is brought
up to date, this code can be removed and this package will depend
on citeproc-hs.

`Text.CSL.Pandoc`
-----------------

Those who use pandoc as a library (e.g. in a web application) will
need to use this module to process citations.

The module exports two functions, `processCites`, which is pure and
accepts a style and a list of references as arguments, and
`processCites'`, which lives in the IO monad and derives the style
and references from the document's metadata.

