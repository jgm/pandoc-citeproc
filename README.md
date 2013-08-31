pandoc-citeproc
===============

This package provides a library and executable to facilitate the use of
citeproc with pandoc 1.12 and greater.  (Earlier versions of pandoc have
integrated citeproc support.)

The current version of the package includes code from citeproc-hs,
which has not been updated for some time.  When citeproc-hs is brought
up to date, this code can be removed and this package will depend
on citeproc-hs.

`pandoc-citeproc`
-----------------

The `pandoc-citeproc` executable is a filter that takes a JSON-encoded
Pandoc document, formats citations and adds a bibliography, and
returns a JSON-encoded pandoc document.

To process citations with pandoc, call pandoc-citeproc as a filter:

    pandoc --filter pandoc-citeproc input.md -s -o output.html

pandoc-citeproc will look for the following metadata fields in
the input:

`bibliography`:  A path, or YAML list of paths, of bibliography
files to use.  These may be in any of the formats supported by
bibutils.

    Format            File extension
    ------------      --------------
    MODS              .mods
    BibLaTeX          .bib
    BibTeX            .bibtex
    RIS               .ris
    EndNote           .enl
    EndNote XML       .xml
    ISI               .wos
    MEDLINE           .medline
    Copac             .copac
    JSON citeproc     .json

`references`:  A YAML list of references.  Each reference is a YAML
object.  The format is essentially CSL JSON format.  Here is an example:

    - id: doe2006
      author:
        family: Doe
        given: [John, F.]
      title: Article
      page: 33-34
      issued:
        year: 2006
      type: article-journal
      volume: 6
      container-title: Journal of Generic Studies

`csl`: Path to a CSL style file.  If the file is not found relative
to the working directory, pandoc-citeproc will look in the
`$HOME/.csl` directory (or `C:\Users\USERNAME\AppData\Roaming\csl`
in Windows 7).

`csl-abbrevs`:  Path to a CSL abbreviations JSON file. The format
is described [here](http://citationstylist.org/2011/10/19/abbreviations-for-zotero-test-release).  Here is a short example:

    { "default": {
        "container-title": {
                "Lloyd's Law Reports": "Lloyd's Rep",
                "Estates Gazette": "EG",
                "Scots Law Times": "SLT"
        }
      }
    }

The metadata must contain either `references` or `bibliography` or
both as a source of references.  `csl` and `csl-abbrevs` are optional.
If `csl` is not provided, `chicago-author-date.csl` will be used by
default.

`biblio2yaml`
-------------

`biblio2yaml` will convert an existing bibliography (in any of the formats
listed above) into a YAML bibliography of the sort that can be included
in the `references` field of pandoc's metadata.

Simplest usage is

    biblio2yaml BIBFILE

which will convert BIBFILE and print the result to stdout.
The format will be derived from BIBFILE's extension, according to the
table above.

`biblio2yaml` can also be used as a pipe, taking inptu from stdin,
in which case the format must be specified explicitly using the
`-f/--format` flag.

`Text.CSL.Pandoc`
-----------------

Those who use pandoc as a library (e.g. in a web application) will
need to use this module to process citations.

The module exports two functions, `processCites`, which is pure and
accepts a style and a list of references as arguments, and
`processCites'`, which lives in the IO monad and derives the style
and references from the document's metadata.

