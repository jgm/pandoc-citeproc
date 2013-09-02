---
title: pandoc-citeproc
section: 1
footer: pandoc-citeproc manual
date: August 31, 2013
...

# NAME

pandoc-citeproc - filter to resolve citations in a pandoc document.

# SYNOPSIS

pandoc-citeproc [*options*]

# DESCRIPTION

The `pandoc-citeproc` executable is a filter that takes a JSON-encoded
Pandoc document, formats citations and adds a bibliography, and
returns a JSON-encoded pandoc document.

To process citations with pandoc, call pandoc-citeproc as a filter:

    pandoc --filter pandoc-citeproc input.md -s -o output.html

The bibliography will be put into a pandoc `Div` container with
class `references`.

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

The contents of fields will be interpreted as markdown when
appropriate:  so, for example, emphasis and strong emphasis can
be used in title fileds. Simple tex math will also be
parsed and rendered appropriately.

`csl` or `citation-style`: Path to a CSL style file.  If the file is not found
relative to the working directory, pandoc-citeproc will look in the
`$HOME/.csl` directory (or `C:\Users\USERNAME\AppData\Roaming\csl` in Windows
7).

`citation-abbreviations`:  Path to a CSL abbreviations JSON file. The format
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
both as a source of references.  `csl` and `citation-abbreviations`
are optional.  If `csl` is not provided, `chicago-author-date.csl` will be
used by default.

# OPTIONS

`-h, --help`
:   Print usage information.

`-V, --version`
:   Print version.

# AUTHORS

Andrea Rossato and John MacFarlane.

# SEE ALSO

`pandoc` (1), `biblio2yaml` (1).

The pandoc-citeproc source code and all documentation may be downloaded
from <http://github.com/jgm/pandoc-citeproc/>.
