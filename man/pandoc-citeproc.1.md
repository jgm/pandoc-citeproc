# NAME

pandoc-citeproc - filter to resolve citations in a pandoc document.

# SYNOPSIS

pandoc-citeproc [options] [file..]

# DESCRIPTION

The `pandoc-citeproc` executable has two modes, filter mode and convert mode.

## Filter mode

Run without options, it acts as a filter that takes a JSON-encoded Pandoc
document, formats citations and adds a bibliography, and returns a JSON-encoded
pandoc document.

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

## Convert mode

If the option `--bib2yaml` or `--bib2json` is used, `pandoc-citeproc` will
not process citations; instead, it will treat its input (from stdin or
files) as a bibliography and convert it either to a pandoc YAML metadata
section, suitable for inclusion in a pandoc document (`--bib2yaml`), or
as a CSL JSON bibliography, suitable for import to zotero (`--bib2json`).

The `--format` option can be used to specify the bibliography format,
though when files are used, `pandoc-citeproc` can generally guess this
from the extension.

This mode supersedes the old `biblio2yaml` program.

# OPTIONS

`-h, --help`
:   Print usage information.

`-V, --version`
:   Print version.

`-y, --bib2yaml`
:   Convert bibliography to YAML suitable for inclusion in pandoc metadata.

`-j, --bib2json`
:   Convert bibliography to CSL JSON suitable for import into Zotero.

`-f` *FORMAT*, `--format=`*FORMAT*
:   Specify format of bibliography to be converted.  Legal values are
    `biblatex`, `bibtex`, `ris`, `endnote`, `endnotexml`, `isi`,
    `medline`, `copac`, and `json`.

# AUTHORS

Andrea Rossato and John MacFarlane.

# SEE ALSO

`pandoc` (1).

The pandoc-citeproc source code and all documentation may be downloaded
from <http://github.com/jgm/pandoc-citeproc/>.

---
title: pandoc-citeproc
section: 1
footer: pandoc-citeproc manual
date: June 4, 2014
...
