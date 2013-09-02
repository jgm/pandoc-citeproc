---
title: biblio2yaml
section: 1
footer: pandoc-citeproc manual
date: August 31, 2013
...

# NAME

biblio2yaml - convert bibliographic database to YAML suitable for pandoc.

# SYNOPSIS

biblio2yaml [*options*] [*file*]

# DESCRIPTION

`biblio2yaml` will convert an existing bibliography (in any of the formats
listed above) into a YAML bibliography of the sort that can be included
in the `references` field of pandoc's metadata.

Simplest usage is

    biblio2yaml FILE

which will convert FILE and print the result to stdout.
The format will be derived from FILE's extension, according to this
table:

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

`biblio2yaml` can also be used as a pipe, taking input from stdin,
in which case the format must be specified explicitly using the
`-f/--format` flag.

# OPTIONS

`-f` *FORMAT*, `--format=`*FORMAT*
:   Specify format of bibliography to be converted.  Legal values are
    `biblatex`, `bibtex`, `ris`, `endnote`, `endnotexml`, `isi`,
    `medline`, `copac`, and `json`.

`-h, --help`
:   Print usage information.

`-V, --version`
:   Print version.

# AUTHORS

John MacFarlane, Andrea Rossato.

# SEE ALSO

`pandoc` (1), `pandoc-citeproc` (1).

The `biblio2yaml` source code and all documentation may be downloaded
from <http://github.com/jgm/pandoc-citeproc/>.
