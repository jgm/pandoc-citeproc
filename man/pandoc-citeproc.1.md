---
title: pandoc-citeproc
section: 1
...

# NAME

pandoc-citeproc - filter to resolve citations in a pandoc document.

# SYNOPSIS

pandoc-citeproc [options] [file..]

# DESCRIPTION
The `pandoc-citeproc` executable has two modes, filter mode and convert mode.

## Filter mode

Run without options, it acts as a filter that takes a JSON-encoded Pandoc
document, formats citations and adds a bibliography, and returns a JSON-encoded
pandoc document.  Citations will be resolved, and a bibliography will be
inserted into a Div element with id `refs`. If no such Div
exists, one will be created and appended to the end of the document
(unless the `suppress-bibliography` metadata field is set to a
true value).  If you wish the bibliography to have a section
header, put the section header at the end of your document.
(See the `pandoc_markdown` (5) man page under "Citations" for
details on how to encode citations in pandoc's markdown.)

To process citations with pandoc, call pandoc-citeproc as a filter:

    pandoc --filter pandoc-citeproc input.md -s -o output.html

pandoc-citeproc will look for the following metadata fields in
the input:

`bibliography`
:   A path, or YAML list of paths, of bibliography
    files to use.  These may be in any of the formats supported by
    bibutils.

    Format            File extension
    ------------      --------------
    BibLaTeX          .bib
    BibTeX            .bibtex
    Copac             .copac
    CSL JSON          .json
    CSL YAML          .yaml
    EndNote           .enl
    EndNote XML       .xml
    ISI               .wos
    MEDLINE           .medline
    MODS              .mods
    RIS               .ris

    Note that `.bib` can generally be used with both BibTeX and BibLaTeX
    files, but you can use `.bibtex` to force BibTeX.

`references`
:   A YAML list of references.  Each reference is a YAML
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

`csl` or `citation-style`
:   Path or URL of a CSL style file.
    If the file is not found relative to the working directory,
    pandoc-citeproc will look in the `$HOME/.csl` directory (or
    `C:\Users\USERNAME\AppData\Roaming\csl` in Windows 7).  If this is left
    off, pandoc-citeproc will look for `$HOME/.pandoc/default.csl`,
    and if this is not present, it will use
    `chicago-author-date.csl`, looking first in `$HOME/.csl` and
    then in its own data files.

`link-citations`
:   If this has a true value, citations in author-date and numerical
    styles will be hyperlinked to their corresponding bibliography entries.
    The default is not to add hyperlinks.

`citation-abbreviations`
:   Path to a CSL abbreviations JSON file.  If the file is not found
    relative to the working directory, pandoc-citeproc will look in the
    `$HOME/.csl` directory (or `C:\Users\USERNAME\AppData\Roaming\csl`
    in Windows 7).  The format is described
    [here](http://citationstylist.org/2011/10/19/abbreviations-for-zotero-test-release).
    Here is a short example:

        { "default": {
            "container-title": {
                    "Lloyd's Law Reports": "Lloyd's Rep",
                    "Estates Gazette": "EG",
                    "Scots Law Times": "SLT"
            }
          }
        }

`lang`
:    Locale to use in formatting citations.  If this is not set, the
     locale is taken from the `default-locale` attribute of the CSL
     file.  `en-US` is used if a locale is not specified in either the
     metadata or the CSL file.  (For backwards compatibility,
     the field `locale` can be used instead of `lang`, but this
     `lang` should be used going forward.)

`suppress-bibliography`
:    If this has a true value, the bibliography will be left off.
     Otherwise a bibliography will be inserted into each
     Div element with id `refs`. If there is no such Div,
     one will be created at the end of the document.

`reference-section-title`
:    If this has a value, a section header with this title will be
     added before the bibliography.  If `reference-section-title`
     is not specified and the document ends with a section header,
     this final header will be treated as the bibliography header.

The metadata must contain either `references` or `bibliography` or
both as a source of references.  `csl` and `citation-abbreviations`
are optional.  If `csl` is not provided, a default stylesheet
will be used (either `~/.pandoc/default.csl` or a version of
`chicago-author-date.csl`).

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
    `medline`, `copac`, `mods`, and `json`.

# NOTES

## General

If you use a biblatex database, closely follow the specifications in the
"Database Guide" section of the biblatex manual (currently 2.8a).

If you use a CSL-YAML or CSL-JSON database, or a CSL-YAML metadata section in
your markdown document, follow the "Citation Style Language 1.0.1 Language
Specification" (<http://citationstyles.org/downloads/specification.html>).
Particularly relevant are
<http://citationstyles.org/downloads/specification.html#appendix-iii-types>
(which neither comments on usage nor specifies required and optional fields) and
<http://citationstyles.org/downloads/specification.html#appendix-iv-variables>
(which does contain comments).

## Titles: Title vs. Sentence Case

If you are using a bibtex or biblatex bibliography, then observe
the following rules:

  - English titles should be in title case.  Non-English titles should
    be in sentence case, and the `langid` field in biblatex should be
    set to the relevant language.  (The following values are treated
    as English:  `american`, `british`, `canadian`, `english`,
    `australian`, `newzealand`, `USenglish`, or `UKenglish`.)

  - As is standard with bibtex/biblatex, proper names should be
    protected with curly braces so that they won't be lowercased
    in styles that call for sentence case.  For example:

        title = {My Dinner with {Andre}}

  - In addition, words that should remain lowercase (or camelCase)
    should be protected:

        title = {Spin Wave Dispersion on the {nm} Scale}

    Though this is not necessary in bibtex/biblatex, it is necessary
    with citeproc, which stores titles internally in sentence case,
    and converts to title case in styles that require it.  Here we
    protect "nm" so that it doesn't get converted to "Nm" at this stage.

If you are using a CSL bibliography (either JSON or YAML), then observe
the following rules:

  - All titles should be in sentence case.

  - Use the `language` field for non-English titles to prevent their
    conversion to title case in styles that call for this. (Conversion
    happens only if `language` begins with `en` or is left empty.)

  - Protect words that should not be converted to title case using
    this syntax:

        Spin wave dispersion on the <span class="nocase">nm</span> scale

## Conference Papers, Published vs. Unpublished

For a formally published conference paper, use the biblatex entry type
`inproceedings` (which will be mapped to CSL `paper-conference`).

For an unpublished manuscript, use the biblatex entry type `unpublished`
without an `eventtitle` field (this entry type will be mapped to CSL
`manuscript`).

For a talk, an unpublished conference paper, or a poster presentation, use the
biblatex entry type `unpublished` with an `eventtitle` field (this entry type
will be mapped to CSL `speech`). Use the biblatex `type` field to indicate the
type, e.g. "Paper", or "Poster". `venue` and `eventdate` may be useful too,
though `eventdate` will not be rendered by most CSL styles. Note that `venue`
is for the event's venue, unlike `location` which describes the publisher's
location; do not use the latter for an unpublished conference paper.

# AUTHORS

Andrea Rossato and John MacFarlane.

# SEE ALSO

`pandoc` (1), `pandoc_markdown` (5).

The pandoc-citeproc source code and all documentation may be downloaded
from <http://github.com/jgm/pandoc-citeproc/>.
