Contributing to pandoc-citeproc
===============================

Have a question?
----------------

Ask on [pandoc-discuss].

Found a bug?
------------

Bug reports are welcome!  Please report all bugs on
pandoc-citeproc's [issue tracker].

Before you submit a bug report, search the [open issues] *and* [closed issues]
to make sure the issue hasn't come up before. Also, check the
[pandoc(1)] man page and the [Citations section from the pandoc
User's Guide] for anything relevant.

Make sure you can reproduce the bug with the latest released
version of pandoc and pandoc-citeproc (or, even better, the
development version).

Your report should give detailed, *reproducible* instructions, including

  * the pandoc version (check using `pandoc --version`)
  * the pandoc-citeproc version (check using `pandoc-citeproc --version`)
  * the exact command line used
  * the exact input used
  * the output received
  * the output you expected instead

A small test case (just a few lines) is ideal.  If your input is large,
try to whittle it down to a *minimum working example*.

For an example of an ideal bug report, see
[#267](https://github.com/jgm/pandoc-citeproc/issues/267).

Patches and pull requests
-------------------------

Patches and pull requests are welcome.  If your proposed change might
be controversial, it is a good idea to discuss it first on [pandoc-discuss].

Please follow these guidelines:

1.  Each patch (commit) should make a single logical change (fix a bug, add
    a feature, clean up some code, add documentation).  Everything
    related to that change should be included (including tests and
    documentation), and nothing unrelated should be included.

2.  The first line of the commit message should be a short description
    of the whole commit (ideally <= 50 characters).  Then there should
    be a blank line, followed by a more detailed description of the
    change.

3.  Follow the stylistic conventions you find in the existing
    code.  Use spaces, not tabs, and wrap code to 80 columns.
    Always include type signatures for top-level functions.

4.  Your code should compile without warnings (`-Wall` clean).

5.  Run the tests to make sure your code does not introduce new bugs.
    (See below under [Tests](#tests).)  All tests should pass.

6.  You should add a regression test for the bug you are fixing.  (See
    below under [Tests](#tests).)

7.  If you are adding a new feature, include updates to the
    markdown source for the man page [pandoc-citeproc(1)].

8.  All code must be released under the general license
    governing pandoc-citeproc.

9.  It is better not to introduce new dependencies.  Dependencies on
    external C libraries should especially be avoided.

10. We aim for compatibility with ghc versions from 7.10.3 to the
    latest release.

Tests
-----

Tests can be run as follows:

    cabal install --only-dependencies --enable-tests
    cabal configure --enable-tests
    cabal build
    cabal test

or, if you're using [stack],

    stack setup
    stack test

The test program is `tests/test-pandoc-citeproc.hs`.

The easiest way to create a test is to make a small markdown
file that includes the needed references in metadata, and that
tests the issue. In the case of issue
[#267](https://github.com/jgm/pandoc-citeproc/issues/267) this
would be

```
Foo [@k].

# References {-}

---
csl: tests/journal-of-common-market-studies.csl
references:
- id: k
  type: article-journal
  author:
  - family: Koshland
    given: D. E.
  issued:
  - year: '1988'
  title: Random samples
  container-title: Science
  page: '1261'
  volume: '240'
  issue: '4857'
  language: en-US
```

Save this as `issue267.md`.  Since `tests` doesn't currently
contain `journal-of-common-market-studies.csl`, you'd need to
add this file and commit it to the repository.

Then convert the markdown file to native:

    pandoc issue267.md -s -o tests/issue267.in.native

Then run pandoc-citeproc on this this to generated the "expected" result:

    pandoc tests/issue267.in.native -s -F pandoc-citeproc -o tests/issue267.expected.native

Check the resulting file carefully for correctness. (You can
convert it to HTML using pandoc to make this easier.) If all is
well, add tests/issue267.*.native to the repository.

[pandoc-discuss]: http://groups.google.com/group/pandoc-discuss
[pandoc-citeproc(1)]: https://github.com/jgm/pandoc-citeproc/blob/master/man/pandoc-citeproc.1.md
[issue tracker]: https://github.com/jgm/pandoc-citeproc/issues
[open issues]: https://github.com/jgm/pandoc-citeproc/issues
[closed issues]: https://github.com/jgm/pandoc-citeproc/issues?q=is%3Aissue+is%3Aclosed
[Citations section from the pandoc User's Guide]: https://pandoc.org/MANUAL.html#citations


