#!/bin/bash

help() {
  cat <<EOF
Usage: ./mktest.sh issueXXX.md
                   [-g : generate a new test case from a .md file]
                   [-d : diff expected and actual]
                   [-e : generate TWO html previews, issueXXX.actual.html and issueXXX.expected.html]
                   [-o : open the HTML preview(s)]
                   [-p : generate and open an html preview]
                   [-r : reverse native back to markdown]
                   [-s : use system pandoc]

For most commands, issueXXX.md may not exist, but it's just a convenient way of
addressing the native test files named tests/issueXXX.*.native.

pandoc-citeproc binary:

    The default is to use the built pandoc-citeproc executable (ie $ stack exec pandoc --).
    This is the one that $ stack build outputs.
    You can use the one on your normal \$PATH with the -s flag, or specify
    a particular one:

    $ CITEPROC="/usr/local/bin/pandoc-citeproc" ./mktest.sh ...

    With your regular path, this might be a previous passes-tests build (ie from
    $ make) in ~/.local/bin, so be careful.

    The same applies for PANDOC="...".

If you're making a new test, generate the native files with:

    $ touch issueXXX.md
    # ... edit issueXXX.md
    $ ./mktest.sh issueXXX.md -g

    You can use

    $ ./mktest.sh issueXXX.md -gpo

    ... to also open up an HTML preview in your browser (using open or xdg-open).

    Note that ideally your test case fails without your proposed changes, so you
    should see a difference with

    $ CITEPROC="/path/to/buggy/pandoc-citeproc" ./mktest.sh issueXXX.md -d
      (see -d flag below)

For reproducing a single test diff with colours or in a GUI diff tool, you can
produce the actual and expected outputs:

    $ ./mktest.sh issueXXX.md -d
    # => ./test-diffs/issueXXX.expected.native and ./test-diffs/issueXXX.actual.native are produced.
    # => a diff is shown

    You may specify your diff tool, which is 'git diff --no-index --' by default

    $ export DIFFTOOL="ksdiff"
    $ ...

Similarly, you may want the HTML for both, in case reading diffs is tricky:

    $ ./mktest.sh issueXXX.md -e [-o]
    # => ./test-diffs/issueXXX.expected.html and ./test-diffs/issueXXX.actual.html are produced.
    # => Adding -o will open both in your browser.

If you're trying to understand one of the failing tests and you want some markdown,
(maybe you need to rewrite a test, hopefully not) try "reverse mode":

    # given tests/issueXXX.*.native
    $ ./mktest.sh issueXXX.md -r

    The conversion will be sitting in issueXXX.md.
    Also use -po by itself to open the expected output as HTML.

EOF
  exit
}


# default to built pandoc
stack_pandoc=$(stack exec which pandoc)
PANDOC="${PANDOC:-$stack_pandoc}"
stack_citeproc=$(stack exec which pandoc-citeproc)
CITEPROC="${CITEPROC:-$stack_citeproc}"
GENERATE=0
REVERSE=0
SHOULD_PREVIEW=0
DOUBLE_PREVIEW=0
OPEN=0
DIFF=0
DIFFTOOL="${DIFFTOOL:-git diff --no-index --}"

reext() {
  echo $(echo $1 | cut -f 1 -d .)$2
}

needfile() {
  if ! test -s $1; then
    echo "no such file $IN_NATIVE"
    exit 1
  fi
}

openit() {
  if test "$OPEN" -eq 1; then
    if hash open 2>/dev/null; then
      open $1
    elif hash xdg-open 2>/dev/null; then
      xdg-open $1
    fi
  fi
}

MD="none"
if test "$1" != "-h"; then
  MD="$1"
  shift 1
fi

while getopts ":sgrdpeoh" opt; do
  case ${opt} in
    s ) PANDOC="$(which pandoc)"; CITEPROC="$(which pandoc-citeproc)" ;;
    g ) GENERATE=1 ;;
    r ) REVERSE=1 ;;
    d ) DIFF=1 ;;
    p ) SHOULD_PREVIEW=1 ;;
    e ) DOUBLE_PREVIEW=1 ;;
    o ) OPEN=1 ;;
    h ) help ;;
    \? ) help ;;
  esac
done
shift $((OPTIND -1))

# echo using   PANDOC=\""$PANDOC"\"
# echo using CITEPROC=\""$CITEPROC"\"

OUTDIR="./test-diffs"
mkdir -p $OUTDIR
IN_NATIVE=tests/$(reext $MD .in.native)
EXPECTED_NATIVE=tests/$(reext $MD .expected.native)

if test "$GENERATE" -eq 1; then
  needfile $MD
  $PANDOC $MD -s -o $IN_NATIVE && \
    $PANDOC $IN_NATIVE -s -F $CITEPROC -o $EXPECTED_NATIVE
elif test "$REVERSE" -eq 1; then
  needfile $IN_NATIVE
  pandoc $IN_NATIVE -f native -s -t markdown -o $MD
  exit $?
fi

if test "$DIFF" -eq 1; then
  needfile $IN_NATIVE
  needfile $EXPECTED_NATIVE
  ACTUAL=$OUTDIR/$(reext $MD .actual.native)
  EXPECTED=$OUTDIR/$(reext $MD .expected.native)
  $PANDOC $IN_NATIVE -F $CITEPROC -s -o $ACTUAL
  $PANDOC $EXPECTED_NATIVE -s -o $EXPECTED
  if git diff --no-index -- $EXPECTED $ACTUAL 2>&1 >/dev/null; then
    exit 0
  else
    $DIFFTOOL $EXPECTED $ACTUAL
    exit $?
  fi

elif test "$DOUBLE_PREVIEW" -eq 1; then
  needfile $IN_NATIVE
  needfile $EXPECTED_NATIVE
  ACTUAL=$OUTDIR/$(reext $MD .actual.html)
  EXPECTED=$OUTDIR/$(reext $MD .expected.html)
  $PANDOC $IN_NATIVE -F $CITEPROC --metadata title:$ACTUAL -s -t html5 -o $ACTUAL
  openit $ACTUAL
  $PANDOC $EXPECTED_NATIVE --metadata title:$EXPECTED -s -t html5 -o $EXPECTED
  openit $EXPECTED

elif test "$SHOULD_PREVIEW" -eq 1; then
  needfile $EXPECTED_NATIVE
  EXPECTED=$OUTDIR/$(reext $MD .expected.html)
  $PANDOC $EXPECTED_NATIVE --metadata title:$EXPECTED -s -t html5 -o $EXPECTED
  openit $EXPECTED

fi

