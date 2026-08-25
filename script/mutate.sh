#!/usr/bin/env sh

# Applies one mutation, runs one narrow test subtree, prints the FIRST failing
# assertion, and puts the file back. Run it with --help for the usage.
#
# Why it is shaped this way; each of these is a hazard that already cost this
# project something, so none of them is a style preference:
#
# - The backup lives outside the tree and comes back from a `trap`, because
#   `git checkout` has lost real edits and a stray `Foo.hs.bak` shows up in a
#   concurrent session's `git status`.
# - `--test-show-details=direct` is passed rather than relied on. Measured
#   2026-08-24 on cabal 3.16.1.0, the default (`failures`) does print the
#   failing case's message, so this is not what makes the parser work -- it
#   pins the setting, and streams rather than routing through a log file, so a
#   `cabal.project` that ever set `never` or `failures`-by-log cannot silently
#   leave the caller with a summary and no assertion.
# - No `--timeout` is passed. `Pawl.Test.testTree` sets per-subtree budgets with
#   `Tasty.localOption`, which beats the command line, so one here would be
#   ineffective for those subtrees and misleading everywhere else (#2113).
# - PATTERN is required rather than defaulting to the whole suite, so that a
#   mutation run stays a minute instead of twenty.
# - A corrupt shared GHC job semaphore (`semWait: invalid argument`) is retried
#   once with `--no-semaphore -j4`, and can also be worked around by passing
#   those flags in, because cabal has been seen to HANG on the corrupt semaphore
#   rather than exit, which no retry can reach. They are not passed
#   unconditionally: the semaphore is what keeps concurrent worktrees off each
#   other's cores.
# - Nothing is killed and nothing runs in the background. `pkill` reaches other
#   agents' worktrees.
# - `sed -i` is spelled differently by GNU and BSD sed, and both are reachable
#   here (the dev shell supplies GNU, macOS supplies BSD), so the mutation is
#   written through a temporary file instead.

set -o errexit
set -o nounset

self=$0

usage() {
  cat <<'EOF'
mutate.sh FILE SED_EXPR PATTERN [CABAL_ARG...]

Applies SED_EXPR to FILE, runs the tasty subtree PATTERN selects, prints the
first failing assertion, and restores FILE.

  FILE       the file to mutate
  SED_EXPR   a sed script; it must actually change FILE
  PATTERN    a tasty pattern, passed as -p; required, and keep it narrow
  CABAL_ARG  extra arguments for `cabal test`; `--no-semaphore -j4` when the
             shared GHC semaphore is corrupt and cabal hangs on it instead of
             failing, which the built-in retry cannot reach

This reports WHICH assertion went red. It does NOT judge whether that assertion
is the gameplay-level one. A cheap proxy ordered ahead of the behavioural
assertion -- a prompt count, a zone size, a list length -- absorbs the mutation
and reports itself, and reading its label as confirmation is the mistake this
script exists to make visible, not to make for you. Deciding is still yours;
see docs/agents/implementing.md, "Mutation testing".

Exit status is the outcome, not the test run's:

  0  RED           a case failed; its first assertion is printed
  1  NOTHING RED   the subtree passed, so nothing in it observes the mutation
  2  usage error, or a mutation that changed nothing
  3  the mutated source did not build, or the run produced no tasty summary
  4  PATTERN matched no tests at all

Run it from the repository root; it builds the `pawl-test-suite` target, so it
needs cabal.project.local in place for +pedantic like any other build.
EOF
}

case ${1:-} in
  -h | --help)
    usage
    exit 0
    ;;
esac

if [ $# -lt 3 ]; then
  echo "$self: expected FILE SED_EXPR PATTERN [CABAL_ARG...]" >&2
  usage >&2
  exit 2
fi

file=$1
sed_expr=$2
pattern=$3
shift 3

if [ ! -f "$file" ]; then
  echo "$self: no such file: $file" >&2
  exit 2
fi

work=$(mktemp -d)
backup=$work/original
cp "$file" "$backup"

restore() {
  cp "$backup" "$file"
  rm -rf "$work"
}
trap restore EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

sed "$sed_expr" "$backup" >"$work/mutated"
cp "$work/mutated" "$file"

if cmp -s "$backup" "$file"; then
  echo "$self: the sed expression changed nothing, so there is no mutation" >&2
  exit 2
fi

log=$work/output

# --test-option, singular, passes one argument verbatim; the plural
# --test-options takes a string that cabal splits on whitespace, so a pattern
# naming a test case by its own words arrives as several arguments and tasty
# rejects the second one.
run() {
  cabal test pawl-test-suite \
    --test-show-details=direct \
    --test-option=-p \
    --test-option="$pattern" \
    --test-option=--hide-successes \
    ${1:+"$@"} >"$log" 2>&1 || true
}

run "$@"
if grep -q 'semWait: invalid argument' "$log"; then
  echo 'shared GHC semaphore is corrupt; retrying with --no-semaphore -j4'
  run --no-semaphore -j4
fi

echo "MUTATION: $sed_expr"
echo "FILE:     $file"
echo "PATTERN:  $pattern"
echo

summary=$(grep -E '^(All [0-9]+ tests passed|[0-9]+ out of [0-9]+ tests failed)' "$log" || true)

if [ -z "$summary" ]; then
  echo 'MUTATION DID NOT COMPILE (or the suite never reported)'
  echo 'A real red arrives this way: -Wunused-local-binds under +pedantic turns'
  echo 'a delete-the-use mutation into a build failure. Neutralize the value'
  echo 'instead of deleting its use, then re-run.'
  echo
  cat "$log"
  exit 3
fi

case $summary in
  'All 0 tests passed'*)
    echo "PATTERN MATCHED NO TESTS: $pattern"
    echo 'tasty reports an empty selection as a pass, so this is not a green.'
    exit 4
    ;;
  'All '*)
    echo "NOTHING WENT RED ($summary)"
    echo 'The mutated line has no observer IN THIS SUBTREE -- which is a finding'
    echo 'either way: either the case proves nothing, or the pattern is pointed'
    echo 'at the wrong one. Per docs/agents/implementing.md, do not read it as'
    echo 'coverage and do not close the issue on it.'
    exit 1
    ;;
esac

echo "RESULT: $summary"
echo
echo 'FIRST FAILING ASSERTION (the gameplay-level one, or a proxy ahead of it?):'
first=$(awk '
  state == 2 { next }
  /^[[:space:]]*$/ { if (state == 1) state = 2; next }
  {
    indent = match($0, /[^ ]/) - 1
    if (state == 1) {
      if (indent > failIndent) { print; next }
      state = 2
      next
    }
    if ($0 ~ /:[[:space:]]+FAIL/) {
      for (i = 0; i < indent; i++) if (i in header) print header[i]
      print
      failIndent = indent
      state = 1
      next
    }
    header[indent] = $0
    for (i = indent + 1; i <= depth; i++) delete header[i]
    depth = indent
  }
' "$log")

# Falling back to the whole log rather than printing nothing: the summary says a
# case failed, so silence here would mean the block layout changed under this
# parser, and silence reads far too much like "no failure".
if [ -n "$first" ]; then
  printf '%s\n' "$first"
else
  echo '(no FAIL block recognized; the whole run follows)'
  cat "$log"
fi

exit 0
