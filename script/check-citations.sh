#!/usr/bin/env sh

# Every `CR <number>` in the tree must name a rule `docs/rules.txt` actually
# defines. A CR update renumbers rules without breaking anything, and an agent
# writing from memory invents subrules that never existed; both failures are
# invisible to the compiler and to the test suite.
#
# Usage: `check-citations.sh [FILE...]`, from the repository root, defaulting to
# every tracked file but the frozen ones below. Reports each unresolvable
# citation as `FILE:LINE` and exits 1.
#
# `there is no CR X` is the fixed wording for a deliberate statement that the
# rule does not exist, and is skipped. Nothing else is exempt.
#
# What counts as a defined rule is `script/cr.hs`'s answer, not one this script
# re-derives: it is the same question the lookup script asks, so a revision
# that reshapes the document must not be able to move one answer without the
# other. This script only decides what a citation looks like.

set -o errexit
set -o nounset

# `docs/progress.md` and `docs/superpowers/` are frozen records of what was true
# under an older revision of the rules, so a renumbering does not make them
# wrong. Naming them explicitly still checks them.
if [ "$#" -eq 0 ]; then
  set -- $(git ls-files | grep -v -e '^docs/progress\.md$' -e '^docs/superpowers/')
fi

defined=$(mktemp)
trap 'rm -f "$defined"' EXIT
script/cr.hs --list > "$defined"

# An empty list would report every citation in the tree, which reads as a
# thousand bad citations rather than as one broken parse.
if [ ! -s "$defined" ]; then
  echo 'script/cr.hs --list defined no rules at all' >&2
  exit 1
fi

# Not `exec`: that would replace this shell before the trap can remove the
# temporary file.
awk '
  FNR == NR { defined[$0] = 1; next }
  {
    rest = $0
    while (match(rest, /CR [0-9]+(\.[0-9]+[a-z]*)?/)) {
      before = substr(rest, 1, RSTART - 1)
      rule = substr(rest, RSTART + 3, RLENGTH - 3)
      rest = substr(rest, RSTART + RLENGTH)
      if (before !~ /there is no $/ && !(rule in defined)) {
        printf "%s:%d: no such rule: CR %s\n", FILENAME, FNR, rule
        status = 1
      }
    }
  }
  END { exit status }
' "$defined" "$@"
