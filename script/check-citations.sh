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

set -o errexit
set -o nounset

# `docs/progress.md` and `docs/superpowers/` are frozen records of what was true
# under an older revision of the rules, so a renumbering does not make them
# wrong. Naming them explicitly still checks them.
if [ "$#" -eq 0 ]; then
  set -- $(git ls-files | grep -v -e '^docs/progress\.md$' -e '^docs/superpowers/')
fi

exec awk '
  FNR == NR {
    # A section and a numbered rule lead with `100.` or `605.1.`; a subrule
    # leads with `605.1a`, no trailing period.
    if ($1 ~ /^[0-9]+(\.[0-9]+[a-z]*)?\.?$/) {
      rule = $1
      sub(/\.$/, "", rule)
      defined[rule] = 1
    }
    next
  }
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
' docs/rules.txt "$@"
