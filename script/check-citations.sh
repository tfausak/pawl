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
# A citation still counts when a comment wrapped between the `CR` and the
# number. Matching within a single line silently skipped every one of those,
# and wrapped prose is where a citation is likeliest to rot unread. Such a
# citation is reported at the line the `CR` sits on, and the exemption spans
# the wrap as well.

set -o errexit
set -o nounset

# `docs/progress.md` and `docs/superpowers/` are frozen records of what was true
# under an older revision of the rules, so a renumbering does not make them
# wrong. Naming them explicitly still checks them.
if [ "$#" -eq 0 ]; then
  set -- $(git ls-files | grep -v -e '^docs/progress\.md$' -e '^docs/superpowers/')
fi

exec awk '
  # `before` is the text leading up to the citation, which is all the exemption
  # needs; for a wrapped citation that is the tail of the previous line.
  function check(rule, before, file, line) {
    if (before !~ /there is no $/ && !(rule in defined)) {
      printf "%s:%d: no such rule: CR %s\n", file, line, rule
      status = 1
    }
  }

  FNR == NR {
    # A section and a numbered rule lead with `100.` or `605.1.`; a subrule
    # leads with `605.1a`, no trailing period. Accepting either spelling on
    # either shape is deliberate: the document is not consistent, writing
    # `119.1d.` with a period and `606.5` without.
    if ($1 ~ /^[0-9]+(\.[0-9]+[a-z]*)?\.?$/) {
      rule = $1
      sub(/\.$/, "", rule)
      defined[rule] = 1
    }
    next
  }
  {
    # A wrap cannot span two files. `FNR` restarts on the first record of each
    # file, and an empty file in the list contributes no record at all, so this
    # fires for every file that could carry a wrap in.
    if (FNR == 1) pending = 0

    # The previous line ended in a bare `CR`, so the number opens this one,
    # behind whatever leader the comment syntax uses.
    if (pending) {
      head = $0
      sub(/^[ \t]*(--+|#+|\/\/|\*|>)?[ \t]*/, "", head)
      if (match(head, /^[0-9]+(\.[0-9]+[a-z]*)?/)) {
        check(substr(head, RSTART, RLENGTH), pendingBefore, pendingFile, pendingLine)
      }
      pending = 0
    }

    rest = $0
    while (match(rest, /CR [0-9]+(\.[0-9]+[a-z]*)?/)) {
      before = substr(rest, 1, RSTART - 1)
      rule = substr(rest, RSTART + 3, RLENGTH - 3)
      rest = substr(rest, RSTART + RLENGTH)
      check(rule, before, FILENAME, FNR)
    }

    # Hold a trailing bare `CR` over to the next line. The leading character
    # class keeps this off words that merely end in those two letters. The
    # `index` guard is what keeps the default whole-tree sweep cheap: it skips
    # the anchored match on every line of the card corpus, which is most of
    # the tree and contains no citations at all.
    if (index($0, "CR") && $0 ~ /(^|[^A-Za-z0-9])CR[ \t]*$/) {
      pendingBefore = $0
      sub(/CR[ \t]*$/, "", pendingBefore)
      pendingFile = FILENAME
      pendingLine = FNR
      pending = 1
    }
  }
  END { exit status }
' docs/rules.txt "$@"
