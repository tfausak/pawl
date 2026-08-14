#!/usr/bin/env sh

# An ELISION citation must die in the commit that closes its issue. `(#N)` alone
# cannot be checked for that, because it carries two genres grep cannot tell
# apart: the elision -- "this is not implemented (#N)" -- and the historical
# reference -- "this bug was #1116", "landed in #1303" -- which legitimately
# outlives the issue it names. Mixed, a rotted elision is invisible: two of them
# sat in the tree claiming a capability was missing that had landed months
# earlier, and one made an issue look unreachable for weeks.
#
# The discriminator is the WORDING the elision genre already uses, so nothing had
# to be retrofitted onto hundreds of sites. A comment paragraph that says "not
# implemented" is an elision paragraph, and every `(#N)` in it must name an OPEN
# issue. A paragraph that elides something in other words marks the citation
# `(gap #N)` instead, which is checked wherever it appears. A historical
# reference inside an elision paragraph is written without the parentheses --
# `see #1116` -- which is how the two are told apart when they share one.
#
# Usage: `check-gaps.sh [FILE...]`, from the repository root, defaulting to every
# tracked Haskell file. Reports each dead citation as `FILE:LINE` and exits 1.
#
# NEEDS THE NETWORK, for the reason `check-census.sh` gives: an issue's state
# lives on GitHub. It runs in CI and by hand, not as a `.hooky.kdl` hook.

set -o errexit
set -o nounset

if [ "$#" -eq 0 ]; then
  set -- $(git ls-files '*.hs')
fi

closed=$(mktemp)
trap 'rm -f "$closed"' EXIT
gh issue list --state closed --limit 5000 --json number --jq '.[].number' >"$closed"

awk '
  function flush(  i) {
    if (elides) {
      for (i = 1; i <= cited; i++) {
        if (numbers[i] in closed) {
          printf "%s:%d: closed issue cited by an elision: #%s\n", file, lines[i], numbers[i]
          status = 1
        }
      }
    }
    elides = 0
    cited = 0
  }

  FNR == NR { closed[$1] = 1; next }

  FNR == 1 { flush() }

  {
    file = FILENAME
    rest = $0
    # `(gap #N)` is checked wherever it appears, so it needs no paragraph.
    while (match(rest, /\(gap #[0-9]+\)/)) {
      number = substr(rest, RSTART + 6, RLENGTH - 7)
      rest = substr(rest, RSTART + RLENGTH)
      if (number in closed) {
        printf "%s:%d: closed issue cited by a gap: #%s\n", FILENAME, FNR, number
        status = 1
      }
    }
  }

  # A comment paragraph runs to the first blank comment line or the first line of
  # code, which is where the citations gathered below are judged.
  !/^[[:space:]]*--/ { flush(); next }
  /^[[:space:]]*--[[:space:]]*$/ { flush(); next }

  {
    if (tolower($0) ~ /not implemented/) { elides = 1 }
    rest = $0
    while (match(rest, /\(#[0-9]+\)/)) {
      number = substr(rest, RSTART + 2, RLENGTH - 3)
      rest = substr(rest, RSTART + RLENGTH)
      cited = cited + 1
      numbers[cited] = number
      lines[cited] = FNR
    }
  }

  END { flush(); exit status }
' "$closed" "$@"
