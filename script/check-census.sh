#!/usr/bin/env sh

# A census issue's "Not implemented" block must not name a capability the tree
# already carries. Every census-tracked unit is supposed to move its row in the
# PR that lands it, and twice now one did not: PR #1485 left amass under #876's
# not-implemented block, PR #1527 left ward under #877's. Prose did not fix it,
# so this checks it.
#
# Usage: `check-census.sh`, from the repository root. Reports each stale row as
# `#ISSUE: RULE NAME -- Type.Constructor exists` and exits 1.
#
# NEEDS THE NETWORK, which is why it is not a `.hooky.kdl` hook: the censuses
# live in GitHub issue bodies rather than in the tree, and `hooky fix` is offline
# and fast. A hook that skipped itself when `gh` could not reach GitHub would
# pass vacuously exactly when it mattered. Committing a copy of the tables and
# syncing it would put the rot one level down instead of removing it. So this
# runs in CI, where the network is a given, and by hand.
#
# The evidence is EPONYMY: a row's name, stripped to letters and digits, naming
# a constructor of the type that census tracks. That is a partial test by
# design -- a capability landed under another name (CR 701.16 investigate is
# `Effect.Create`) is invisible to it -- but it is exact in the direction it
# does speak, and both misses it is here to catch were eponymous.
#
# CR 116's census (#875) is not checked. Its rows are keyed to the subsystem
# each is gated on rather than to a constructor name ("Turn a face-down creature
# face up" is `Action.TurnFaceUp`), so there is nothing mechanical to compare.

set -o errexit
set -o nounset

status=0
body=$(mktemp)
trap 'rm -f "$body"' EXIT

# issue, module, type
check() {
  gh issue view "$1" --json body --jq .body >"$body"
  awk -v issue="$1" -v module="$2" -v type="$3" '
    # The tracked type first: every constructor of its data declaration.
    FNR == NR {
      if ($0 ~ "^data " type "( |$)") { inside = 1; next }
      if (inside && $0 ~ /^  deriving/) { inside = 0 }
      if (inside && match($0, /^(  [=|] |    )[A-Z][A-Za-z0-9_]*/)) {
        name = $0
        sub(/^(  [=|] |    )/, "", name)
        sub(/[^A-Za-z0-9_].*$/, "", name)
        exists[toupper(name)] = name
        ctors = ctors + 1
      }
      next
    }
    # Then the census body: the rows of its "Not implemented" block, which are
    # `NNN.N Name` cells separated by `|`.
    /^#+ / { block = ($0 ~ /Not implemented/) }
    block {
      n = split($0, cells, /\|/)
      for (i = 1; i <= n; i++) {
        if (match(cells[i], /[0-9]+\.[0-9]+[a-z]* +[A-Za-z]/)) {
          row = substr(cells[i], RSTART)
          rule = row
          sub(/ .*$/, "", rule)
          name = row
          sub(/^[^ ]+ +/, "", name)
          sub(/ +$/, "", name)
          key = toupper(name)
          gsub(/[^A-Z0-9]/, "", key)
          rows = rows + 1
          if (key in exists) {
            printf "#%s: %s %s -- %s.%s exists\n", issue, rule, name, type, exists[key]
            bad = 1
          }
        }
      }
    }
    # A body whose block was renamed away parses to nothing and would pass
    # vacuously, which is the failure `script/format-json.sh` run bare has.
    END {
      if (!rows) {
        printf "#%s: no rows under a \"Not implemented\" heading\n", issue
        bad = 1
      }
      if (!ctors) {
        printf "%s: no constructors of %s\n", module, type
        bad = 1
      }
      exit bad
    }
  ' "$2" "$body" || status=1
}

check 876 source/libraries/types/Pawl/Types/Effect.hs Effect
check 877 source/libraries/types/Pawl/Types/Keyword.hs Keyword

exit "$status"
