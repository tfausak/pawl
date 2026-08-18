#!/usr/bin/env sh

# A census issue tracks a run of CR rules, and this checks it by the evidence
# rules below.
#
# EPONYMY, in `check`: a census's "Not implemented" block must not name a
# capability the tree already carries. Every census-tracked unit is supposed to
# move its row in the PR that lands it, and twice now one did not: PR #1485 left
# amass under #876's not-implemented block, PR #1527 left ward under #877's.
# Prose did not fix it, so this checks it.
#
# COVERAGE, in `check_rules`: every keyword rule in `docs/rules.txt` is a row in
# one of a census's two blocks, and every rule number in either block is a rule.
# Eponymy is blind to a rule that is in NEITHER block, because it only ever
# reads rows that exist -- which is how CR 702.143 foretell stayed untracked on
# #877 from PR #1487 (`Keyword.Foretell`, `Action.Foretell`,
# `data/cards/augury-raven.json`) until a manual read, and how CR 702.169 solved
# sat under "Not implemented" after PR #1708 landed it as `Designation.Solved`,
# eponymous with no `Keyword` constructor at all. Coverage sees both.
#
# WRITTEN NAMES, in `check_names`: the `Type.Constructor` tokens a census writes
# in backticks are exactly the constructors of the type it tracks, minus a
# documented exempt list. This is what serves #875 (CR 116), whose prose names
# ("Play a land", "Roll the planar die") will never be eponymous with a
# constructor -- but whose implemented rows write the constructor out instead.
# Set equality has no blind spot in either direction, and the reverse direction
# is what earns it: a PR that adds an `Action` arm and leaves #875 alone goes
# red, which is the #1485/#1527 shape prose twice failed to prevent.
#
# Usage: `check-census.sh`, from the repository root. Reports each stale row as
# `#ISSUE: RULE NAME -- Type.Constructor exists`, each coverage defect as
# `#ISSUE: CR RULE has no row` or `#ISSUE: row RULE is no rule`, and each name
# mismatch as `#ISSUE: Type.Constructor has no row` or `#ISSUE: row names
# Type.Constructor, which is no constructor`, and exits 1. The censuses are read
# once per assertion, so a run makes one `gh` call per assertion.
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
# Coverage's evidence is the CR itself, not the tree. The obvious alternative --
# every constructor of the tracked type is named somewhere in the body -- was
# measured and is unusable: #876 tracks CR 701 keyword actions while `Effect` is
# the whole effect vocabulary, so `Draw`, `GainLife`, `PhaseOut` and forty more
# are named by no row and never will be. The rule numbers are the only thing the
# two censuses and the CR agree on. See #1786.
#
# CR 701.1 and CR 702.1 are the prose definitions of a keyword action and a
# keyword ability, and both bodies say in as many words that they are excluded.
# They separate mechanically rather than by a hardcoded pair: their heading text
# after the number is a sentence and so contains a period, where every keyword
# heading is a bare title -- `702.163. For Mirrodin!`, `702.186. ∞ (Infinity)`,
# `702.145. Daybound and Nightbound`. Measured over every CR 701 and CR 702
# heading in `docs/rules.txt`, exactly those two carry a period in the title.
#
# Only the two blocks count as row locations. #876's "The rows that are a
# subsystem, not an opcode" and #877's "The rows that are not one-line
# additions" annotate rows that also appear in the blocks, and keeping that true
# is the invariant the row-to-CR direction enforces. Prose inside a block is
# harmless as long as the rules it names are real: #876's "CR 701.10 double and
# CR 701.12 exchange are not in this list" passes for that reason.
#
# Written names must be BACKTICK-ANCHORED. #875's own status paragraph reads
# "`Pawl.Types.Action.Action` carries an arm per implemented row", and an
# unanchored `Action\.[A-Z]...` pattern reads `Action.Action` out of it -- a name
# no constructor has, so the check would be red on arrival. Requiring a backtick
# immediately before the type name drops the qualified form while still admitting
# a future `Pawl.Engine.Action.Foo` written in backticks, which a "reject when
# preceded by a period" rule would not.
#
# The `Action` arms `Pass`, `Cast`, `Activate` and `ActivateManaAbility` are
# exempt, and #875's body already names all four in prose. CR 116.1 defines a
# special action as one a player takes with priority that does not use the stack,
# and CR 116.2 enumerates the twelve; passing, casting and activating are not
# among them (CR 117.1a is the cast permission, CR 601 and CR 602 the
# procedures). They are the priority actions AROUND the special ones, so #875
# carries no row for them. `Concede` is absent for a different reason -- CR
# 104.3a puts it outside the priority enumeration and it is no `Action`
# constructor at all, so set equality never sees it.

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

# issue, rule prefix
check_rules() {
  gh issue view "$1" --json body --jq .body >"$body"
  awk -v issue="$1" -v prefix="$2" '
    # The CR first: every `70N.M.` heading whose title is a bare keyword name.
    FNR == NR {
      if (match($0, "^" prefix "\\.[0-9]+\\. ")) {
        number = substr($0, 1, RLENGTH - 2)
        title = substr($0, RLENGTH + 1)
        if (title !~ /\./) {
          rule[number] = 1
          rules = rules + 1
        }
      }
      next
    }
    # Then the census body: every rule number under either block heading.
    /^#+ / { block = ($0 ~ /^#+ +(Implemented|Not implemented) *$/) }
    block {
      rest = $0
      while (match(rest, prefix "\\.[0-9]+")) {
        row[substr(rest, RSTART, RLENGTH)] = 1
        rows = rows + 1
        rest = substr(rest, RSTART + RLENGTH)
      }
    }
    # As in `check`, a body whose headings were renamed away parses to nothing,
    # and so does a CR file this prefix no longer matches.
    END {
      for (number in rule) {
        if (!(number in row)) {
          printf "#%s: CR %s has no row\n", issue, number
          bad = 1
        }
      }
      for (number in row) {
        if (!(number in rule)) {
          printf "#%s: row %s is no rule\n", issue, number
          bad = 1
        }
      }
      if (!rules) {
        printf "#%s: no CR %s rule headings in docs/rules.txt\n", issue, prefix
        bad = 1
      }
      if (!rows) {
        printf "#%s: no rows under either block heading\n", issue
        bad = 1
      }
      exit bad
    }
  ' docs/rules.txt "$body" || status=1
}

# issue, module, type, exempt constructors separated by spaces
check_names() {
  gh issue view "$1" --json body --jq .body >"$body"
  awk -v issue="$1" -v module="$2" -v type="$3" -v exempt="$4" '
    BEGIN {
      n = split(exempt, e, / /)
      for (i = 1; i <= n; i++) skip[e[i]] = 1
    }
    # The tracked type first, by the constructor pattern check() already uses.
    FNR == NR {
      if ($0 ~ "^data " type "( |$)") { inside = 1; next }
      if (inside && $0 ~ /^  deriving/) { inside = 0 }
      if (inside && match($0, /^(  [=|] |    )[A-Z][A-Za-z0-9_]*/)) {
        name = $0
        sub(/^(  [=|] |    )/, "", name)
        sub(/[^A-Za-z0-9_].*$/, "", name)
        ctors = ctors + 1
        if (!(name in skip)) ctor[name] = 1
      }
      next
    }
    # Then the census body: every `Type.Constructor` written in backticks.
    {
      rest = $0
      while (match(rest, "`" type "\\.[A-Z][A-Za-z0-9_]*")) {
        name = substr(rest, RSTART + length(type) + 2, RLENGTH - length(type) - 2)
        rest = substr(rest, RSTART + RLENGTH)
        if (name in skip) continue
        rows = rows + 1
        row[name] = 1
      }
    }
    # Exempt names are dropped from BOTH sides, so a row that did name an exempt
    # constructor is ignored rather than falsely reported.
    END {
      for (name in ctor) {
        if (!(name in row)) {
          printf "#%s: %s.%s has no row\n", issue, type, name
          bad = 1
        }
      }
      for (name in row) {
        if (!(name in ctor)) {
          printf "#%s: row names %s.%s, which is no constructor\n", issue, type, name
          bad = 1
        }
      }
      # As in check(), a body or a module that parses to nothing would pass
      # vacuously. The row guard counts non-exempt tokens only, so a body edited
      # down to nothing but exempt names is red; the constructor guard counts
      # every arm, so it fires on a renamed type rather than on a wide exempt
      # list.
      if (!rows) {
        printf "#%s: no `%s.` rows\n", issue, type
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

check_rules 876 701
check_rules 877 702

check_names 875 source/libraries/types/Pawl/Types/Action.hs Action \
  'Pass Cast Activate ActivateManaAbility'

exit "$status"
