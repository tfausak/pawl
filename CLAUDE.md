# CLAUDE.md

Guidance for Claude Code (and other agents) working in this repository.

## What Pawl is

A pure Haskell rules engine for *Magic: The Gathering*, structured as a virtual
machine with two halves:

- A closed half: the comprehensive rules (turn structure, priority, the stack,
  zones, state-based actions, the layer system, combat). Finite; it can
  genuinely be finished.

- An open half: a first-order, non-recursive, statically-analyzable effect DSL,
  loaded as runtime data, never as Haskell modules. Grows forever; that's fine.

The invariant that keeps them apart: the closed half depends on a
*classification* of effects (which layer, is it a mana ability, does it
target), never on the *identity* of an effect. The rules core must never write
`case effect of DealDamage{} -> ...`. Fusing the two halves is the single
failure mode that sinks this project --- audit for it. Keywords are the
exception that proves the rule: rule 702 is part of the rulebook, so
`case keyword of Flying` is the same kind of act as casing on `Phase`.

The second invariant: the engine never makes a player's choice. Eliding a
prompt is legitimate only for indistinguishable options, and every elision
carries an issue. Where the rules leave nothing to ask, don't prompt.

Design notes live in `docs/`. Read them by section, on demand --- reading a
whole doc "for context" front-loads tens of thousands of tokens before the
first question.

- What's next?
  `gh issue list`

- Architecture rationale:
  `docs/design.md`

- Rules ground truth:
  `docs/rules.txt`, grepped by rule number --- never memory, and never a rule
  number quoted in an issue or a brief. `script/cr.hs --number 702.21a` prints
  one rule; it errors rather than guessing when the number does not exist.

- Prior-art evidence:
  `docs/prior-art-lessons.md`

- What the milestone era established:
  `docs/progress.md` (frozen), newest first

- A landed unit's authoritative detail:
  `docs/superpowers/`

- Dispatched as a subagent?
  `docs/agents/` --- `implementing.md` if you hold the build and will open a PR,
  `researching.md` if you are read-only

## Workflow

`CONTRIBUTING.md` has the loop --- issue, branch, TDD, draft PR --- and applies
to agents as written. What it doesn't say:

- Agents usually work in a fresh `git worktree`, which starts without the
  gitignored `cabal.project.local`. Copy it in from the primary checkout before
  the first build, or a locally green build says nothing about CI.

- That file sets `semaphore: True`, so concurrent `cabal` runs in *different*
  worktrees contend on one shared GHC job semaphore --- that is the mechanism
  behind one-build-at-a-time. A killed or crashed run corrupts it; the symptom
  is `semWait: invalid argument`, and `cabal test --no-semaphore -j4` gets
  through. The likeliest killer is not a deliberate one: a tool timeout reaping
  a backgrounded `cabal` corrupts the semaphore exactly as a `pkill` does.
  Never `pkill -f 'cabal test'` --- it reaches into other agents' worktrees.
  Match your own worktree path, or wait.

- Derive against `origin/main`, not the working checkout. The primary checkout
  drifts --- it sat 47 commits behind during one drain run --- so a grep there
  reports a type as absent that landed hours ago, and every line number in an
  issue body is stale besides. `git fetch` first, then read through
  `git show origin/main:<path>` or a worktree cut from it.

- Self-review the branch before opening the PR, and fix the findings on the
  branch. At minimum: re-check every CR citation against `docs/rules.txt`, and
  re-read every comment the change touched for prose the rewrite made wrong.
  Those two reliably catch real defects here. Scale the effort to the diff.

- The PR body carries the case for merging, since the owner reads every PR that
  lands. Give all of:
  - what changed and why, with `Closes #N`
  - the CR citations behind it
  - the design calls made, and the alternatives rejected
  - how it was verified: suite count before -> after and the proving test
  - whether the diff makes the rules core case on an effect's *identity* --- an
    explicit "no" is cheap
  - what was deferred

- Keep the prose terse --- PR bodies, issue comments and code comments alike.
  Say what needs saying and stop: a line each for the points above rather than a
  section each, and a citation in place of a quoted rule. Do the verification
  work in full; just don't write it up at length.

- Mark the PR ready for review once the self-review's findings are pushed and
  the suite is green, then report it and stop. Don't wait for CI. Don't start
  the next unit either: one unit at a time per checkout, since two branches
  contend for `HEAD`.

- STAGE, then `hooky fix`. It acts on staged files, which is exactly the set
  you changed, and it covers far more than CI's `Ormolu` job: `.hooky.kdl`
  wires up ormolu, hlint, cabal-gild, `cabal check`, nixfmt, JSON formatting
  and the builtin lint rules. Running those tools one at a time both misses
  hooks and takes longer.

  Staging is the whole trick --- an unstaged file is skipped, which is where
  "hooky fix isn't enough" comes from. It rewrites files in place and then
  tells you to `git add` again. `--all` sweeps the tree instead, but it takes
  two minutes against about one second, so reach for it only when you suspect
  something landed unstaged.

- After a PR merges, before picking up the next unit, spend a few minutes on
  what the cycle taught. Three questions: did anything catch you that your own
  checks didn't (CI, a review, the owner)? did you violate an instruction? did
  you learn a project fact the repo doesn't already record? Write the answers
  down --- a project fact belongs here, in the section it bears on, folded into
  the next unit's PR rather than a PR of its own. Skip it when there is
  genuinely nothing; ceremonial notes cost more than they return.

- Most of what's left is card-driven --- it fires when a card demands it, so
  the backlog is a menu rather than a queue. Working one means finding the real
  card and adding it to `data/cards/`; that is expected, not a side quest, and
  "no producer in the pool" describes the work rather than excusing it. What is
  forbidden is building a capability no card exercises: per `docs/design.md`
  section 4, an effect is not done until a card exercises it in a
  gameplay-level test. The one good reason to stop is a *rules* reason --- the
  card turns out not to exercise the thing after all.

- The labels worth knowing are `elision`, `gap`, `rules-correctness` and `bug`,
  plus the expiry triggers `expires:milestone`, `expires:card-driven`,
  `expires:subsystem` and `expires:synthetic`. Priority labels are the owner's:
  prefer high-priority issues when picking, and never set one yourself.

  `gap` and `rules-correctness` are orthogonal and often both apply: `gap` says
  the capability does not exist, `rules-correctness` that behaviour observably
  diverges. `elision` is the one that excludes `rules-correctness` --- an
  elision is sound only while the options are indistinguishable, so once the
  divergence is observable it is not an elision any more.

  `expires:card-driven` does NOT mean "wait". It means the work includes adding
  a card to the pool to exercise the behavior, which is ordinary work. What the
  labels never say is which issues are ready: an issue carrying no `expires:*`
  is one whose trigger already fired, usually because the producer is in
  `data/cards/` transcribed a clause short.

  An `area:*` label goes on where one genuinely fits; leave it off rather than
  mislabel. Beyond the subsystem names there are `area:keywords` (CR 701/702),
  `area:effects` (the effect DSL --- opcodes, filters, quantities, slots,
  modes, durations), `area:cards` (card data, faces, layouts, schemas, lints),
  `area:stack` (CR 601/602/608) and `area:variants` (CR 313/315, subgames,
  Commander, the Ring, dungeons).

- `_scratch/AllPrintings.json` is a dated MTGJSON dump, so it is sound for
  FINDING a card and unsound for ruling one out --- it misses every set printed
  after it was taken. Confirm an absence against Scryfall
  (`curl -s 'https://api.scryfall.com/cards/named?fuzzy=<name>'`) before writing
  "no such card exists"; Goblin Plate Mail is absent from the dump and real.
  When grepping the dump, note there is no space after the colon (`"name":"Foo"`)
  and that `rulings` sorts before `text`, so a hit near a name is usually
  ruling boilerplate rather than oracle text.

- When no printing can reach the rule, write a synthetic card as
  `data/cards/synthetic-*.json`. A real card wins whenever one exists, and "I
  could not find one" is not the test --- search first. All five sources are
  acceptable, in the preference order regular > Arena > playtest > un-set >
  synthetic (`docs/design.md` section 6), and a better-ranked card replaces a
  worse-ranked one whenever it turns up; an issue whose only producer is a
  digital-only printing is ordinary card-driven work. Legitimate when you can
  cite the rule it exercises and nothing in the CR forbids such a card
  existing (two lands that both set land subtypes); an issue in that
  position waits under `expires:synthetic` until someone writes the card.
  Illegitimate when the absence is rules-*enforced*, because then the
  elision is provably sound (half a battle: CR 310 makes the first battle need
  the whole subsystem) --- close that issue as wontfix.

- File the issue, cite it inline. When you elide something, open an issue
  carrying the status, rationale and expiry trigger, then leave a comment at
  the code site stating only what is *not* implemented, plus `(#N)`. Never
  write the expiry into the comment --- an in-code expiry is a promise nothing
  checks, and it drifted at a 23% rate before the tracker existed. That comment
  dies in the commit that closes the issue. A comment citing the test that
  *proves* a behavior is a different genre and outlives the issue.

  Which genre a `(#N)` is has to be readable, because `script/check-gaps.sh`
  checks the elision genre for exactly that death: an elision comment whose
  issue has closed is a comment claiming a capability is missing that landed.
  It reads the WORDING --- a comment paragraph saying "not implemented" is an
  elision paragraph, and every `(#N)` in it must be open. An elision phrased
  otherwise marks its citation `(gap #N)`, and a historical reference that
  shares a paragraph with an elision drops the parentheses (`see #1116`).

  A BLOCKED issue records its blocker as a GitHub dependency, not as prose:

  ```
  gh api -X POST repos/tfausak/pawl/issues/<N>/dependencies/blocked_by \
    -F issue_id="$(gh api repos/tfausak/pawl/issues/<BLOCKER> --jq .id)"
  ```

  The metadata is the whole record --- don't also write a `Blocked by #N`
  comment. A closed blocker KEEPS its link and renders as satisfied, which is
  how a reader sees an issue became workable; removing it destroys the signal.
  A blocker with no issue of its own is an untracked deficiency: file it, then
  link it. Read a capability's dependents
  (`gh api repos/tfausak/pawl/issues/N/dependencies/blocking`) when it lands,
  and say in the PR body which issues it unblocked.

- A spec or plan is optional, not ceremony --- write one when the unit warrants
  it and commit it in the same PR. If you are following a plan, work its tasks
  strictly in order, and never edit the plan, weaken an assertion, or delete a
  test to make a check pass. If the plan looks wrong, stop and say so.

## Before you consider a change done

1.  Distrust the issue body. Six cycles of one drain run found a stated blocker
    already removed by unrelated work. Re-derive the status against the tree
    before planning against it, and correct the issue in a comment when it's
    wrong.

2.  Verify a scripted edit's blast radius. Bulk `sed`/Python rewrites land in
    comments, strings, and the middle of multi-line case bodies. Read the diff
    stat and confirm the shape before staging.

3.  Mutate the change away and re-run. A green suite is not evidence the test
    proves anything, and in this repository it repeatedly has not been. Break
    the line you just wrote, confirm the new test *fails*, put it back. Three
    tests in one drain run would otherwise have shipped green and proved
    nothing. If nothing fails, say so in the PR rather than implying coverage.

4.  Find the sites `-Werror` won't. A `{}` or `_` pattern absorbs a new
    constructor or field silently, and the recurring ones are
    `Pawl.Engine.Event`'s `eventBindings` fallthrough, `Pawl.TriggerSpec`'s
    hand-kept `everyTriggerCondition` and `representativeEvents`, and
    `Pawl.CardSpec`'s filter and keyword traversals --- the last of which
    already dropped a payload once, when landwalk's `Subtype` became a `Filter`.
    Grep the sibling constructor, read every hit, and record in the PR which
    ones you read and why each is right as it stands.

## Code conventions

`docs/style-guide.md` is the style guide and is not repeated here. The
project-specific rules it doesn't cover:

- No API stability obligations. The project has no consumers: rename functions
  and modules, split or merge them, change signatures freely --- never add
  deprecation shims or compat re-exports.

- One type per module under `Pawl.Types.<TypeName>`, holding the type and its
  instances only; cross-type logic lives in other `Pawl.*` modules.

- Language extensions come from the allowlist in `.hlint.yaml`. Using one it
  doesn't name means adding it there in the same change, with the reason.

- Constructors take a `Mk` prefix and never pun the type name ---
  `newtype Name = MkName Text`. A type whose smart constructor maintains an
  invariant uses `UnsafeX` instead, marking that the bare constructor sidesteps
  it. Unwrap through an `unwrap` field.

- Short names, disambiguated by module --- `Pawl.Types.Mana`, imported as
  `Mana`, rather than long prefixes.

- A new `Pawl.Types.Keyword` constructor that CARRIES A PAYLOAD owes a matching
  `Pawl.Types.KeywordFamily` constructor in the same change --- that type is how
  a card says "a creature with toxic" rather than "with toxic 2", and it is
  owed at the keyword, not at the first card that asks. `-Werror` catches the
  missing `Pawl.Engine.Keyword.familyOf` arm but not a missing family, since
  answering `Nothing` compiles.

## Adding a module

Pick the sublibrary by what the module *is*, not by who calls it. Each is
`source/libraries/<name>/`, declared as a `library <name>` stanza in
`pawl.cabal` whose comment states its scope:

- `spec`:
  the abstract testing interface, and nothing else

- `extra`:
  quality-of-life helpers for packages pawl doesn't own

- `decimal`:
  the arbitrary-precision decimal number

- `slug`:
  slugifying text

- `uri`:
  URI components, and the encoding each of them takes

- `json`:
  RFC-compliant JSON types and codecs

- `json-pointer`:
  RFC 6901 JSON pointers

- `json-schema`:
  JSON Schema documents, modelled as plain JSON values

- `json-codec`:
  the shape of a codec, and the generic helpers every codec is written in
  terms of

- `exceptions`:
  the catalog of exceptions pawl throws

- `types`:
  type definitions, without behavior

- `codec`:
  JSON encoders and decoders for `pawl:types` types

- `registry`:
  answering "what card is this name?", plus a file-backed answer

- `engine`:
  the rules engine itself

Every library module belongs to one of those; only the executable, benchmark
and test suite sit outside, under `source/executable/`, `source/benchmark/` and
`source/test-suite/`. A sublibrary may only depend on ones above it in that
table; adding an edge that inverts it means the module is in the wrong place.

`exposed-modules` is generated by a `-- cabal-gild: discover` directive ---
don't hand-edit it. Adding or deleting a module means staging `pawl.cabal`
along with the module so `hooky fix` regenerates it; an unstaged `.cabal` is
skipped.

### Where the tests go

Outside the engine, the spec sits next to the module it covers
(`Pawl.Json.Value` -> `Pawl.Json.ValueSpec`), is an ordinary exposed module of
that sublibrary, and exposes
`spec :: (Applicative m, Monad n) => Spec.Spec m n -> n ()` built from
`Pawl.Spec` --- codec specs need `(Monad m, Monad n)`, since
`Common.assertJson` binds. Wire it into the `spec` function in `Main.hs`.
Shipping the specs as exposed modules is deliberate: `Pawl.Spec` is base-only,
so they cost essentially nothing and stay runnable by any consumer.

In the engine, tests go in the subsystem spec under `source/test-suite/Pawl/`
that near-mirrors the module under test (`Pawl.Engine.Foo` -> `Pawl.FooSpec`
--- the specs keep the shorter name, since the test suite covers more than the
engine). Each exposes `tests :: TestTree` and heads with a comment listing the
modules it covers; `Main.hs` only aggregates them. A new subsystem gets a new
`Pawl.<Area>Spec` wired into `Main.hs`'s `testTree` and added to the test-suite
`other-modules` list. Shared fixtures live in `Pawl.Support`, imported
`qualified ... as S` --- the one documented exception to
alias-to-last-component; a group-local helper stays with its group.
