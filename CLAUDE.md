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

Design notes live in `docs/`. Read them by section, on demand --- a whole doc
"for context" costs tens of thousands of tokens.

- What's next?
  `gh issue list`

- Architecture rationale:
  `docs/design.md`

- Rules ground truth:
  `docs/rules.txt`, grepped by rule number --- never memory, and never a rule
  number quoted in an issue or a brief; those have been wrong.

- Prior-art evidence:
  `docs/prior-art-lessons.md`

- What the milestone era established:
  `docs/progress.md` (frozen), newest first

- A landed unit's authoritative detail:
  `docs/superpowers/`

- Dispatched as a subagent?
  `docs/agents/` --- `implementing.md` if you hold the build and will open a PR,
  `researching.md` if you are read-only, `drain-loop.md` if you orchestrate.

## Workflow

`CONTRIBUTING.md` has the loop --- issue, branch, TDD, draft PR --- and applies
to agents as written. What it doesn't say:

- A fresh `git worktree` has no gitignored `cabal.project.local`, so `pedantic`
  and `-Werror` are off and a green build says nothing about CI. Copy it in
  from the primary checkout before the first build.

- That file sets `semaphore: True`, so concurrent `cabal` runs in different
  worktrees share one GHC job semaphore --- builds are one at a time. A killed
  run corrupts it (a `pkill`, or a tool timeout reaping a backgrounded `cabal`);
  the symptom is `semWait: invalid argument`, and `cabal test --no-semaphore
  -j4` gets through. Never `pkill -f 'cabal test'` --- it reaches other agents'
  worktrees. Match your own worktree path, or wait.

- Derive against `origin/main`, not the working checkout, which drifts:
  `git fetch`, then `git show origin/main:<path>` or a worktree cut from it.
  Line numbers in issue bodies are stale; grep for identifiers.

- Self-review the branch before opening the PR, and fix the findings on the
  branch. At minimum: re-check every CR citation against `docs/rules.txt`, and
  re-read every comment the change touched. Both reliably catch real defects.

- The PR body carries the case for merging. A line each:
  - what changed and why, with `Closes #N` --- bare text, since backticks break
    the link. Never write close, fix or resolve next to an issue number you do
    not intend to close, in any phrasing including a denial ("does not close
    #N" closed it); write "related to #N". A keyword in any branch commit
    survives the squash.
  - the CR citations behind it
  - the design calls made, and the alternatives rejected
  - how it was verified: suite count before -> after, the proving test, and for
    each mutation the assertion it reddened, named. A mutation reported only as
    "went red" is one nobody diagnosed.
  - whether the rules core cases on an effect's *identity* --- an explicit "no"
  - what was deferred

- Keep the prose terse --- PR bodies, issue comments and code comments alike,
  and a citation in place of a quoted rule. Do the verification work in full;
  write it up short.

- Mark the PR ready once the self-review's findings are pushed and the suite is
  green, report it, and stop. Don't wait for CI. Don't start the next unit ---
  one unit at a time per checkout, since two branches contend for `HEAD`.

- STAGE, then `hooky fix`. It acts on staged files only --- an unstaged file is
  skipped, which is where "hooky fix wasn't enough" comes from --- and runs
  every check `.hooky.kdl` wires up: CI's ormolu, hlint, cabal-gild, `cabal
  check` and nixfmt, plus the JSON formatter, the citation check and the
  builtin lint rules. It rewrites in place, so `git add` again afterwards.
  `--all` sweeps the tree in two minutes instead of one second; use it only
  when you suspect something landed unstaged.

- After a PR merges, before the next unit, ask: did anything catch you that
  your own checks didn't? did you violate an instruction? did you learn a
  project fact the repo doesn't record? A project fact belongs here, in the
  section it bears on, folded into the next unit's PR. Skip it when there is
  genuinely nothing.

- Most of what's left is card-driven: working an issue means finding the real
  card and adding it to `data/cards/`. That is the work, not a side quest, and
  "no producer in the pool" describes it rather than excusing it. What is
  forbidden is a capability no card exercises: per `docs/design.md` section 4,
  an effect is not done until a card exercises it in a gameplay-level test. The
  one good reason to stop is a *rules* reason --- the card turns out not to
  exercise the thing after all.

- Labels: `elision`, `gap`, `rules-correctness` and `bug`, plus the expiry
  triggers `expires:milestone`, `expires:card-driven`, `expires:subsystem` and
  `expires:synthetic`. Priority labels are the owner's: prefer high-priority
  issues when picking, never set one. `gap` (the capability does not exist) and
  `rules-correctness` (behaviour observably diverges) often both apply;
  `elision` excludes `rules-correctness`, since an elision is sound only while
  the options are indistinguishable. `expires:card-driven` does NOT mean wait
  --- it means add the card. An issue carrying no `expires:*` is one whose
  trigger already fired. An `area:*` label goes on where one genuinely fits:
  beyond the subsystem names there are `area:keywords` (CR 701/702),
  `area:effects` (the effect DSL), `area:cards` (card data, faces, layouts,
  schemas, lints), `area:stack` (CR 601/602/608) and `area:variants` (CR
  313/315, subgames, Commander, the Ring, dungeons).

- Verify Oracle text with `curl -s
  'https://api.scryfall.com/cards/named?fuzzy=<name>'`; WebFetch gets 403s.
  `_scratch/AllPrintings.json` is a dated MTGJSON dump: sound for FINDING a
  card, unsound for ruling one out. When grepping it there is no space after
  the colon (`"name":"Foo"`), and `rulings` sorts before `text`, so a hit near
  a name is usually ruling boilerplate.

- `_scratch/` also holds permissively licensed prior art --- `phase`, `mtgish`,
  `argentum-engine`; `docs/agents/implementing.md` says what each is good for.
  Consult them AFTER deriving the rule from `docs/rules.txt`, since reading
  someone else's model first imports it; the CR wins every disagreement.
  Everything under `_scratch/` is gitignored and may be absent --- a skipped
  step, not a blocked one.

- When no printing can reach the rule, write `data/cards/synthetic-*.json`.
  Search first: a real card wins whenever one exists, in the order regular >
  Arena > playtest > un-set > synthetic (`docs/design.md` section 6), and a
  better-ranked card replaces a worse-ranked one whenever it turns up; an
  issue whose only producer is digital-only is ordinary card-driven work.
  Legitimate when you can cite the rule it exercises and nothing in the CR
  forbids such a card existing; that issue waits under `expires:synthetic`.
  Illegitimate when the absence is rules-*enforced*, because then the elision
  is provably sound --- close that issue as wontfix.

- File the issue, cite it inline. An elision gets an issue carrying the status,
  rationale and expiry trigger, and a comment at the code site stating only
  what is *not* implemented, plus `(#N)`. Never write the expiry into the
  comment --- nothing checks it. That comment dies in the commit that closes
  the issue; a comment citing the test that *proves* a behavior is a different
  genre and outlives it.

  `script/check-gaps.sh` reads the WORDING: a comment paragraph saying "not
  implemented" is an elision paragraph, and every `(#N)` in it must be open.
  An elision phrased otherwise marks its citation `(gap #N)`; a historical
  reference sharing a paragraph with an elision drops the parentheses (`see
  #1116`).

  A BLOCKED issue records its blocker as a GitHub dependency, not as prose:

  ```
  gh api -X POST repos/tfausak/pawl/issues/<N>/dependencies/blocked_by \
    -F issue_id="$(gh api repos/tfausak/pawl/issues/<BLOCKER> --jq .id)"
  ```

  Don't also write a `Blocked by #N` comment. A closed blocker KEEPS its link
  --- that is how a reader sees an issue became workable. A blocker with no
  issue is an untracked deficiency: file it, then link it. When a capability
  lands, read its dependents
  (`gh api repos/tfausak/pawl/issues/N/dependencies/blocking`) and say in the
  PR body which issues it unblocked.

- A spec or plan is optional; commit one in the same PR when the unit
  warrants it. If you are following one, work its tasks in order, and never
  edit the plan, weaken an assertion, or delete a test to make a check pass.
  If the plan looks wrong, stop and say so.

## Before you consider a change done

1.  Distrust the issue body --- its status, its blockers and its size estimate
    have all been wrong. Re-derive against the tree before planning, and
    correct the issue in a comment when it's wrong.

2.  Verify a scripted edit's blast radius. Bulk `sed`/Python rewrites land in
    comments, strings, and the middle of multi-line case bodies. Read the diff
    and confirm the shape before staging.

3.  Mutate the change away and re-run. A green suite is not evidence the test
    proves anything. Break the line you just wrote, confirm the new test
    *fails*, put it back. Read WHICH assertion failed: if it is not the
    gameplay-level one, an assertion ahead of it absorbed the mutation and the
    behaviour is unproven. If nothing fails, say so in the PR rather than
    implying coverage. `docs/agents/implementing.md` lists the traps.

4.  Find the sites `-Werror` won't. A `{}` or `_` pattern absorbs a new
    constructor or field silently; the recurring ones are
    `Pawl.Engine.Event`'s `eventBindings` fallthrough, `Pawl.Engine.Filter`'s
    `boundSlots` (nine arms then `_ -> Set.empty`), `Pawl.TriggerSpec`'s
    hand-kept `everyTriggerCondition` and `representativeEvents`, and
    `Pawl.CardSpec`'s filter and keyword traversals. No codec is forced either
    --- every `Arm.tagged` list carries its own `_ -> Nothing`, and only
    `Designation`'s `Arm.enum` derives, so a new constructor compiles with no
    codec arm and no round-trip test (#1715). Grep the sibling constructor, read
    every hit, and record in the PR which ones you read and why each is right as
    it stands.

5.  Closing #N means moving every census row that cites it. #875, #876 and #877
    annotate implemented rows with the issue numbers of what those rows still
    don't do, and `script/check-census.sh` checks rule numbers and written
    constructor names but never issue numbers --- so a closed number inside a
    row is invisible to it. `grep` the issue number in the three bodies before
    opening the PR, and edit the row in the same PR.

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
  a card says "a creature with toxic" rather than "with toxic 2". `-Werror`
  catches the missing `Pawl.Engine.Keyword.familyOf` arm but not a missing
  family, since answering `Nothing` compiles.

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
