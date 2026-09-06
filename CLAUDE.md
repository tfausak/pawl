# CLAUDE.md

Guidance for Claude Code (and other agents) working in this repository. Rules
only: the incidents that motivated them are in git history and the PRs that
added them.

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
  from the primary checkout before the first build --- by absolute path, never
  by `cd`ing there. Then `script/warm-worktree.sh seed <your worktree>`, which
  clones a finished build of `origin/main` in: GHC reuses every module whose
  source is unchanged, so the first build costs only your own diff. The
  script does the `cabal.project.local` copy too.

- Working in a worktree, NEVER `cd` to the primary checkout, not even to read.
  The isolation guard redirects `git` but not `python`, `grep`, `sed` or
  `cabal`, so every file edit made there lands on whatever branch the owner
  has checked out, alongside their uncommitted changes.

- The isolation guard refuses a command whose text holds the bare word
  `source` (the tree's top directory), a heredoc beside another command, or a
  `sed` expression carrying parentheses, a pipe or a `$` --- `script/mutate.sh`'s
  argument most of all. Grep as `grep -rn X . --include='*.hs'` or against a
  quoted deeper path, and run a heredoc as a command of its own.

- Every worktree shares ONE stash stack, so a stash made in yours can be popped
  by an agent in another --- losing your edits and landing theirs in your tree.
  Never stash; copy the file aside and move it back.

- Prefix every `cabal` call with `script/with-build-lock.sh`, which caps
  builds at two machine-wide (the machine has 8 GB). Never `pkill -f 'cabal
  test'` --- it reaches other agents' worktrees; kill your own PID. Never pipe
  `cabal` into `head` or anything else that closes the pipe early: it
  deadlocks on SIGPIPE while holding a lock slot. Redirect to a file and read
  the file.

- Derive against `origin/main`, not the working checkout, which drifts:
  `git fetch`, then `git show origin/main:<path>` or a worktree cut from it.
  Line numbers in issue bodies are stale; grep for identifiers.

- Self-review the branch before opening the PR, and fix the findings on the
  branch. At minimum: re-check every CR citation against `docs/rules.txt`, and
  re-read every comment the change touched.

- The PR body carries the case for merging. A line each:
  - what changed and why, with `Closes #N` --- bare text, since backticks break
    the link. Never write close, fix or resolve next to an issue number you do
    not intend to close, in any phrasing including a denial; write "related to
    #N". A keyword in any branch commit survives the squash.
  - the CR citations behind it
  - the design calls made, and the alternatives rejected
  - how it was verified: suite count before -> after, the proving test, and for
    each mutation the assertion it reddened, named.
  - whether the rules core cases on an effect's *identity* --- an explicit "no"
  - what was deferred

- Keep the prose terse --- PR bodies, issue comments, commit messages and code
  comments alike, and a citation in place of a quoted rule. Do the verification
  work in full; write it up short. A commit message is a subject line and, at
  most, two sentences of why. A PR body is the bullets above and nothing else.
  The one thing never to trim is the mutation line --- which assertion
  reddened, named.

- Mark the PR ready once the self-review's findings are pushed and the suite is
  green, report it, and stop. Don't wait for CI. Don't start the next unit ---
  one unit at a time per checkout.

- STAGE, then `hooky fix`. It acts on staged files only, runs every check
  `.hooky.kdl` wires up, and rewrites in place, so `git add` again afterwards.
  `--all` sweeps the tree in two minutes; use it only when you suspect
  something landed unstaged.

- After a PR merges, before the next unit, ask: did anything catch you that
  your own checks didn't? did you violate an instruction? did you learn a
  project fact the repo doesn't record? A project fact belongs here, in the
  section it bears on, as a rule and not as its story, folded into the next
  unit's PR. Skip it when there is genuinely nothing.

- Most of what's left is card-driven: working an issue means finding the real
  card and adding it to `data/cards/`. That is the work, not a side quest, and
  "no producer in the pool" describes it rather than excusing it. What is
  forbidden is a capability no card exercises: per `docs/design.md` section 4,
  an effect is not done until a card exercises it in a gameplay-level test.
  The one good reason to stop is a *rules* reason --- the card turns out not
  to exercise the thing after all.

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
  `/cards/search` answers 400 to every regex query unless a `User-Agent`
  header is sent. `_scratch/AllPrintings.json` is a dated MTGJSON dump: sound
  for FINDING a card, unsound for ruling one out. When grepping it there is no
  space after the colon (`"name":"Foo"`), and `rulings` sorts before `text`, so
  a hit near a name is usually ruling boilerplate.

- `_scratch/` also holds permissively licensed prior art --- `phase`, `mtgish`,
  `argentum-engine`; `docs/agents/implementing.md` says what each is good for.
  Consult them AFTER deriving the rule from `docs/rules.txt`; the CR wins every
  disagreement. Everything under `_scratch/` is gitignored and may be absent
  --- a skipped step, not a blocked one.

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

  Nothing checks any of this, and no script should: the WORDING is the
  convention, because a `grep` is what finds these. A comment paragraph saying
  "not implemented" is an elision paragraph, an elision phrased otherwise
  marks its citation `(gap #N)`, and a historical reference sharing a
  paragraph with an elision drops the parentheses (`see #1116`). Closing an
  issue means grepping its bare number and rewriting every elision that cited
  it, in the same PR.

  A filed follow-up names the real card that needs it. A gap no card needs is
  folded in or dropped, not filed; `docs/agents/implementing.md` has the rule.

  A BLOCKED issue records its blocker as a GitHub dependency, not as prose:

  ```
  gh api -X POST repos/tfausak/pawl/issues/<N>/dependencies/blocked_by \
    -F issue_id="$(gh api repos/tfausak/pawl/issues/<BLOCKER> --jq .id)"
  ```

  Don't also write a `Blocked by #N` comment. A closed blocker KEEPS its link.
  A blocker with no issue is an untracked deficiency: file it, then link it.
  When a capability lands, read its dependents
  (`gh api repos/tfausak/pawl/issues/N/dependencies/blocking`) and say in the
  PR body which issues it unblocked.

- A spec or plan is optional; commit one in the same PR when the unit
  warrants it. If you are following one, work its tasks in order, and never
  edit the plan, weaken an assertion, or delete a test to make a check pass.
  If the plan looks wrong, stop and say so.

## Before you consider a change done

1.  Distrust the issue body --- its status, its blockers and its size estimate
    are routinely wrong. Re-derive against the tree before planning, and
    correct the issue in a comment when it's wrong.

2.  Verify a scripted edit's blast radius. Bulk `sed`/Python rewrites land in
    comments, strings, and the middle of multi-line case bodies, and inside
    comment groups that COUNT their members. Read the diff and confirm the
    shape before staging; move an inserted arm out of a counted group and give
    it its own comment.

3.  Mutate the change away and re-run. A green suite is not evidence the test
    proves anything. Break the line you just wrote, confirm the new test
    *fails*, put it back. Read WHICH assertion failed: if it is not the
    gameplay-level one, an assertion ahead of it absorbed the mutation and the
    behaviour is unproven. If nothing fails, say so in the PR rather than
    implying coverage. `script/mutate.sh` runs one mutation and prints the
    assertion; `docs/agents/implementing.md` lists the traps.

4.  Find the sites `-Werror` won't. A `{}` or `_` pattern absorbs a new
    constructor or field silently; the recurring ones are
    `Pawl.Engine.Event.Binding`'s `eventBindings` fallthrough, `Pawl.Engine.Filter`'s
    `overBoundSlots` (the slot-naming arms then `_ -> pure predicate`, and both
    `boundSlots` and `renameBound` are it), `Pawl.ZoneTriggerSpec`'s
    hand-kept `everyTriggerCondition` and `representativeEvents`, and
    `Pawl.CardSpec`'s filter and keyword traversals. No `Arm.tagged` codec is
    forced --- every such list carries its own `_ -> Nothing`, so a new
    constructor compiles with no codec arm and no round-trip test (#2262). An
    `Arm.enum` codec derives and needs neither; check which kind the type has.
    Grep the sibling constructor, read every hit, and record in the PR which
    ones you read and why each is right as it stands.

    A NEW FIELD's invisible site is positional record construction in the test
    suite, which absorbs it in argument order. Grep every construction site of
    the type by hand.

    A new field or disjunct on `Pawl.Types.ProjectedCharacteristics` or
    `Pawl.Engine.Filter`'s `View` must be filled in EVERY builder ---
    `Pawl.Engine.Projection.View`'s `viewOfCard`, `viewOfCharacteristics` and
    `copiableCharacteristics`, and `Pawl.Engine.Count`'s `viewOfSnapshot`.
    Filling one compiles clean and the rest silently answer the old value.

    WIDENING AN EXISTING FUNCTION'S RESULT has no tripwire at all: no type
    changes, so `-Werror` is silent and a mutation proves only that the test is
    sensitive to your line, never that you found the other readers. Grep the
    function's callers, and say in the PR body which paths you drove and which
    you did not.

5.  Closing #N means moving every census row that cites it. #875, #876 and #877
    annotate implemented rows with the issue numbers of what those rows still
    don't do, and nothing checks the three bodies against anything. Read them
    for the issue number before opening the PR, and edit the row in the same
    PR; landing a capability a census tracks means that same edit.

## Code conventions

`docs/style-guide.md` is the style guide and is not repeated here. The
project-specific rules it doesn't cover:

- No API stability obligations. The project has no consumers: rename functions
  and modules, split or merge them, change signatures freely --- never add
  deprecation shims or compat re-exports.

- One type per module under `Pawl.Types.<TypeName>`, holding the type and its
  instances only; cross-type logic lives in other `Pawl.*` modules.

- A constructor's haddock is ONE line saying what it means, plus its CR
  citation. Not a paragraph, not the rule's text, not a worked example, not the
  history of how it came to exist. Two things earn more than a line: an
  elision paragraph, which states what is NOT implemented and cites its issue,
  and a note naming the test that PROVES a behaviour. Both outlive the
  one-liner; nothing else does.

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

Which sublibrary a module belongs to, how `exposed-modules` is generated, and
where its spec goes: `docs/adding-a-module.md`. Read it before adding or
deleting a module.
