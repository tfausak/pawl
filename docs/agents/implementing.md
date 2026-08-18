# Implementing a unit

Read this first when you are dispatched to work an issue end to end and open a
PR. `CLAUDE.md` and `CONTRIBUTING.md` still apply and override anything here;
this file is the standing procedure a dispatch brief would otherwise repeat, so
the brief carries only what is specific to your unit.

You hold the build. The one other thing that may be building is the previous
unit, if its PR went red on CI and its agent was sent back. Nothing else is.

## Start from the brief

A brief spares you the re-derivation: the producer's Oracle text, its card
JSON, the edit sites, a drafted red test, the mutations. Start there rather
than from the issue. What you owe it is verification, not repetition ---
`curl` the Oracle text and diff it against the brief's JSON (a brief has
carried a wrong mana cost), grep the sibling to confirm the edit sites are
complete, run the drafted test and watch it go red for the reason the brief
predicts.

Distrust the issue's estimate of SIZE as much as its status. The cheapest first
move on a "needs new machinery" issue is to find the funnel --- the one
function every path to the behaviour goes through --- and read what it already
gates or orders. Several units turned out to be a test and a comment.

## Before the first build

Copy `cabal.project.local` in from the primary checkout, as a BARE command of
its own, and confirm it is there. The worktree-isolation guard refuses a
compound command wholesale, and its error names the other half; one run read
the refusal as being about the chained `git` and built unpedantic until CI
caught it.

## Running the suite

    cabal test --test-options '--timeout 5s --hide-successes'

While iterating, run only the subtree you are in --- add `-p Detain`, a tasty
pattern over the `Spec.describe` group names --- and the whole suite before
each commit and before the push; an engine change reaches specs you did not
open. Redirect output to a file and read the file: piping `cabal test` stalls
it for minutes and, backgrounded, leaves the output empty. One build at a
time; no `cabal clean`; keep the optimizer on (`-O0` was measured: the cold
build saves under a minute and the suite then blows its budget on cases that
take 0.02s at `-O1`).

The timeout catches infinite loops; it is not an assertion about speed. A few
cases run 1-2s unloaded, and the machine is shared, so a lone TIMEOUT on one of
those is background noise --- re-run it unloaded before investigating; a real
hang fails at any budget. Two subtrees carry their own budgets via
`Tasty.localOption` in `Main.hs`; CI sets 5s suite-wide through `flake.nix`'s
`testFlags`.

## Enumerate the edit sites in one pass

Compiler round trips are the loop's main cost. When you add a constructor or a
field, grep an existing sibling to enumerate every site and patch them all
before rebuilding --- do not let `-Werror` name them one build at a time. Then
find the sites `-Werror` will *not* name (`CLAUDE.md`, "Before you consider a
change done"), and say in the PR which ones you read and why each is correct.
"I read all of them" is a finding; silence is not.

## Prose the compiler cannot check

A comment asserting a limit the engine used to have ("pawl's projection does
not reach a hand", "walks the battlefield only", "all four of
`Pawl.Types.Affected`") becomes false the moment the limit lifts, and nothing
catches it: no issue number for `script/check-gaps.sh`, nothing for `-Werror`.
It is not mechanically checkable, so do not propose a script for it.

When your change lets a capability reach somewhere it previously did not, grep
the tree before you push for prose asserting the old limit --- the name of what
you widened, the zone or type it now reaches, and the absolutes such claims use
(`only`, `never`, `does not`, `no card`). Sweep `source/test-suite/` along with
the libraries, and the files you edited along with the ones you did not; both
are where survivors turn up. Re-derive every hit and rewrite it to say what it
actually rests on, worded so a later grep finds it.

Adding a card does the same thing from the other direction. A comment that
counts producers --- "the pool's one statement of it", "no card in the pool
writes this", "the rulebook's only" --- is falsified by the card that becomes
the second one, and the card's own PR is the only place that is visible. So
when the card you are adding is the first printed producer of a construct, or
the first to write two halves of a pattern at once, grep the construct's type
and constructor names for the counting absolutes (`one`, `only`, `no card`,
`the pool's`) before you push, and rewrite each hit to say what the pool now
holds. PR #1788 made three such comments false, and only that sweep found them.

## Stale reads

pawl's recurring defect shape is a consumer reading a snapshot where the rule
asks about live state --- a condition that went derived while its consumer
stayed stored, or a gate reading the bindings captured when resolution began,
so a slot an earlier clause defined is invisible to a later one. When your
change adds a gate, a prompt or a condition, ask what it reads and WHEN that
was captured; `Pawl.Engine.Resolve.gateHolds` and its callers are where this
has bitten.

## Mutation testing

`CLAUDE.md` requires mutating the change away and re-running. How:

- **One mutation at a time, and read the failure.** Red is not the bar: it
  must go red for the *intended* reason, in the intended place. A mutation that
  fails a dozen unrelated cases, or the target case with a different message
  than the behaviour predicts, has proved something else.
- **Order the assertions so the mutation reaches the real one.** A cheap proxy
  --- a prompt count, a zone size, a list length --- placed before the
  gameplay-level assertion absorbs the mutation and goes red first, and the
  failure you read is about the proxy. The behaviour under test asserts first;
  the proxy is a supporting check after it.
- **Never `git checkout <file>` to revert a mutation** --- real edits have been
  lost that way. Copy the file to a backup first and move it back.
- **Build a negative as a pair of boards differing in exactly one thing.** A
  negative assembled on its own board passes for reasons you did not choose.
- **Keep the mutated binding referenced.** Deleting a use trips
  `-Wunused-local-binds` under `-Werror`, so a real red comes back as a build
  failure. Neutralize the value instead --- `const Map.empty . f`, `filter
  (const False)`, a `seq`.
- **Run the mutation through the NARROWEST path that shows the behaviour.** A
  test that drives the whole priority loop can answer for the wrong reason: a
  settle sweeps a conditional effect whose condition is already false, so the
  loop cannot tell "never started" from "started and was swept".
- **Report a mutation you could not run.** If a behaviour holds by
  construction, or `-Werror` rejects the mutated source, there is nothing to
  break. Say plainly, at the code site and in the PR, that the assertion is a
  regression fence rather than a proof.
- **A mutation that leaves the suite green means the change has no observer.**
  Do not close the issue. Add the card that gives the line an observer; failing
  that it is `wontfix` or `expires:synthetic`.

After any merge from `origin/main`, run the suite first --- a card file in a
superseded wire spelling makes the registry throw `InvalidCorpus` and aborts
the WHOLE corpus load, so every case dies at once; grep the corpus for the old
spelling rather than eyeballing your diff --- then re-run the load-bearing
mutations, since a bad conflict resolution can neuter a test while leaving the
suite green.

## Vacuity traps

Each of these has shipped a green-but-meaningless test in this repository:

- **Negative cast-gate assertions pass for unrelated reasons** --- no mana,
  wrong timing, a non-empty stack. Build every negative board with the same
  mana as the positive board that succeeds, and prove the branch flips only on
  the thing under test.
- **`Activate.activatable` answers `False` for a mana ability on every board**
  (CR 605.3b keeps them off the stack). Never route a negative through it;
  assert at gameplay level.
- **Two-player boards collapse "that player", "an opponent" and "the defending
  player" onto one seat.** Three seats disambiguate.
- **A fixture player decked by CR 104.3c** loses before the assertion runs.
  Stock the libraries whenever the fixture draws or advances turns.
- **Numeric coincidences.** Distinct values everywhere; if two readings of the
  rule produce the same number on your board, the test cannot tell them apart.
- **A prompt short-circuits when candidates equal the count**, so "the player
  was asked" can pass because they never were. Offer more candidates than the
  choice needs.
- **A token exiled from a graveyard ceases to exist** (CR 111.7), so a token
  victim makes an exile assertion unobservable.
- **An answerer can silently repair the assertion.** One that searches for a
  legal option finds the right one again after your mutation. Pin the answer by
  index, and check the assertion reads the engine's output.
- **An answerer that BUILDS a recipient silently loses the target.** A
  `Prompt.ChooseTargets` over `Pool.Creatures` offers `Recipient.ToCreature`;
  a hand-built `Recipient.ToObject` of the same permanent is a different
  recipient, and CR 608.2b's re-read at resolution drops it with no error.
  FILTER the offered set instead.
- **A pure `Prompt r -> r` answerer cannot tell two structurally identical
  prompts apart.** Same constructor, same decider, same controller, same
  candidate set means the second call is indistinguishable from the first, so an
  answerer that pins one answer answers both the same way and the test is green
  whatever the engine did. Thread state instead --- an answerer in `State.State`
  counting or indexing its calls, as `Pawl.CopySpec` and `Pawl.ManaSpec` do with
  `countingAnswer` --- and assert on the sequence.
- **A fixture supplies preconditions your assertion silently rests on.** The
  `Pawl.Support` builders settle what they place (`S.addCreature` writes
  `Sickness.Settled`), stock what they draw from, and leave what they place
  untapped; a permanent that pays `{T}` in your test may be paying only because
  of that. Name the precondition the behaviour needs and assert it on the board,
  or the test proves the fixture rather than the change --- and a hand-built
  negative that lacks it differs from the positive in two things, not one.
- **A `Pawl.Support` counter may index by a different question than your
  assertion's wording.** `S.countOnBattlefieldByName` takes a `PlayerId`, but
  `Game.zoneMembers` indexes the battlefield by OWNER (CR 108.1), so it cannot
  see who CONTROLS anything --- an assertion about a controller written through
  it is green under both readings. Read the helper before trusting its
  parameter's name; `Projection.controllerOf` is the control question.
- **Two conditions a board cannot tell apart.** Name the other reading of the
  rule and check the board distinguishes them --- same zone for two
  destinations, same timestamp for two layers, one player holding two roles.
  If not, change the board, not the assertion.

## Cards

Never take a card's printed values on trust from a brief OR an issue body; both
have carried a wrong mana cost and a card claimed to be in the pool that was
not. Fetch the Oracle text yourself (`CLAUDE.md` has the `curl`) and diff.

If a clause cannot be expressed, say which, and whether the omission leaves
pawl's card **stricter** or **weaker** than printed. Weaker in the controller's
favour is the dishonest direction and disqualifies the card --- find another
producer or stop. Stricter is admissible with an issue and an inline `(#N)`.

**A stale transcription looks exactly like a missing capability.** Before
concluding the engine cannot express something, grep `data/cards/` for a card
that already uses it. Report a short transcription in the same stricter/weaker
terms. Having fixed one card, **sweep the corpus for its siblings** --- one
grep turns a one-card fix into a claim about the corpus.

### Prior art, when you need a producer or a field shape

`CLAUDE.md` says when to consult these. Each is one grep:

- **`_scratch/phase`** (MIT) finds the CARD: `crates/engine/tests/integration/`
  holds a thousand-plus test files named for the card and the rule, plus
  rule-keyed files under `rules/`. Not a design reference --- its parser is the
  truth and its core still carries a card-identity check.
- **`_scratch/mtgish`** (MIT) gives the effect's SHAPE: the pool as first-order
  typed ASTs in `data/mtgish.lines.json`, vocabulary in
  `rust_syntax/src/mtg_types.rs`; also "which cards use this construct". It
  carries no marker for text its parser could not express, so presence is not
  evidence the rules text came through whole.
- **`_scratch/argentum-engine`** (MIT) answers MODELING questions --- same
  base/projected split, same no-escape-hatch bet.

A green test in another engine means someone considered the case, never that
their answer is right. Ported code carries the MIT notice; card text is Wizards
IP whatever the engine's license says.

## Prompts

Do not mint a bespoke resolution-time choice. `Pawl.Types.Prompt`'s
`ChooseRingBearer`, `ChooseBolster`, `ChooseAmass`, `ChooseBlight` and
`ChooseCopyTarget` share one posture --- choose, not target; filter the
candidates rather than trusting the answer; raise the prompt only when two or
more candidates make it a real choice. Follow them unless the rule you are
implementing says otherwise, and then say so in the PR.

## Follow-ups: fold in or file

- **Fold it in** when it lives in files you already have open, is small (a
  clause on the card you are adding, a sibling arm, a lint, a comment made
  wrong), needs no design call and no new card, and its proof fits in the same
  spec. Say in the PR what you folded in. Two adjacent issues in one PR is
  fine, not scope creep.
- **File it** when it needs its own card, its own design decision, or touches
  files outside your unit --- and cite it inline where the code elides it.

Each filed leaf costs a whole unit's fixed overhead later; a follow-up that
takes ten minutes now and forty as its own dispatch is folded in.

## Git and the PR

- Branch off latest `origin/main`, named `issue-slug`. Never commit to `main`.
- Immediately before the self-review and the push, `git fetch` and merge
  `origin/main` again, however recently you last did --- a PR armed just before
  you were dispatched typically lands mid-run. Then re-run the suite and the
  load-bearing mutations against the merged state.
- **Never force-push after opening.** `git merge origin/main` and keep the
  merge commit; a rebase discards the one auto-merge adds. Resolve conflicts by
  taking **both** sides, then re-run the mutations.
- **Landing a capability a census tracks means editing the census in the same
  PR** --- #875 (CR 116 special actions), #876 (CR 701 keyword actions), #877
  (CR 702 keyword abilities). `script/check-census.sh` catches the eponymous
  case on #876 and #877, and holds #875's rows to set equality with `Action`'s
  constructors by the `Action.X` names they write. A row landed under another
  name, or under no constructor of the tracked type at all, is yours alone.
- **Landing a capability means reading what it unblocked**; `CLAUDE.md` has
  the query. Say in the PR which dependents are now workable.
- **Closing #N means re-deriving every inline `(#N)` in the tree**, not just the
  site you fixed; other sites cite the same issue for their own reasons. Find
  them by grepping the BARE number over the whole tree, not by trusting the file
  set a brief or an issue body names --- that set has been incomplete three
  times in one session, and each miss was a live elision citing an issue the PR
  closed, which is CI's Tracker job red on merge. A citation can also arrive
  after the brief was written, from a PR that landed while you worked, so grep
  again after the last merge from `origin/main`.

Before pushing: stage, `hooky fix` (`CLAUDE.md`), stage again. Two hooks bite
differently by hand: `script/format-json.sh` takes `MODE FILE...` and passes
vacuously when run bare, and `script/check-citations.sh` defaults to the whole
tree --- run it bare after taking a CR update, since a renumbering breaks
citations in files you never touched. The same renumbering breaks them in issue
bodies, where nothing checks anything: after taking a CR update, grep the open
tracker for the renumbered rules and correct the bodies in a comment. Two checks are NOT hooks because they
read GitHub: `script/check-census.sh` and `script/check-gaps.sh`. Both take a
second and run in CI's Tracker job, which you will not be waiting for, so run
them yourself.

Then stop. Do not wait on CI, and do not start another unit.
