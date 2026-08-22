# Implementing a unit

Read this first when you are dispatched to work an issue end to end and open a
PR. `CLAUDE.md` and `CONTRIBUTING.md` still apply and override anything here;
this file is the standing procedure a dispatch brief would otherwise repeat, so
the brief carries only what is specific to your unit.

You hold the build. The one other thing that may be building is the previous
unit, if its PR went red on CI and its agent was sent back.

## Start from the brief, and distrust half of it

A brief spares you the re-derivation: the producer's Oracle text, its card
JSON, the edit sites, a drafted red test, the mutations. Start there rather
than from the issue. What you owe it is verification, not repetition.

Its two halves are not equally reliable. Findings the researcher could *read*
--- whether a blocker landed, which producer works, the edit-site set, CR
citations --- hold up, and routinely change a unit's scope. Anything needing a
compiler does not: in one nine-unit run every brief carried at least one wrong
wire spelling, stale line number, or mutation prediction. `curl` the Oracle
text and diff it against the brief's JSON, grep the sibling to confirm the edit
sites, and re-derive the mutations yourself.

**A mutation the brief predicted red that comes back green is a finding, not a
formality** --- it happened three times in that run. Either the board cannot
discriminate, or the site has no observer. Diagnose which before proceeding; do
not reword the prediction to match the result.

Distrust the issue's estimate of SIZE as much as its status, in both
directions: one issue's "site" line named one file and the unit needed a new
`GameEvent` across seventy-odd forced arms. The cheapest first move is to find
the funnel --- the one function every path to the behaviour goes through ---
and read what it already gates or orders. Several units turned out to be a test
and a comment.

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
it for minutes and, backgrounded, leaves the output empty. One build at a time;
no `cabal clean`; keep the optimizer on (`-O0` was measured: the cold build
saves under a minute and the suite then blows its budget on cases that take
0.02s at `-O1`).

The timeout catches infinite loops; it is not an assertion about speed. A few
cases run 1-2s unloaded and the machine is shared, so a lone TIMEOUT is
background noise --- re-run it unloaded first; a real hang fails at any budget.
Two subtrees carry their own budgets via `Tasty.localOption` in `Main.hs`; CI
sets 5s suite-wide through `flake.nix`'s `testFlags`.

## Enumerate the edit sites in one pass

Compiler round trips are the loop's main cost. When you add a constructor or a
field, grep an existing sibling to enumerate every site and patch them all
before rebuilding --- do not let `-Werror` name them one build at a time. Then
find the sites `-Werror` will *not* name (`CLAUDE.md`, "Before you consider a
change done"), and say in the PR which ones you read and why each is correct.
"I read all of them" is a finding; silence is not.

## Prose the compiler cannot check

A comment asserting a limit the engine used to have becomes false the moment
the limit lifts, and nothing catches it. It is not mechanically checkable, so
do not propose a script for it.

Three sweeps, all yours:

- **When your change widens a capability**, grep for prose asserting the old
  limit --- the name of what you widened, the zone or type it now reaches, and
  the absolutes such claims use (`only`, `never`, `does not`, `no card`). Sweep
  `source/test-suite/` too, and the files you did not edit.
- **When you add a card**, grep the construct's type and constructor names for
  counting absolutes (`one`, `only`, `no card`, `the pool's`). A comment that
  counts producers is falsified by the card that becomes the second one, and
  the card's own PR is the only place that is visible. PR #1788 made three such
  comments false.
- **When you would write a NEW negative**, don't write the bare form. Three PRs
  in one session introduced a false absolute in the very PR that deleted an
  older one, each because a negative Scryfall result is evidence about the
  QUERY, and every query was built from pawl's identifier rather than the
  printed template.

Two shapes are admissible for a negative. Prefer the citation: the claim is
almost always the REASON a slot is unbuilt, which makes it an elision, so `Not
implemented: ... (#N)` or `(gap #N)` ties it to something a reader can check.
Where there is genuinely nothing to cite, record the QUERY and its date instead
of the conclusion --- Scryfall `o:"deals damage to" o:"that source"`,
2026-08-18, no hit. Either shape: build the query from the printed template
(from a card in `data/cards/` or the CR's wording, never from a constructor
name), scope the noun (`the pool` reads as `data/cards/` in some comments and
as every printing ever released in others), and name the card that WOULD refute
you. `Pawl.Types.SpendManaAsThough`'s `only` field is the model in the tree.

## Stale reads

pawl's recurring defect shape is a consumer reading a snapshot where the rule
asks about live state --- a condition that went derived while its consumer
stayed stored, or a gate reading the bindings captured when resolution began,
so a slot an earlier clause defined is invisible to a later one. When your
change adds a gate, a prompt or a condition, ask what it reads and WHEN that
was captured; `Pawl.Engine.Resolve.gateHolds` and its callers are where this
bites.

Related: one writer, every road. When you record an event, grep for every
function that performs the action --- `Daytime.turnDue` reaches
`Game.turnFaceOver` directly, bypassing `Resolve.turnOver`, so recording in the
opcode's arm alone would have missed the day/night road entirely.

## Mutation testing

`CLAUDE.md` requires mutating the change away and re-running. How:

- **One mutation at a time, and read the failure.** Red is not the bar: it must
  go red for the *intended* reason, in the intended place. A mutation that
  fails a dozen unrelated cases, or the target case with a different message
  than the behaviour predicts, has proved something else.
- **Name the assertion the mutation reddened, every time.** Not "the case went
  red" --- the assertion's own message --- then ask whether it is the
  gameplay-level assertion this unit exists to prove. If not, a cheap proxy
  ahead of it (a prompt count, a zone size, a list length) absorbed it and
  reported itself, and the real assertion may never have run. Reorder so the
  behavioural assertion precedes every proxy, keep the proxy after it, re-run.
  Do this even when the red looks right: a proxy's message usually names the
  same objects the behaviour does, which is why reading it as confirmation is
  the easy mistake, and agents who had read this advice have made it anyway.
  `Pawl.ExpirySpec`'s "CR 514.2 / 611.2a neither cleanup nor the handoff into
  bob's turn reaches it" is the worked example in the tree.
- **Ordering the gameplay assertion first is necessary, not sufficient.** It
  also has to be able to DIFFER under the mutation: if the board has not
  advanced far enough, both readings produce the same value and the assertion
  is vacuous however early it sits. PR #1806's counter case resolved only the
  stack's TOP object, which cannot tell "countered" from "still on the stack".
  Ask what a wrong implementation would have produced at the moment you read
  the value; for a counter that means resolving the stack down.
- **Never `git checkout <file>` to revert a mutation** --- real edits have been
  lost that way. Copy the file to a backup and move it back.
- **Build a negative as a pair of boards differing in exactly one thing.** A
  negative assembled on its own board passes for reasons you did not choose.
- **Keep the mutated binding referenced.** Deleting a use trips
  `-Wunused-local-binds` under `-Werror`, so a real red comes back as a build
  failure. Neutralize the value instead --- `const Map.empty . f`, `filter
  (const False)`, a `seq`.
- **Run the mutation through the NARROWEST path that shows the behaviour.** A
  test driving the whole priority loop can answer for the wrong reason.
- **Report a mutation you could not run.** If a behaviour holds by
  construction, or `-Werror` rejects the mutated source, say plainly --- at the
  code site and in the PR --- that the assertion is a regression fence rather
  than a proof.
- **A mutation that leaves the suite green means the change has no observer.**
  Do not close the issue. Add the card that gives the line an observer; failing
  that it is `wontfix` or `expires:synthetic`. Where you keep the line anyway
  because the CR states it, say so at the site and in the PR, and never let the
  green read as coverage.

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
  mana as the positive board that succeeds.
- **`Activate.activatable` answers `False` for a mana ability on every board**
  (CR 605.3b). Never route a negative through it; assert at gameplay level.
- **Two-player boards collapse "that player", "an opponent" and "the defending
  player" onto one seat.** Three seats disambiguate.
- **A fixture player decked by CR 104.3c** loses before the assertion runs.
  Stock the libraries whenever the fixture draws or advances turns.
- **Numeric coincidences.** Distinct values everywhere.
- **A prompt short-circuits when candidates equal the count**, so "the player
  was asked" can pass because they never were. Offer more candidates than
  needed.
- **A token exiled from a graveyard ceases to exist** (CR 111.7), so a token
  victim makes an exile assertion unobservable.
- **An answerer can silently repair the assertion.** One that searches for a
  legal option finds the right one again after your mutation. Pin the answer by
  index, and check the assertion reads the engine's output.
- **An answerer that BUILDS a recipient silently loses the target.** A
  `Prompt.ChooseTargets` over `Pool.Creatures` offers `Recipient.ToCreature`; a
  hand-built `Recipient.ToObject` of the same permanent is a different
  recipient, and CR 608.2b's re-read at resolution drops it with no error.
  FILTER the offered set instead.
- **A pure `Prompt r -> r` answerer cannot tell two structurally identical
  prompts apart**, so it answers both the same way and the test is green
  whatever the engine did. Thread state instead --- an answerer in
  `State.State` counting or indexing its calls, as `Pawl.CopySpec` and
  `Pawl.ManaSpec` do with `countingAnswer` --- and assert on the sequence.
- **A fixture supplies preconditions your assertion rests on.**
  `Pawl.Support`'s builders settle what they place (`S.addCreature` writes
  `Sickness.Settled`), stock what they draw from, and leave what they place
  untapped. Name the precondition the behaviour needs and assert it on the
  board, or the test proves the fixture --- and a hand-built negative lacking
  it differs in two things.
- **A `Pawl.Support` counter may index by a different question than your
  assertion's wording.** `S.countOnBattlefieldByName` takes a `PlayerId`, but
  `Game.zoneMembers` indexes the battlefield by OWNER (CR 108.3), so it cannot
  see who CONTROLS anything. `Projection.controllerOf` is the control question.
- **Two conditions a board cannot tell apart.** Name the other reading of the
  rule and check the board distinguishes them --- same zone for two
  destinations, same timestamp for two layers, one player holding two roles. If
  not, change the board, not the assertion.
- **A test asserting only one arm of a rule that states exclusions.** CR
  701.27g excludes both a front face that was previously turned and a melded
  permanent; a board showing only the positive case proves neither.

## Cards

Never take a card's printed values on trust from a brief OR an issue body; both
have carried a wrong mana cost and a card claimed to be in the pool that was
not. Fetch the Oracle text yourself (`CLAUDE.md` has the `curl`) and diff. For
a double-faced card read the `card_faces` array, not the top-level text.

If a clause cannot be expressed, say which, and whether the omission leaves
pawl's card **stricter** or **weaker** than printed. Weaker in the controller's
favour is the dishonest direction and disqualifies the card --- find another
producer or stop. Stricter is admissible with an issue and an inline `(#N)`.

**A stale transcription looks exactly like a missing capability.** Before
concluding the engine cannot express something, grep `data/cards/` for a card
that already uses it. Having fixed one card, **sweep the corpus for its
siblings**. A wrong value survives precisely where no test pays it --- Life and
Limb carried `{G}{G}` for a `{3}{G}` card because nothing ever cast it.

### Prior art, when you need a producer or a field shape

`CLAUDE.md` says when to consult these. Each is one grep:

- **`_scratch/phase`** (MIT) finds the CARD: `crates/engine/tests/integration/`
  holds a thousand-plus test files named for the card and the rule, plus
  rule-keyed files under `rules/`. Not a design reference --- its core still
  carries a card-identity check.
- **`_scratch/mtgish`** (MIT) gives the effect's SHAPE: the pool as first-order
  typed ASTs in `data/mtgish.lines.json`, vocabulary in
  `rust_syntax/src/mtg_types.rs`. It carries no marker for text its parser
  could not express, so presence is not evidence the rules text came through
  whole.
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
more candidates make it a real choice. Follow them unless the rule says
otherwise, and then say so in the PR.

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
- **When your sites collide with an unmerged PR**, branch off that PR's branch
  instead and keep yours a draft, then `git rebase --onto main <old-base>` once
  it squash-merges and re-run the suite and the load-bearing mutations against
  the merged state. Say in the PR body which base it was cut from.
- Immediately before the self-review and the push, `git fetch` and merge
  `origin/main` again, however recently you last did --- a PR armed just before
  you were dispatched typically lands mid-run. Then re-run the suite and the
  load-bearing mutations against the merged state.
- **Never force-push after opening.** `git merge origin/main` and keep the
  merge commit; a rebase discards the one auto-merge adds. Resolve conflicts by
  taking **both** sides, then re-run the mutations.
- **Measure the suite count, never infer it.** Report before -> after, and say
  so again after any merge from `origin/main` moved the baseline.
- **Landing a capability a census tracks means editing the census in the same
  PR** --- #875 (CR 116 special actions), #876 (CR 701 keyword actions), #877
  (CR 702 keyword abilities). Nothing checks the three bodies, so every row is
  yours: the eponymous case, a row landed under another name, and a row under
  no constructor of the tracked type at all. Re-read the row as it stands --- a
  PR earlier in the same session may already have edited it.
- **Landing a capability means reading what it unblocked**; `CLAUDE.md` has the
  query. Say in the PR which dependents are now workable.
- **Closing #N means re-deriving every inline `(#N)` in the tree**, not just
  the site you fixed. Grep the BARE number over the whole tree, not the file
  set a brief or issue body names --- that set has been incomplete three times
  in one session, each miss a live elision claiming a capability was missing
  that had landed months earlier. Grep again after the last merge from
  `origin/main`.
- **Do not close an issue your unit only narrowed.** Where a sibling site keeps
  it open, write "related to #N", never a closing keyword --- in any branch
  commit, since it survives the squash --- and retitle the issue to name what
  is left.

Before pushing: stage, `hooky fix` (`CLAUDE.md`), stage again. One hook bites
differently by hand: `script/format-json.sh` takes `MODE FILE...` and passes
vacuously when run bare.

Nothing checks a `CR` citation. Re-reading the rule in `docs/rules.txt` is the
only check there is: a citation naming a real rule that does not say what the
sentence claims passed the old script silently, which is how `CR 108.1` sat in
this file citing ownership (CR 108.3) through weeks of green CI. A CR update
renumbers rules in the tree and in issue bodies alike, so after taking one,
grep the renumbered rules across both and correct them --- the tree in the PR,
the bodies in a comment.

Then stop. Do not wait on CI, and do not start another unit.
