# Implementing a unit

Read this first when you are dispatched to work an issue end to end and open a
PR. `CLAUDE.md` and `CONTRIBUTING.md` still apply and override anything here;
this file is the standing procedure a dispatch brief would otherwise repeat, so
the brief carries only what is specific to your unit.

You hold the build. Nothing else is building while you are.

## Before you plan

`CLAUDE.md` says to distrust an issue body's claims. Distrust its estimate of
SIZE the same way: three units in a row needed no new machinery at all, because
a funnel the engine already routes through, or an ordering it already imposes,
satisfied the rule outright --- and in two of them the issue's own analysis had
overstated the work.

So the cheapest first move on a "needs new machinery" issue is to find that
funnel --- the one function every path to the behaviour goes through --- and
read what it already gates or orders. When it turns out to satisfy the rule,
the unit is a test and a comment.

## Before the first build

Copy `cabal.project.local` in from the primary checkout. A fresh worktree does
not have it, so `+pedantic` and `-Werror` are off and a locally green build
says nothing about CI.

Run that `cp` as a BARE command of its own, and confirm the file is there before
the first build. The worktree-isolation guard refuses a compound command
wholesale, and its error names the other half --- one run read the refusal as
being about the `git` it was chained with, and every build and mutation in it
silently ran unpedantic until CI caught it.

## Running the suite

    cabal test --test-options '--timeout 5s --hide-successes'

The timeout catches infinite loops. It is not an assertion about speed, so a
case that sits near the budget is not a regression to chase --- and 2s, the
figure this used to name, was near the budget: seven cases run 1.0-1.7s
unloaded and one of them timed out twice on nothing but concurrent build load.
The machine is shared --- the owner's own build and test run alongside agents'
worktrees, all contending on one GHC job semaphore --- so a TIMEOUT on one of
those seven is background noise. Re-run it unloaded before investigating; a
real hang fails at any budget. Two subtrees carry their own budgets via
`Tasty.localOption` in `Main.hs`, which beats the command line. CI sets 5s
suite-wide through `flake.nix`'s `testFlags`.

Never pipe `cabal test` or `cabal build` output. It stalls a ~30s suite for
minutes, and under a backgrounded run it also blinds you: `cabal test | tail`
leaves the output file EMPTY, so the run cannot be read at all. Redirect to a
file and read the file. One build at a time. No `cabal clean`; the incremental
build under `pedantic` is sufficient.

Adding or deleting a module means staging `pawl.cabal` along with the module,
so that `hooky fix` regenerates its `exposed-modules`; an unstaged `.cabal` is
skipped.

## Enumerate the edit sites in one pass

Compiler round trips are the loop's main cost. When you add a constructor or a
field, grep an existing sibling to enumerate every site, and patch them all
before rebuilding --- do not let `-Werror` name them one build at a time.

Then find the sites `-Werror` will *not* name. See `CLAUDE.md`'s note on `{}`
and `_` patterns for where those live, and say in the PR which ones you read
and why each is correct as it stands. "I read all of them" is a finding;
silence is not.

## Prose the compiler cannot check

A comment asserting a limit the engine used to have becomes false the moment
the limit lifts, and nothing catches it: it carries no issue number, so
`script/check-gaps.sh` never looks at it, and `-Werror` has nothing to say
about a sentence. One sweep found roughly twenty --- "pawl's projection does
not reach a hand", "walks the battlefield only", "a card in a library has no
projection" --- each true when written.

This genre is not mechanically checkable, and proposing a script for it wastes
a cycle: the elision check works because that genre has fixed wording and an
issue number to test against, and prose rot has neither. Judging one of these
comments means knowing what the code now does, which is the thing no grep can
answer.

The discipline instead: when your change lets a capability reach somewhere it
previously did not, grep the tree before you push for prose asserting the old
limit --- the name of what you widened, the zone or type it now reaches, and
the absolutes these claims are written in (`only`, `never`, `does not`, `no
card`). Re-derive every hit and rewrite it to say what it actually rests on.

Sweep `source/test-suite/` along with the libraries, and the file you are
editing along with the ones you are not. Both are where the survivors keep
turning up: the sites #1562 had to clean up afterwards were split between the
engine and the specs, and included files the widening PR had itself edited.
Every example in this section is an engine module, which is plausibly why the
specs were skipped.

The alternation that found the projection family, as a starting point rather
than a checklist --- widen it with the words your own change makes false:

    git grep -niE 'no projection|not projected|never projected|projection does not reach|walks the battlefield|only view'

Then write the replacement so a later grep finds it. A comment that enumerates
a type's arms BY NAME is turned up by the grep `CLAUDE.md` already asks for
when you add a constructor; "all four of `Pawl.Types.Affected`" is turned up by
nothing, and that one had gone false against a five-arm type.

## Stale reads

pawl's recurring defect shape is a consumer reading a snapshot where the rule
asks about live state --- a condition that went derived while its consumer
stayed stored, or a gate reading the bindings captured when resolution began,
so a slot an earlier clause defined is invisible to a later one. When your
change adds a gate, a prompt or a condition, ask what it reads and WHEN that
was captured; `Pawl.Engine.Resolve.gateHolds` and its callers are where this
has bitten.

## Mutation testing

`CLAUDE.md` requires mutating the change away and re-running. What it does not
say:

- **Apply the mutations one at a time and read the failure.** Red is not the
  bar: it must go red for the *intended* reason, and only in the intended
  place. A mutation that fails a dozen unrelated cases, or fails the target
  case with a different message than the behaviour predicts, has proved
  something else.

- **Never `git checkout <file>` to revert a mutation.** An agent lost real
  edits that way. Copy the file to a backup first and move the backup back.

- **Build a negative as a pair of boards differing in exactly one thing.** The
  positive and the negative share mana, seats, timing and stock; the single
  difference is the thing under test. A negative assembled on its own board
  passes for reasons you did not choose.

- **Keep the mutated binding referenced.** Deleting a use rather than changing
  an answer makes `-Werror` reject the source on `-Wunused-local-binds`, so the
  suite never runs and a real red comes back as a build failure. Neutralize the
  value instead --- `const Map.empty . f`, a `filter (const False)`, a `seq` ---
  so every binding still has a use.

- **Run the mutation through the NARROWEST path that shows the behaviour.** A
  test that drives the whole priority loop can answer a mutation for the wrong
  reason: a settle sweeps a conditional effect whose condition is already false,
  so the loop cannot tell "never started" from "started and was swept". Resolving
  through the single step under test made the same mutation discriminate.

- **Report a mutation you could not run.** If a behaviour holds by construction
  rather than by a guard you wrote, or `-Werror` rejects the mutated source so
  the suite never runs, there is nothing to break. Say plainly that the
  assertion is a regression fence rather than a proof --- at the code site and
  in the PR. Do not let a fence read as coverage. Several units have turned on
  this.

- **A mutation that leaves the suite green means the change has no observer.**
  Do not close the issue as done. Add the card that gives the line an observer;
  failing that it is `wontfix` or `expires:synthetic`.

Re-run at least the load-bearing mutations after any merge from `origin/main`.
A bad conflict resolution can neuter a test while leaving the suite green.

Run the suite before anything else after that merge, too. A card file written in
a superseded wire spelling makes the registry throw `InvalidCorpus` and aborts
the WHOLE corpus load, so every case dies at once and it reads as a
catastrophic regression. It is usually one stale card file, and the loader stops
at the first bad one -- so grep the corpus for the old spelling rather than
eyeballing your own diff.

## Vacuity traps

These have each shipped a green-but-meaningless test in this repository:

- **Negative cast-gate assertions pass for unrelated reasons.** A spell may be
  uncastable for want of mana, for timing, or because the stack is not empty.
  Build every negative board with the same mana as the positive board that
  succeeds, and prove the branch flips only on the thing under test.

- **`Activate.activatable` answers `False` for a mana ability on every board**
  (CR 605.3b keeps them off the stack). Never route a negative through it;
  assert at gameplay level instead.

- **Two-player boards collapse "that player", "an opponent" and "the defending
  player" onto one seat.** Three seats disambiguate. If a mutation that swaps
  one for another does not go red, the board is too small.

- **A fixture player decked by CR 104.3c** loses before the assertion runs.
  Stock the libraries whenever the fixture draws or advances turns.

- **Numeric coincidences.** Distinct values everywhere --- if two readings of
  the rule produce the same number on your board, the test cannot tell them
  apart.

- **A prompt short-circuits when candidates equal the count**, so "the player
  was asked" can pass because they were never asked at all. Give the choice
  more candidates than it needs.

- **A token exiled from a graveyard ceases to exist** (CR 111.7), so a token
  victim makes an exile assertion unobservable.

- **An answerer can silently repair the assertion.** If the fixture's answerer
  picks by searching for a legal option rather than by index, it will find the
  right one again after your mutation, and the test stays green while the
  engine's own choice is broken. Pin the answer, and check the assertion
  actually reads the engine's output.

- **An answerer that BUILDS a recipient silently loses the target.** A
  `Prompt.ChooseTargets` over `Pool.Creatures` offers `Recipient.ToCreature`, so
  a hand-built `Recipient.ToObject` of the same permanent is a different
  recipient. The binding is stored and the ability looks correctly targeted; CR
  608.2b's re-read at resolution then drops it and the resolution applies
  nothing --- no error, and no failed assertion pointing at the cause. FILTER
  the offered set instead of constructing a recipient.

- **Two conditions a board cannot tell apart.** Before writing the assertion,
  name the other reading of the rule and check the board distinguishes them ---
  same zone for two different destinations, same timestamp for two layers, the
  same player holding two roles. If it does not, change the board, not the
  assertion.

## Cards

Verify Oracle text with `curl` against `api.scryfall.com` --- WebFetch gets
403s here, and the vendored dumps under `_scratch/` are stale. Never transcribe
a card's printed values from a brief OR from an issue body; both have carried a
wrong mana cost --- `{3}{R}` for a card printed `{3}{R}{R}` --- and a card
claimed to be in the pool that was not.

If a clause cannot be expressed, say which, and say whether the omission leaves
pawl's card **stricter** or **weaker** than printed. Weaker in the controller's
favour is the dishonest direction and disqualifies the card --- find another
producer or stop. Stricter is admissible with an issue and an inline `(#N)`.

**A stale transcription looks exactly like a missing capability.** Before
concluding the engine cannot express something, grep `data/cards/` for a card
that already uses it. One issue filed as "no optional as-enters life payment
exists" was a single card transcribed a clause short, against a capability that
had been in the tree for months. Report it in the same stricter/weaker terms:
a card missing a clause plays wrong, and which way it errs is the finding.

Having fixed one card, **sweep the corpus for its siblings** --- every other
card written in the same shape. It is one grep, and it turns a one-card fix
into a claim about the corpus; without it the next instance of the same defect
waits for an issue of its own.

## Prompts

Do not mint a bespoke resolution-time choice. `Pawl.Types.Prompt`'s
`ChooseRingBearer`, `ChooseBolster`, `ChooseAmass`, `ChooseBlight` and
`ChooseCopyTarget` all share one posture --- choose, not target; filter the
candidates rather than trusting the answer; raise the prompt only when two or
more candidates make it a real choice. Read those constructors and their
comments first, and follow them unless the rule you are implementing says
otherwise, in which case say so in the PR. Four units in one run each had to be
told this separately.

## Git and the PR

- Branch off latest `origin/main`, named `issue-slug`. Never commit to `main`.
- Immediately before the self-review and the push, `git fetch` and merge
  `origin/main` again --- even if you merged earlier, and even if you merged
  minutes ago. A PR armed just before you were dispatched typically lands
  mid-run; three units in a row hit conflicts this way. Then re-run the suite
  and the load-bearing mutations against the merged state.
- Open the PR as a draft; mark it ready once the self-review findings are
  pushed and the suite is green.
- **Never force-push after opening.** `origin/main` moves; `git merge
  origin/main` and keep the merge commit. A rebase discards it.
- Resolve conflicts by taking **both** sides, then re-run the mutations.
- **Landing a capability a census tracks means editing the census in the same
  PR.** The censuses are #875 (CR 116 special actions), #876 (CR 701 keyword
  actions) and #877 (CR 702 keyword abilities). PR #1485 landed amass and left
  #876's row under "not implemented", PR #1527 landed ward and left #877's, so
  `script/check-census.sh` now catches the eponymous case --- run it, and edit
  the body it names. #875 and a row landed under another name are still yours
  alone.
- **Landing a capability means reading what it unblocked.** `gh api
  repos/tfausak/pawl/issues/N/dependencies/blocking` lists the issues that
  named yours as their blocker; say in the PR body which of them are now
  workable. Leave the link in place --- a satisfied one is the record.
- **Closing #N means re-deriving every inline `(#N)` in the tree, not just the
  one at the site you fixed.** Other sites cite the same issue for their own
  reasons. One PR found three of four `(#379)` sites were guarded off by
  unrelated work, and their comments now carry the argument instead of a
  number; a citation left pointing at a closed issue says nothing.

`Closes #N` must be bare plain text in the PR body --- backticks break the
link. **Never write close, fix or resolve next to an issue number you do not
intend to close, in any phrasing including a denial.** PR #832 wrote "Does not
close #797" and closed #797. Write "related to #N" or "advances #N". This
applies to commit messages too: a keyword in any branch commit survives the
squash.

Before pushing, STAGE your files and run `hooky fix`. One command, about a
second, and it runs every check CI runs (ormolu, hlint, cabal-gild, `cabal
check`, nixfmt) plus two CI does not: the JSON formatter and the citation
check. Do not run the tools one at a time, and do not reach for `--all` unless
you suspect something landed unstaged --- it sweeps the tree for two minutes to
say the same thing. Stage again afterwards; `hooky fix` rewrites in place.

Two of the hooks bite differently when you run them by hand:

- `script/format-json.sh` takes `MODE FILE...`. Run bare it iterates over
  nothing and passes vacuously, so give it the corpus.
- `script/check-citations.sh` checks every `CR <number>` against
  `docs/rules.txt` and defaults to the whole tree. Run it bare after taking a
  CR update, since a renumbering breaks citations in files you never touched.
  It found eight wrong citations on one branch, two of them rule numbers that
  never existed --- write CR numbers from `docs/rules.txt`, never from memory,
  and treat a number a brief or an issue hands you as memory: one brief cited
  CR 118 for paying life, which this revision numbers 119.

Two more checks are NOT hooks, because they read GitHub and `hooky fix` is
offline: `script/check-census.sh` (a census row still saying "not implemented"
about a constructor that exists) and `script/check-gaps.sh` (an elision comment
citing a closed issue). Both take no arguments, take a second, and run in CI's
Tracker job --- which you will not be waiting for, so run them yourself before
you push.

Then stop. Do not wait on CI, and do not start another unit.
