# Implementing a unit

Read this first when you are dispatched to work an issue end to end and open a
PR. `CLAUDE.md` and `CONTRIBUTING.md` still apply and override anything here;
this file is the standing procedure a dispatch brief would otherwise repeat, so
the brief carries only what is specific to your unit.

You hold the build. Nothing else is building while you are.

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

    cabal test --test-options '--timeout 2s --hide-successes'

The timeout catches infinite loops. It is not an assertion about speed, so a
case that sits near the budget is not a regression to chase. Two subtrees carry
their own, looser budgets via `Tasty.localOption` in `Main.hs`, which beats the
command line; that is what keeps the small global figure safe.

Never pipe `cabal test` or `cabal build` output --- it stalls a ~30s suite for
minutes. One build at a time. No `cabal clean`; the incremental build under
`pedantic` is sufficient.

Adding or deleting a module needs `cabal-gild pawl.cabal` run directly, since
`hooky fix` acts only on staged files.

## Enumerate the edit sites in one pass

Compiler round trips are the loop's main cost. When you add a constructor or a
field, grep an existing sibling to enumerate every site, and patch them all
before rebuilding --- do not let `-Werror` name them one build at a time.

Then find the sites `-Werror` will *not* name. See `CLAUDE.md`'s note on `{}`
and `_` patterns for where those live, and say in the PR which ones you read
and why each is correct as it stands. "I read all of them" is a finding;
silence is not.

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

- **Two conditions a board cannot tell apart.** Before writing the assertion,
  name the other reading of the rule and check the board distinguishes them ---
  same zone for two different destinations, same timestamp for two layers, the
  same player holding two roles. If it does not, change the board, not the
  assertion.

## Cards

Verify Oracle text with `curl` against `api.scryfall.com` --- WebFetch gets
403s here, and the vendored dumps under `_scratch/` are stale. Never transcribe
from a brief; briefs have carried a wrong mana cost and a card claimed to be in
the pool that was not.

If a clause cannot be expressed, say which, and say whether the omission leaves
pawl's card **stricter** or **weaker** than printed. Weaker in the controller's
favour is the dishonest direction and disqualifies the card --- find another
producer or stop. Stricter is admissible with an issue and an inline `(#N)`.

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

`Closes #N` must be bare plain text in the PR body --- backticks break the
link. **Never write close, fix or resolve next to an issue number you do not
intend to close, in any phrasing including a denial.** PR #832 wrote "Does not
close #797" and closed #797. Write "related to #N" or "advances #N". This
applies to commit messages too: a keyword in any branch commit survives the
squash.

Before pushing, run what CI runs:

    ormolu --mode check $(git ls-files '*.hs')
    hlint .
    script/format-json.sh check data/cards/*.json

That script takes `MODE FILE...`. Run bare it iterates over nothing and passes
vacuously, so always give it the corpus.

Then stop. Do not wait on CI, and do not start another unit.
