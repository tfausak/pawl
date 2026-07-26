# Branch/PR development workflow — design

**Date:** 2026-07-25
**Issue:** #202
**Status:** accepted

This is deliberately the **last** document in `docs/superpowers/specs/`. The
workflow it describes retires the spec-and-plan ceremony that filled this
directory; writing that retirement down as one final spec is the bookend, not a
contradiction.

## 1. Why now

Three facts converged:

- **The milestones are done.** M0–M5.6, the M5.5 and M5.6 interstitials, the
  mulligan-adjacent closures, and the Auras unit have all landed. M6 (#9) and
  M7 (#10) are ordinary issues, picked up whenever they are picked up. GitHub
  milestones were retired on 2026-07-24.
- **The remaining work is issue-shaped, not milestone-shaped.** What is left is
  a small actionable backlog plus ~127 `expires:card-driven` issues that are a
  *menu of what a card can force*, not a queue. Neither is a milestone.
- **`main` is already protected.** A ruleset went active on 2026-07-25 (§5).
  Committing directly to `main` no longer works, which the process docs did not
  yet reflect — `CLAUDE.md` and `docs/workflow.md` still described the
  milestone loop, and the standing instruction was to commit straight to `main`
  and never push.

The process, in other words, had already stopped matching the repository.

## 2. The unit of work

**One PR is one logical chunk of work.**

Typically that is one issue. Two deliberate exceptions:

- **A large issue may span several PRs.** Splitting is fine and often better
  than one unreviewable diff. Each PR must be independently mergeable and must
  leave `main` green on its own — building warning-clean with the suite
  passing. Only the final PR says `Closes #N`; the earlier ones say
  `Part of #N`.
- **A single PR may close several issues.** This is the normal case in the
  card-driven loop, where one card forces a subset of the dormant
  `expires:card-driven` backlog at once.

What a unit looks like in practice:

| Kind of work | The unit |
|---|---|
| Backlog drain | One actionable issue (`rules-correctness`, `bug`, `gap`) |
| Card-driven loop | One opcode or classification, **proven by a card** |
| Genuinely large work (#126 serialization, #9 the transpiler) | Still one logical chunk, but large enough to earn a written spec and plan committed inside the PR |

The card-driven loop's unit deserves emphasis, because it is the one place the
obvious reading is wrong. Per `design.md` §4, the unit is **one opcode proven by
a card, not one card**. An effect is not done until a card exercises it in a
gameplay-level test; the card is the *proof*, not the deliverable. A card adding
no new opcode coverage is not a unit of work. **Breadth is the progress signal;
card count is not** — chasing the count manufactures motion without progress.

Card selection ordering: **pool-unlocking primitives first**. The invasive cards
and the pool-unlocking cards are largely the same cards, so "hardest first" and
"simplest first" is a false choice. Once the pool is open, selection is free —
work backward from whichever dormant gap you most want retired.

## 3. The cycle

This replaces `docs/workflow.md`'s five-step milestone loop.

1. **Pick.** `gh issue list`. If no issue covers the work, file one first — the
   issue is the spec.
2. **Branch.** Cut from current `main`. Name it `<issue>-<slug>`, e.g.
   `29-combat-damage-departed-blockers`, so the branch↔issue link is legible
   from `git branch` alone.
3. **Work.** TDD is non-negotiable: write the failing test, run it, watch it
   fail, then implement. Commit as coarsely or finely as convenient — squash
   merge collapses the branch to one commit, so intra-branch granularity is a
   working convenience, not a historical record. `cabal build` warning-clean;
   `hooky fix` then `hooky run`.
4. **Audit.** Run `/code-review` on the branch diff **before** opening the PR —
   the invariant audit (does the rules core case on an effect's *identity*
   anywhere in this diff?) and the rules-correctness pass (every rules claim
   checked against `rules.txt`, never memory). Fix findings on the branch. This
   is the descendant of the whole-branch review that the retired close step
   carried; `progress.md` records it catching real defects, so it is not
   optional decoration.
5. **Open the PR**, with a body written to be merge-ready (§4).
6. **Hand off and stop.** Report the PR link and CI status. Do not start the
   next unit — see §6 on concurrency.

## 4. The PR body carries the case

The repository owner is the only person who merges. Everything an agent
contributes therefore terminates at *"open a PR that is likely to be merged"*,
and the PR body is the artifact that makes that case. It absorbs what the
retired spec, plan, and close-out documents used to carry.

**There is deliberately no `.github/pull_request_template.md`.** Guidance has
exactly two homes: `CLAUDE.md` for what an agent needs, `CONTRIBUTING.md` for
what a human needs. A template is a third copy that drifts from both, and a
GitHub-injected checklist is the wrong tool for prompting the party that
already reads `CLAUDE.md` on every spawn. The operative list lives in
`CLAUDE.md`'s "Working a unit"; `CONTRIBUTING.md` carries the human-facing
summary.

The body says:

- **What and why** — the chunk of work, and `Closes #N` / `Part of #N`.
- **Rules basis** — the CR citations, each verified against `rules.txt`.
- **Approach** — the design calls made, and the alternatives rejected. *(Was:
  the spec.)*
- **Verification** — build warning-clean, `hooky run` clean, suite count before
  → after, and which test proves the behavior.
- **Invariant check** — an explicit statement about whether the diff makes the
  rules core case on effect identity. Fusing the two halves is the project's
  single named failure mode; an explicit "no" per PR is cheap.
- **Deferred** — what is *not* implemented, with issues filed and `(#N)` cited
  at the code site.

## 5. Repository configuration

The active ruleset on `main`:

- Pull request required, **0 required approving reviews**
- **Squash-only** merges
- Required status check: **`Test`** (which needs `Build` and `Meta`, so those
  run transitively)
- No force-push, no branch deletion
- **No bypass actors** — the owner's admin rights do not exempt them

**The loose required-check set is deliberate and should not be "fixed".**
`Ormolu`, `HLint`, `Gild`, `Cabal` and `Bench` all run on every PR but cannot
block a merge. The reasoning: `hooky` already runs those same checks locally as
a pre-commit hook, so they are enforced where the work happens; and keeping them
non-blocking means an outside contributor's PR with bad formatting can be merged
and tidied afterward rather than bounced. A red `Ormolu` on an agent-authored PR
is not a policy hole — it means `hooky` was skipped, which is a bug in the work,
not in the ruleset. **Every check should be green at hand-off.**

CI takes roughly six minutes per PR on a warm cache, across macOS, Linux and
Windows.

## 6. Concurrency

**One unit at a time, in the single checkout.** Sessions do not run concurrently
on different units. Branch-per-unit makes a shared checkout actively hazardous —
two sessions would contend for `HEAD` — and git worktrees were considered and
rejected: each worktree needs its own `dist-newstyle`, and cold Haskell builds
are expensive enough that the parallelism does not pay for itself here.

A practical consequence: after opening a PR, work stops until the owner merges
it. That is the intended shape, not a stall.

## 7. What retires

| Retired | What replaces it |
|---|---|
| `docs/superpowers/specs/` as a required step | The GitHub issue |
| `docs/superpowers/plans/` as a required step | The PR body; a real plan only when the work warrants one, committed in the same PR |
| The "close" session | The PR, plus `/code-review` before opening it |
| `docs/progress.md` entries | The merged PR and its issue |
| `CLAUDE.md`'s milestone status bullet | `gh issue list` |
| One-small-commit-per-task on `main` | Squashed PRs; intra-branch commits are working state |

Nothing is deleted. `docs/superpowers/{specs,plans}/` and `docs/progress.md`
remain as the historical record of M0–M5.6 and Auras, and stay authoritative for
*what those milestones established*. They simply stop growing.

Spec and plan documents are still **available** for work large enough to want
them — the point is that they are no longer mandatory ceremony on a one-line
citation fix.

## 8. Documentation changes

- **`docs/workflow.md`** — rewritten from "The milestone loop" to the
  development loop of §3. The model-tiering table and the context-discipline
  section survive; the doc map is updated. Absorbs the card-driven loop guidance
  of §2, which currently lives only in untracked scratch.
- **`CLAUDE.md`** — the ~60-line milestone status bullet collapses to a short
  pointer, "Executing a plan" is rewritten around working a unit on a branch,
  and the never-merge / never-push-to-`main` rule is stated. `CLAUDE.md` is
  loaded into every session *and every subagent spawn*, so this is a real and
  recurring context saving.
- **`CONTRIBUTING.md`** — gains a workflow section (branch → PR → ruleset → the
  owner merges). Also corrects the stale claim that there is no test suite yet.
- **`docs/progress.md`** — a header note freezing it as the milestone record.
- **`docs/design.md`** — §3 claims "M0 through M4h are complete", stale by M5,
  M5.5, M5.6 and Auras. Corrected, with M6/M7 marked as ordinary issues.

## 9. Non-goals

An agent working this repository does not: push to `main`, merge a PR, enable
auto-merge, delete a branch, or force-push. Those are the owner's actions.

`deleteBranchOnMerge` is currently `false`. Flipping it to `true` would tidy
merged branches automatically; it is the owner's call and is left unchanged.
