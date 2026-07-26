# The development loop

How work gets picked, built, and landed. The process companion to `design.md`
(what to build and why) and `progress.md` (what the milestone era built).

This file used to describe a milestone loop. That loop is retired: M0–M5.6 and
the card-driven Auras unit are done, and everything remaining is issue-driven.
The reasoning is in
`docs/superpowers/specs/2026-07-25-branch-pr-workflow-design.md`.

## The loop

`main` is protected and takes changes only through a pull request. **Only the
repository owner merges.** An agent's work therefore terminates at *opening a
PR that is likely to be merged* — not at a merge.

1. **Pick.** `gh issue list`. If no issue covers the work, file one first — the
   issue is the spec.
2. **Branch.** Cut from current `main`, named `<issue>-<slug>`, e.g.
   `29-combat-damage-departed-blockers`. The branch↔issue link should be
   legible from `git branch` alone.
3. **Work.** TDD, non-negotiable: write the failing test, run it, watch it
   fail, then implement. `cabal build` warning-clean; `hooky fix` then
   `hooky run`. Commit as often as is convenient — squash merge collapses the
   branch to one commit, so intra-branch granularity is working state, not a
   historical record.
4. **Audit.** Run `/code-review` on the branch diff **before** opening the PR:
   the invariant audit (does the rules core case on an effect's *identity*
   anywhere in this diff?) and the rules-correctness pass (every rules claim
   checked against `rules.txt`, never memory). Fix findings on the branch.
   This is the descendant of the whole-branch review that the milestone loop's
   close step carried — `progress.md` records that pass catching real defects,
   so it is not optional decoration.
5. **Open the PR.** `.github/pull_request_template.md` prompts for what the
   merger needs. Every CI check should be green at hand-off.
6. **Hand off and stop.** Report the PR and its CI status. Do not start the
   next unit — one unit at a time, one checkout (see below).

## The unit of work

**One PR is one logical chunk of work**, usually one issue.

- **A large issue may span several PRs.** Splitting is fine, and often better
  than one unreviewable diff. Each PR must be independently mergeable and must
  leave `main` green on its own. Only the final PR says `Closes #N`; the
  earlier ones say `Part of #N`.
- **A single PR may close several issues.** This is the normal case in the
  card-driven loop, where one card forces a subset of the dormant backlog at
  once.

### The card-driven loop

Most of what remains is the `expires:card-driven` backlog. **It is not a
worklist** — it is the *menu of what a card can force*. Each card implemented
forces some subset of it; those get built on demand and closed as a side
effect. Never build one speculatively.

- **The unit is one opcode or classification, proven by a card — not one
  card.** Per `design.md` §4, an effect is not done until a card exercises it
  in a gameplay-level test. The card is the *proof*, not the deliverable. A
  card that adds no new opcode coverage is not a unit of work.
- **Breadth is the progress signal; card count is not.** Chasing the count
  early manufactures motion without progress, and is a burnout trap.
- **Pool-unlocking primitives first.** "Hardest first, to prove it works"
  versus "simplest first, to open the pool" is a false choice: the invasive
  cards and the pool-unlocking cards are largely the same cards. Once the pool
  is open, selection is free — work backward from whichever dormant gap you
  most want retired.
- **Pause and ask when a card needs a ruling call**, or forces a decision a
  human should make. Staying in the loop is a feature, not a bug.

## Concurrency

**One unit at a time, in the single checkout.** Branch-per-unit makes a shared
checkout actively hazardous — two sessions would contend for `HEAD`. Git
worktrees were considered and rejected: each needs its own `dist-newstyle`, and
cold Haskell builds are expensive enough that the parallelism does not pay for
itself here.

After opening a PR, work stops until the owner merges it. That is the intended
shape, not a stall.

## Why the required-check set is loose

The ruleset on `main` requires a pull request (**0 approving reviews**),
squash-only merges, no force-push and no branch deletion, and exactly one
status check: **`Test`**. `Ormolu`, `HLint`, `Gild`, `Cabal` and `Bench` run on
every PR but cannot block a merge. There are no bypass actors — the owner's
admin rights do not exempt them.

**That looseness is deliberate. Do not "fix" it.** `hooky` runs those same
checks locally as a pre-commit hook, so they are enforced where the work
happens; and keeping them non-blocking means an outside contributor's PR with
bad formatting can be merged and tidied afterward rather than bounced. A red
`Ormolu` on an agent-authored PR is not a policy hole — it means `hooky` was
skipped, which is a bug in the work. **Every check should be green at
hand-off.**

CI takes roughly six minutes per PR on a warm cache, across macOS, Linux and
Windows.

## Specs and plans

`docs/superpowers/specs/` and `docs/superpowers/plans/` hold the milestone era's
specs and plans. They remain authoritative for what M0–M5.6 and Auras
established, and they stop growing.

Writing a new spec or plan is **optional, not ceremony**. When a unit is big
enough that you want one — the serialization codec (#126), the transpiler (#9)
— write it and commit it in the same PR as the work. For a one-line citation
fix, the issue is the spec and the PR body is the plan.

If you are following a plan document: work tasks **strictly in order**, tick
each `- [ ]` as you finish that step, and **never** edit the plan, weaken an
assertion, or delete a test to make a check pass. If the plan looks wrong,
**stop and say so** — it has been wrong before. A test failing against correct
code is a plan bug: fix the plan's test, not the engine. Progress check:
`grep -c -- '- \[ \] \*\*Step' <plan>` must reach `0` — use *that* grep, not
the naive `grep -c -- '- \[ \]'`, which can never reach 0 because the plan
template quotes the checkbox syntax in prose.

## Model tiering

Spend where errors cascade; economize where the rails are strong. Execution
burns the most tokens by far and has the strongest guardrails — the
failing-test-first discipline, the `-Werror` pedantic build, `hooky`, and the
rule that plan assertions must never be weakened all make executor mistakes
loud rather than silent. That is where the cheap model goes.

| Work | Model | Why |
|---|---|---|
| Design calls, deciding what a unit is | Fable or Opus | Highest leverage, lowest volume; design errors cascade into everything downstream |
| Writing a spec or plan, when the unit warrants one | Opus | Plan bugs are expensive — executors follow them faithfully, and the plan has been wrong before |
| Orchestrating a unit's execution | Opus | Dispatches and judges; low volume, needs judgment |
| Implementer subagents | Sonnet | The volume phase; rails catch mistakes. Set via the Agent tool's `model` parameter |
| `/code-review` — invariant audit, rules-correctness | Fable or Opus | Fusing the halves is the project's single named failure mode; don't economize on the auditor |
| Search subagents, `rules.txt` citation checks, mechanical chores | Haiku | Lookup and transcription, not judgment |

For interactive sessions, set the tier with `/model` at session start.
`/model opusplan` — Opus in plan mode, Sonnet for execution — is a coarse
built-in version of the same split.

## Context discipline

- **`CLAUDE.md` is loaded into every session and every subagent spawn.** It
  stays a pointer, not an archive: current status plus where the detail lives.
  Every line added there is paid for on every future spawn.
- **Read docs by section, on demand.** The doc map below says where each kind
  of answer lives; grep or jump to the section rather than reading the file.
  Reading whole docs "for context" front-loads tens of thousands of tokens
  before the first question.
- **Subagents get task-sized context.** One task, file pointers, the relevant
  invariant — not the plan, not the spec, not the history.
- **A unit is ordinarily one session.** When a unit is large enough to warrant
  a written spec and plan, giving each its own session still pays: the
  artifacts are the handoff, and compaction costs tokens and loses fidelity.

| Question | Where |
|---|---|
| What's next | `gh issue list` |
| Architecture rationale | `design.md`, the relevant § only |
| What the milestone era established | `progress.md` (frozen), newest entry first |
| What's left: elisions, gaps, bugs | GitHub Issues — `gh issue list -l elision` |
| How work gets picked, built, and landed | this file |
| A landed milestone's authoritative detail | Its spec, then its plan, under `docs/superpowers/` |
| Rules ground truth | `rules.txt`, grepped by rule number — never memory |
| Prior-art evidence | `prior-art-lessons.md`, cited § only |
