# Branch/PR Development Workflow Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the retired milestone loop with a documented branch/PR workflow, so the process docs match the repository's actual protection rules and the issue-driven work ahead.

**Architecture:** Documentation only — no Haskell changes, no test-suite changes. Six files: one new (`.github/pull_request_template.md`), one rewritten (`docs/workflow.md`), three edited (`CLAUDE.md`, `CONTRIBUTING.md`, `docs/design.md`), one frozen (`docs/progress.md`). Verification is `hooky run` plus greps that assert the retired vocabulary is gone.

**Tech Stack:** Markdown, `hooky` (ormolu/hlint/cabal-gild/cabal-check/file-hygiene runner), `gh` CLI, GitHub rulesets.

**Spec:** `docs/superpowers/specs/2026-07-25-branch-pr-workflow-design.md`

## Global Constraints

- **This is a documentation change.** No file under `source/` is touched. `cabal build` and the test suite are unaffected; do not modify them.
- **Work on branch `202-branch-pr-workflow`**, already created and already carrying the spec commit. Do not create a new branch and do not switch to `main`.
- **Never commit to `main`, never merge a PR, never push to `main`, never force-push.** Only the repository owner merges.
- **Stage explicit paths.** Use `git add <path>`, never `git add -A` — concurrent sessions may leave foreign files in `git status`, and `_scratch/` is gitignored.
- **`hooky` acts on staged files only.** The sequence is always: `git add <paths>` → `hooky fix` → `git add <paths>` again (fix reformats) → `hooky run`. If it says "hooks skipped", nothing was staged and nothing was checked.
- **Commit messages** follow the existing convention: `type(scope): subject`, imperative, lowercase after the colon. Every commit in this plan ends with a `Part of #202` trailer line — only the PR itself closes the issue.
- **Prose style:** these docs use em dashes, bold for load-bearing claims, and cite CR rules by number. Match the surrounding voice. Do not add trailing whitespace (file hygiene will reject it) and end every file with exactly one newline.
- **Suite size is currently 1162 tests** (`docs/progress.md` line 2509). Use that number where a template needs an example.

---

## File Structure

| File | Status | Responsibility after this plan |
|---|---|---|
| `.github/pull_request_template.md` | Create | Prompts a PR author for what the merger needs: what/why, rules basis, approach, verification, invariant check, deferrals |
| `docs/workflow.md` | Rewrite whole | The development loop — how work is picked, built, audited, and landed. Absorbs the card-driven loop guidance currently stranded in untracked `_scratch/` |
| `CLAUDE.md` | Edit two sections | Auto-loaded pointer file. "Current work and tracking" collapses from ~60 lines to a short status; "Executing a plan" becomes "Working a unit" |
| `CONTRIBUTING.md` | Edit two sections | Human-facing contributor guide. Gains a Workflow section; the stale "no test suite yet" claim is corrected |
| `docs/progress.md` | Edit header | Frozen historical record of the milestone era |
| `docs/design.md` | Edit one paragraph | §3's stale completion claim corrected; M6/M7 marked as ordinary issues |

Task order matters: the PR template (Task 1) and `workflow.md` (Task 2) are what the later files point *at*, so they land first. Tasks 3–5 are independent of each other.

---

### Task 1: The pull request template

**Files:**
- Create: `.github/pull_request_template.md`

**Interfaces:**
- Consumes: nothing.
- Produces: the path `.github/pull_request_template.md`, referenced by name in Task 2 (`docs/workflow.md`) and Task 4 (`CONTRIBUTING.md`). GitHub auto-populates PR bodies from this exact path — do not rename it.

- [x] **Step 1: Verify the template does not exist yet**

Run:
```bash
ls .github/pull_request_template.md
```
Expected: `No such file or directory`. (If it exists, stop — the plan's assumption is wrong.)

- [x] **Step 2: Create the template**

Create `.github/pull_request_template.md` with exactly this content:

```markdown
<!-- One PR per logical chunk of work. See docs/workflow.md. -->

## What and why

<!--
Closes #N — or "Part of #N" when a large issue is spanning several PRs.
Each PR must be independently mergeable and leave main green.
-->

## Rules basis

<!--
The CR citations behind this change, each checked against docs/rules.txt.
Never recalled from memory — recalled rules go stale, and have been wrong before.
Delete this section if the change touches no rules.
-->

## Approach

<!-- The design calls made, and the alternatives rejected. -->

## Verification

- [ ] `cabal build all --enable-tests --enable-benchmarks` is warning-clean
- [ ] `hooky fix` applied, `hooky run` passes
- [ ] Suite: 1162 -> 1162
- [ ] Proving test:

## Invariant check

<!--
Does this diff make the rules core case on an effect's *identity* rather than on a
classification? Fusing the closed and open halves is the project's single named
failure mode. An explicit "no" is cheap; say it.
-->

## Deferred

<!--
What is not implemented, with the issue filed and (#N) cited at the code site.
"Nothing" is a fine answer.
-->
```

- [x] **Step 3: Stage, format, and lint**

Run:
```bash
git add .github/pull_request_template.md
hooky fix
git add .github/pull_request_template.md
hooky run
```
Expected: hooks pass. If file hygiene complains about trailing whitespace or a missing final newline, fix it and rerun.

- [x] **Step 4: Verify the checked-in template has no trailing whitespace**

Run:
```bash
grep -n ' $' .github/pull_request_template.md
```
Expected: no output (exit status 1).

- [x] **Step 5: Commit**

```bash
git commit -m "chore(github): add a pull request template

Carry what the merger needs on the PR itself: what and why, the CR
citations behind it, how it was verified, an explicit invariant check,
and what was deferred. The PR body replaces the spec and plan documents
the milestone loop produced.

Part of #202"
```

---

### Task 2: Rewrite `docs/workflow.md`

**Files:**
- Rewrite: `docs/workflow.md` (currently 82 lines, titled "The milestone loop")

**Interfaces:**
- Consumes: `.github/pull_request_template.md` from Task 1 (referenced by path).
- Produces: `docs/workflow.md` as the single process doc. Tasks 3 and 4 point at it by path. Section names other files may reference: "The loop", "The unit of work", "Why the required-check set is loose", "Specs and plans", "Model tiering", "Context discipline".

**Note:** this file absorbs the card-driven loop guidance that currently exists only in untracked `_scratch/post-m5-phase2-card-loop.md`. `_scratch/` is gitignored, so those scratch files need no git action — they are superseded, and may be deleted locally or left alone.

- [x] **Step 1: Confirm what the current file claims, so the rewrite is a real replacement**

Run:
```bash
head -8 docs/workflow.md
grep -c 'milestone' docs/workflow.md
```
Expected: title `# The milestone loop`, and a nonzero count (currently 12). This count going to a small number is the check in Step 4.

- [x] **Step 2: Replace the entire file**

Overwrite `docs/workflow.md` with exactly this content:

```markdown
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
```

- [x] **Step 3: Stage, format, and lint**

Run:
```bash
git add docs/workflow.md
hooky fix
git add docs/workflow.md
hooky run
```
Expected: hooks pass.

- [x] **Step 4: Verify the retired vocabulary is gone and the new content is present**

Run:
```bash
head -1 docs/workflow.md
grep -c 'One PR is one logical chunk of work' docs/workflow.md
grep -n 'Output: `docs/superpowers' docs/workflow.md
```
Expected: `# The development loop`; count `1`; and **no output** from the third grep — the old "Output:" lines pointing at spec/plan paths must be gone.

- [x] **Step 5: Verify the internal doc references resolve**

`workflow.md` lives in `docs/`, so its relative references must resolve from there.

Run:
```bash
ls docs/superpowers/specs/2026-07-25-branch-pr-workflow-design.md
ls .github/pull_request_template.md
ls docs/design.md docs/progress.md docs/rules.txt docs/prior-art-lessons.md
```
Expected: all six paths exist.

- [x] **Step 6: Commit**

```bash
git commit -m "docs(workflow): replace the milestone loop with the development loop

Work is issue-driven now: pick an issue, branch, TDD, /code-review, open
a PR, stop. Records why the ruleset's required-check set is deliberately
loose, and absorbs the card-driven loop guidance that had been stranded
in untracked scratch — the unit is one opcode proven by a card, breadth
is the progress signal, pool-unlocking primitives first.

Part of #202"
```

---

### Task 3: Shrink and retarget `CLAUDE.md`

**Files:**
- Modify: `CLAUDE.md` — the "Current work and tracking" section (lines 36–138) and the "Executing a plan" section (lines 180–202)

**Interfaces:**
- Consumes: `docs/workflow.md` from Task 2 (referenced by path).
- Produces: a `CLAUDE.md` whose status is self-maintaining (it points at `gh issue list` rather than restating milestone history), so no future unit needs to rewrite it.

**Why this task matters beyond tidiness:** `CLAUDE.md` is auto-loaded into every session *and every subagent spawn*. The status bullet is currently ~60 lines of Auras implementation detail. Removing it is a recurring context saving, not a cosmetic edit.

- [x] **Step 1: Confirm the current size, so the reduction is measurable**

Run:
```bash
wc -l CLAUDE.md
sed -n '36,40p' CLAUDE.md
```
Expected: 244 lines total; the section header `## Current work and tracking` followed by the long `- **Status: M0–M5, ...` bullet.

- [x] **Step 2: Replace the status and tracking bullets**

Replace everything from line 36's `## Current work and tracking` through line 110 (the bullet ending `Follow it for all\n  milestone work.`) with:

```markdown
## Current work and tracking

- **Status: the closed half is built.** M0–M5.6 have landed, together with the
  M3.5 cards-as-data and M5.5 count/compare interstitials, M4.5's closed-half
  gap census, the mulligan-adjacent closures, and the card-driven **Auras**
  unit. `docs/progress.md` is the frozen record of what each established.
  **Work is now issue-driven, not milestone-driven** — `gh issue list` says
  what is next. M6 (the transpiler) and M7 (interpreters) are ordinary issues,
  #9 and #10.
- **The development workflow is `docs/workflow.md`** — pick an issue, branch,
  TDD, `/code-review`, open a PR, stop. **One PR per logical chunk of work.
  Never commit to `main`, never merge a PR, never push to `main`** — `main`'s
  ruleset requires a pull request, and only the repository owner merges.
```

**Keep the three bullets that follow unchanged** — "Keywords are closed half…", "Outstanding work is tracked in GitHub Issues…", and "File the issue, cite it inline…". They are evergreen and were not part of the milestone framing.

- [x] **Step 3: Verify the deleted range removed only what was intended**

Run:
```bash
grep -c 'Keywords are closed half' CLAUDE.md
grep -c 'File the issue, cite it inline' CLAUDE.md
grep -c 'Outstanding work is tracked in GitHub Issues' CLAUDE.md
grep -c 'Control Magic' CLAUDE.md
```
Expected: `1`, `1`, `1`, and `0`. The first three bullets survive; the Auras implementation detail is gone.

- [x] **Step 4: Rewrite the "Executing a plan" section**

Replace the section that begins `## Executing a plan` and runs through the bullet ending `Where the rules leave nothing to ask, don't prompt.` with:

```markdown
## Working a unit

Work happens on a branch and lands as a pull request. `docs/workflow.md` is the
full loop; the load-bearing rules:

- **One PR per logical chunk of work**, usually one issue. A large issue may
  span several PRs — each independently mergeable, each leaving `main` green;
  only the last says `Closes #N`, the others `Part of #N`.
- **Branch from current `main`**, named `<issue>-<slug>`, e.g.
  `29-combat-damage-departed-blockers`.
- **TDD is not optional:** write each failing test and actually run it to watch
  it fail before implementing.
- **Commit granularity inside a branch does not matter** — squash merge
  collapses it. Commit as often as is convenient.
- **Run `/code-review` on the branch before opening the PR** — the invariant
  audit and the rules-correctness pass. Fix findings on the branch.
- **Every CI check green at hand-off.** Only `Test` blocks a merge, and that
  looseness is deliberate (`workflow.md` says why); a red `Ormolu` means
  `hooky` was skipped, which is a bug in the work.
- A spec or plan is **optional, not ceremony** — write one when the unit
  warrants it and commit it in the same PR. If you are following a plan: tasks
  strictly in order, and **never** edit the plan, weaken an assertion, or
  delete a test to make a check pass. If the plan looks wrong, **stop and say
  so** — it has been wrong before.
- The two invariants outrank everything: the engine never cases on a card's
  identity (only classifications), and never makes a player's choice. Eliding a
  prompt is legitimate only for indistinguishable options, and every elision
  carries an issue. Where the rules leave nothing to ask, don't prompt.
```

- [x] **Step 5: Verify the section rename and the size reduction**

Run:
```bash
grep -c 'Executing a plan' CLAUDE.md
grep -c 'Working a unit' CLAUDE.md
grep -c 'one small complete commit on `main`' CLAUDE.md
wc -l CLAUDE.md
```
Expected: `0`, `1`, `0`, and a total well under 200 lines (down from 244).

- [x] **Step 6: Verify no reference to the retired loop survives**

Run:
```bash
grep -n 'milestone workflow\|milestone loop\|session-per-phase' CLAUDE.md
```
Expected: no output.

- [x] **Step 7: Stage, format, and lint**

Run:
```bash
git add CLAUDE.md
hooky fix
git add CLAUDE.md
hooky run
```
Expected: hooks pass.

- [x] **Step 8: Commit**

```bash
git commit -m "docs(claude): point at the issue tracker instead of milestone history

The status bullet was ~60 lines of Auras implementation detail, paid on
every session and every subagent spawn. Milestones are done, so status
is now a short pointer and 'gh issue list' answers what is next.
'Executing a plan' becomes 'Working a unit', carrying the branch/PR
rules and the never-merge-or-push-to-main constraint.

Part of #202"
```

---

### Task 4: Add a workflow section to `CONTRIBUTING.md`

**Files:**
- Modify: `CONTRIBUTING.md` — line 27 (the stale test-suite claim) and a new section before "Commits and versioning" (line 88)

**Interfaces:**
- Consumes: `docs/workflow.md` (Task 2) and `.github/pull_request_template.md` (Task 1), both referenced by path.
- Produces: nothing later tasks depend on.

**Audience note:** `CONTRIBUTING.md` is written for a human outside contributor, not for an agent. Keep it shorter and less prescriptive than `workflow.md`, and do not mention agents, subagents, or model tiering.

- [x] **Step 1: Confirm the stale claim is present**

Run:
```bash
grep -n 'There is no test suite yet' CONTRIBUTING.md
```
Expected: a hit on line 27. (There are 1162 tests — the claim is years-of-work out of date.)

- [x] **Step 2: Correct the testing claim**

Replace line 27, which reads:

> There is no test suite yet. When there is, `cabal test` will run it.

with the following (note the outer fence below is **five** backticks purely so
this plan can quote fenced blocks — the text you write into `CONTRIBUTING.md`
uses ordinary three-backtick fences):

`````markdown
```sh
cabal test    # the tasty suite
cabal bench   # the tasty-bench benchmarks
```

Build `all` when you touch anything the suites use — they break separately from
the library:

```sh
cabal build all --enable-tests --enable-benchmarks
```
`````

- [x] **Step 3: Add the Workflow section**

Insert immediately **before** the `## Commits and versioning` heading:

```markdown
## Workflow

`main` is protected: it takes changes only through a pull request, and only the
repository owner merges them.

1. **File or pick an issue.** It is the spec for the work.
2. **Branch from current `main`**, named `<issue>-<slug>` — say,
   `29-combat-damage-departed-blockers`.
3. **Work, with tests.** Commit as often as you like; merges are squashed, so
   a branch's internal history is working state, not a record.
4. **Open a pull request.** `.github/pull_request_template.md` prompts for what
   a reviewer needs: what and why, the rules citations behind it, how it was
   verified, and what was deferred.
5. **Wait for CI.** `Test` is the only check that blocks a merge. The others —
   `Ormolu`, `HLint`, `Gild`, `Cabal` — are deliberately non-blocking: `hooky`
   already runs them locally, and a contributor's PR won't be bounced over
   formatting. It will be merged and tidied.

**One pull request per logical chunk of work.** A large issue may span several,
each independently mergeable and each leaving `main` green; only the last one
closes the issue.

`docs/workflow.md` has the longer version, including how work is picked.
```

- [x] **Step 4: Verify both edits landed**

Run:
```bash
grep -c 'There is no test suite yet' CONTRIBUTING.md
grep -c '^## Workflow' CONTRIBUTING.md
grep -n '^## ' CONTRIBUTING.md
```
Expected: `0`; `1`; and a heading list in which `## Workflow` appears immediately before `## Commits and versioning`.

- [x] **Step 5: Stage, format, and lint**

Run:
```bash
git add CONTRIBUTING.md
hooky fix
git add CONTRIBUTING.md
hooky run
```
Expected: hooks pass.

- [x] **Step 6: Commit**

```bash
git commit -m "docs(contributing): document the branch and pull request workflow

Describe how a change actually lands: file or pick an issue, branch,
open a PR, and note that only the owner merges. Says plainly that the
non-blocking formatting checks are deliberate, so a contributor's PR is
tidied rather than bounced. Also corrects the claim that there is no
test suite — there are 1162 tests.

Part of #202"
```

---

### Task 5: Freeze the historical records

**Files:**
- Modify: `docs/progress.md` — insert a note after the intro, before the first `- **M0 is complete**` bullet (around line 17)
- Modify: `docs/design.md` — line 176, the paragraph beginning "This section is the **forward plan**."

**Interfaces:**
- Consumes: `docs/workflow.md` from Task 2 (referenced by path from both files).
- Produces: nothing later tasks depend on.

**Why both in one task:** each is a single paragraph asserting the same fact — the milestone era ended — and a reviewer would accept or reject them together.

- [x] **Step 1: Confirm both stale states**

Run:
```bash
grep -n 'M0 through M4h are complete' docs/design.md
grep -n 'This file is' docs/progress.md
```
Expected: a hit in `design.md` around line 176 (its claim predates M5, M5.5, M5.6 and Auras), and a hit near the top of `progress.md`.

- [x] **Step 2: Freeze `progress.md`**

Insert this paragraph after the intro paragraph that ends `history does not change.` and before the `- **M0 is complete**` bullet:

```markdown
**This log is closed.** It covers the milestone era — M0 through M5.6, the
interstitials, and the card-driven Auras unit — which ended on 2026-07-25. Work
after that is issue-driven and lands as pull requests; the merged PR and the
issue it closes are the record, and no further entries are appended here. See
`workflow.md`.
```

- [x] **Step 3: Correct `design.md` §3**

Replace line 176 in full:

```markdown
This section is the **forward plan**. For what has actually landed — one distilled entry per completed milestone, with its gate card, the decision it proved, the opcodes/types it added, and every elision and its named expiry — see the completion log in `progress.md`. Milestones **M0 through M4h are complete** — the whole of M4 (M4a–M4g, §3's split table below) plus its fast-follow M4h.
```

with:

```markdown
This section **was** the forward plan, and is now largely the record of the path actually taken. Milestones **M0 through M5.6 are complete**, as is the card-driven Auras unit; for what each established — gate card, the decision it proved, the opcodes and types it added, every elision and its expiry — see the completion log in `progress.md`. Only M6 and M7 remain, and they are no longer milestones: they are ordinary GitHub issues, #9 and #10, picked up when they are picked up. Work is issue-driven now — see `workflow.md`.
```

Keep it as a single line: `design.md` is written with one paragraph per line, unwrapped. Do not re-wrap it.

- [x] **Step 4: Verify both edits, and that design.md's one-paragraph-per-line style survived**

Run:
```bash
grep -c 'M0 through M4h' docs/design.md
grep -c 'M0 through M5.6 are complete' docs/design.md
grep -c 'This log is closed' docs/progress.md
awk 'NR==176 {print NF}' docs/design.md
```
Expected: `0`; `1`; `1`; and a word count well above 40 on line 176, confirming the paragraph is still one unwrapped line.

- [x] **Step 5: Stage, format, and lint**

Run:
```bash
git add docs/progress.md docs/design.md
hooky fix
git add docs/progress.md docs/design.md
hooky run
```
Expected: hooks pass.

- [x] **Step 6: Commit**

```bash
git commit -m "docs: close the milestone log and correct the path's status

progress.md is frozen at the end of the milestone era; the merged PR and
its issue are the record from here. design.md §3 claimed M0-M4h were
complete, stale by M5, M5.5, M5.6 and Auras, and still framed itself as
a forward plan when only M6 and M7 remain — and those are ordinary
issues now.

Part of #202"
```

---

### Task 6: Final sweep and open the pull request

**Files:**
- No file changes expected. This task verifies the branch as a whole and hands it off.

**Interfaces:**
- Consumes: every preceding task.
- Produces: an open PR against `main`, and the plan's own completion.

- [x] **Step 1: Confirm the branch state**

Run:
```bash
git status --short
git log --oneline main..HEAD
```
Expected: a clean working tree, and six commits — the spec plus Tasks 1–5.

- [x] **Step 2: Sweep the tracked docs for surviving references to the retired process**

Run:
```bash
grep -rn 'milestone loop\|milestone workflow\|the milestone loop' \
  --include='*.md' . \
  --exclude-dir=_scratch --exclude-dir=.git --exclude-dir=dist-newstyle \
  --exclude-dir=plans --exclude-dir=specs
```
Expected: hits only where the retirement is being *described* (`docs/workflow.md`'s intro, the spec, this plan). Any hit that instructs a reader to *follow* the milestone loop is a miss — fix it and amend the relevant commit.

Files under `docs/superpowers/plans/` and `specs/` are excluded on purpose: they are the frozen historical record and must not be rewritten.

- [x] **Step 3: Verify the plan's own progress check**

Run:
```bash
grep -c -- '- \[ \] \*\*Step' docs/superpowers/plans/2026-07-25-branch-pr-workflow.md
```
Expected: `0`. Use *that* grep, not `grep -c -- '- \[ \]'` — this file quotes checkbox syntax in prose and in the PR template, so the naive grep can never reach zero.

- [x] **Step 4: Verify the build is untouched**

This plan changes no Haskell. Confirm rather than assume:

```bash
git diff --stat main..HEAD -- source/ pawl.cabal
```
Expected: no output — no file under `source/` and not `pawl.cabal` was modified.

- [x] **Step 5: Run `/code-review` on the branch**

Per `docs/workflow.md`'s step 4, audit the branch diff before opening the PR. For a documentation-only branch the invariant audit is trivially satisfied (no Haskell changed), so the review's real job is: does any doc now *state* something false about the rules, the ruleset, or the engine? Fix findings on the branch.

- [x] **Step 6: Push the branch**

```bash
git push -u origin 202-branch-pr-workflow
```

- [x] **Step 7: Open the pull request**

`Rules basis` is not applicable — a documentation change cites no CR rules — so
state that explicitly rather than deleting the heading silently. `Verification`
reports `hooky run` clean and the suite unchanged at 1162, since no Haskell was
touched.

```bash
gh pr create --base main --title "Adopt a branch/PR development workflow" --body "$(cat <<'EOF'
## What and why

Closes #202.

Milestones M0–M5.6 and the Auras unit are done, `main` now carries a ruleset
that requires pull requests, and the remaining work is issue-driven. The process
docs still described the retired milestone loop and still told the reader to
commit directly to `main` — which the ruleset no longer permits.

This replaces that loop with a branch/PR workflow: pick an issue, branch, TDD,
`/code-review`, open a PR, stop. One PR per logical chunk of work — usually one
issue, though a large issue may span several independently mergeable PRs.

## Rules basis

Not applicable — no comprehensive-rules claim is made or changed. No file under
`source/` is touched.

## Approach

The issue becomes the spec and the PR body becomes the plan, so
`docs/superpowers/{specs,plans}/` and `docs/progress.md` are frozen as the
milestone era's historical record rather than deleted. The invariant audit that
the retired close step carried moves to `/code-review` on the branch, before the
PR is opened.

Two things are deliberately *not* done, and are written down as such so a later
reader does not "fix" them: the ruleset's required-check set stays loose
(`hooky` enforces formatting locally, and an outside contributor's PR should be
merged and tidied rather than bounced), and git worktrees are rejected for
concurrent units because a per-worktree `dist-newstyle` costs more than the
parallelism returns.

## Verification

- [x] No Haskell changed — `git diff --stat main..HEAD -- source/ pawl.cabal` is empty
- [x] `hooky fix` applied, `hooky run` passes on every commit
- [x] Suite: 1162 -> 1162 (untouched)
- [x] No tracked doc still instructs a reader to follow the milestone loop

## Invariant check

No. The diff contains no Haskell, so the rules core cannot have gained a case on
an effect's identity.

## Deferred

`deleteBranchOnMerge` is still `false`; flipping it is the owner's call.
EOF
)"
```

- [x] **Step 8: Report and stop**

Report the PR URL and its CI status. **Do not merge it, do not enable auto-merge, and do not start another unit** — only the repository owner merges.

---

## Out of scope

Recorded so a reader does not mistake these for oversights:

- **The ruleset is not modified.** Its loose required-check set is deliberate; `docs/workflow.md` now says why.
- **`deleteBranchOnMerge` stays `false`.** Flipping it is the owner's call.
- **`_scratch/post-m5-phase1-issue-cleanup.md` and `_scratch/post-m5-phase2-card-loop.md` need no git action** — `_scratch/` is gitignored. Phase 2's substance moves into `docs/workflow.md` in Task 2; Phase 1's list is drained and superseded.
- **No CI job is added.** A mechanical invariant-grep job was considered and rejected in favour of `/code-review`.
- **The agent's stored memory** (`commit-style`, which currently says "commit directly to main; never push", and `milestone-workflow-doc`) lives outside the repository and is updated separately from this branch.
