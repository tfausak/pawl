# The milestone loop

How milestones get built, and how to spend model tokens doing it. This is the
process companion to `design.md` (what to build and why) and `progress.md`
(what was built). It applies to every remaining milestone — the loop retires
only when the comprehensive rules are fully implemented. Treat a deviation as
a bug: fix this doc or follow it.

## The loop

One cycle = one milestone, lettered sub-milestone, or phase. **Each numbered
step below is a fresh session** — the artifact each step produces is the
handoff to the next. Never ride one session across a step boundary: compaction
costs tokens to summarize and loses fidelity, and the artifacts exist
precisely so that continuity is not needed.

1. **Orient.** `CLAUDE.md`'s status bullet (auto-loaded) says what's next.
   If more context is needed: the milestone's own subsection of `design.md`
   §3, and the newest relevant entry of `progress.md`. Never read either
   file whole.
2. **Spec** — the brainstorming skill. Seed the session with the milestone's
   `design.md` subsection (plus the umbrella spec, for phased work); pull
   other docs only when a specific question needs them, not preemptively.
   Every rules claim checked against `rules.txt` and cited by number.
   Output: `docs/superpowers/specs/YYYY-MM-DD-<slug>-design.md`.
3. **Plan** — the writing-plans skill, against the finished spec. Not
   brainstorming again: the spec settled the design questions, and
   re-litigating them at frontier-model prices is the most expensive form of
   waste in the loop. Output: `docs/superpowers/plans/YYYY-MM-DD-<slug>.md`.
4. **Execute** — the subagent-driven-development skill. Tasks strictly in
   order, one small complete commit each, TDD non-negotiable (see
   `CLAUDE.md`, "Executing a plan"). Each implementer subagent receives one
   task's text plus file pointers — never the whole plan or spec.
5. **Close.** Review (the invariant audit and rules-correctness pass),
   completion entry appended to `progress.md`, `CLAUDE.md`'s status bullet
   **replaced** (never appended — history lives in `progress.md`), the
   `design.md` / umbrella-spec checkbox ticked.

## Model tiering

Spend where errors cascade; economize where the rails are strong. Execution
burns the most tokens by far and has the strongest guardrails — the
failing-test-first discipline, the `-Werror` pedantic build, hooky, and the
rule that plan assertions must never be weakened all make executor mistakes
loud rather than silent. That is where the cheap model goes.

| Work | Model | Why |
|---|---|---|
| Spec / brainstorming | Fable or Opus | Highest leverage, lowest volume; spec errors cascade into everything downstream |
| Plan writing | Opus | Plan bugs are expensive — executors follow them faithfully, and the plan has been wrong before |
| Execution — orchestrating session | Opus | Dispatches and judges; low volume, needs judgment |
| Execution — implementer subagents | Sonnet | The volume phase; rails catch mistakes. Set via the Agent tool's `model` parameter |
| Review — invariant audit, rules-correctness | Fable or Opus | Fusing the halves is the project's single named failure mode; don't economize on the auditor |
| Search subagents, `rules.txt` citation checks, mechanical chores | Haiku | Lookup and transcription, not judgment |

For interactive sessions, set the tier with `/model` at session start (the
fresh-session-per-step structure makes this natural). `/model opusplan` —
Opus in plan mode, Sonnet for execution — is a coarse built-in version of
the same split.

## Context discipline

- **`CLAUDE.md` is loaded into every session and every subagent spawn.** It
  stays a pointer, not an archive: current status plus where the detail
  lives. Every line added there is paid for on every future spawn.
- **Read docs by section, on demand.** The doc map below says where each
  kind of answer lives; grep or jump to the section rather than reading the
  file. Reading whole docs "for context" front-loads tens of thousands of
  tokens before the first question.
- **Subagents get task-sized context.** One task, file pointers, the
  relevant invariant — not the plan, not the spec, not the history.

| Question | Where |
|---|---|
| What's next / current status | `CLAUDE.md` status bullet |
| Forward path, architecture rationale | `design.md`, the relevant § only |
| What landed, and what each milestone established | `progress.md`, newest entry first |
| What's left: elisions, gaps, bugs | GitHub Issues — `gh issue list -l elision` |
| A milestone's authoritative detail | Its spec, then its plan, under `docs/superpowers/{specs,plans}/` |
| Rules ground truth | `rules.txt`, grepped by rule number — never memory |
| Prior-art evidence | `prior-art-lessons.md`, cited § only |
