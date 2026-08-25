# The drain loop

The goal string below is pasted into a `/goal`; the procedure under it is what
the orchestrator reads from this file once it starts. Both live here because
they are coupled to `docs/agents/implementing.md` and
`docs/agents/researching.md` --- changing a role file usually means changing
this. `/goal` caps its condition at 4000 characters, which is why the procedure
is not the goal.

## Shape

Two lanes.

- **The build lane** runs one implementation agent at a time, in an isolated
  worktree; `jobs: $ncpus` already saturates the machine. Its unit of exclusion
  is the BUILD, not the merge: a ready PR waits ten-plus minutes on CI, and a
  lane that waits for the merge idles for all of it (~30% of wall-clock per
  unit, measured). Dispatch the next unit when the current one's PR is ready,
  and let auto-merge keep the merges serial.
- **The research lane** runs read-only agents alongside. They never touch the
  build, so they are free wall-clock. Anything the implementer would otherwise
  re-derive costs the same tokens on either lane and only one is the critical
  path. Push it here --- but see "Research one unit ahead" for the bound.

Measured 2026-08-16: ~25 min from dispatch to PR-ready, ~5 of them compiling
(cold build ~2.5 min, incremental ~2); ~12 min from ready to merged; the CI
Build job ~13 min. The rest is reading and writing, so wall-clock is bought by
taking work off the build lane, not by thinking less. The backlog closes at
about the rate it grows, so the birth rate and the fold-in rate are the levers
--- hence clusters, the fold-in rule, and the standing staleness sweep.

When the build lane's queue empties, that is a signal, not a gap: a design
question for the owner, not a speculative dispatch.

## Bounding the goal

"Until there are no issues left" is not reachable: closing a unit surfaces
roughly one new issue. Bound it instead: a unit count, a wall-clock box, "until
no `priority-high` remains", or **"until N consecutive dispatches come back
blocked"** --- the best convergence signal, since a depleted tier returns
decompositions instead of PRs.

## The goal string

Paste this, with the bound filled in:

---

Work the pawl issue backlog autonomously until <BOUND>. First `git fetch` and
read `git show origin/main:docs/agents/drain-loop.md`; follow its "Procedure"
section exactly --- it is the standing procedure for this loop and carries the
dispatch, research, merging and scheduling rules; do not improvise around it.
Re-read it whenever a merged PR touches it. Derive everything against
`origin/main`, never the working checkout.

---

## Procedure

**Dispatch.** Pick an unassigned issue with no `blocked` label, preferring
`priority-high`. **Do not rank by open dependents.** The dependency graph is a
shallow forest, not a tree: the overwhelming majority of merged units report
`dependencies/blocking` empty and unblock nothing, so "a capability that
unblocks three issues" describes almost no issue in the backlog. Rank by
`priority-high`, then by whatever is file-disjoint from the build.

`blocked` now covers two things: an issue-to-issue blocker, and an issue
awaiting a design call from the owner (#146, #1828, #2167 carry the label with
no linked blocker). Neither is dispatchable unattended.

Dispatch an implementation agent, with `isolation: "worktree"`, to work it end
to end and open a PR. Its brief must open with: read
`docs/agents/implementing.md` first, then `CLAUDE.md` and `CONTRIBUTING.md`.
Everything else is specific to the unit.

**Dispatch on ready, not on merge.** The moment a unit's PR is marked ready,
dispatch the next one. Two guards make this safe: the next brief must be
FILE-DISJOINT from the PR still in flight (see "Scheduling"), and it derives
against `origin/main`, so a later `git merge origin/main` brings the in-flight
PR in cleanly. If the in-flight PR goes red, send its agent back; the two
builds share the semaphore and that is fine.

**When nothing is file-disjoint, stack rather than idle.** Dispatch the next
unit off the in-flight PR's branch as a draft, and have it rebase `--onto main`
after that PR squash-merges, re-running the suite and the load-bearing
mutations against the merged state. This worked twice in one run on
`Projection.hs` and on `Game.hs`. Prefer a disjoint issue where one exists;
stack when the alternative is an idle lane.

**Dispatch a cluster only when one issue's edit sites contain the others'.**
A cluster is one dispatch, one worktree, one PR closing every issue in it, and
when it is real the per-unit fixed cost is paid once. But a shared TOPIC is not
a cluster: the eleven topic trackers #2190--#2200 each assert shared machinery
in the body, and every one of them split into unrelated units under triage.
Require the researcher to name the function or constructor every issue in the
cluster edits; without that, dispatch them separately.

**Research one unit ahead, not more.** The in-flight implementation rewrites
the files the next brief is derived against, so research further ahead than one
unit is derived against a tree that no longer exists --- a research pass was
killed mid-run for exactly this, its subject function being rewritten by the
active unit. Keep one read-only agent running alongside the build, and make its
subject file-disjoint from the build for the same reason the next dispatch must
be. Its brief must open with: read `docs/agents/researching.md` first.

Standing assignments, in order of yield:

- turn the NEXT issue (or cluster) into a dispatch-ready brief carrying what
  the implementer would otherwise re-derive --- see "Writing a brief" in the
  role file, which now says which fields to carry and which to stop writing
- the **staleness sweep** the role file describes, against the TREE. Sweeping
  card names for fired `expires:card-driven` triggers is a closed seam: two
  independent passes over every open card-driven issue found none
- check whether a claimed missing capability still is missing

**Expect research to change the unit, not just describe it.** In one nine-unit
run it changed the scope or verdict of every issue it touched: one had no
producer left and was relabelled instead of built, two would have been declined
as blocked on framings that were stale, one would have shipped a fix that
introduced a rules violation. Read the brief's verdict before dispatching, and
be willing to relabel, retitle or close instead of building.

**But expect the implementer to change it again.** Over a day's brief-driven
units, re-derivation survived contact and prediction did not: the issue body
was the most frequently corrected artefact in the run, and no drafted wire
spelling, and almost no drafted board or predicted mutation, survived. Treat a
brief's predictions as leads for the implementer, never as decisions the
dispatcher has already made.

**Relay briefs by path.** A researcher writes its brief to a file and returns
the path. Pass the path to the implementation agent; do not retype it.

**The scratchpad is shared, so give every agent a subdirectory named for its
unit.** Concurrent lanes write to one directory. One agent's backup file was
overwritten by another's, and restoring it injected a different unit's
in-flight code into the tree --- a corruption no build catches, because both
sides compile.

**The GHC job semaphore breaks under concurrency.** `CLAUDE.md` describes the
symptom and the escape (`cabal test --no-semaphore -j4`); what the loop adds is
frequency. With several lanes live it is a recurring event, not a rare one, and
the commonest cause is a tool timeout reaping a backgrounded `cabal`. Tell
agents to run `cabal` in the foreground with a generous timeout, and never to
`pkill` by pattern.

**Merging.** Arm auto-merge (squash) on each PR. The ruleset requires branches
be up to date, so an armed auto-merge silently stalls at `BEHIND` --- poll
`mergeStateStatus` and run `gh pr update-branch`. That adds a merge commit to
the agent's branch, which is why agents must not force-push. On a conflict,
send the agent back to merge `origin/main`, resolve by taking both sides, and
**re-run its mutations**.

**Scheduling, by subsystem rather than by a predicted file list.** Never have
two units in flight that edit the same file, unless they are deliberately
stacked; `Event.hs` and `CardSpec.hs` are where conflicts cluster. But a
researcher's precise files-touched list was wrong every time a PR reported on
it --- naming modules that do not exist, the wrong spec file, a placement that
turns out to be an import cycle --- where a coarse "this unit is in the mana
subsystem" would have been right each time. Ask for the subsystem plus the one
or two files the unit certainly rewrites, and treat anything finer as a guess.
A predicted collision is a guess too: verify one before you let it stall a
dispatch.

**Before dispatching any fix to CI, read the failing job log.** A job that died
in `Set up job` looks like a real failure and is not. Retry logic must handle
`cancel` as well as `fail`.

**Expect main to move.** The owner lands work in parallel; derive against
`origin/main`, never the working checkout.

**Blocked is a good outcome.** If a unit cannot land unattended, the agent
should add the `blocked` label, link the blocker as a GitHub dependency the way
`CLAUDE.md` says, and report that. Where the blocker is a decision rather than
an issue, the label goes on with no link and the report names the question for
the owner. A decomposition beats a half-landed unit.
