# The drain loop

The goal string below is pasted into a `/goal`; the procedure under it is what
the orchestrator reads from this file once it starts. Both live here because
they are tightly coupled to `docs/agents/implementing.md` and
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
  re-derive --- Oracle text, card JSON, edit sites, the red test --- costs the
  same tokens on either lane and only one is the critical path. Push it here.

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
`priority-high`, then the issue with the most open dependents (`gh api
repos/tfausak/pawl/issues/N/dependencies/blocking`) --- a capability that
unblocks three issues outranks a leaf. Dispatch an implementation agent, with
`isolation: "worktree"`, to work it end to end and open a PR. Its brief must
open with: read `docs/agents/implementing.md` first, then `CLAUDE.md` and
`CONTRIBUTING.md`. Everything else in the brief is specific to the unit.

**Dispatch on ready, not on merge.** The moment a unit's PR is marked ready,
dispatch the next one. Two guards make this safe: the next brief must be
FILE-DISJOINT from the PR still in flight (the researcher annotates every brief
with the files it touches; if the only candidates overlap, pick a different
issue rather than waiting), and it derives against `origin/main`, so a later
`git merge origin/main` brings the in-flight PR in cleanly. If the in-flight PR
goes red, send its agent back; the two builds share the semaphore and that is
fine.

**Dispatch clusters, not only issues.** Ask the researcher for clusters (see
the role file). A cluster is one dispatch, one worktree, one PR closing every
issue in it; the per-unit fixed cost is the same as for a leaf and the issues
cannot conflict if one agent holds them all. Do not force one.

**Research in parallel.** Always keep at least one read-only agent running
alongside the build. Its brief must open with: read
`docs/agents/researching.md` first. Dispatch the next round when you dispatch
the build agent, not when you go idle. Standing assignments, in order of yield:

- turn the next few queued issues (or clusters) into dispatch-ready briefs
  carrying what the implementer would otherwise re-derive --- see "Writing a
  brief" in the role file
- the **staleness sweep** the role file describes
- check whether a claimed missing capability still is missing

**Relay briefs by path.** A researcher writes its brief to a file and returns
the path. Pass the path to the implementation agent; do not retype it.

**Merging.** Arm auto-merge (squash) on each PR. The ruleset requires branches
be up to date, so an armed auto-merge silently stalls at `BEHIND` --- poll
`mergeStateStatus` and run `gh pr update-branch`. That adds a merge commit to
the agent's branch, which is why agents must not force-push. On a conflict,
send the agent back to merge `origin/main`, resolve by taking both sides, and
**re-run its mutations**.

**Scheduling.** Never have two units in flight that edit the same file;
`Event.hs` and `CardSpec.hs` are where conflicts cluster. The researcher's
files-touched annotation is what dispatch-on-ready leans on.

**Before dispatching any fix to CI, read the failing job log.** A job that
died in `Set up job` looks like a real failure and is not. Retry logic must
handle `cancel` as well as `fail`.

**Expect main to move.** The owner lands work in parallel; derive against
`origin/main`, never the working checkout.

**Blocked is a good outcome.** If a unit cannot land unattended, the agent
should add the `blocked` label, link the blocker as a GitHub dependency the way
`CLAUDE.md` says, and report that. A decomposition beats a half-landed unit.
