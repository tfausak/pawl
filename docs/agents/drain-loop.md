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
  is the BUILD, not the merge. Dispatch the next unit when the current one's PR
  is ready --- CI is no longer the ceiling, so nothing but scheduling paces it.
- **The audit lane** runs read-only agents alongside. It does NOT brief the
  next unit --- see "Do not brief ahead". Its standing jobs are the cross-unit
  audit and the clustering pass.

Measured 2026-08-16: ~25 min from dispatch to PR-ready, ~5 of them compiling
(cold build ~2.5 min, incremental ~2); ~12 min from ready to merged; the CI
Build job ~13 min. The rest is reading and writing, so wall-clock is bought by
taking work off the build lane, not by thinking less. The backlog closes at
about the rate it grows, so the birth rate and the fold-in rate are the levers
--- hence clusters, the fold-in rule, and the standing staleness sweep.

Re-measured 2026-08-31, after PR #2794 made CI build incrementally: the Build
job runs 5--7 min, against 18 on the last PR before it, and ready-to-merged is
one CI cycle or less --- #2797 merged 64s after ready, its checks already green
from the push before it, and #2800, whose checks started at ready, took 9 min.
Dispatch-to-ready is unchanged, so the lane is now agent-bound and the binding
constraint on the loop as a whole is the token budget.

Measured the same day, in subagent tokens: an implementation dispatch costs
130--300k, median ~200k; an audit round 140--220k. An estimated 80--100k of a
dispatch is fixed whatever its size --- the role docs, re-deriving against
`origin/main`, worktree setup, the first build --- so a multi-issue dispatch
costs about what a single-issue one does: one closed three issues for 128k, and
the run's cheapest unit, 87k, was a two-issue cluster. Sending an agent back
with audit findings costs ~15--25k incremental. Issues closed per token is what
the loop is really spending, and it is what the clustering pass, the fold-in
rule and the audit rule each buy.

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

**Dispatch.** Pick an unassigned issue with no `needs-planning` label,
preferring `priority-high`. **Do not rank by open dependents.** The
dependency graph is a shallow forest, not a tree: the overwhelming majority of
merged units report `dependencies/blocking` empty and unblock nothing, so "a
capability that unblocks three issues" describes almost no issue in the
backlog. Rank by `priority-high`, then by whatever is file-disjoint from the
build.

`needs-planning` covers two things: an issue-to-issue blocker, and an issue
awaiting a design call from the owner (#146, #1828, #2167 carry the label with
no linked blocker). Neither is dispatchable unattended.

Dispatch an implementation agent, with `isolation: "worktree"`, to work it end
to end and open a PR. Its brief must open with: read
`docs/agents/implementing.md` first, then `CLAUDE.md` and `CONTRIBUTING.md`.
Everything else is specific to the unit.

**Dispatch on ready, not on merge.** The moment a unit's PR is marked ready,
dispatch the next one. The lane is agent-bound, so an idle build lane is the
loop's only outright waste; never hold a dispatch waiting on a merge. Two
guards make this safe: the next brief must be FILE-DISJOINT from the PR still
in flight (see "Scheduling"), and it derives against `origin/main`, so a later
`git merge origin/main` brings the in-flight PR in cleanly. If the in-flight
PR goes red, send its agent back; the two builds share the semaphore and that
is fine.

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
Two issues qualify when they share an edit site OR take the SAME fix shape in
adjacent code: #2534 and #2535 were one bracket around two neighbouring folds,
dispatched an hour apart as two units, and should have been one PR. Closing two
or three issues from one PR is the good case, not a liberty. What does not
qualify is a shared topic with no shared shape.

**Do not brief ahead.** A pre-implementation brief does not buy throughput:
with and without one the lane landed 1--2 PRs an hour (2026-08-16, when the
ceiling was CI). The implementer re-derives everything anyway, and corrected
the brief every time it was tried --- a wrong precedent, a vacuous control, an
unnecessary `GameState` field, a producer that proved nothing. Dispatch
straight off the issue, and tell the agent the issue body is the artefact most
often wrong.

**Audit every few merges.** This is one of the audit lane's two standing jobs,
and the only mechanism that looks ACROSS units. Two units each correct alone
can compose wrong and no single unit's mutations see it: three consecutive
rounds each found a real defect (#2505, #2529, and #2555 --- a regression the
run itself had introduced five units earlier). Read the merged diffs, not the
tests. A comment-only unit is the same shape from the other side --- nothing
red catches a false CR classification. Its brief must open with: read
`docs/agents/researching.md` first.

Act on a finding by sending the unit's agent back, not by filing and
re-dispatching: a send-back costs ~15--25k against ~200k for a fresh unit, and
catches the defect before the merge rather than after.

**Run a clustering pass every ten or fifteen merges.** The audit lane's other
standing job, and how the fixed cost above gets recovered: one read-only agent
groups the dispatchable backlog by the code each issue names, and what it
returns is dispatched under "Dispatch a cluster" above.
`docs/agents/researching.md` has the method. Measured 2026-08-31: 172k tokens
for thirteen high-confidence clusters and seven medium, plus stale issues found
on the way; its first cluster closed two issues for 193k, about one unit's
spend recovered at once. The backlog turns over, so re-run the pass rather than
working an old list.

Weight its claims unequally. Shared edit sites and containment held; "issue X
unblocks issue Y" is a lead to verify, not a fact. Of the first five clusters
dispatched three held, and both failures came from the pass's reasoning about
OTHER issues --- one rested on a comment citation a later commit had
re-pointed, the other on a blocker that was closed and had never been the real
blocker. Two of the three that held held for a different reason than the pass
gave, so tell the implementer to verify the cluster itself and to split it back
out if it is not one unit.

Sweeps that do not need an agent: fired `expires:card-driven` triggers (a
closed seam --- two independent passes found none), and re-checking whether a
claimed missing capability still is missing. Both are `gh` queries; run them
inline rather than spending a lane on them.

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

Over 2026-08-31's run essentially every issue dispatched was stale in some way.
`docs/agents/researching.md` names the shapes, several of which are not what
"stale" suggests.

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

**Reap after every merge.** A finished unit leaves a worktree under
`.claude/worktrees/`, its placeholder `worktree-agent-*` branch, the unit's own
branch, and often a `cabal` process stalled on the semaphore at 0% CPU. None of
it goes away on its own, and a stalled `cabal` keeps a build slot the live lanes
need. Once a unit's PR is merged, from the primary checkout: `git worktree
remove --force` its worktree (unlock first), `git worktree prune`, `git fetch
--prune`, then delete every local branch whose upstream is `[gone]` or that has
no commit beyond `origin/main`, skipping any branch a live worktree has checked
out. Kill a stalled `cabal` by its PID after `lsof -p <pid> -d cwd` shows a
finished worktree, never by pattern. Leave any branch you did not create that
still carries commits, and say so; the owner keeps review branches.

**Merging.** Arm auto-merge (squash) on each PR. The ruleset requires branches
be up to date, so every merge invalidates every other armed PR and the queue
drains at exactly one per CI cycle however many are open. Since PR #2794 that
cycle is minutes, so the old cap of two open PRs neither costs nor buys
anything: do not pause a dispatch for it. The rest holds whenever several PRs
are open at once. When several sit green and `BEHIND`, do NOT update them all:
that is what starves them, each losing the race to the next merge. If any PR is
already up to date, wait for it; if none is, update the OLDEST one only and
wait for it to merge. Poll `mergeStateStatus` and run `gh pr update-branch`.
That adds a merge commit to the agent's branch, which is why agents must not
force-push. Arming does not
stick: a push to the branch can drop it, and a PR reported ready is not an
armed one --- re-check `autoMergeRequest` after arming and after every push,
including the one that resolves a conflict. On a conflict, send the agent back
to merge `origin/main`, resolve by taking both sides, and **re-run its
mutations**.

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
should add the `needs-planning` label, link the blocker as a GitHub
dependency the way `CLAUDE.md` says, and report that. Where the blocker is a
decision rather than an issue, the label goes on with no link and the report
names the question for the owner. A decomposition beats a half-landed unit.
