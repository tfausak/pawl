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
  next unit --- see "Do not brief ahead". Its standing job is the cross-unit
  audit; the blocker-consumer scan that used to be a clustering-agent pass is
  now a scripted `gh` query run inline before each dispatch (see
  "Procedure").

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
with audit findings costs 5k to 80k incremental, median near 40k (measured
2026-09-05). Issues closed per token is what
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
preferring `priority-high`. Below that tier, rank by dispatch shape, measured
2026-08-18/09-22 in net closes per PR:

(i) **Blocker-consumer pairs** (+1.40), found by a scripted scan, not an
agent: for every open issue, `gh api
repos/tfausak/pawl/issues/N/dependencies/blocked_by`, and keep the pair when
the blocker is open, itself unblocked, and its only open dependent is the
consumer. Run it inline before dispatching.

(ii) **Single `bug` or `rules-correctness` issues** (+0.49), picked by hand.

(iii) **Card-demand singles** (+0.07): count the real cards the gap unblocks
--- a `grep -c` of the Oracle phrase over a local MTGJSON dump (see
`CLAUDE.md`), or a Scryfall `/cards/search` with a `User-Agent` header when
there is none --- and take the largest count that is file-disjoint from the build. Put the
count in the brief; the implementer picks the producer from those cards.

Take the highest-ranked option that is file-disjoint from the build. A
**subsystem root** (an unblocked issue with many open dependents) is
dispatched only when the owner wants that subsystem opened, expecting it to
spawn slices --- picked on its own merits it measured ~-1 net closes per PR.
**Do not rank by open dependents otherwise.** The dependency graph is a
shallow forest: the overwhelming majority of merged units unblock nothing, so
"a capability that unblocks three issues" describes almost no issue in the
backlog.

`needs-planning` covers two things: an issue-to-issue blocker, and an issue
awaiting a design call from the owner (#146, #1828, #2167 carry the label with
no linked blocker). Neither is dispatchable unattended.

Dispatch an implementation agent, with `isolation: "worktree"`, to work it end
to end and open a PR. Its brief must open with: read
`docs/agents/implementing.md` first, then `CLAUDE.md` and `CONTRIBUTING.md`.
Everything else is specific to the unit. **Model**: opus, for every unit and
every audit. A 2026-09-22/23 sonnet trial over four engine units was no
cheaper in tokens than opus on the same shapes, and both audited units
carried a CR divergence.

**Dispatch on ready, not on merge.** The moment a unit's PR is marked ready,
dispatch the next one. The lane is agent-bound, so an idle build lane is the
loop's only outright waste; never hold a dispatch waiting on a merge. Two
guards make this safe: the next brief must be FILE-DISJOINT from the PR still
in flight (see "Scheduling"), and it derives against `origin/main`, so a later
`git merge origin/main` brings the in-flight PR in cleanly. If the in-flight
PR goes red, send its agent back; the two builds queue on the build lock and
that is fine.

**When nothing is file-disjoint, stack rather than idle.** Dispatch the next
unit off the in-flight PR's branch as a draft, and have it rebase `--onto main`
after that PR squash-merges, re-running the suite and the load-bearing
mutations against the merged state. This worked twice in one run on
`Projection.hs` and on `Game.hs`. Prefer a disjoint issue where one exists;
stack when the alternative is an idle lane.

**A blocker-consumer pair from the scan is the default dispatch shape; a
single issue is the fallback.** The clustering-agent pass this replaced
grouped by shared TOPIC as often as shared code, and topic isn't enough: the
eleven topic trackers #2190--#2200 each asserted shared machinery in the body
and every one split into unrelated units under triage. Two issues still
qualify as one dispatch outside the scan's own pairs when they visibly share
an edit site or take the SAME fix shape in adjacent code (#2534 and #2535
were one bracket around two neighbouring folds, dispatched an hour apart as
two units and should have been one PR) --- verify that against the tree
before dispatching, never take it on an issue body's word. Closing two or
three issues from one PR is the good case, not a liberty.

**Do not brief ahead.** A pre-implementation brief does not buy throughput:
with and without one the lane landed 1--2 PRs an hour (2026-08-16, when the
ceiling was CI). The implementer re-derives everything anyway, and corrected
the brief every time it was tried --- a wrong precedent, a vacuous control, an
unnecessary `GameState` field, a producer that proved nothing. Dispatch
straight off the issue, and tell the agent the issue body is the artefact most
often wrong.

**Audit on a SIGNAL, not on a merge count, for correctness only.** This is the
audit lane's standing job, and the only mechanism that looks ACROSS units. Two
units each correct alone can compose wrong and no single unit's mutations see
it: three consecutive rounds each found a real defect (#2505,
#2529, and #2555 --- a regression the run itself had introduced five units
earlier). Read the merged diffs, not the tests. Its brief must open with: read
`docs/agents/researching.md` first.

The merge count stopped paying for it. Measured 2026-09-12, over one run: five
cross-unit rounds cost ~970k subagent tokens for no finding at the bar below
--- one filed issue (#3658) and one fold-in between them. So run a cross-unit
round when a merged unit raises one of these, and not otherwise:

- it changed a shared function's semantics for its EXISTING callers (#3659's
  `ForEach` binding change);
- it renamed or widened a widely-called function (#3654's substitution-family
  rename);
- it added a field to a widely-constructed record (#3667's cast-road tag).

Each is a change whose blast radius no single unit's mutations can reach, which
is what the three older findings had in common too.

An audit round costs as much as an implementation, so its brief bounds what
counts as a finding: behaviour that diverges from the CR, a rules-core arm that
cases on an effect's identity, a false CR classification in a comment, a
mutation the PR body reports that could not have reddened the named assertion.
Prose is not a finding. A wording nit, a haddock over one line, a stale
citation that still points at the right rule: the auditor fixes none of these,
reports none of these, and no "fix N comment nits" unit is dispatched for
them. They are corrected by the next unit that edits the file, under the
self-review rule in `CLAUDE.md`.

Act on a finding by sending the unit's agent back, not by filing and
re-dispatching: a send-back costs 5k to 120k against ~200k for a fresh unit
(re-measured 2026-09-07), and catches the defect before the merge rather than
after.

**A follow-up a unit files in its own files, while still under the size
signal, is a send-back too.** Hold its worktree past the merge instead of
reaping it, and send the same agent back on a new branch to fold it in once
`origin/main` has the merge --- don't dispatch it fresh. A fresh dispatch pays
the ~80--100k fixed cost of a new worktree again for work the agent already
has the context for.

**A unit past the size signal gets its own round, BEFORE auto-merge is
armed.** Measured 2026-09-06/07 over 33 units: every unit past ~300k subagent
tokens or ~300 tool uses carried a defect the audit found, and no unit under
it did. The recurring class was a read of the printed card where the copiable
values were owed (`CLAUDE.md` item 4 now names it). Arming on ready and
auditing in parallel turns each finding into a second PR, a second CI cycle
and a window where `main` is wrong; the focused round takes about ten minutes,
so hold the arm and the worktree until it reports. Cut subsystem "first
slices" so they stay under the signal: a brief that needs more than three
proving assertions is two units.

**Re-run the scan before every dispatch, not from a stale list**; the backlog
turns over. The clustering-agent pass this replaced cost 172k tokens a round
and its "issue X unblocks issue Y" reasoning about OTHER issues was still
wrong twice in its first five clusters --- once on a comment citation a later
commit had re-pointed, once on a blocker that had never been the real one.
The scan reads the dependency graph directly instead of reasoning about it, so
it has nothing to get wrong the same way, but confirm the blocker is actually
open and unblocked before dispatching --- the API can lag a just-closed issue.

Sweeps that do not need an agent: the blocker-consumer scan above, fired
`expires:card-driven` triggers (a closed seam --- two independent passes found
none), and re-checking whether a claimed missing capability still is missing.
All are `gh` queries; run them inline rather than spending a lane on them.

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

**Keep the warm worktree at `origin/main`, and seed every dispatch from it.**
`script/warm-worktree.sh refresh` resets `.claude/worktrees/warm` to
`origin/main` and builds it through the lock; run it after every merge, in the
background, since the build is incremental and the lane does not wait on it.
`script/warm-worktree.sh seed DIR` clones its `dist-newstyle`, and the `-O0`
`dist-mutate` that `script/mutate.sh` builds in, into a fresh worktree, which
`implementing.md` tells the agent to do before its first build. Measured
2026-09-04 on a loaded machine: a cold `cabal build all` in a fresh worktree
took 463s; seeded from a build 80 files behind, 414s with 843 of 1528 modules
recompiled; seeded from a build at the same commit, 24s with none. Measured
2026-09-25: a seeded `dist-mutate` at the same commit rebuilt in 26s where a
cold one took 284s. Renaming the directory is a new configuration and rebuilds
from scratch, so it keeps its name. A donor that has fallen behind still pays off, so a missed refresh is a
slower dispatch, not a broken one. The warm worktree is never reaped and
never dispatched into.

**Builds are counted, machine-wide.** The GHC job semaphore shares compile
slots; it does not stop several worktrees running `cabal` at once, and with
three going an 8 GB machine is unusable. Every `cabal` invocation in every
lane goes through `script/with-build-lock.sh`, which queues on lock files and
takes over a lock whose owner has died. It admits two builds at once
(`PAWL_BUILD_SLOTS`, measured 2026-09-02: one `cabal test` peaks near 1.8 GB
resident); set it to one if the machine is unhappy. Put that in every brief;
lanes beyond the slot count just queue.

**Backgrounded builds are how lanes stall.** The GHC job semaphore is gone
(2026-09-02; it corrupted on every killed run and cabal hung on it), so the
remaining way a lane wedges is a tool timeout reaping a backgrounded `cabal`
while it holds a lock slot. Tell agents to run `cabal` in the foreground with
a generous timeout, to split long runs by `--pattern`, and never to `pkill`
by pattern; a run at 0% CPU for ten minutes is killed by its own PID.

**Reap after every merge, then refresh the warm worktree.** A finished unit
leaves a worktree under `.claude/worktrees/`, its placeholder
`worktree-agent-*` branch, the unit's own branch, and often a `cabal` process
stalled on the semaphore at 0% CPU. None of it goes away on its own, and a
stalled `cabal` keeps a build slot the live lanes need. Once a unit's PR is merged, from the primary checkout: `git worktree
remove --force` its worktree (unlock first), `git worktree prune`, `git fetch
--prune`, then delete every local branch whose upstream is `[gone]` or that has
no commit beyond `origin/main`, skipping any branch a live worktree has checked
out. Kill a stalled `cabal` by its PID after `lsof -p <pid> -d cwd` shows a
finished worktree, never by pattern. Leave any branch you did not create that
still carries commits, and say so; the owner keeps review branches. The warm
worktree is exempt from all of this: leave it, and run
`script/warm-worktree.sh refresh` in the background once the reap is done.
Reaping a worktree also makes its agent unresumable, so the send-back path in
the audit section above no longer reaches it. When an audit round is about to
run over a unit, reap it only once the audit has reported.

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
