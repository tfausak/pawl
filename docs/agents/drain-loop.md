# The drain loop

The goal string below is meant to be pasted into a `/goal`; the procedure
under it is what the orchestrator reads from this file once it starts. Both
are kept here because they are tightly coupled to
`docs/agents/implementing.md` and `docs/agents/researching.md` --- changing a
role file usually means changing this.

`/goal` caps its condition at 4000 characters, which is why the procedure is
not the goal: one revision put the whole thing in the string and it was
rejected at 4684. Keep the goal short and let it point here.

## Shape

Two lanes.

- **The build lane** runs one implementation agent at a time, in an isolated
  worktree. `jobs: $ncpus` already saturates the machine, so a second
  concurrent build does not help --- but a unit is not done building when its
  PR is marked ready. It then waits ten to thirteen minutes on CI, and for one
  run the build lane sat idle for every one of those: the next worktree's cold
  build began within seconds of the previous merge, never before. That idle was
  ~30% of the wall-clock per unit. So the lane's unit of exclusion is the
  BUILD, not the merge: dispatch the next unit when the current one's PR is
  ready, and let auto-merge keep the merges serial.
- **The research lane** runs read-only agents alongside it. They never touch
  the build, so they are free wall-clock. Keep this lane busy: dispatch the
  next round of research *when you dispatch the build agent*, not when you
  notice you are idle. Anything the implementer would otherwise re-derive ---
  the producer's Oracle text, the card JSON, the edit sites, the red test ---
  costs the same tokens on either lane and only one of them is the critical
  path. Push it here.

Where a unit's time went, measured over 2026-08-16's 28 merges: ~25 min from
dispatch to PR-ready, of which ~5 min was compiling (a cold build is ~2.5 min,
an incremental round trip ~2); ~12 min from ready to merged; and the CI Build
job at ~13 min, ~4 of them saving a nix cache. The rest of the 25 is reading
and writing, which is tokens --- so wall-clock is bought by taking work off
the build lane, not by thinking less.

The backlog closes at about the rate it grows: 2026-08-16 closed 28 and filed
21.
So the close rate is not the lever the month turns on; the birth rate and the
fold-in rate are. That is why the prompt asks for clusters, a fold-in rule,
and a standing staleness sweep alongside the dispatches.

When the build lane's queue empties, that is a signal, not a gap. The answer is
a design question for the owner, not a speculative dispatch.

## Bounding the goal

"Until there are no issues left" is not reachable and should not be used.
Across one 51-unit run the backlog went 299 -> 300, because closing a unit
surfaces real gaps that were invisible before --- roughly one filed issue per
unit landed. That is the project working, not drift.

Bound it instead: a unit count, a wall-clock box, "until no `priority-high`
remains", or **"until N consecutive dispatches come back blocked"**. That last
is the best convergence signal --- when the readily-dispatchable tier depletes,
the loop starts returning decompositions instead of PRs, and three of the final
four dispatches in that run did exactly that.

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
`isolation: "worktree"`, to work it end to end and open a PR. Its
brief must open with: read `docs/agents/implementing.md` first, then
`CLAUDE.md` and `CONTRIBUTING.md`. Everything else in the brief should be
specific to the unit --- the role file carries the standing rules.

**Dispatch on ready, not on merge.** The moment a unit's PR is marked ready for
review, dispatch the next one. Do not wait for CI or the merge; that wait is a
third of the wall-clock and the machine is idle for it. Two guards make this
safe: the next brief must be FILE-DISJOINT from the PR still in flight (the
researcher annotates every brief with the files it touches; if the only
candidates overlap, pick a different issue rather than waiting), and it derives
against `origin/main` as usual --- the in-flight PR is not in its base, so a
later `git merge origin/main` will bring it in cleanly exactly because the
files are disjoint. If the in-flight PR goes red, send its agent back to fix
it; the two builds share the semaphore and it is fine. The next unit's cold
build (~2.5 min) now falls inside the previous unit's CI wait, so there is no
separate worktree pre-warm step to run.

**Dispatch clusters, not only issues.** Ask the researcher for clusters: two to
four open issues in the same area that touch the same files and would be
worked by the same person in one sitting --- a card's several sub-clauses, a
family of filters, a capability and the issues that only exist because it was
missing. A cluster is one dispatch, one worktree, one PR closing every issue
in it. The per-unit fixed cost (reading the role files, the cold build, the PR
body, CI, your own turns) is the same for a cluster as for a leaf, and the
issues cannot conflict with each other if one agent holds them all. Do not
force a cluster: an issue that stands alone is dispatched alone.

**Research in parallel.** Always keep at least one read-only agent running
alongside the build. Its brief must open with: read
`docs/agents/researching.md` first. Dispatch the next round when you dispatch
the build agent, not when you go idle. Standing assignments, in order of yield:

- turn the next few queued issues (or clusters) into dispatch-ready briefs, and
  make each brief carry what the implementer would otherwise re-derive: the
  card JSON written out, the edit sites enumerated, and the red test drafted
  --- see "Writing a brief" in the role file. The implementer should start at
  "make it green".
- the **staleness sweep** the role file describes: re-derive the oldest
  untouched issue bodies against `origin/main`. A close by re-derivation costs
  a few thousand read-only tokens and no build.
- check whether a claimed missing capability still is missing.

**Relay briefs by path.** A researcher writes its brief to a file and returns
the path. Pass the path to the implementation agent. Do not retype the content
--- that is where citation errors enter.

**Merging.** Arm auto-merge (squash) on each PR. The ruleset requires branches
be up to date, so an armed auto-merge silently stalls at `BEHIND` --- poll
`mergeStateStatus` and run `gh pr update-branch`. That adds a merge commit to
the agent's branch, which is why agents must not force-push. On a conflict,
send the agent back to merge `origin/main`, resolve by taking both sides, and
**re-run its mutations** --- a bad resolution can neuter a test while leaving
the suite green.

**Scheduling.** Never have two units in flight that edit the same file.
Conflicts in one run clustered almost entirely on `Event.hs` and
`CardSpec.hs`. The researcher annotates each brief with the files it touches;
that annotation is what dispatch-on-ready leans on.

**Before dispatching any fix to CI, read the failing job log.** A `Gild`
failure once looked like a rebase artifact; the log showed the job died in `Set
up job` and never ran. Retry logic must handle `cancel` as well as `fail`.

**Expect main to move.** The owner lands work in parallel. That is what
worktrees are for. Derive against `origin/main`, never the working checkout.

**Blocked is a good outcome.** If a unit cannot land unattended, the agent
should add the `blocked` label, link the blocker as a GitHub dependency the way
`CLAUDE.md` says --- not as a `Blocked by #N` comment --- and report that. A
decomposition beats a half-landed unit.
