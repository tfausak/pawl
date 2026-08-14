# The drain loop

The prompt below is meant to be copy-pasted into a `/goal`. It is kept here
because it is tightly coupled to `docs/agents/implementing.md` and
`docs/agents/researching.md` --- changing a role file usually means changing
this.

## Shape

Two lanes.

- **The build lane** runs exactly one implementation agent at a time, in an
  isolated worktree. `jobs: $ncpus` already saturates the machine, so a second
  concurrent build does not help.
- **The research lane** runs read-only agents alongside it. They never touch
  the build, so they are free wall-clock. Keep this lane busy: dispatch the
  next round of research *when you dispatch the build agent*, not when you
  notice you are idle.

When the build lane's queue empties, that is a signal, not a gap. The answer is
a design question for the owner, not a speculative dispatch.

## Setting the goal

"Until there are no issues left" is not reachable and should not be used.
Across one 51-unit run the backlog went 299 -> 300, because closing a unit
surfaces real gaps that were invisible before --- roughly one filed issue per
unit landed. That is the project working, not drift.

Bound it instead: a unit count, a wall-clock box, "until no `priority-high`
remains", or **"until N consecutive dispatches come back blocked"**. That last
is the best convergence signal --- when the readily-dispatchable tier depletes,
the loop starts returning decompositions instead of PRs, and three of the final
four dispatches in that run did exactly that.

## The prompt

Copy from here.

---

Work the backlog autonomously until <BOUND>.

**Dispatch.** Pick an unassigned issue with no `blocked` label, preferring
`priority-high`. Dispatch ONE implementation agent at a time, with `isolation:
"worktree"`, to work it end to end and open a PR. Its brief must open with:
read `docs/agents/implementing.md` first, then `CLAUDE.md` and
`CONTRIBUTING.md`. Everything else in the brief should be specific to the unit
--- the role file carries the standing rules.

**Research in parallel.** Always keep at least one read-only agent running
alongside the build. Its brief must open with: read
`docs/agents/researching.md` first. Dispatch the next round when you dispatch
the build agent, not when you go idle. Highest-yield standing assignments: turn
the next few queued issues into dispatch-ready briefs; sweep issue bodies
nobody has re-read for blockers that have since landed; check whether a claimed
missing capability still is missing.

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

**Scheduling.** Avoid dispatching two units that edit the same file. Conflicts
in one run clustered almost entirely on `Event.hs` and `CardSpec.hs`. Ask the
researcher to annotate each brief with the files it touches.

**Before dispatching any fix to CI, read the failing job log.** A `Gild`
failure once looked like a rebase artifact; the log showed the job died in `Set
up job` and never ran. Retry logic must handle `cancel` as well as `fail`.

**Expect main to move.** The owner lands work in parallel. That is what
worktrees are for. Derive against `origin/main`, never the working checkout.

**Blocked is a good outcome.** If a unit cannot land unattended, the agent
should add the `blocked` label, link the blocker as a GitHub dependency the way
`CLAUDE.md` says --- not as a `Blocked by #N` comment --- and report that. A
decomposition beats a half-landed unit.

---

Copy to here.
