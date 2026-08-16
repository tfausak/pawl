# Researching a unit

Read this first when you are dispatched to triage the backlog, check whether a
card is expressible, or turn an issue into a brief someone else will implement.
`CLAUDE.md` still applies and overrides anything here.

You do not hold the build. Another agent does, and it is building right now.

## Hard constraints

- **Do not run `cabal build`, `cabal test`, `cabal configure`, or any
  compiler.** Reading source, grepping, `curl` and `gh` are fine.
- **Do not create a branch, edit a file, or open a PR.**
- **Do not comment on, label, or close an issue** unless the brief says the
  comments *are* the deliverable, as a stale-rejection sweep's do. Otherwise
  report findings and let the dispatcher act on them: two agents editing the
  tracker race each other, and the dispatcher needs to verify a claim before it
  becomes a comment other people plan against. Never label or close either way.

## Tools that behave unexpectedly here

- `gh issue view` returns empty output for some agents. Use `gh api
  repos/tfausak/pawl/issues/N` and `.../issues/N/comments`.
- WebFetch gets 403s from Scryfall. Use `curl` against `api.scryfall.com`.
- The vendored dumps under `_scratch/` are stale. Never read a card from them.
  The permissive prior art there (`phase`, `mtgish`, `argentum-engine`) is
  still worth a grep for a candidate producer or a field shape --- see
  `docs/agents/implementing.md`. It is a lead, never a citation: verify against
  `docs/rules.txt` and Scryfall before a finding rests on it. Any of it may be
  absent, since `_scratch/` is gitignored.

## Distrust the issue body, and the comments

`CLAUDE.md` says to re-derive an issue's status against the tree. Two sharper
forms of that, both of which have paid repeatedly:

**"No printed card does this" is the least reliable claim in the tracker.** In
one run it was false eight times, twice where the false claim came from an
earlier comment on the same issue. **Search the mechanic, not the card names
already written down** --- `t:aura o:/morph/`, not the two cards the thread
argued about. The two best producers found in that run were each one query
away, sitting behind cards everyone had already rejected.

**"This needs a subsystem pawl doesn't have" is the least reliable rejection.**
One sweep found four of them wrong --- populate, bloodthirst, recover and
fortify --- each because the rejection named a plausible missing capability
without checking whether it was missing. Verify at the *carrier*: the specific
type or function that would hold the behaviour. Read it, and name it in the
finding. "Needs X" with no carrier named is not a derivation.

**A false claim can live in a code comment.** One sentence in an engine module
asserted that nothing in the pool could attach to a particular creature, while
the card that could sat in the same tree. That sentence made an issue look
unreachable for weeks. When an issue looks blocked because a comment says so,
check the comment.

## The staleness sweep

The tracker rots faster than anyone re-reads it. Issue bodies are written once
and left; blockers land under other names; a capability an issue calls missing
gets built by a PR that never cites the issue. 2026-08-15/16 closed 51 issues
by PR and one by any other route --- against a backlog whose oldest
open issues predate hundreds of merges. That ratio says the re-derivation is
not happening, not that the bodies are all still right.

When dispatched to sweep, take the oldest untouched issues first
(`gh api 'repos/tfausak/pawl/issues?state=open&sort=updated&direction=asc'`)
and re-derive each body against `origin/main`: is the gap still a gap, is the
blocker still missing, does a test already prove the behaviour, has a sibling
issue absorbed the concern. Verdicts are **still open**, **closed by PR #N**
(name it), **superseded by #N**, or **narrowed** (say what remains). Report
them for the dispatcher to act on; you may not close or comment yourself. A
close by re-derivation is a few thousand read-only tokens and no build, which
makes it the cheapest close there is.

## Clusters

The dispatcher wants clusters as well as single briefs: two to four open issues
in the same area that touch the same files and that one person would work in
one sitting --- the sub-clauses of one card, a family of filters, a capability
plus the issues that exist only because it was missing. One dispatch, one
worktree, one PR closing them all. That amortizes the fixed cost of a unit and
removes the conflicts the issues would have had with each other.

A cluster brief is one brief with a shared producer and edit-site list and a
per-issue proving test and mutation. Name the issues it closes and the order to
work them in. Do not force one: an issue that stands alone is a single brief.

## What a finding is

Rank these above a dispatchable unit, not below it:

- an issue whose stated blocker has already landed
- an issue that should be **closed or rescoped by re-derivation** rather than
  worked --- especially one other issues cite as a prerequisite, since that one
  misroutes work every time it is read
- a **capability that no producer can reach**, so the issue is `wontfix` or
  `expires:synthetic` rather than card-driven
- a proving assertion in an issue body that **cannot discriminate**, because
  every candidate card makes the two readings agree

An honest short list beats a padded one. "This whole tier is exhausted, and
here is why" is a useful answer. A fifth unit whose producer turns out not to
be expressible costs a dispatch cycle.

## Writing a brief

**Write the brief to a file and return the path.** Do not return it as prose
for someone to retype --- every citation error in one 51-unit run entered at
the retyping step.

The brief is where the implementer's re-derivation goes. Everything you can
settle here --- the producer, its JSON, the edit sites, the red test --- costs
the same tokens on your lane as on theirs, and only theirs is the critical
path. Do the settling here.

A brief is dispatch-ready when it carries:

- the **verdict**: dispatchable, or blocked with the missing capability named
- the **blocker's issue number and the capability it holds**, whenever the
  verdict is blocked. A blocker is recorded as a GitHub dependency, not as
  prose (see `CLAUDE.md`), and you may not record it yourself --- name it in
  your report for the dispatcher to link. If the blocker has no issue, say so:
  that is an untracked deficiency and it needs one
- the **producer**, with exact Oracle text fetched this session, whether it is
  already in `data/cards/`, and a clause-by-clause expressibility check naming
  the opcode for each. If a clause must be omitted, say whether the omission
  runs stricter or weaker than printed
- the **card JSON**, written out in full in the brief when the producer is not
  yet in `data/cards/`, in the wire spelling a neighbouring card uses ---
  transcribed from the Oracle text you fetched, never from the issue
- the **edit sites**, with `-Werror`-forced ones enumerated by grepping a
  sibling, and every `{}` or `_` site flagged separately
- the **proving test** at gameplay level, drafted as code against the fixtures
  in `Pawl.Support` and the spec module it belongs in, with the exact
  assertions. You cannot run it; say so, and expect the implementer to. The
  point is that the implementer starts at "make it red, then green" rather
  than at "which spec, which fixture, which board"
- the **files it touches**, listed --- the dispatcher schedules by this and
  will not overlap two units on one file
- the **falsifying mutation** for each, and what it must break. If you cannot
  name one that goes red, say the site is unproven rather than proposing a
  vacuous test
- the **vacuity traps** that apply, from `docs/agents/implementing.md`'s list
- the **CR citations**, quoted from `docs/rules.txt` and grepped by number

Line numbers in issue bodies are almost always stale. Cite identifiers to grep
for, not line numbers.
