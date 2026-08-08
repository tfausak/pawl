# Researching a unit

Read this first when you are dispatched to triage the backlog, check whether a
card is expressible, or turn an issue into a brief someone else will implement.
`CLAUDE.md` still applies and overrides anything here.

You do not hold the build. Another agent does, and it is building right now.

## Hard constraints

- **Do not run `cabal build`, `cabal test`, `cabal configure`, or any
  compiler.** Reading source, grepping, `curl` and `gh` are fine.
- **Do not create a branch, edit a file, or open a PR.**
- **Do not comment on, label, or close an issue.** Report findings; the
  dispatcher acts on them. Two agents editing the tracker race each other, and
  the dispatcher needs to verify a claim before it becomes a comment other
  people plan against.

## Tools that behave unexpectedly here

- `gh issue view` returns empty output for some agents. Use `gh api
  repos/tfausak/pawl/issues/N` and `.../issues/N/comments`.
- WebFetch gets 403s from Scryfall. Use `curl` against `api.scryfall.com`.
- The vendored dumps under `_scratch/` are stale. Never read a card from them.

## Distrust the issue body, and the comments

`CLAUDE.md` says to re-derive an issue's status against the tree. Two sharper
forms of that, both of which have paid repeatedly:

**"No printed card does this" is the least reliable claim in the tracker.** In
one run it was false eight times, twice where the false claim came from an
earlier comment on the same issue. **Search the mechanic, not the card names
already written down** --- `t:aura o:/morph/`, not the two cards the thread
argued about. The two best producers found in that run were each one query
away, sitting behind cards everyone had already rejected.

**A false claim can live in a code comment.** One sentence in an engine module
asserted that nothing in the pool could attach to a particular creature, while
the card that could sat in the same tree. That sentence made an issue look
unreachable for weeks. When an issue looks blocked because a comment says so,
check the comment.

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

A brief is dispatch-ready when it carries:

- the **verdict**: dispatchable, or blocked with the missing capability named
- the **producer**, with exact Oracle text fetched this session, whether it is
  already in `data/cards/`, and a clause-by-clause expressibility check naming
  the opcode for each. If a clause must be omitted, say whether the omission
  runs stricter or weaker than printed
- the **edit sites**, with `-Werror`-forced ones enumerated by grepping a
  sibling, and every `{}` or `_` site flagged separately
- the **proving test** at gameplay level, with the exact assertions
- the **falsifying mutation** for each, and what it must break. If you cannot
  name one that goes red, say the site is unproven rather than proposing a
  vacuous test
- the **vacuity traps** that apply, from `docs/agents/implementing.md`'s list
- the **CR citations**, quoted from `docs/rules.txt` and grepped by number

Line numbers in issue bodies are almost always stale. Cite identifiers to grep
for, not line numbers.
