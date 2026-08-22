# Researching a unit

Read this first when you are dispatched to triage the backlog, check whether a
card is expressible, or turn an issue into a brief someone else will implement.
`CLAUDE.md` still applies and overrides anything here.

You do not hold the build. Another agent does, and it is building right now.

## Hard constraints

- **Do not run `cabal build`, `cabal test`, `cabal configure`, or any
  compiler.** Reading source, grepping, `curl` and `gh` are fine.
- **Do not create a branch, check out a branch, edit a file, or open a PR.**
  Read other refs with `git show <ref>:<path>`.
- **Do not comment on, label, or close an issue** unless the brief says the
  comments *are* the deliverable. Report findings and let the dispatcher act.

The implementer may be rewriting a file you are deriving against. The
dispatcher keeps you file-disjoint from the build, but say in the brief which
ref you derived against so a stale derivation is visible.

## Tools that behave unexpectedly here

- `gh issue view` returns empty output for some agents. Use `gh api
  repos/tfausak/pawl/issues/N` and `.../issues/N/comments`.
- Oracle text and `_scratch/`: `CLAUDE.md`. Prior art is a lead, never a
  citation --- verify against `docs/rules.txt` and Scryfall first.

## Findings, not predictions

Split what you write by whether reading can settle it.

**Reading settles these, and they are where your value is.** Producer hunts,
Oracle text, whether a stated blocker has landed, edit-site enumeration,
cross-file greps, CR citations, which half of a split issue you are aiming at.
In one nine-unit run this class changed the scope or the verdict of every issue
researched: two would have been declined as blocked, one would have been built
in a form `design.md` §4 forbids, one would have shipped a fix that introduced
a rules violation.

**Reading cannot settle these, and in that same run every brief got at least
one wrong.** Which assertion a mutation reddens (three predicted-red came back
green). Wire spellings (two would not have compiled). Line numbers (stale in
every brief). Draft them anyway --- drafting the mutation is what forces the
discrimination argument --- but mark them unverified, and never phrase them as
instructions. The implementer re-derives this class regardless; a confident
wrong prediction costs them a test redesign mid-unit.

Cite identifiers to grep for, never line numbers.

## Distrust the issue body, and the comments

`CLAUDE.md` says to re-derive an issue's status against the tree. Sharper
forms, all of which have paid repeatedly:

**"No printed card does this" is the least reliable claim in the tracker** ---
false eight times in one run, twice from an earlier comment on the same issue.
Search the mechanic, not the card names already written down: `t:aura
o:/morph/`, not the two cards the thread argued about.

**A negative usually swept one axis and never ran the sibling.** #635's "no
producer" was true for basic land types and false for creature types, which the
same text-changer reaches. #683's "blocked on five capabilities" was true of
the canonical card pair and false of a cheaper one nobody had looked for.
Before recording a negative, name the axis you swept and ask what the
neighbouring one would return.

**"This needs a subsystem pawl doesn't have" is the least reliable rejection**
--- populate, bloodthirst, recover and fortify were each rejected for a
capability that was not missing. Verify at the *carrier*: the type or function
that would hold the behaviour. Read it, and name it. "Needs X" with no carrier
named is not a derivation.

**A named producer can be unusable for a reason the body never checked.** #638
named three cards that all say "target *activated* ability", which no `Filter`
can narrow. Check the producer against the vocabulary, not just against print.

**Check which issue you are aiming at, and which HALF.** A sweep splits an
issue and leaves the original title over both. Read the body for "split out of
#N" or "the other half stays on #N", then say which half your brief closes.

**A false claim can live in a code comment.** When an issue looks blocked
because a comment says nothing in the pool can do X, check the comment.

## The staleness sweep

Bodies are written once and left; blockers land under other names; a capability
an issue calls missing gets built by a PR that never cites it. Take the oldest
untouched issues first (`gh api
'repos/tfausak/pawl/issues?state=open&sort=updated&direction=asc'`) and
re-derive each against `origin/main`: is the gap still a gap, is the blocker
still missing, does a test already prove it, has a sibling absorbed it.

Verdicts are **still open**, **closed by PR #N** (name it), **superseded by
#N**, or **narrowed** (say what remains). A close by re-derivation is the
cheapest close there is.

## Clusters

The dispatcher wants clusters as well as single briefs: two to four open issues
in the same area touching the same files that one person would work in one
sitting. One dispatch, one PR closing them all.

A cluster brief has a shared producer and edit-site list, and a per-issue
proving test and mutation. Name the issues it closes and the order to work
them. Do not force one.

## What a finding is

Rank these above a dispatchable unit, not below it:

- an issue whose stated blocker has already landed
- an issue that should be **closed or rescoped by re-derivation** ---
  especially one other issues cite as a prerequisite
- a **capability no producer can reach**, so the issue is `wontfix` or
  `expires:synthetic` rather than card-driven
- a **producer that cannot be authored**, so the issue needs a different one
- a proving assertion that **cannot discriminate**, because every candidate
  card makes the two readings agree

An honest short list beats a padded one. "This tier is exhausted, and here is
why" is a useful answer.

## Writing a brief

**Write the brief to a file and return the path.** Retyped prose is where
citation errors enter.

Everything you settle here costs the same tokens on your lane as on theirs, and
only theirs is the critical path. A brief is dispatch-ready when it carries:

- the **verdict**: dispatchable, or blocked with the missing capability named
- the **ref you derived against**
- the **blocker's issue number and the capability it holds**, when blocked. You
  may not link the dependency yourself --- name it for the dispatcher. If it
  has no issue, say so: that is an untracked deficiency
- the **producer**, with Oracle text fetched this session, whether it is
  already in `data/cards/`, and a clause-by-clause expressibility check naming
  the opcode for each. If a clause must be omitted, say whether the omission
  runs stricter or weaker than printed
- the **composition check**: for each opcode, the engine path the producer
  reaches it by and what that path supplies. See below
- the **card JSON** when the producer is not in `data/cards/`, transcribed from
  the Oracle text you fetched and in a neighbouring card's wire spelling ---
  flagged unverified, since wire shapes are the class you cannot check
- the **edit sites**, `-Werror`-forced ones enumerated by grepping a sibling,
  every `{}` or `_` site flagged separately
- the **proving test** at gameplay level, drafted against `Pawl.Support`'s
  fixtures and the spec module it belongs in
- the **discrimination argument** for that test --- the buggy trace and the
  correct trace walked over the exact board, and the quantity where they
  diverge. "It fails today" is not this argument
- the **files it touches** --- the dispatcher schedules by this
- the **falsifying mutation** for each site, flagged unverified. If you cannot
  name one that goes red, say the site is unproven rather than proposing a
  vacuous test
- the **vacuity traps** that apply, from `docs/agents/implementing.md`
- the **CR citations**, quoted from `docs/rules.txt` and grepped by number

## Discriminating power

A drafted test is a claim about TWO implementations, and is worth dispatching
only if the buggy one fails it. Deriving that the code is wrong is not deriving
that your board can see it wrong.

- Name the asserted quantity and give its value under each implementation.
  Equal values mean the board cannot discriminate: change the BOARD, never the
  assertion.
- Walk the buggy implementation to the END, through the rules that fire on the
  way. #1683's board asserted a hand of one; CR 400.3 sends the found card to
  its OWNER's hand, so both implementations end at one card. A second card in
  one library separates them.
- Justify every element of the board --- why two seats and not three, why two
  different lands. An unjustifiable element is usually the one hiding the
  collapse.
- Prefer a quantity a partial fix cannot reach by another route. Assert both
  the count and the identity of the thing counted, gameplay count first.
- Where the divergence is a TIME rather than a value, say which moment is read.
  An effect that ends and one that pauses agree at the first read.

This is not the mutation bullet from another angle: the mutation asks whether
the assertion is wired to the code, this asks whether the BOARD can tell the
two answers apart. When no board separates them, say so --- that is a finding.

## Composition, not just existence

The expressibility check passes on cards whose opcodes are inert TOGETHER,
because what an atom answers depends on the path it is reached by. Showstopping
Surprise needed five opcodes; all existed, but the sweep reaching them built
its `Filter.Context` through `Filter.contextFor`, whose slot map is empty ---
so "each OTHER creature" was `Not (IsBound "target")` against nothing, and the
card damaged its own target to death (#1876).

The symptom is a VACUOUS ATOM: one that cannot be false, or cannot be true,
because the field it reads was never filled on this path. It compiles, the
codec round-trips, the card loads, and nothing is red.

For each opcode the producer needs:

- Trace from the producer's shape to the call and name the function that builds
  the context. `Filter.Context`'s context-relative fields are empty in
  `Filter.contextFor`; its header says which positions that is honest for.
- Ask what the atom answers against an empty field. `Filter.IsBound`,
  `SameNameAsBound`, `IsHostOfSource` and `ControlledByRecipient` answer False
  rather than raising; `Quantity.AgainstSlot` answers unanswered. Neither is
  distinguishable from a rule that does not apply.
- Read the guardrail and say which applies. `Pawl.CardSpec` fences some fields
  with a position lint; `slotObjects` and `recipient` carry none.
- Where two opcodes must see each other, say whether one builder supplies both.
  `IsBound` sees a slot only where the resolution's map is handed over
  (`Resolve.effectContext`), and reads single and group bindings alike, where
  `Quantity.AgainstSlot` and the other singular readers take the single one
  (`Filter.slotOneObject`). A target slot's own filter is matched before any of
  that exists (`Target.admittedGiven`).

When reading cannot settle it, say so: "these opcodes exist and I did not
verify they compose along <path>" is honest and points the implementer at the
first thing to build. A brief that lists opcodes and stops has claimed
composition.
