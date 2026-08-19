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
  comments *are* the deliverable. Otherwise report findings and let the
  dispatcher act: two agents editing the tracker race each other, and a claim
  needs verifying before it becomes a comment other people plan against.

## Tools that behave unexpectedly here

- `gh issue view` returns empty output for some agents. Use `gh api
  repos/tfausak/pawl/issues/N` and `.../issues/N/comments`.
- Oracle text and `_scratch/`: `CLAUDE.md`. Prior art there is a lead, never a
  citation --- verify against `docs/rules.txt` and Scryfall before a finding
  rests on it.

## Distrust the issue body, and the comments

`CLAUDE.md` says to re-derive an issue's status against the tree. Two sharper
forms, both of which have paid repeatedly:

**"No printed card does this" is the least reliable claim in the tracker** ---
false eight times in one run, twice from an earlier comment on the same issue.
**Search the mechanic, not the card names already written down** --- `t:aura
o:/morph/`, not the two cards the thread argued about.

**"This needs a subsystem pawl doesn't have" is the least reliable rejection**
--- populate, bloodthirst, recover and fortify were each rejected for a
capability that was not missing. Verify at the *carrier*: the specific type or
function that would hold the behaviour. Read it, and name it in the finding.
"Needs X" with no carrier named is not a derivation.

**Check which issue you are aiming at, and which HALF of it.** A sweep splits an
issue in two and leaves the original title standing over both. #160 and #1631
wore one title, and a brief was derived against the half that was not the unit.
Before deriving anything, read the body for a "split out of #N" or "the other
half stays on #N" line and read the issue it names; then say in the brief which
half it closes and which it leaves.

**A false claim can live in a code comment.** When an issue looks blocked
because a comment says nothing in the pool can do X, check the comment.

## The staleness sweep

Issue bodies are written once and left; blockers land under other names; a
capability an issue calls missing gets built by a PR that never cites it. When
dispatched to sweep, take the oldest untouched issues first
(`gh api 'repos/tfausak/pawl/issues?state=open&sort=updated&direction=asc'`)
and re-derive each body against `origin/main`: is the gap still a gap, is the
blocker still missing, does a test already prove the behaviour, has a sibling
absorbed the concern. Verdicts are **still open**, **closed by PR #N** (name
it), **superseded by #N**, or **narrowed** (say what remains). Report them; you
may not close or comment yourself. A close by re-derivation is the cheapest
close there is.

## Clusters

The dispatcher wants clusters as well as single briefs: two to four open issues
in the same area that touch the same files and that one person would work in
one sitting --- the sub-clauses of one card, a family of filters, a capability
plus the issues that exist only because it was missing. One dispatch, one
worktree, one PR closing them all.

A cluster brief is one brief with a shared producer and edit-site list and a
per-issue proving test and mutation. Name the issues it closes and the order to
work them in. Do not force one: an issue that stands alone is a single brief.

## What a finding is

Rank these above a dispatchable unit, not below it:

- an issue whose stated blocker has already landed
- an issue that should be **closed or rescoped by re-derivation** rather than
  worked --- especially one other issues cite as a prerequisite
- a **capability that no producer can reach**, so the issue is `wontfix` or
  `expires:synthetic` rather than card-driven
- a proving assertion in an issue body that **cannot discriminate**, because
  every candidate card makes the two readings agree

An honest short list beats a padded one. "This whole tier is exhausted, and
here is why" is a useful answer.

## Writing a brief

**Write the brief to a file and return the path.** Retyped prose is where
citation errors enter.

The brief is where the implementer's re-derivation goes. Everything you can
settle here costs the same tokens on your lane as on theirs, and only theirs is
the critical path. A brief is dispatch-ready when it carries:

- the **verdict**: dispatchable, or blocked with the missing capability named
- the **blocker's issue number and the capability it holds**, when blocked. A
  blocker is a GitHub dependency (`CLAUDE.md`), and you may not record it
  yourself --- name it for the dispatcher to link. If it has no issue, say so:
  that is an untracked deficiency
- the **producer**, with exact Oracle text fetched this session, whether it is
  already in `data/cards/`, and a clause-by-clause expressibility check naming
  the opcode for each. If a clause must be omitted, say whether the omission
  runs stricter or weaker than printed
- the **composition check**: for each opcode named above, the engine path the
  producer reaches it by and what that path supplies. Per-clause existence is
  not composition. See "Composition, not just existence" below
- the **card JSON**, in full when the producer is not yet in `data/cards/`, in
  the wire spelling a neighbouring card uses --- transcribed from the Oracle
  text you fetched, never from the issue
- the **edit sites**, with `-Werror`-forced ones enumerated by grepping a
  sibling, and every `{}` or `_` site flagged separately
- the **proving test** at gameplay level, drafted as code against the fixtures
  in `Pawl.Support` and the spec module it belongs in, with the exact
  assertions. You cannot run it; say so
- the **discrimination argument** for that test --- the buggy trace and the
  correct trace, walked over the exact board you drafted, and the asserted
  quantity where they diverge. See "Discriminating power" below. A brief
  asserting only that the test "fails today" has not made this argument
- the **files it touches** --- the dispatcher schedules by this and will not
  overlap two units on one file
- the **falsifying mutation** for each, and what it must break. If you cannot
  name one that goes red, say the site is unproven rather than proposing a
  vacuous test
- the **vacuity traps** that apply, from `docs/agents/implementing.md`
- the **CR citations**, quoted from `docs/rules.txt` and grepped by number

Cite identifiers to grep for, not line numbers.

## Discriminating power

A drafted test is a claim about TWO implementations, and it is worth
dispatching only if the buggy one fails it. Deriving that the code is wrong is
not deriving that the board you drafted can see it wrong; the gap between those
two has shipped a brief whose test the bug satisfied, caught by the
implementer's mutation coming back green.

So before the test goes in the brief, walk both implementations over the exact
board and write the walks down.

- Name the asserted quantity --- a count, a zone's contents, a controller ---
  and give its value under each implementation. Equal values mean the board
  cannot discriminate: change the BOARD, never the assertion.
- Walk the buggy implementation to the END, through the rules that fire on the
  way. #1683's board gave each player one basic land and asserted a hand of
  one; the cross product does raise the extra search, but CR 400.3 sends the
  found card to its OWNER's hand, so both implementations end at one card
  each. A second card in one library separates them.
- Justify every element of the board --- why two seats and not three, why two
  different lands and not the same one. An element you cannot justify is
  usually the one hiding the collapse.
- Prefer a quantity a partial fix cannot reach by another route. A count that
  mere de-duplication would also repair proves less than the identity of the
  thing counted; assert both, gameplay count first.
- Where the divergence is in a TIME rather than a value, say which moment is
  read. An effect that ends and one that merely pauses agree at the first read
  and differ at the second, so a test asserting only the first proves the
  weaker claim.

This is not the falsifying-mutation bullet from another angle, and neither
substitutes for the other: the mutation asks whether the assertion is wired to
the code, this asks whether the BOARD can tell the two answers apart. When no
board separates them, say so --- "every candidate board makes the two readings
agree" is already a finding, and it is the one listed under "What a finding is".

## Composition, not just existence

The expressibility check asks whether each clause has an opcode. It passes on
cards whose opcodes are inert TOGETHER, because what an atom answers depends on
the path it is reached by and not on the atom. Showstopping Surprise needed
`EachMatching`, `IsBound`, `AgainstSlot`, `Power` and a `DealDamage` dealer; all
of them existed, and the sweep reaching them built its `Filter.Context` through
`Filter.contextFor`, whose slot map is empty --- so "each OTHER creature" was
`Not (IsBound "target")` against nothing, the exclusion never fired, and the card
damaged its own target to death. The brief verified every piece exists; the
implementer found by building that they do not compose (#1876).

The symptom is a VACUOUS ATOM: one that cannot be false, or cannot be true,
because the field it reads was never filled on this path. It compiles, the codec
round-trips, the card loads, and nothing is red.

So for each opcode the producer needs, name the path and the context it is
evaluated in.

- Trace from the producer's shape to the call, and name the function that builds
  the context there. `Filter.Context`'s context-relative fields are empty in
  `Filter.contextFor`; its header says which positions that is the honest answer
  for, and a path outside that list wanted a different builder.
- Ask what the atom answers against an empty field. `Filter.IsBound`,
  `SameNameAsBound`, `IsHostOfSource` and `ControlledByRecipient` each answer
  False rather than raising, and `Quantity.AgainstSlot` answers unanswered ---
  neither distinguishable from a rule that genuinely does not apply.
- Read the guardrail, and say which one applies. `Pawl.CardSpec` fences some of
  those fields with a position lint that keeps a card out of the positions where
  they are empty; `slotObjects` and `recipient` carry no lint, so a card can
  reach them and the brief must reason about them itself.
- Where two opcodes must see each other, say whether one builder supplies both.
  A slot is visible to `IsBound` only where the resolution's own map is handed
  over (`Resolve.effectContext`), and only where it names exactly one object ---
  a group binding is invisible to it (#1532). A target slot's own filter is
  matched before any of that exists (`Target.admittedGiven`), and an
  intervening-"if" reads a map filtered differently again
  (`Binding.objectSlots`).

When reading cannot settle it, say so: "these opcodes exist and I did not verify
they compose along <path>" is an honest brief and points the implementer at the
first thing to build. A brief that lists opcodes and stops has verified existence
and claimed composition.
