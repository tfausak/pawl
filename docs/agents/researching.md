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

Split what you write by whether reading can settle it. Measured over a day's
run of brief-driven units, the split is stark: what the researcher could settle
by reading survived in most of the PRs that reported on it, and what the
researcher could only guess at was corrected in nearly every one.

**Reading settles these, and they are where your value is.** Whether the issue
body is still true --- the loop's highest-yield research act, and the thing
that day's PRs corrected more often than anything else. Producer hunts and
Oracle text, whether a stated blocker has landed, edit-site enumeration,
cross-file greps, elision and census sweeps, CR citations, which half of a
split issue you are aiming at.

**Reading cannot settle these.** Wire spellings: not one drafted spelling in
that run was right as written. Drafted test boards: one in eleven survived.
Falsifying mutations: wrong more often than right, and wrong in BOTH directions
--- predicted-red came back green (#2258, #2219, #2238), predicted-green came
back red (#2256, #2232, #2227). Which files a unit touches. Line numbers.

Draft only the ones that force an argument --- drafting the mutation is what
forces the discrimination argument --- and mark them unverified in the HEADING,
not a footnote. The one robust meta-signal in that run: where a brief flagged
something unverified, the flag itself was almost always right.

**Never tell the implementer not to chase something.** #2227's brief predicted
a mutation green and wrote "do not add a third seat to chase them"; a fourth
board reddened it, so the prediction nearly bought a fenced line where a proof
existed. A prediction that argues for weaker coverage is worse than none.

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

**Sweep the tree, not the card pool.** Re-checking whether an
`expires:card-driven` issue's producer has arrived in `data/cards/` is a closed
seam: two independent passes over every open card-driven issue found no fired
trigger. That label does not mean wait --- it means the unit is "add the card",
and the sweep will not find it for you. The open seam is engine-internal:
issues whose stated gap, blocker or carrier is stale against the tree.

## Clusters

A cluster is two to four open issues that one person would work in one sitting:
one dispatch, one PR closing them all. It needs a shared producer and edit-site
list, and a per-issue proving test. Name the issues it closes and the order to
work them.

**A shared TOPIC is not a cluster, and topic is what a body's claim of shared
machinery usually turns out to mean.** Every one of the eleven topic trackers
#2190--#2200 asserts shared machinery in its body; every one of them split into
unrelated units under triage. Before proposing a cluster, name the function or
constructor all its issues edit. If you cannot, you have found a topic, and the
issues dispatch separately.

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
only theirs is the critical path. But a field that gets corrected is not free
either: it costs the implementer a mid-unit redesign. Carry the fields below at
the strength stated and no higher.

**At full strength.** These are what pays for the brief.

- the **verdict**: dispatchable, or blocked with the missing capability named
- the **re-derivation of the issue body and its blockers** against the tree ---
  say which of its claims are false, not only that it is stale
- the **blocker's issue number and the capability it holds**, when blocked. You
  may not link the dependency yourself --- name it for the dispatcher. If it
  has no issue, say so: that is an untracked deficiency
- the **producer**, with Oracle text fetched THIS SESSION, never copied from
  the body --- #2261's brief inherited the body's wrong mana cost by copying
  it. Say whether the card is already in `data/cards/`, and give a
  clause-by-clause expressibility check naming the opcode for each. If a clause
  must be omitted, say whether the omission runs stricter or weaker than
  printed
- the **edit sites**, `-Werror`-forced ones enumerated by grepping a named
  sibling constructor, every `{}` or `_` site flagged separately. The
  highest-volume field and the best survival rate of any non-trivial one; a
  miss costs a compile error and a phantom costs reading time, both cheap
- the **elision and census sweep** by bare issue number
- the **vacuity traps** that apply, from `docs/agents/implementing.md`
- the **design call**, when derived from the CR and a named carrier
- the **ref you derived against**, one line

**Flagged unverified, in the heading.**

- the **composition check** --- as a question, not an answer. See below
- the **CR citations**, grepped by number from `docs/rules.txt`. Right about as
  often as wrong, and a CR number must never stand in for a code pointer:
  #2227's brief pointed at `Combat.attackers` where CR 508.4 wants
  `declaredAttackers`, a rules error dressed as a code pointer
- the **discrimination argument**, and only its NEGATIVE half. See below

**Do not write these.** Each was wrong essentially every time it was reported
on, and each has a cheaper substitute that was right every time.

- *Card JSON and Haskell wire spellings.* Substitute: "spell it after
  `data/cards/<neighbour>.json`, which writes the same payload", plus the
  Oracle text. A neighbour existed in every case; the transcription was wrong
  in every case.
- *The drafted board.* Substitute: the asserted quantity and its value under
  each competing reading. That is the half implementers kept and used; the
  fixture calls, seat counts and helper names are the half they threw away.
- *A files-touched list, as a scheduling artefact.* Substitute: the subsystem
  name and the one or two files the unit certainly rewrites. The precise list
  named modules that do not exist (#2206), the wrong spec file (#2215) and a
  module placement that is an import cycle (#2247).
- *A falsifying mutation as a prediction.* Substitute, per site, either "I
  found no observer in `data/cards/` --- expect green" or silence. Never name
  the expected colour, never name the assertion, never advise against chasing
  one.

## Discriminating power

A drafted test is a claim about TWO implementations, and is worth dispatching
only if the buggy one fails it. Deriving that the code is wrong is not deriving
that your board can see it wrong.

**Your negative claims land; your positive ones do not.** Over that day's run,
every surviving discrimination claim was the researcher saying a board *will
not* work --- #2242's "the obvious one-leg board cannot discriminate, which is
why legs two and three exist", #2218's flagged collapsing case. Every claim
that a board *would* work was corrected. So: state which boards collapse and
why, name the quantity and its value under each reading, and stop there. The
board itself is the implementer's to build.

Handing over a board that cannot discriminate is the expensive failure, not a
neutral one: #2255's brief planted the spell with `S.spellOnStack`, which never
chooses a mode, so the spell resolved to nothing and every leg of the test read
identically.

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

**Write this section as a question.** It is the field this file argues hardest
for, and the field the day's implementers corrected most often: proposed call
shapes were wrong (#2225, #2247), a claimed existing proof turned out to be a
card no spec plays (#2263), and the run's one real composition defect ---
`Effect.Search` building `Filter.contextFor Nothing Nothing`, so a new
context-relative atom would be vacuously False --- was found by the
implementer, not the brief (#2234). What worked was flagging it as open and
naming the first function to read (#2241, #2225). Do the trace below to find
the question; hand over the question and the function, not a verdict.

The expressibility check passes on cards whose opcodes are inert TOGETHER,
because what an atom answers depends on the path it is reached by. Showstopping
Surprise needed five opcodes; all existed, but the sweep reaching them built
its `Filter.Context` through `Filter.contextFor`, whose slot map is empty ---
so "each OTHER creature" was `Not (IsBound "target")` against nothing, and the
card damaged its own target to death (see #1876, the PR that fixed it). The
live issue for the remaining callers is **#2141** --- cite that, not the PR
number. A brief copying #1876 out of this file cited a merged PR at the
implementer (#2225).

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
