# Tracking migration — git-bug, elisions and gaps become GitHub Issues

## 0. Why now

The project started without a GitHub remote, so tracking accreted in four
places: **git-bug** (`refs/bugs/*`, explicitly labelled a "temporary pre-flight
tool" in `CLAUDE.md`), **`EXPIRES` / "named deferral" comments** in the source,
**per-spec deferral sections** (every spec has one — `## 7. Expiries`, `## 8.
Deferred, with named expiries`, or a near-variant), and the **ranked GAP list**
in `docs/mtgish-gap-census.md`. The remote now exists and has Issues enabled.

The problem is not tidiness, it is **drift**. Five of the source's elision
comments name a milestone that has already shipped:

| Site | Comment says | Reality |
|---|---|---|
| `Combat.hs:172` | EXPIRES at M2 | M2 complete |
| `Combat.hs:49` | EXPIRES at M4+ | M4 complete |
| `Type/Combat.hs:34` | EXPIRES at M3 | M3 complete |
| `Damage.hs:106` | EXPIRES at M3 | M3 complete |
| `Turn.hs:53` | EXPIRES at M4 | M4 complete |

An in-code expiry naming a milestone is a promise nothing checks. The milestone
lands, the plan does not think to grep, and the comment silently becomes a lie.
`Turn.hs:53` is the sharp case: it says a second combat phase (CR 500.8) forces
`dropSkippedCombatSteps` to drop steps positionally rather than globally, M4
shipped, and the function still drops every combat step in the schedule. That is
not a stale comment, it is an unfixed defect the milestone walked past.

A tracker with a milestone field and a close event does check. That is the whole
argument for moving.

## 1. What migrates

**Everything that represents outstanding work**, in three groups.

### 1.1 git-bug — all 19

Seven open, twelve closed. The closed ones migrate too, as closed issues, so
that citations to *closed* bugs stay resolvable.

**The citation surface is larger than it looks: 57 references to 10 distinct
hashes**, spread across `source/` (5), `docs/progress.md` and `docs/design.md`
(14), and **landed specs and plans (38)**.

These are not all rewritable. `CLAUDE.md` keeps each milestone's spec and plan
"as reference" — they are historical documents recording what was true when
written, and a landed plan is a record of executed work. Rewriting hashes inside
them would falsify that record. So:

| Location | Action |
|---|---|
| `source/` (5 sites) | Rewrite to `#N` — live code must cite a live tracker |
| `progress.md`, `design.md` (14) | Rewrite to `#N` — living documents |
| Landed specs and plans (38) | **Leave untouched** |

The 38 untouched citations would dangle once `refs/bugs/*` is deleted. They are
resolved instead by a **hash → issue mapping table**, appended to this spec as
§10. That is the durable artifact that lets the git-bug refs be deleted without
orphaning a single historical reference, and it costs one table rather than 38
edits to files that should not change.

### 1.2 Elisions — the source comments and the spec sections

The source carries ~58 comment sites matching `EXPIRES` / `expiry` / `elision` /
`elided` / `deferr`. These deduplicate substantially — six sites cite "the P4
spec, section 8", four cite "the P3b spec, section 8" — so the distinct-deferral
count is materially lower than the site count.

**The source comments are the ground truth for what is still live**, because a
cashed expiry has had its comment removed. **The spec sections are the prose**,
already written and already precise; they become issue bodies with light
editing. Deferrals listed in a spec whose comment no longer exists in the source
are cashed and are **not** filed.

### 1.3 GAPs — as phase tracking issues, not as GAP issues

The ranked GAP list in the census is **already fully superseded** by the M4.5
umbrella spec's phase table. Five of the twelve shipped (GAP-L2→P1, GAP-L1→P2,
GAP-L5→P3a, GAP-L7cda→P3b, GAP-T→P4) and every survivor has a phase:

| GAP | Phase |
|---|---|
| GAP-R (replacement event coverage) | P5 |
| GAP-D (conditional/event durations) | P6 |
| GAP-P (player continuous effects) + GAP-Co *modification* half | P7 |
| GAP-Co *payment* half | P8 |
| GAP-F (target-filter language) | P9 |
| GAP-C + GAP-S (poison/energy) | P10 |
| GAP-Z (Command zone, phasing) | P11 |

So there is no such thing as filing "the GAPs" separately. What gets filed is
**one tracking issue per unlanded phase** — P5 through P11, plus M5, M6, M7 —
each labelled with the GAP it discharges. GAP-P and GAP-R already exist as
git-bugs (`c5a985d`, `48b17cb`); those migrate into the P7 and P5 issues rather
than becoming duplicates.

## 2. What does not migrate

- **Code `TODO`/`FIXME`/`XXX` comments.** There are none. The single grep hit is
  the word "hacking" in a `CastSpec` comment.
- **Memories.** They are conventions (`no-api-stability`,
  `prefer-boot-libraries`), not work. A separate staleness pass is warranted —
  `m3-split-analysis` still names the M3a spec as the next step — but it
  produces no issues.
- **`docs/progress.md`.** See §5.
- **`docs/mtgish-gap-census.md`.** It keeps its derivation and its "what is not
  a gap" guard (§5 there), which is load-bearing against misreading the open
  half as a backlog. Its **ranked table** is superseded and gets a pointer to
  the project board.

## 3. The comment convention

An issue is created; the comment **shrinks to a citation**. It does not vanish.

```haskell
-- before
-- CR 613.7 timestamp order within a layer. EXPIRES: CR 613.8b dependency
-- ordering is deferred; a dependency-aware reorder would override this.
-- Deferred -- no M3c card falsifies it; existence …

-- after
-- CR 613.7 timestamp order within a layer; CR 613.8b dependency ordering
-- is not implemented (#42).
```

The split is by **what drifts**:

| Content | Home | Why |
|---|---|---|
| Status, priority, expiry trigger, rationale | Issue (sole copy) | These change; a tracker records change |
| "This code is deliberately incomplete in this way" | Comment (sole copy) | This is true until fixed; it is point-of-use |

Nothing is duplicated, so nothing can drift. The comment dies in the same commit
that closes the issue.

**This keeps `CLAUDE.md`'s second invariant satisfiable as written** — "every
elision carries a documented expiry naming the milestone that kills it" — with
the expiry now named by the issue's milestone rather than by prose. Deleting the
comments outright would have required amending that invariant and would have
left a reviewer at `Projection.hs:611` with no local signal that timestamp-only
ordering is deliberate.

`ProjectionSpec.hs:372` already uses exactly this shape with a git-bug hash. The
migration generalizes an existing precedent; it does not invent one.

## 4. Taxonomy

**Labels.**

- Kind: `elision`, `gap`, `perf`, `chore`, `rules-correctness` (plus the stock
  `bug`, `documentation`).
- Expiry trigger: `expires:milestone` (a scheduled phase retires it) versus
  `expires:card-driven` (it fires when a card demands it, with no date).
  This distinction is **not expressible as a GitHub milestone** and roughly half
  of pawl's expiries are card-driven — `Mana.hs:202` expires "the moment mana
  sources differ in any way a player could care about". It must be a label.
- Area: `layers`, `combat`, `mana`, `events`, `triggers`, `prompts`,
  `replacement`, `targeting`, `test-suite`.

**Milestones.** `M4.5 P5` … `M4.5 P11`, `M5`, `M6`, `M7`. Only issues with a
scheduled home get one; card-driven elisions get none, by design.

**Project board.** One board carrying the ordering that GitHub issues lack —
seeded from the census's ranked list as absorbed into the phase table.

**Issue body template** for an elision: site at time of filing (a locator, not a
contract), CR rule, what is elided, what correct behaviour would be, and the
**expiry trigger** as its own field.

## 5. `progress.md` keeps a narrowed charter

It does **not** go away. Three reasons it is not reproducible from closed
issues:

1. **No issues will exist for M0–M4h.** Only open work migrates. Nothing in
   GitHub will describe M2c, M3c or M4f at all, so "progress is implicit in the
   tracker" would be true only of progress the tracker never recorded.
2. **It is load-bearing in the session bootstrap.** `docs/workflow.md:19`
   directs each new session to read `design.md` §3 plus the newest
   `progress.md` entry — one file read for the whole arc. `design.md:422` points
   at a specific entry, and eight plan files reference it.
3. **It records arguments, not work.** The M4f entry's core is "counters must be
   typed per-kind counts, because a permanent holding both a +1/+1 and a −1/−1
   counter falsifies a net-`Integer` model under CR 704.5q/122.3" — plus a rules
   correction (`design.md` said layer 7d; CR 613.4c says 7c) and a theorem
   explicitly marked *not* an elision. A closed issue records that work
   happened. It does not record what the work established, and the second is not
   derivable from the first.

**What is removed:** the "Named deferred expiries: …" clause from each entry —
~65 lines, and precisely the content that just demonstrated the drift. Each is
replaced by a pointer to the milestone's issue label.

**What stays:** gate cards, the decision proved, rules corrections,
opcodes/types added, falsifiers-as-tests.

## 6. `CLAUDE.md` changes

- The tracking bullet: git-bug → GitHub Issues, with the `gh issue` commands.
- A new standing convention: **file the issue, cite it inline.** Without this,
  P5 onward regenerates the drift and this migration is repeated at M5. The
  elision invariant's wording is unchanged (§3).

## 7. Execution order

**Phase A — GitHub side, no repo writes.** Labels, milestones, board. Then
issues, filed in a deliberate order so numbers group: git-bug (open, then
closed), phase trackers P5–P11 + M5–M7, then elisions by area.

**Phase B — repo side, needs a clean tree.** Backfill `#N` into the ~58 comment
sites and shrink them; triage the five stale comments (§8); thin `progress.md`;
rewrite the 19 rewritable git-bug citations in `source/`, `progress.md` and
`design.md`; record the hash → issue table in §10; update `CLAUDE.md` and the
census's ranked table; delete `refs/bugs/*`.

Phase B is normal repo work: `cabal build` warning-clean, `hooky fix` then
`hooky run`, tests green. Comment-only edits cannot change behaviour, but the
build must still be clean because comment reflow can push a line past the
formatter's limits.

## 8. The five stale comments are triaged, not converted

Each of the five in §0 is read before anything is filed. Three outcomes:

- **Cashed** — the elision was resolved and only the comment survived. Delete
  the comment, file nothing.
- **Live, milestone misnamed** — file with a corrected trigger.
- **Unfixed defect** — the milestone walked past it. File as `bug`, not
  `elision`.

Blind conversion would file phantom issues; blind deletion would silently drop
a real defect. Neither is acceptable. §11 records what the triage actually
found — including that this section's guess about `Turn.hs:53` was wrong.

## 9. Exit criterion

- `git-bug bug` has no data; `refs/bugs/*` is gone.
- No `EXPIRES`/deferral comment in `source/` lacks a `#N` citation, and no
  citation in `source/`, `progress.md` or `design.md` names a git-bug hash.
- Every hash cited by a landed spec or plan appears in §10's mapping table, so
  no historical reference is orphaned.
- Every open issue's site citation resolves to real code.
- `progress.md` carries no per-entry expiry inventory.
- `CLAUDE.md` documents the file-and-cite convention.
- Build warning-clean, `hooky run` passes, tests green.

## 10. git-bug hash → issue mapping

Filled in during Phase A. Resolves the 38 citations left in place inside landed
specs and plans (§1.1).

| git-bug | Issue | Title |
|---|---|---|
| `a7ec85e` | #15 | M0 — complete game with zero cards |
| `16dd59d` | #16 | Fill in project metadata TODOs |
| `be3848a` | #17 | M1a — casting a creature |
| `d1ebd83` | #18 | M1b — combat |
| `5f50eec` | #19 | CR 508.8: declare blockers/combat damage steps not skipped |
| `675e8df` | #20 | M2a — the keyword seam |
| `b9164e2` | #21 | Test suite deadlocks under tasty's default parallelism |
| `b79d36b` | #22 | M2b — first strike and conditional turn structure |
| `14138aa` | #23 | Add Swamp/Forest basic lands + per-player mono-color decks |
| `15de615` | #24 | Setup: emptyGame/matchup agreement by construction |
| `fa2c488` | #25 | M3c: projection is ~10x slower |
| `83f1a55` | #26 | Gap: copy mechanism / layer 1 (GAP-L1) |
| `f90e0c4` | #11 | CR 613.8b topological applies-to dependency |
| `65ce714` | #12 | payCost must prompt when mana sources are distinguishable |
| `b998924` | #13 | OfAbility resolution reads by id, not LKI (CR 608.2g) |
| `c7a0077` | #14 | Generalize `Quantity.X`'s reserved slot to `Quantity.Bound` |
| `6afb561` | #1 | M3f replacement seam — absorbed by the P5 tracker |
| `48b17cb` | #1 | GAP-R replacement event coverage — absorbed by the P5 tracker |
| `c5a985d` | #3 | GAP-P player continuous effects — absorbed by the P7 tracker |

## 11. What the triage actually found (§8, executed)

Recorded because three of the five predictions in §0 and §8 were wrong, and the
corrections are the point of having triaged rather than converted.

| Site | Predicted | Actual |
|---|---|---|
| `Type/Combat.hs:34` | live elision | **Cashed.** `struckFirst` *is* the CR 510.4 snapshot the comment asks for; `Damage.dealCombatDamage:207` stores it and 210–213 reads it for "had neither … as the first step began" while reading double strike live — verbatim CR 510.4. No issue filed; comment replaced with what the code does. |
| `Damage.hs:106` | live elision | **Half cashed.** The Mindslaver half landed at M3g — line 111 threads `Decide.deciderFor`. Only banding (CR 702.22j) survives (#32). |
| `Turn.hs:53` | **unfixed defect** (§8's prediction) | **Live elision, misnamed expiry.** Nothing constructs a second combat *phase*: `spliceSecondDamage` adds a second combat damage *step*, and `skipEmptyCombat` runs at declare-attackers, so the two never interleave. `Seq.filter` is still correct (#31). |
| `Combat.hs:49` | live elision | Confirmed live, expiry misnamed — card-driven, not M4 (#30). |
| `Combat.hs:172` | live elision | Confirmed live, and `isBlocked` turns out to have **no caller in `source/library/`** — test-only, so it cannot affect gameplay as written (#28). |

**Two gaps had no comment and no spec entry**, found only by reading the code
during the pass: `Damage.hs:92` reads `blockersOf` without filtering departed
blockers, reachable since M3a added instant-speed removal (#29); and
`Event.stateTriggers`' `Source`-equality suppression check (#55). A third,
CR 208.2a/208.5's undeterminable-number substitution, was in the P3b spec but
had been read as part of the freeze residual rather than as its own deferral
(#65).

**Refinement to §1.2's rule.** "File what the source comments mark" needed a
carve-out: a spec deferral with no code site is either cashed *or* an unbuilt
feature that never had a site to mark (copy-spell, hybrid mana, protection).
Those are tracked by their phase and remain in the spec; filing one issue per
future feature would have been noise. Each milestone entry in `progress.md`
therefore points at its spec's deferral section for the full list, and cites
issues only for the subset with a live code site.
