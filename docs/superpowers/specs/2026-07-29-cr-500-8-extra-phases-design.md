# Finishing CR 500.8's extra phases (and CR 612's reach into filters)

Closes #393, #394, #395, #52.

## The problem

PR #396 fixed one half of the turn schedule's notion of "this phase": skipping a
combat phase no longer deletes a *later* combat phase's steps. It left three
follow-ups, and a fourth defect held together only by a comment.

1. **#393** — `Effect.AddCombatAndMainPhase` is nullary. CR 500.8 does not fix
   *which* phases an effect adds, and real cards vary; the opcode was built at
   exactly the width of Aggravated Assault.
2. **#394** — no `Filter` atom says "creatures that attacked this turn", so
   Relentless Assault and its family are unauthorable.
3. **#395** — `Resolve.rewriteEffect` reaches only `ModifyTarget`'s
   `Modification`, so CR 612's word swap does not reach a `Filter` carried by an
   effect.
4. **The splice position.** `Turn.spliceCombatAndMainPhase` conses at the head of
   `GameState.remaining`. That equals "directly after this phase" *only* because
   the one card that can say it resolves in a main phase, which CR 505.2 says has
   no steps. An effect resolving inside a *stepped* phase would splice the new
   phases **inside** the current one. Today that argument lives in a comment at
   `Resolve.hs`; no card can falsify it.

#52 arrives as a consequence: the card that proves #393 hardest (Full Throttle)
carries a stated-duration delayed trigger, which CR 603.7b describes and pawl
does not model.

## CR ground truth

Every rule below was read from `docs/rules.txt` at the line given.

| Rule | Line | What it fixes here |
|---|---|---|
| 500.8 | 2129 | "adding the phases directly after the specified phase"; "the most recently created phase will occur first" |
| 501.1 | 2144 | the beginning phase's three steps — its last is the draw step |
| 505.1a | 2176 | an added main phase is a **postcombat** main phase |
| 505.2 | 2180 | "The main phase has no steps" |
| 506.1 | 2196 | the combat phase's five steps, in order |
| 511.3 | 2420 | "After the end of combat step ends, the combat phase is over" |
| 512.1 | 2424 | the ending phase's two steps — its last is cleanup |
| 514.2 | 2438 | all "until end of turn" and "this turn" effects end at cleanup |
| 508.3a | 2297 | "Whenever [a creature] attacks" triggers on **declaration** |
| 603.7b | 2616 | a delayed ability fires once "**unless it has a stated duration, such as 'this turn'**" |
| 611.2c | 2909 | a resolved spell's affected set is frozen when the effect begins |
| 612.1 | 2934 | a text change applies to "any words or symbols printed on that object" |

Note what CR 603.7b gives us: the stated-duration case is written into the rule
that pawl currently implements only half of, so #52 is a gap in an existing
citation rather than a new mechanism.

There is **no CR rule for "for the first time each turn."** The nearest, CR
603.2h, is about "Do this only once each turn" — an instruction qualifier, not a
trigger qualifier. Aurelia's phrase is plain card text narrowing a CR 508.3a
trigger event, and the code comment will say exactly that rather than manufacture
a citation.

## Design

### A. One notion of "this phase", in `Pawl.Turn`

```haskell
-- Split what is left of the turn into THIS phase's remaining steps and
-- everything after it.
thisPhase :: Phase -> Seq Phase -> (Seq Phase, Seq Phase)
```

The boundary is the current phase's **final step**: end of combat (CR 511.3),
draw (CR 501.1), cleanup (CR 512.1). A main phase has no steps at all (CR 505.2),
so its prefix is empty. The final step goes in the *prefix* — it belongs to the
phase it ends.

When the current phase's final step is not in `remaining`, the prefix is
**empty**: the phase is already over as far as the schedule shows, so "directly
after this phase" is the head. This is the deliberate opposite of
`dropSkippedCombatSteps`'s present fallback, which treats a missing end of combat
step as "the whole schedule is this phase". Both are unreachable from
`skipEmptyCombat`, which runs as the declare attackers step ends; the new
direction is the safer one, because it drops nothing rather than potentially
over-dropping.

Two callers:

- `dropSkippedCombatSteps` filters the prefix instead of doing its own
  `Seq.breakl`. **Behaviour is unchanged on every reachable input**: the end of
  combat step is never one of the two steps dropped, so moving it from the suffix
  into the prefix is unobservable — the existing comment already says so in as
  many words. The only difference is the unreachable fallback above.
- `splicePhases` (below) inserts at the boundary.

This is the point of the change. The schedule gets one place that answers "where
does this phase end", and CR 511.3 is cited once rather than twice.

### B. `Effect.AddPhases` (#393)

New `Pawl.Type.ExtraPhase`:

```haskell
data ExtraPhase = ExtraCombat | ExtraMain
```

`Effect.AddCombatAndMainPhase` becomes `Effect.AddPhases [ExtraPhase]`. A payload
rather than a sibling opcode per shape, which is what #393 asks for.
`ExtraCombat` expands to CR 506.1's five steps; `ExtraMain` expands to
`PostcombatMain` per CR 505.1a.

```haskell
splicePhases :: Phase -> [ExtraPhase] -> Seq Phase -> Seq Phase
```

The list is inserted **as one block** at `thisPhase`'s boundary, in written
order. CR 500.8's "the most recently created phase will occur first" governs two
*separate* effects adding phases after the same phase, not the order within one
effect's own list — Full Throttle's "two additional combat phases" are two phases
from one effect and simply run in sequence. Cons-at-head remains the behaviour
for a main phase, so Aggravated Assault's schedule is byte-identical.

`Resolve` passes `GameState.phase gs` as the current phase. The CR 307.5
argument in `spliceCombatAndMainPhase`'s comment — "the resolving phase is always
a main phase" — is **deleted, not moved**: it is exactly the assumption this
change removes.

Codec: the opcode goes from nullary to tagged-with-value.
`aggravated-assault.json` becomes `AddPhases [ExtraCombat, ExtraMain]`.

### C. `Filter.AttackedThisTurn` (#394)

A new atom, plus `Filter.View.attackedThisTurn :: Bool`, folded from
`GameState.events` for `GameEvent.AttackerDeclared` naming this candidate.

This is **not** `IsAttacking`. `Combat.attackers` is wiped by `Combat.clearCombat`
when the end of combat step ends (CR 511.3), so by the time a second combat
phase's main phase resolves Relentless Assault, the first combat's attackers are
gone from the live record. The event log is the right footing: it is cleared at
turn handoff, which is precisely "this turn", and CR 608.2i is the rule that
makes a look-back record legitimate at all.

The field is **lazy**, like `attachedToCreature`, so a filter that does not
contain the atom never folds the log.

`Projection.filterReads` returns `Set.empty` for it, and for a stronger reason
than `IsAttacking`'s: history is not a projected aspect and no `Modification`
writes the event log, so no CR 613 layer can move a set selected by this atom —
the argument `IsToken` already makes.

Off-battlefield candidates, players and event snapshots read `False`, the same
vacuous posture `attacking` takes.

### D. `SelfAttacks` gains a frequency

```haskell
data TriggerFrequency = EveryTime | FirstTimeEachTurn
```

`TriggerCondition.SelfAttacks` takes one. `FirstTimeEachTurn` matches only when
the bearer's `AttackerDeclared` event is the **sole** such event for that bearer
in `GameState.events` — the same log C reads, and `matchesTrigger` already
receives the `GameState`.

A payload rather than a `SelfAttacksFirstTimeEachTurn` sibling, on #393's own
reasoning. `hanweir-garrison.json` gains `"EveryTime"`.

Object identity does the right thing for free: CR 400.7 mints a new object on a
zone change, so a creature that left and returned is a different id and attacks
"for the first time" again — which is correct.

### E. Stated-duration delayed triggers (#52)

`Effect.ArmDelayedTrigger AbilityName (Maybe Duration)`. `Nothing` is CR 603.7b's
default one shot; `Just d` is its stated duration. `Nothing` rather than a
`Duration` arm meaning "once", because once-ness is not a duration — CR 603.7b
words it as the absence of one.

- `DelayedTrigger` gains `expiry :: Maybe Expiry`, armed by `Expiry.arm` exactly
  as a continuous effect's is.
- `Event.delayedPending` keeps an entry that fired if it has an expiry, instead
  of always evicting it. One predicate change.
- `Expiry.dropAtCleanup`, `dropAtTurnOf` and `sweepConditional` each learn about
  `GameState.delayedTriggers`. All three, not just the one Full Throttle needs:
  leaving two stores unswept is a silent leak waiting for the next card.

`TurnScope`'s existing comment already marks this seam — "its once-ness comes
from the delayed store (CR 603.7b), never from the scope" — so Full Throttle's
"at the beginning of each combat this turn" decomposes as
`StepBegins (Combat BeginningOfCombat) EachTurn` armed with `UntilEndOfTurn`,
whose CR 514.2 cleanup expiry is what bounds it to this turn.

`tidal-wave.json` is the only card using the opcode; it gains an explicit
`null` duration.

### F. `Filter.rewrite` (#395)

```haskell
Pawl.Filter.rewrite    :: [(Subtype, Subtype)] -> Filter    -> Filter
Pawl.ObjectRef.rewrite :: [(Subtype, Subtype)] -> ObjectRef -> ObjectRef
```

`Filter.rewrite` maps `HasSubtype` through the pairs and recurses through
`And`/`Or`/`Not`; every other atom is returned unchanged. `ObjectRef.rewrite`
leaves `InSlot` alone and rewrites `EachMatching`'s filter.

`Resolve.rewriteEffect` then threads it through every arm carrying a `Filter`:
`Search`, `Destroy`, `PlayerSacrifices`, `AttachTarget`, `Untap`. It lives in
`Pawl.Filter`, the module that owns the type's logic, mirroring where
`Projection.rewriteModification` sits relative to `Modification`.

Scope is held to the issue's: effects. Tokens and emblems keep the carve-out the
existing comments record, and a `Filter` in a *static ability* or a trigger
condition is untouched — CR 612's rewrite is applied here to the resolving
effect.

## Cards

All four verified live against Scryfall, not recalled.

| Card | Cost / type | Proves |
|---|---|---|
| **Relentless Assault** | `{2}{R}{R}` Sorcery | C — "Untap all creatures that attacked this turn. After this main phase, there is an additional combat phase followed by an additional main phase." Needs nothing but the atom; it is #394's named card. |
| **Aurelia, the Warleader** | `{2}{R}{R}{W}{W}` Legendary Creature — Angel 3/4 | A, B, D — "Flying, vigilance, haste. Whenever Aurelia attacks for the first time each turn, untap all creatures you control. After this phase, there is an additional combat phase." |
| **Full Throttle** | `{4}{R}{R}` Sorcery | B, C, E — "After this main phase, there are two additional combat phases. At the beginning of each combat this turn, untap all creatures that attacked this turn." |
| **Boil** | `{3}{R}` Instant | F — "Destroy all Islands", pointed at by **Magical Hack**, already in the pool. |

Aurelia is the only card in Magic's whole extra-phase family that forces **A**:
her trigger resolves during the declare attackers step, where `remaining` still
holds this phase's own blockers, damage and end of combat. A head-cons would put
the added combat phase inside the current one. She also builds a combat phase
directly after a combat phase — the arrangement #393 notes nothing in the pool
can construct.

"For the first time each turn" is load-bearing, not decoration: without it
Aurelia adds a combat phase every combat and the game never ends.

She needs a new `Angel` subtype — the routine edit-site set for one. Flying,
vigilance, haste and `Legendary` all exist.

Full Throttle's two adjacent added combat phases are the second, independent
exerciser of A's boundary, and the only two-element `AddPhases` payload.

Boil's filter is `HasSubtype Island` — "all Islands", faithfully, without an
added `HasCardType Land` narrowing the card does not print.

## Tests

**Unit (`TurnSpec`).**

- `thisPhase` from inside a combat phase ends at that phase's end of combat, with
  a *second* combat phase left whole in the suffix.
- `thisPhase` from a main phase yields an empty prefix (CR 505.2).
- `thisPhase` when the phase's final step is absent yields an empty prefix.
- `splicePhases` from a combat phase lands the block after end of combat; from a
  main phase, at the head.
- `dropSkippedCombatSteps`'s four existing tests pass **unchanged** and still
  mean what they meant.

**Gameplay.**

- Aurelia attacks: the added combat phase runs after the first combat phase ends,
  not inside it — asserted on the whole remaining schedule, which is the shape
  that falsifies a head-cons.
- Aurelia attacks in that second combat phase: no third phase is added.
- Relentless Assault untaps a creature that attacked and leaves a tapped
  non-attacker tapped — the assertion that distinguishes `AttackedThisTurn` from
  "creatures you control".
- Relentless Assault after the combat phase has ended still finds its attackers,
  which is what `Combat.clearCombat` would have lost.
- Full Throttle: two combat phases and no added main phase, and the delayed
  trigger fires at **both** — the CR 603.7b assertion that fails today.
- Full Throttle's delayed trigger is gone after cleanup (CR 514.2).
- Magical Hack names Island → Forest and then Boil destroys the Forest while the
  Island survives.

Each gameplay test is written first and seen red.

## Deliberately not done

- **Savage Beating**, on three blockers, none of which this concern needs:
  **#377** for "creatures you control gain double strike until end of turn" (CR
  611.2c's frozen filter-selected set has no opcode, and `Affected.Matching` is
  the dynamic set, which would wrongly catch creatures entering later); **#399**
  for entwine (CR 702.42); **#400** for "Cast this spell only during combat on
  your turn" (CR 601.3 — `Card` has no cast-*timing* axis, and legality today is
  `isInstant || sorcerySpeed`). It proves nothing about CR 500.8 that Aurelia and
  Full Throttle do not. Its second mode, the one this branch would enable, is the
  only part that is free.
- The other extra-phase cards, each blocked on one capability: Port Razer and
  Bloodthirster on "can't attack a player it has already attacked this turn";
  Hellkite Charger on "you may pay {5}{R}{R}. If you do" mid-resolution;
  Najeela on #377 and #380.
- **No card adds only a main phase.** Scryfall has none, so `ExtraMain`'s
  standalone use stays unexercised — it exists because `[ExtraPhase]` is the
  honest shape of CR 500.8, not because a card demands it alone.
- A `Filter` inside a static ability or trigger condition is not text-rewritten
  (F's stated scope).

## Risks

- **Termination.** Extra phases threaten `playGame`'s termination argument
  (#338). Aurelia self-limits via D; Full Throttle is a one-shot sorcery adding a
  fixed two. Aggravated Assault could already loop, gated by its cost, so this
  change adds no new unbounded path.
- **The `Expiry` sweeps** now touch a second store. The risk is a missed sweep
  leaving a delayed trigger armed forever; the cleanup test pins the one Full
  Throttle uses, and the other two are written by symmetry with the continuous
  effect store they already sweep.
- **`dropSkippedCombatSteps`'s fallback direction changes.** Unobservable from
  its only caller, argued above, and its comment is rewritten rather than left
  describing the old behaviour.

## Does the rules core case on an effect's identity?

No. `Turn` takes a `Seq Phase` and an `ExtraPhase` list and knows nothing about
effects. The new `case effect of` work is confined to `Pawl.Resolve`, that
module's stated charter. `Filter.rewrite` cases on `Filter`, a characteristic
predicate, never on which effect carried it.
