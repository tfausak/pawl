# M3f the event pipeline (replacements + triggers) — design

Design for milestone **M3f**, the sixth letter of M3 (see the split table in
`docs/design.md`): **the event pipeline — triggered abilities (CR 603) and
replacement effects (CR 614), built on one substrate: the zone-change event.**
This letter is not the M3 go/no-go — that verdict arrived at the end of M3d — but
it cashes two of the three ABI decisions the M3 section says M3 owes beyond the
gate cards: **the event substrate** (the "atom" pattern — every observable
mutation flows through one change-and-emit helper) and **effect ≡ event**
(one vocabulary wiring replacements and triggers together, the MedeaMelana shape;
`prior-art-lessons.md` §10.2). It builds directly on M3e's ability-on-stack
machinery.

The gate card, Scryfall-verified (`api.scryfall.com/cards/named`, fetched
2026-07-18):

| Card | Cost | Type line | Oracle text |
|---|---|---|---|
| **Rest in Peace** | `{1}{W}` | Enchantment | "When this enchantment enters, exile all graveyards. / If a card or token would be put into a graveyard from anywhere, exile it instead." |

Rest in Peace is the whole-card gate, and it is the right one because **every
clause is about a single event — a zone change.** Its first line is an
enters-the-battlefield **triggered ability** (CR 603.6a); its second is a
graveyard-bound **replacement effect** (CR 614.1a, "instead"). The ETB is a zone
change *into* the battlefield; dying, discarding, milling, and a spell resolving
to the graveyard are all zone changes *into* the graveyard. One substrate — the
zone-change event — with two consumers: **614 rewrites the event before it
happens, 603 observes it after.** This is why the card falsifies a battlefield-only
"when a creature dies" hook: it must catch a resolving spell going stack→graveyard
and a discarded card going hand→graveyard, neither of which is a creature death.

This is a types-and-architecture spec, not an implementation plan.

## 0. The core idea and the phased spine

**M3f makes `changeZone` a first-class event and wires both halves of the CR
603/614 pipeline to it.** This generalizes the change-and-emit funnel that already
exists for damage (`Damage.applyDamage` writing `GameState.damageEvents`, which
the CR 704.5h SBA reads) — the M2c/M3b "atom" pattern, now extended from combat
damage to zone changes.

| Axis | Mechanism | Gate |
|---|---|---|
| **Replacement (CR 614)** | `changeZone` consults active replacement effects and rewrites the event's destination *before* moving; a graveyard-bound object is redirected to exile | Rest in Peace, line 2 |
| **Triggering (CR 603)** | `changeZone` emits the *resolved* event; at every CR 117.5 priority boundary, abilities whose condition matches an emitted event are put on the stack (APNAP), resolve, and cease | Rest in Peace, line 1 (ETB) |

**One decision is forced by the gate.** Rest in Peace must catch a creature
killed by a **state-based action** (lethal combat damage → the creature is
destroyed → it *would* go to the graveyard → exiled instead). SBA burial lives in
`Sba.checkStateBasedActions`, which is pure (`GameState -> GameState`). So the
replacement seam at `changeZone` **must apply purely** — `changeZone` stays
`GameState -> GameState` and consults a purely-computed rewrite. This is
comfortable for Rest in Peace: a single replacement, deterministic, with no CR 616
ordering choice to prompt. The general monadic replacement path (multiple racing
replacements, CR 616's affected-player ordering prompt, replacements that require
a choice) is the milestone's headline expiry (§7). Triggered abilities are
different: they reach the stack inside the already-monadic priority loop, so their
target/mode prompting is unaffected by the pure seam.

**The phased spine** (the M3c/M3d/M3e structure — land the understood machinery,
keep the interesting seam isolated):

1. **Replacement at the funnel (CR 614).** The `ZoneChange` event value, the
   `ReplacementEffect` leaf family, `changeZone` rewriting and emitting, and the
   `Pawl.Event` module that owns event/pattern matching. Gate: **Rest in Peace
   placed directly on the battlefield** (no trigger yet) redirects every
   graveyard-bound move to exile — a creature dying via SBA, a resolving Lightning
   Bolt from the stack, a discarded card from hand.
2. **Triggered abilities (CR 603) + the ETB opcode.** The `TriggeredAbility` /
   `TriggerCondition` leaf families, `Source.OfTrigger`, the shared
   `Resolve.resolveEffects` executor (refactored out of `resolveAbility`), the
   `Effect.ExileAllGraveyards` opcode, and the CR 117.5 SBA-then-triggers loop in
   the engine. Gate: **the whole card** — cast Rest in Peace, it resolves and
   enters, its ETB triggers, goes on the stack, resolves, and exiles every
   graveyard; and thereafter its replacement exiles new arrivals.

M3f stays **one milestone, one spec, one plan**: the phases are ordered commits;
the plan owns the decomposition.

## Goal and scope

**Exit criterion.** Deterministic tests demonstrate all of:

- **Replacement at the funnel (Phase 1, CR 614.1a/614.6).** With Rest in Peace on
  the battlefield, an object that *would* be put into a graveyard is exiled
  instead, from **every** source zone the test can reach:
  - a creature dealt lethal combat damage is exiled by the CR 704.5g SBA rather
    than buried (the replacement fires *inside* pure `checkStateBasedActions`);
  - a resolving **Lightning Bolt** is exiled from the stack instead of going to
    its owner's graveyard (CR 608.2n's move is a `changeZone` like any other);
  - a discarded card is exiled from hand instead of the graveyard (CR 514.2).
  Without Rest in Peace, each of these goes to the graveyard — the negative
  control. CR 614.5: the redirect applies at most once (its output, exile, no
  longer matches the graveyard pattern, so there is no self-loop).
- **The ETB trigger (Phase 2, CR 603.6a).** Casting Rest in Peace (paid with white
  mana) puts it on the stack; it resolves and enters the battlefield; its ETB
  ability triggers, and at the next CR 117.5 boundary is put on the stack as an
  `OfTrigger` object; on resolution it exiles all cards from all graveyards and
  **ceases to exist** (CR 608.2n). The falsifier: an engine that scans only the
  entering permanent's own text, or that never emits the enters event, never fires
  the trigger.
- **The whole card (Phase 2).** After Rest in Peace's ETB has resolved, a creature
  that then dies is exiled by the replacement (line 2), not returned by any
  battlefield-only hook — the two halves coexist on one event.
- **CR 603.2g / 614.6 ordering.** The event emitted for trigger detection is the
  *resolved* (post-replacement) event: a graveyard move that was redirected to
  exile does not fire a hypothetical "goes to graveyard" trigger. (No dies-trigger
  card exists in M3f; this is asserted structurally — the emit happens after the
  rewrite — and named so the plan does not rebuild it backwards.)

The `DecisionLog` replays deterministically with the cast of Rest in Peace, the
ETB trigger placement (no target choice), and the redirected zone changes in the
serialized path.

**Non-goals.**

- **No tokens.** Rest in Peace's replacement names "a card *or* token," but tokens
  (CR 111, `Create` CR 701.6) are M4 vocabulary. The replacement's predicate is
  written object-and-zone-general ("any object that would enter a graveyard →
  exile"), which is correct for tokens the day they exist; the literal "token
  dying → exiled, then the token ceases (CR 111.7)" branch is a named expiry (§7).
  The card-based non-death cases (stack spell, discard, SBA death) falsify the
  battlefield-only hook without them.
- **No CR 616 ordering, no monadic replacements.** Exactly one replacement effect
  is active in every M3f scenario, so there is never a choice of *which*
  replacement applies first (CR 616.1) and never a replacement that requires a
  player's choice. The pure seam holds by construction. The general path — a
  monadic `changeZone` split into a pure "what would happen" and a prompting
  "apply," plus a CR 616 ordering Prompt — is the headline expiry (§7).
- **No unified event log.** M3f introduces `GameState.zoneChanges` for the
  zone-change substrate; combat's `damageEvents` stays a separate log with its own
  (704.5h) reader. Folding both into one `events` pipeline (the full effect ≡ event
  unification) waits until a second event *kind* has a replacement or trigger
  consumer — burn/lifelink and combat-damage triggers at M4 (§7).
- **Only the enters (battlefield) trigger pass.** Trigger detection is designed as
  the three passes prior art demands — battlefield / phase-step / leaves-battlefield
  (`prior-art-lessons.md` §81) — but only the **battlefield/enters** pass is
  populated. Leaves-the-battlefield and dies triggers (which need last-known
  information, CR 603.6/603.10), "at the beginning of [step]" phase triggers (CR
  603.2b), the CR 603.3b two-part placement, the intervening-`if` clause (CR
  603.4), optional/`may` triggers (CR 603.5), and delayed triggered abilities (CR
  603.7) are all named expiries (§7).
- **No last-known information.** Rest in Peace's ETB is forward-looking — the
  entered permanent is on the battlefield when the trigger resolves. LTB/dies
  triggers, which read a snapshot of an object that has left, are deferred with the
  LTB pass.
- **No `Effect` generality beyond the gate.** `ExileAllGraveyards` is Rest in
  Peace's exact shape (a targetless bulk exile of every graveyard). Exile-from-a-
  named-zone, exile-a-target, and exile-with-a-return are deferred (§7).
- **No new targeting, X, modes, or counters.** Untouched from M3e.

## 1. New and grown types

**`Pawl.Type.ZoneChange`** (new) — the event value, the substrate both halves
read:

```haskell
data ZoneChange = MkZoneChange
  { object :: ObjectId,  -- the RESULTING object (post-move id, CR 400.7)
    from :: Zone,        -- carried for the future LTB pass; M3f reads `to`
    to :: Zone
  }
  deriving (Eq, Ord, Show)
```

`object` is the *resulting* object's id — the fresh incarnation `changeZone` mints
in the destination (CR 400.7). This is exactly what the enters-pass needs: for an
event with `to = Battlefield`, the scan looks up `object` on the battlefield and
checks *that* permanent's `SelfEnters` triggers (§3), with no battlefield diffing
or new-id threading. `from` is populated now (it is free — `changeZone` knows the
object's current zone) though only `to` is read in M3f; the LTB pass will need
`from` and a last-known-information snapshot of the object *before* the move (a
separate mechanism, deferred with that pass), so the substrate should not grow a
field later. Note the asymmetry the pipeline handles cleanly: replacements read the
*proposed* change (pre-move — the old id, the requested destination), while the
*emitted* event carries the resulting id and the resolved destination.

**`Pawl.Type.ReplacementEffect`** (new) — the third leaf family (design.md's M3g
note: "replacement specifications, classified by the event pattern they
intercept"), distinct from `Effect` (one-shot) and `Modification` (continuous):

```haskell
-- CR 614.1a. Classified by the event pattern it intercepts: a zone change whose
-- destination is `whenDestination` is rewritten to head for `toDestination`
-- instead. Rest in Peace = RedirectZoneChange Graveyard Exile (any object, from
-- any source zone).
data ReplacementEffect = RedirectZoneChange
  { whenDestination :: Zone,
    toDestination :: Zone
  }
  deriving (Eq, Ord, Show)
```

One inhabitant, but structured as a pattern (destination) + rewrite (destination),
not a Rest-in-Peace-specific tag — the closed half asks the classification, never
the identity. "From anywhere" and "a card or token" are the *absence* of a source
restriction and the *object-generality* of the predicate; both fall out of keying
on destination alone.

**`Pawl.Type.TriggeredAbility`** (new) — CR 603.1's "[condition], [effect]":

```haskell
data TriggeredAbility = MkTriggeredAbility
  { condition :: TriggerCondition,
    -- Reuses the Effect vocabulary, like ActivatedAbility. Rest in Peace:
    -- [ExileAllGraveyards].
    effects :: [Effect],
    -- The same slot/target machinery as a spell or activated ability. Rest in
    -- Peace's ETB targets nothing: empty.
    targetSpecs :: Map SlotName TargetSpec
  }
  deriving (Eq, Ord, Show)
```

**`Pawl.Type.TriggerCondition`** (new) — the event pattern that fires a trigger,
the trigger-side analog of `ReplacementEffect`'s pattern:

```haskell
-- CR 603.6a: "When this ... enters." Fires when the object bearing the ability
-- enters the battlefield. A general "whenever a [type] enters" (any permanent) is
-- a future condition; M3f has only the self-enters shape.
data TriggerCondition = SelfEnters
  deriving (Eq, Ord, Show)
```

**`Pawl.Type.Effect`** grows one targetless opcode:

```haskell
  | ExileAllGraveyards  -- NEW (Rest in Peace's ETB)
```

`ExileAllGraveyards` moves every card in every player's graveyard to exile,
through `changeZone` (so each move is itself an event — harmless here, since a
graveyard→exile move matches no M3f trigger or replacement). It targets nothing;
`slotsOf`, `manaProduced`, `rewriteEffect` gain the arm (`Set.empty` / `Nothing` /
identity — no land-type word, no mana, no slot).

**`Pawl.Type.Source`** grows the trigger incarnation, parallel to M3e's
`OfAbility`:

```haskell
data Source
  = OfCard Printing
  | OfAbility ObjectId ActivatedAbility  -- M3e
  | OfTrigger ObjectId TriggeredAbility  -- NEW: the source permanent + the ability
  deriving (Eq, Ord, Show)
```

`OfTrigger src ab` is the stack object for a triggered ability (CR 603.3: put on
the stack as an object that is not a card). `src` is the source permanent (CR
113.7 / 405.4), read by the executor as the effect source. The ability travels
with the object, so it resolves even if `src` has left (CR 603.3d). `Game.cardOf`
gains `OfTrigger _ _ -> Nothing` (a triggered ability is not a card) beside the
existing `OfAbility` arm. The stack constructor names *what kind of object* this
is — a classification, the same shape as the `OfCard`/`OfAbility` split — while
resolution is shared (§4).

**`Pawl.Type.Card`** grows two fields, empty for all but the gate:

```haskell
  -- CR 614: this card's replacement effects, active while it is on the
  -- battlefield. Read through the projection (Projection.replacementsOf), never
  -- directly, so layer 6 LoseAllAbilities strips them uniformly.
  replacementEffects :: [ReplacementEffect],
  -- CR 603: this card's triggered abilities. Read through
  -- Projection.triggeredAbilitiesOf, never directly, for the same reason.
  triggeredAbilities :: [TriggeredAbility]
```

**`Pawl.Type.GameState`** grows the trigger-detection log, mirroring
`damageEvents`:

```haskell
  -- CR 603 / 117.5: zone-change events emitted since the last time triggers were
  -- placed. changeZone appends the RESOLVED (post-replacement) event; the CR 117.5
  -- boundary scans and drains it. The zone-change analog of damageEvents.
  zoneChanges :: [ZoneChange]
```

**`Pawl.Card`** gains **Rest in Peace** (`{1}{W}`, Enchantment; one `SelfEnters`
triggered ability `[ExileAllGraveyards]` with no slots; one `RedirectZoneChange
Graveyard Exile` replacement) and a **Plains** basic-land printing (a white mana
source, zero-opcode via CR 305.6, mirroring M3d's Island) so Rest in Peace is cast
faithfully rather than hand-placed.

## 2. Phase 1 — the replacement funnel (CR 614)

`changeZone` becomes the single point where a zone change is *proposed*, *rewritten*
by replacements, *performed*, and *emitted* — the change-and-emit funnel for zone
changes, exactly as `Damage.applyDamage` is for damage:

```haskell
changeZone oid dest gs = case lookupObject oid gs of
  Nothing -> gs
  Just obj ->
    let proposed = ZoneChange oid (Object.zone obj) dest
        resolved = Event.applyReplacements (Projection.replacementsAffecting gs) proposed
        -- ... perform the move to (ZoneChange.to resolved), minting a fresh id
        --     and resetting per-incarnation state exactly as today (CR 400.7) ...
     in performed {GameState.zoneChanges = GameState.zoneChanges performed ++ [resolved]}
```

- **Gathering active replacements.** `Projection.replacementsAffecting :: GameState
  -> [ReplacementEffect]` collects the replacement effects of every battlefield
  permanent, read through `Projection.replacementsOf` (per-object, the `abilitiesOf`
  move) so a future LoseAllAbilities strips a permanent's replacements uniformly.
  Rest in Peace is an enchantment and Humility is `AllCreatures`, so the strip is
  not gate-tested — but the accessor is the consistent seam, and reading the card
  directly here would be the one place abilities are *not* projected.
- **Applying them (CR 614.5).** `Event.applyReplacements` folds the active
  replacements over the proposed event, each getting **one** opportunity to affect
  the event or its modifications (CR 614.5 — "doesn't invoke itself repeatedly").
  For Rest in Peace: a `RedirectZoneChange Graveyard Exile` matches a proposed move
  to `Graveyard` and rewrites `to := Exile`; the rewritten event heads for exile,
  which no longer matches, so the fold terminates with no self-loop. `Pawl.Event`
  is the **sole home of casing on `ReplacementEffect`** (the `Resolve`-over-`Effect`
  standing).
- **Emitting the resolved event (CR 603.2g / 614.6).** The event appended to
  `zoneChanges` is the *post-replacement* one: "if an event is replaced, it never
  happens; a modified event occurs instead, which may in turn trigger abilities"
  (CR 614.6). So a graveyard move redirected to exile can only ever fire an
  enters-exile trigger, never a to-graveyard one (CR 603.2g). Emit-after-rewrite is
  the structural guarantee.

Because `changeZone`'s signature is unchanged (`ObjectId -> Zone -> GameState ->
GameState`), every existing caller — SBA burial, discard, the `resolveSpell` bury,
`drawFor`, land play, `putTapped`, sacrifice — is untouched and now funnels through
replacement + emission for free. The SBA-death case is exactly why the seam is pure:
`Sba.checkStateBasedActions` calls `changeZone oid Graveyard` and the redirect must
happen there, in pure code.

**Phase 1 gate (Rest in Peace hand-placed on the battlefield, its ETB not yet
built):** assert the three redirects and their negative controls (§ exit
criterion).

## 3. Phase 2 — triggered abilities (CR 603) and the CR 117.5 loop

**Trigger detection.** A new function in `Pawl.Event`:

```haskell
-- The battlefield/enters pass of the three-pass scan (prior-art-lessons.md §81).
-- For each emitted zone change with `to = Battlefield`, look up its `object` (the
-- newcomer, CR 400.7/603.6a) and collect its SelfEnters triggered abilities.
-- Phase-step and LTB passes are stubs (§7).
triggersFrom :: [ZoneChange] -> GameState -> [PendingTrigger]
```

A `PendingTrigger` names the source permanent, its controller (CR 603.3a — the
controller when it triggered), and the `TriggeredAbility`. `Pawl.Event` is the
**sole home of casing on `TriggerCondition`**. Triggered abilities are read through
`Projection.triggeredAbilitiesOf` (the `abilitiesOf`/`keywordsOf` move), so a
Humility-stripped permanent contributes none.

**Placement — the CR 117.5 loop.** The engine's priority boundary becomes the rule
CR 117.5 states literally:

> Each time a player would get priority, the game first performs all applicable
> state-based actions as a single event, then repeats this until none are
> performed. Then triggered abilities are put on the stack. These steps repeat in
> order until no state-based actions are performed and no abilities trigger. Then
> the player receives priority.

Concretely, `Engine` gains a `settleForPriority :: Game ()` that replaces the bare
`checkSba` at the resolution boundary and (M3f) at the start of `priorityLoop`:

```
repeat:
  run checkSba to fixpoint
  pending <- Event.triggersFrom (drain zoneChanges) gs
  if null pending: stop
  else: put each pending trigger on the stack (APNAP order, CR 603.3b),
        choosing targets as placed (CR 603.3d, via ChooseTargets — none for RiP)
until neither SBAs nor triggers did anything
```

- **APNAP (CR 603.3b).** Placement is in active-player-then-non-active order.
  M3f has a single trigger controlled by one player, so the ordering is trivial;
  the two-part process (trigger-triggering abilities last) and the
  controller-orders-their-own choice are stubbed as expiries (§7). Targets are
  chosen as the ability is placed (CR 603.3d = the CR 601.2c–d process), reusing
  `ChooseTargets`; Rest in Peace's ETB has no slots, so nothing is prompted.
- **Draining `zoneChanges`.** The log is drained when triggers are scanned, so an
  event fires its triggers exactly once (CR 603.2c). Setup's zone changes (opening
  hands, libraries) sit in the log until the first boundary and match nothing
  (no battlefield permanent has a triggered ability then); the plan clears the log
  once at game start for cleanliness regardless.

**Resolution.** `Stack.resolveTop` gains an `OfTrigger` arm beside `OfAbility`,
dispatching on *what kind of stack object* it is (a classification, never card
identity):

```haskell
Source.OfTrigger src ab -> Resolve.resolveEffects oid src (TriggeredAbility.effects ab) (TriggeredAbility.targetSpecs ab)
```

**Phase 2 gate:** cast Rest in Peace with white mana; it resolves, enters, its ETB
triggers and is placed at the next boundary, resolves, and exiles all graveyards;
the ability object ceases. Then a creature dies and is exiled by the replacement —
the whole card on one event substrate.

## 4. The shared executor

`Resolve.resolveAbility` (M3e) is refactored to extract the CR 608.2 executor both
ability kinds share:

```haskell
-- The CR 608.2 effect executor for an ability on the stack (activated OR
-- triggered): re-validate filled slots (CR 608.2b), fold applyEffect over the
-- effects with the source permanent as the effect source (CR 608.2g / 113.7),
-- then the ability ceases (CR 608.2n). Game-monadic because applyEffect prompts
-- (Search, and any future resolution-time choice).
resolveEffects :: ObjectId -> ObjectId -> [Effect] -> Map SlotName TargetSpec -> Game ()
```

`resolveAbility abilId srcId ability = resolveEffects abilId srcId (effects ability)
(targetSpecs ability)`; the `OfTrigger` arm calls `resolveEffects` directly. The
only difference between the two kinds is *creation* — an activated ability is a
player action paying a cost (M3e's `Pawl.Activate`); a triggered ability is placed
by the engine on an event match with no cost — and *creation* is entirely
separate. On the stack they are one executor. `cease` (CR 608.2n) is unchanged and
shared.

## 5. Invariants preserved

- **The two-halves invariant holds through the event pipeline.** No closed-half
  module cases on a card's or an effect's *identity*. `Resolve` remains the sole
  home of `case effect of` (now including `ExileAllGraveyards`); the new
  `Pawl.Event` is the sole home of casing on `ReplacementEffect` and
  `TriggerCondition` — both open-half vocabularies classified by event pattern, the
  same standing `Resolve` has over `Effect` and `Projection` over `Modification`.
  `changeZone` and the CR 117.5 loop *ask* those classifications (does any
  replacement match this destination? does any condition match this event?); they
  never name a card. `Stack.resolveTop` dispatches on the `Source` classification
  (`OfCard`/`OfAbility`/`OfTrigger`), never identity.
- **The engine makes no choice it should not, and elides none it should.** Trigger
  targets are prompted as placed (none for Rest in Peace). The single active
  replacement needs no ordering choice (CR 616 is genuinely absent, not elided);
  the CR 616 ordering Prompt and the CR 603.3b own-order choice are named expiries,
  each due when a second replacement or a second simultaneous trigger first makes
  the choice real.
- **The atom pattern.** Every zone change now flows through one change-and-emit
  helper that performs the move, applies the replacement, and emits the resolved
  event — the substrate M3's ABI-owed event decision names. M3f has no same-zone or
  otherwise no-op move, so the no-op→no-event guard (the "atom" pattern's 603.2f
  concern) is stated but unexercised; the first no-op move is where it becomes
  load-bearing.
- **Conventions.** One type per module (`ZoneChange`, `ReplacementEffect`,
  `TriggeredAbility`, `TriggerCondition` each new); `NamedFieldPuns` per the M3b
  amendment; new sum-type constructors (`SelfEnters`, `OfTrigger`,
  `RedirectZoneChange`, `ExileAllGraveyards`) take no `Mk`-pun; `Mk`-prefixed
  record constructors for the new records.

## 6. Setup, decks, and testing

Testing follows the phased spine (§0), deterministic fixtures only (the white
gate's random-game coverage trails, per the M3a–M3e posture; the post-M3 coverage
tail owns it).

**Phase 1 — replacement at the funnel.** Rest in Peace hand-placed on the
battlefield (settled), then:

- **SBA death → exile.** A creature dealt lethal combat damage: after the SBA
  check it is in *exile*, not any graveyard; the graveyard is empty. Falsifier: a
  battlefield-only "when a creature dies" hook would also catch this, so this case
  alone is insufficient — it is paired with the two below. Cite CR 704.5g / 614.1a.
- **Resolving Lightning Bolt → exile.** Cast Lightning Bolt, let it resolve; assert
  it is in exile, not its owner's graveyard (CR 608.2n's move redirected). This is
  the case a dies-hook misses — the falsifier of the battlefield-only design. Cite
  CR 608.2n / 614.
- **Discard → exile.** A cleanup-step (or forced) discard: the card is exiled, not
  in the graveyard. Cite CR 514.2 / 614.
- **Negative controls.** Each of the three, with no Rest in Peace present, goes to
  the graveyard. And CR 614.5: assert the redirected object is in exile exactly
  once (no re-redirect).

**Phase 2 — the ETB trigger and the whole card.**

- **ETB fires.** Cast Rest in Peace (white mana from Plains); it resolves and
  enters. Assert: at the next boundary an `OfTrigger` object is on the stack; on
  resolution every pre-existing graveyard card is in exile and the trigger object
  has ceased (not in any graveyard; object count returns). Cite CR 603.6a /
  608.2n. Falsifier in a comment: never emitting the enters event, or scanning only
  the entering card's own text, leaves the graveyards untouched.
- **The whole card.** With Rest in Peace resolved and out, a creature dies and is
  exiled by the replacement (line 2) — both halves on one event. Cite CR 603 / 614.
- **Emit-after-rewrite (CR 603.2g).** Structural assertion that the `zoneChanges`
  entry for a redirected graveyard move records `to = Exile` (so it could only fire
  an enters-exile trigger), not `Graveyard`.

**Setup and decks.** `emptyGame` unchanged. A Plains printing joins `Pawl.Card`;
the white fixtures cast or place Rest in Peace and assign timestamps from
`freshTimestamp` as M3c–M3e do. Rest in Peace is a **deterministic fixture, out of
the random decks**. Lightning Bolt (M3a) supplies the stack-spell falsifier.

**Properties** (`runMatch`, both matchups): every M2d/M3a–M3e invariant as it
stands — conservation (now: an exiled object is conserved in exile, not lost),
termination, ids, no floating mana at end of step, life never increases (unchanged
— nothing gains life), combat happens, green-black engagement. Replay determinism
now covers the Rest in Peace cast, ETB placement, and the redirected zone changes.
The benchmark stays on `redDeck`; throughput is *watched* for the per-`changeZone`
replacement gather and the per-boundary trigger scan, not asserted (the M3e
posture).

## 7. What M3f preserves, and the expiries it opens

**Preserves:** the two invariants (§5), the numeric/mana/timestamp models, the
M3b–M3e projection shape and source-liveness, the deterministic-fixture posture,
M3e's activation and the shared executor it now feeds.

**Expiries this milestone opens:**

- **The monadic replacement path + CR 616 ordering — the headline expiry.** M3f's
  replacement seam is pure and assumes exactly one applicable replacement. The
  general case (multiple replacements racing on one event — CR 616.1's affected/
  controlling player chooses the order — and replacements that require a choice, CR
  614.12a) forces `changeZone` to split into a pure "what would happen" (for
  enumeration/LKI) and a monadic apply with a new ordering Prompt. **Tracked in
  git-bug so it cannot rot**; due at the first scenario with two applicable
  replacements or a choice-bearing replacement.
- **Tokens (CR 111).** The replacement predicate is object-general and will handle
  tokens for free, but the literal "token would go to a graveyard → exiled, then
  the token ceases as an SBA (CR 111.7)" branch waits on `Create` (CR 701.6, M4).
- **The unified event log.** `zoneChanges` and `damageEvents` are separate logs
  with separate readers. Folding them into one `events` pipeline (full effect ≡
  event) is due when a second event kind gains a replacement or trigger consumer —
  M4 burn (damage replacement/prevention, CR 615) and combat-damage triggers.
- **The other two trigger passes + trigger richness.** Only the battlefield/enters
  pass is populated. Leaves-the-battlefield / dies triggers (with LKI, CR
  603.6/603.10), "at the beginning of [step]" phase triggers (CR 603.2b), the CR
  603.3b two-part placement and own-order choice, the intervening-`if` clause (CR
  603.4), optional `may` triggers (CR 603.5), and delayed triggered abilities (CR
  603.7) each land with their first card.
- **`ExileAllGraveyards` generality.** The opcode is Rest in Peace's exact shape.
  Exile-from-a-named-zone, exile-a-target, exile-with-a-later-return, and other
  criteria generalize `Effect`/the exile family when a second exiling card
  disagrees with it.
- **Replacements/triggers under `LoseAllAbilities`.** M3f reads both through the
  projection (`replacementsOf` / `triggeredAbilitiesOf`) for uniformity, but Rest
  in Peace (an enchantment) is not stripped by Humility (`AllCreatures`), so the
  strip is unexercised until a creature carries a replacement or triggered ability.
- **Last-known information.** Deferred with the LTB pass; Rest in Peace's ETB reads
  the live battlefield.

**Explicitly deferred past M3f:**

- **M3g** — the payoff pair: Decider (CR 723, Mindslaver) and re-entrancy
  (Panglacial Wurm), both building on M3e activation and, for Mindslaver, on this
  letter's trigger controller (CR 603.3a).
- **A white matchup for random-game coverage** — the post-M3 tail.
- **X, modes, counterspells, Auras/Equipment (Attach), new card types,
  serialization / AST version field.**
