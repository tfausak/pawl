# Auras (CR 303.4): attachment, `enchant`, and control from a static ability

*Design pass 2026-07-25. The first **card-driven unit** rather than a numbered
milestone: no `docs/design.md` §3 entry, one `docs/progress.md` entry on
completion. Two phases, each its own `writing-plans` plan and its own session per
`docs/workflow.md`. Closes #33, #62 and #114. Every rules claim is checked against
`docs/rules.txt` and cited by number; both gate cards' text is checked against
Scryfall.*

## 0. Why this exists

Auras are the largest single hole in the card pool. Nothing in pawl can attach
one object to another: there is no `Object` field for it, no `Subtype.Aura`, no
`enchant`, and no way for a static ability to name "the enchanted permanent".
Four code sites already carry a comment saying so:

- `source/test-suite/Pawl/CombatSpec.hs:608` — a synthetic steal-without-haste
  fixture stands in for an Aura (#33).
- `source/test-suite/Pawl/ProjectionSpec.hs:455` — Opalescence's "each **other
  non-Aura** enchantment" qualifier is unenforced because `Aura` is not a
  modelled `Subtype` (#114).
- `source/test-suite/Pawl/PowerToughnessSpec.hs:120` — the Darksteel Mutation
  family is blocked on attachment.
- `source/library/Pawl/Engine.hs:166` — settling a permanent held under
  *indefinite* control, across the thief's own untap step, "is the Auras /
  Control Magic phase" (#62).

This unit is deliberately the **pool-unlocking primitive** taken first: the
invasive card and the pool-opening card are the same card.

## 1. The rules (verbatim ground truth)

> **303.2.** When an enchantment spell resolves, its controller puts it onto the
> battlefield under their control.

> **303.4.** Some enchantments have the subtype "Aura." An Aura enters the
> battlefield attached to an object or player. What an Aura can be attached to is
> defined by its enchant keyword ability (see rule 702.5, "Enchant"). Other
> effects can limit what a permanent can be enchanted by.

> **303.4a** An Aura spell requires a target, which is defined by its enchant
> ability.

> **303.4b** The object or player an Aura is attached to is called enchanted. The
> Aura is attached to, or "enchants," that object or player.

> **303.4c** If an Aura is enchanting an illegal object or player as defined by
> its enchant ability and other applicable effects, the object it was attached to
> no longer exists, or the player it was attached to has left the game, the Aura
> is put into its owner's graveyard. (This is a state-based action. See rule 704.)

> **303.4d** An Aura can't enchant itself. If this occurs somehow, the Aura is put
> into its owner's graveyard. An Aura that's also a creature can't enchant
> anything. If this occurs somehow, the Aura becomes unattached, then is put into
> its owner's graveyard. (These are state-based actions. See rule 704.) An Aura
> can't enchant more than one object or player. If a spell or ability would cause
> an Aura to become attached to more than one object or player, the Aura's
> controller chooses which object or player it becomes attached to.

> **303.4e** An Aura's controller is separate from the enchanted object's
> controller or the enchanted player; the two need not be the same. If an Aura
> enchants an object, changing control of the object doesn't change control of the
> Aura, and vice versa. Only the Aura's controller can activate its abilities.
> However, if the Aura grants an ability to the enchanted object (with "gains" or
> "has"), the enchanted object's controller is the only one who can activate that
> ability.

> **303.4f** If an Aura is entering the battlefield under a player's control by
> any means other than by resolving as an Aura spell, and the effect putting it
> onto the battlefield doesn't specify the object or player the Aura will enchant,
> that player chooses what it will enchant as the Aura enters the battlefield. […]

> **303.4g** If an Aura is entering the battlefield and there is no legal object
> or player for it to enchant, the Aura remains in its current zone, unless that
> zone is the stack. In that case, the Aura is put into its owner's graveyard
> instead of entering the battlefield. If the Aura is a token, it isn't created.

> **303.4m** An ability of a permanent that refers to the "enchanted [object or
> player]" refers to whatever object or player that permanent is attached to, even
> if the permanent with the ability isn't an Aura.

> **702.5a** Enchant is a static ability, written "Enchant [object or player]."
> The enchant ability restricts what an Aura spell can target and what an Aura can
> enchant.

> **702.5c** If an Aura has multiple instances of enchant, all of them apply. […]

> **702.5d** Auras that can enchant a player can target and be attached to
> players. Such Auras can't target permanents and can't be attached to permanents.

> **704.5m** If an Aura is attached to an illegal object or player, or is not
> attached to an object or player, that Aura is put into its owner's graveyard.

> **613.1b** Layer 2: Control-changing effects are applied.

> **613.7a** A continuous effect generated by a static ability has the same
> timestamp as the object the static ability is on, or the timestamp of the effect
> that created the ability, whichever is later. […]

> **608.2b** If the spell or ability specifies targets, it checks whether the
> targets are still legal. […] If all its targets, for every instance of the word
> "target," are now illegal, the spell or ability doesn't resolve. It's removed
> from the stack and, if it's a spell, put into its owner's graveyard. […]

> **800.4a** When a player leaves the game, all objects (see rule 109) owned by
> that player leave the game and any effects which give that player control of any
> objects or players end. […]

## 2. Scope

**In.** `Subtype.Aura`; attachment as base object state; `Card.enchant`; an Aura
spell targeting and fizzling per CR 608.2b; entering the battlefield attached per
CR 303.4/303.4b; the CR 704.5m state-based action; an `Affected` constructor for
the enchanted permanent (CR 303.4m); a layer-2 control modification whose player
is derived rather than baked (CR 303.4e/613.1b); the CR 800.4a interaction. Two
gate cards.

**Out, each with an issue filed** (§9). Chiefly: the CR 701.3 `Attach` keyword
action and CR 303.4j (moving an Aura already on the battlefield); CR
303.4f/303.4g/303.4i (entering by any means other than resolving as an Aura
spell); CR 702.5c (multiple `enchant` instances); CR 702.5d ("enchant player");
CR 303.4k (face-down); CR 704.5n/704.5p (Equipment, Fortification, attached
creatures and battles).

**Gate cards** (Scryfall-verified 2026-07-25):

| Card | Cost | Type line | Oracle text |
| --- | --- | --- | --- |
| Unholy Strength | `{B}` | Enchantment — Aura | Enchant creature<br>Enchanted creature gets +2/+1. |
| Control Magic | `{2}{U}{U}` | Enchantment — Aura | Enchant creature<br>You control enchanted creature. |

Unholy Strength is the **control case**: an ordinary Aura riding the existing
`Modification.ModifyPowerToughness`, proving the substrate is not shaped only for
the exotic card. Control Magic is the **falsifier**: layer-2, derived controller,
indefinite duration, crossing turns.

## 3. Architecture

### 3.1 Attachment is base state, not a projection

`Pawl.Type.Object` gains one field:

```haskell
-- CR 303.4b: the object this permanent is attached to ("enchanted"). Base state,
-- NOT projected: attachment is a fact about the object, and no CR 613 layer
-- reads or writes it. Nothing for every permanent that is not an attached Aura.
attachedTo :: Maybe ObjectId
```

`Maybe ObjectId`, not a `Maybe Recipient`: CR 702.5d's enchant-player Auras
cannot be expressed by this field, which is a real modelling limit rather than a
missing producer, and §9 files it as such.

The inverse direction — "what is attached to me" — is deliberately **not** a
field. It is derived by scanning the battlefield, the same posture
`Projection.controls` takes toward control. One direction of truth, no
consistency invariant to maintain across zone changes.

### 3.2 The carrier: `Card.enchant :: Maybe TargetSpec`

CR 702.5a makes enchant a static ability with a payload. `Keyword` is a
payload-free closed-half enumeration, so enchant cannot be a `Keyword`.
`TargetSpec` rather than `Filter`, because CR 702.5d needs the `Pool` axis and
`TargetSpec` already *is* `{pool, filter}` — the enchant-player deferral then
costs a field widening rather than a type change.

Both gate cards carry `Just (MkTargetSpec Pool.Creatures Nothing)`.

**Codec.** `characteristicPT` is the exact precedent (`Pawl.Codec` ~1622 on
encode, ~1692 on decode): `getOpt` + `maybeFrom` on the way in, omitted from the
object when `Nothing` on the way out. **No existing `data/cards/*.json` changes.**

### 3.3 One seam carries both targeting and re-validation

`Card.allTargetSpecs` (`Pawl.Card:34`) and `Card.modesTargetSpecs`
(`Pawl.Card:50`) merge the enchant spec in under a well-known slot name,
`Pawl.Card.enchantSlot = MkSlotName "enchant"`. It lives in `Pawl.Card` rather
than beside `Pawl.Binding`'s well-known names because those are *reserved* —
they exist precisely because they are not targets — and this one is. A lint
forbids any card's own `targetSpecs` from declaring that name, so the merge can
never shadow a declared slot. `Cast.hs:188` builds the cast-time prompt from
`Card.modesTargetSpecs`, and `Resolve.hs:335` re-validates from the same
function, so CR 303.4a's target and CR 608.2b's re-validation both fall out of
the merge.

The enchant slot is a genuine target (CR 303.4a says so), so it lives in the
ordinary target namespace — it is *not* a reserved slot in `Pawl.Binding`'s sense
(`variableX`, `chosenModes`, `copySource`, `triggerSource`, `you`), all of which
exist precisely because they are *not* targets.

**But the seam is not single, and that is the trap this section exists to
name.** Two consumers reach *past* `Card.allTargetSpecs`/`Card.modesTargetSpecs`
and read `Mode.targetSpecs` straight off each mode. Merging in `Pawl.Card` is
invisible to both:

1. **`Target.fillableModes` (`Target.hs:119`) — a correctness bug if missed.**
   Castability is `Set.size (Target.fillableModes …) >= count` (`Cast.hs:70`).
   Reading only `Mode.targetSpecs`, it would judge an Aura with *no legal
   creature on the battlefield* to be castable, and the spell would then be
   countered on resolution — when CR 601.2c means it could never have been cast
   at all. `fillableModes` therefore takes a new
   `Map SlotName TargetSpec` parameter of slots every mode carries in addition
   to its own; `Cast` passes the card's enchant slot, and the three ability
   callers (`Activate.hs:65`, `Activate.hs:101`, `Engine.hs:416`) pass
   `Map.empty`. An ability has no enchant spec, and the empty map makes that a
   fact of the call rather than a special case in the body.

2. **The D4 dataflow lint (`CardSpec.hs:376-398`) — a coverage gap, not a
   break.** `modeOffends` reads `Mode.targetSpecs m` and `cardOffends` walks
   `Modal.modes (Card.spell card)`, so the lint never calls
   `Card.allTargetSpecs` and the enchant slot is simply outside its reach. The
   equality (*every slot an effect reads is declared, and every declared slot is
   read*) keeps holding untouched — an Aura's empty spell mode declares nothing
   and reads nothing.

   So nothing must be *weakened*; something must be *added*. Two positive
   assertions take the enchant slot's place: **every card with `Subtype.Aura`
   declares an `enchant` spec, and every card declaring one is an Aura** (CR
   303.4 / 702.5a), and **no mode declares a slot named `enchant`**, which is
   what makes the merge in §3.3 collision-free. This is the same lint-coverage
   shape #184 records for `Card.mulliganAction`: a new `Card` field lands
   outside the lint family unless it is deliberately walked in.

### 3.4 Resolution: `Stack`'s third branch, and the first fizzling permanent spell

`Pawl.Stack:49` currently reads:

```haskell
if Card.isPermanent (Printing.card printing)
  then Event.changeZone oid Zone.Battlefield
  else Resolve.resolveSpellWith runSubgame oid
```

A permanent spell goes to the battlefield with **no target check at all** —
correct until now, because no permanent spell could target. An Aura spell is the
first one that can, and the first that can be countered on resolution.

The dispatch grows a third branch, on `Card.isAura` — a **subtype read**, the
same closed-half classification as the `Card.isPermanent` beside it (CR 205.3;
CLAUDE.md's keyword note applies verbatim — the invariant forbids casing on an
*effect's identity*, and a subtype is not an effect). It is worth saying out loud
because it looks like a violation at a glance:

1. Re-validate the enchant slot against the current state (CR 608.2b). The fizzle
   test is **lifted out of `Resolve.resolveSpellWith:337-348` into a shared
   helper** so target legality has exactly one implementation, rather than a
   second copy that can drift.
2. If it fizzles: `Event.changeZone oid Zone.Graveyard` — CR 608.2b's own
   sentence, and the same call `resolveSpellWith:350` already makes.
3. Otherwise the Aura **enters already attached**, which CR 303.4 states as a
   property of entering ("An Aura *enters the battlefield attached* to an object
   or player") rather than as a step after it. `Event.changeZoneReturning`
   (`Event.hs:127`) builds the new incarnation with
   `mkObj ts = obj {…}` and then, *before returning*, runs the CR 614.1c entry
   replacement loop and records the `Moved` event. Attaching after it returns
   would make the Aura unattached during both. So `changeZoneReturning` gains a
   seed parameter — `changeZoneAttaching :: ObjectId -> Zone -> Maybe ObjectId ->
   Game (Maybe ObjectId)`, with `changeZoneReturning oid z = changeZoneAttaching
   oid z Nothing` — and the seed is written into `mkObj`. Every existing call
   site is untouched.

   No card in this pool can observe the difference (no entry replacement or ETB
   trigger reads attachment), so this buys ordering correctness rather than a
   passing test. It is cheap enough to be worth taking now instead of leaving a
   latent ordering bug for the first card that does look.

   The bound value is a `Recipient`, so the `ObjectId` is taken from its
   `ToCreature`/`ToObject` tag; a `ToPlayer` cannot arise while `Card.enchant` is
   restricted to object pools, and is rejected rather than guessed at — CR 702.5d
   is deferral 4 in §9.

   `mkObj` must also **reset `attachedTo` to the seed on every other zone
   change** (CR 400.7: the new object has no memory), which is the same reset
   `damage`, `sickness`, `bindings` and `counters` already get on that line.

### 3.5 `Affected.Attached`

```haskell
| -- CR 303.4m: the object this ability's SOURCE is attached to -- "enchanted
  -- creature". Derived from the source's own Object.attachedTo, so it is
  -- dynamic like Matching but is not a predicate over candidates: the set is
  -- {o} when the source is attached to o, and EMPTY when it is unattached
  -- (an Aura in the graveyard, or one the CR 704.5m sweep has not reached).
  -- Carries no payload -- CR 303.4m defines it for any permanent, not just an
  -- Aura, so there is nothing to parameterize.
  Attached
```

This is a third kind of `Affected`, and the module comment should say so:
`TheseObjects` is a fixed set locked at resolution (CR 611.2c), `Matching` is a
predicate re-derived per candidate, and `Attached` is re-derived from the
*source's* state. Unholy Strength is
`MkStaticAbility Attached (ModifyPowerToughness (Literal 2) (Literal 1))`.

`Pawl.Codec` gains the nullary tag pair (`affectedToJson:1016`,
`jsonToAffected:1023`).

### 3.6 CR 704.5m, and the two-pass fall-off

`Sba.performStateBasedActions` (`Sba.hs:114`) gains a fourth classification
alongside CR 704.5f/g/q, computed against the **same pre-pass state** — SBAs are
simultaneous, which the existing pass already honours by projecting once
(`Sba.hs:120`) and judging every object against that projection.

An Aura on the battlefield is put into its owner's graveyard when it is attached
to nothing, or to an id no longer on the battlefield, or to an object its own
enchant spec no longer admits. The third clause reuses `Target.stillLegal`
(`Target.hs:98`) so the legality question has one implementation shared with cast
and resolution. It is a **plain put-into-graveyard**, not a destruction: CR
704.5m says "put into its owner's graveyard", so it goes through
`Event.changeZone`, never `Event.destroy`, and no regeneration shield or
indestructible check applies.

**The two-pass consequence, stated so the tests assert it.** When the enchanted
creature dies to CR 704.5f/g in pass N, the Aura's illegality was judged against
the pre-pass state in which that creature was still there — so the Aura survives
pass N and falls off in pass N+1. That is correct (CR 704.3 repeats until no SBA
is performed), and it means `acted` must report the Aura's departure so the loop
runs again.

**A stale comment to correct.** `Sba.hs:98-100` says "One pass is enough in M1b:
a creature dying cannot cause another SBA". That is false the moment an Aura is
attached to it. `checkStateBasedActions` is `Monad.void performStateBasedActions`
— a single pass — with the CR 704.4 repeat living in `Engine`'s CR 117.5 settle
loop. The plan verifies every path that can bury a creature reaches that loop,
and fixes the comment.

CR 303.4d's first clause (an Aura can't enchant itself) is enforced here too: the
self-attachment is unreachable in this pool because a `Pool.Creatures` enchant
spec cannot name the Aura spell on the stack, but the SBA is written anyway, at
the cost of one predicate. Its second clause (an Aura that's also a creature
can't enchant anything) is live the moment Opalescence animates an Aura — which
is exactly what §3.7 makes impossible for Opalescence itself, and possible in
general.

### 3.7 `Subtype.Aura` closes #114 as a side effect

Adding the constructor retires an unenforceable qualifier. Opalescence reads
"each other **non-Aura** enchantment"; `ProjectionSpec.hs:455` records that the
qualifier is unenforced because `Aura` is not a modelled `Subtype`. It now
becomes `Filter.Not (Filter.HasSubtype Subtype.Aura)` inside Opalescence's
existing `Affected.Matching` filter, and the comment dies in the same commit.

Per the `Subtype` edit-site set, this touches four places: the constructor
(`Pawl.Type.Subtype`, cited `-- CR 205.3h`), `Codec.subtypeToJson`,
`Codec.jsonToSubtype`, and **`Pawl.Mana.subtypeMana`**, whose `case` is fully
exhaustive with no wildcard and needs an explicit `-> Nothing` arm. Card JSON
stores subtypes in `Ord`-canonical (declaration) order.

### 3.8 Layer-2 control from a static ability

`Modification.SetController PlayerId` (`Modification.hs:27-31`) bakes its player
at effect creation — its comment says so explicitly, and `Resolve.applyEffect`'s
`GainControl` arm is its only construction site. Card data cannot name a
`PlayerId`, so Control Magic needs a payload-free sibling:

```haskell
| -- layer 2, CR 613.1b: this object's controller becomes the controller of THIS
  -- effect's SOURCE. Payload-free because the player is DERIVED at projection
  -- time, not baked (contrast SetController, whose PlayerId is fixed at
  -- resolution by CR 611.2c). The static-ability half of control-changing:
  -- Control Magic's "You control enchanted creature."
  --
  -- CR 303.4e: the Aura's controller and the enchanted object's controller are
  -- separate. Deriving from the SOURCE's controller is what keeps them so --
  -- gaining control of the creature does not gain control of the Aura, and
  -- gaining control of the Aura does move the creature.
  SetControllerToSource
```

`Projection.layer` (`Projection.hs:62`) maps it to `Layer.Control` beside
`SetController`.

**Where it applies is the structural change.** `Projection.controllerOf`
(`Projection.hs:860`) is a lean fold over **stored** `GameState.continuousEffects`
that matches only `Affected.TheseObjects`. A static ability is never stored — it
is re-derived each projection from `Card.staticAbilities`. So `controllerOf` must
additionally gather control-granting static abilities from battlefield
permanents, and merge them into the same last-timestamp-wins ordering. CR 613.7a
supplies the timestamp: a static ability's continuous effect has the timestamp of
the object it is on, which is `Object.timestamp`. Both sides are already
`Timestamp`, so the merge is a single `maximumBy`.

`Projection.gather` already has the machinery for the static-ability half
(`Projection.hs:520-530` gathers `(permId, affected)` pairs from every
battlefield permanent, filtered by modification), and `Affected.Attached`
resolves to the enchanted object. The `staticAbilitiesLive` gate (§3.9) applies:
an Aura stripped of its abilities grants no control.

**Correction (landed with #196).** That last sentence is true only of the gate it
names — CR 305.7's land-subtype stripping. It does *not* generalise to layer 6:
control-changing effects are applied in layer 2 (CR 613.1b) and ability-removing
effects in layer 6 (CR 613.1f), so a Humility'd control grant has already been
made when the strip lands, CR 613.8a scopes dependency to effects "applied in the
same layer", and CR 613.6 keeps an effect applying "even if the ability generating
the effect is removed during this process". #196 was filed reading this sentence
the broad way and closed as not-a-bug; the proving tests are ProjectionSpec's
"CR 613.1b before CR 613.1f" group.

### 3.9 The recursion escape (#37's shape)

`controllerOf` consulting static abilities means asking for the Aura's own
controller, which re-enters `controllerOf`. It terminates in this pool — an Aura
enchants a creature, and no creature grants control — but it is a real cycle in
general, and `Projection` already has the answer.

`Projection.liveGiven` (`Projection.hs:547-554`) threads a `Set ObjectId` of
sources already under question; re-entering one short-circuits to the permissive
default, escaping the cycle. Its comment names it "the CR 613.8b loop-escape
analog, not an implementation of it (#37)".

`controllerOf` takes the same shape with `Object.owner` as the escape value: a
cycle means no static ability wins and the object stays with its owner. Like
#37's, the result is order-independent, and like #37's it is an **analog** of CR
613.8b rather than an implementation — so it carries the same comment, cites #37,
and joins that issue's expiry rather than opening a competing one.

### 3.10 Performance: hoist, or reintroduce the O(n³) blowup

`liveGiven`'s neighbours record the lesson (`Projection.hs:542-546`):
recomputing the stripper list inside the recursion "made `project`
O(permanents^3) per state-based-action sweep", so it is hoisted and computed once
per projection.

`controllerOf` is hotter than `project`. Combat, priority, mana and
`Projection.controls` (`Projection.hs:879`) all call it, and `controls` calls it
**per battlefield object**, inside a filter that the SBA sweep runs on every
pass. Gathering every permanent's static abilities inside `controllerOf` would
reintroduce exactly that blowup one level down.

So phase (b) hoists the control-granting static abilities the same way
`setLandSubtypeEffects` is hoisted: computed once, threaded into the fold. The
plan carries a benchmark check against `source/benchmark/Main.hs` rather than
taking this on faith.

### 3.11 Departure (CR 800.4a)

`Pawl.Departure` implements CR 800.4a's clause 3 ("any effects which give that
player control of any objects or players end") by scanning **stored** effects
through `Projection.givesControlTo` (`Projection.hs:888`). A static-ability
control source is not in that scan.

Two comments become false the moment `SetControllerToSource` exists, and are
corrected in phase (b): `Departure.hs:222` ("OWNER overridden by a layer-2
`SetController` and nothing else") and `Departure.hs:261` ("a flat sum with
exactly one construction site for `SetController`").

**The expected finding, which the plan proves rather than assumes:** CR 800.4a's
**clause 1 subsumes clause 3** for Aura control in every case this pool can
produce. A player who leaves owns the Control Magic they cast, so the Aura leaves
the game with them, its static ability goes with it, and the creature reverts
with no clause-3 work at all. A test pins this. If it does not hold — if a
reachable state exists where a departing player controls an Aura they do not own
— `givesControlTo`'s classification is widened to cover the static-ability path,
keeping the case on `Modification` inside `Pawl.Projection` where the invariant
requires it.

## 4. The two phases

**Phase (a) — the attachment substrate.** §3.1–§3.7. Gate card: **Unholy
Strength**. Closes #114. Exit: an Aura can be cast, targets legally, fizzles when
its target leaves, enters attached, modifies the enchanted creature, and falls
off to the graveyard by CR 704.5m.

**Phase (b) — control from a static ability.** §3.8–§3.11. Gate card: **Control
Magic**. Closes #33 and #62. Exit: control derived from a static ability survives
across turns, `CombatSpec`'s synthetic fixture is gone, and CR 800.4a is proven.

Phase (b) depends on (a) entirely. Each phase is one plan and one session per
`docs/workflow.md`; neither is committed until `cabal build` is warning-clean and
`hooky run` passes.

## 5. Card data

Two `data/cards/*.json` files, `unholy-strength.json` and `control-magic.json`,
plus four append-order edits each in `source/test-suite/Pawl/Cards.hs`: a
`<name>Printing` field in `MkCards`, a `loadPrinting "<slug>"` line, the `pure
MkCards {…}` assignment, and an `allPrintings` entry — the last giving automatic
whole-pool codec round-trip coverage. Neither gate card needs to join a deck
bundle.

Both are `Enchantment — Aura` with an empty single spell mode, an `enchant` spec,
and one static ability.

## 6. Testing

Gameplay-level, per `design.md` §4's opcode definition-of-done: an effect is not
done until a card exercises it in a gameplay-level test.

**Phase (a)** — `Pawl.ProjectionSpec` and a new `Pawl.AuraSpec`:

- Cast Unholy Strength on a creature; the creature projects +2/+1 and the Aura's
  `attachedTo` names it.
- The creature dies; the Aura reaches the graveyard by CR 704.5m — asserting the
  **two-pass** fall-off of §3.6, not merely the end state.
- The target is removed in response; the Aura spell is countered on resolution
  and goes to its owner's graveyard (CR 608.2b) — the first permanent spell in
  the pool that can fizzle.
- An Aura with no legal target cannot be cast at all (`Target.fillableModes`).
- Opalescence does not animate an Aura — its own text's "each other **non-Aura**
  enchantment", card text rather than a rule — retiring `ProjectionSpec.hs:455`'s
  comment. Note that `ProjectionSpec.hs:416` attributes the neighbouring "each
  other" to *CR 305.2*, which is the one-land-per-turn rule; that citation is
  wrong and is corrected while the file is open.

**Phase (b)** — `Pawl.CombatSpec`, `Pawl.ProjectionSpec`, `Pawl.DepartureSpec`:

- Control Magic takes a creature; `Projection.controllerOf` reports the Aura's
  controller, and the creature appears in their `Projection.controls`.
- The stolen creature attacks **across the thief's own untap step** — the
  cross-turn indefinite-control settle #62 names, and the reason Act of Treason's
  until-end-of-turn effect could never test it.
- `CombatSpec.hs:608`'s synthetic steal-without-haste fixture is deleted and its
  assertions re-expressed against Control Magic (#33).
- Destroying the Aura reverts control on the next projection.
- Gaining control of the enchanted creature by other means does not move the Aura
  (CR 303.4e).
- A player leaving the game while their Control Magic holds an opponent's
  creature: the creature reverts (CR 800.4a), by clause 1 per §3.11.

## 7. Blast radius

| Module | Change |
| --- | --- |
| `Pawl.Type.Object` | `attachedTo` field |
| `Pawl.Type.Card` | `enchant` field |
| `Pawl.Type.Subtype` | `Aura` constructor |
| `Pawl.Type.Affected` | `Attached` constructor |
| `Pawl.Type.Modification` | `SetControllerToSource` constructor |
| `Pawl.Card` | `isAura`, `enchantSlot`, `enchantSpecs`; `allTargetSpecs`/`modesTargetSpecs` merge the enchant slot |
| `Pawl.Target` | `fillableModes` takes the extra-slots map (§3.3) |
| `Pawl.Activate`, `Pawl.Engine` | three `fillableModes` callers pass `Map.empty` |
| `Pawl.Event` | `changeZoneAttaching`; `attachedTo` reset in `mkObj` |
| `Pawl.Stack` | the Aura resolution branch |
| `Pawl.Resolve` | fizzle test extracted to a shared helper |
| `Pawl.Sba` | CR 704.5m; the stale one-pass comment |
| `Pawl.Projection` | `layer`; `applyModification`; `controllerOf` gathers statics, hoisted, visited-set escape |
| `Pawl.Departure` | two false comments; `givesControlTo` if §3.11's finding does not hold |
| `Pawl.Mana` | the exhaustive `subtypeMana` arm |
| `Pawl.Codec` | subtype pair, `Affected.Attached` pair, `enchant` field |
| `source/test-suite/Pawl/CardSpec.hs` | the D4 lint's Aura arm |
| `source/test-suite/Pawl/Cards.hs` | two printings |

Every `case` on `Modification` in `Pawl.Projection` is exhaustive with no
wildcard (`Projection.hs:54-64, 504, 518, 582, 890`), so the compiler enumerates
the new constructor's sites. Likewise `Affected` in `Projection` and `Codec`.

## 8. Definition of done

1. `cabal build all --enable-tests --enable-benchmarks` warning-clean after a
   `cabal clean`.
2. `hooky fix` applied, `hooky run` passes; HLint clean.
3. Both gate cards play end to end in gameplay-level tests.
4. #33, #62 and #114 closed, each with its code-site comment removed **in the
   same commit** that closes it.
5. Every deferral in §9 filed with its rationale and expiry trigger, and cited at
   its code site as `(#N)` with no expiry written into the comment.
6. One `docs/progress.md` entry; `CLAUDE.md`'s status bullet **replaced**, not
   appended.

## 9. Deferrals to file

Card-driven unless noted.

1. **CR 701.3 `Attach` / CR 303.4j** — no opcode moves an Aura already on the
   battlefield. Fires on the first card that reattaches one.
2. **CR 303.4f / 303.4g / 303.4i** — an Aura entering the battlefield by any
   means other than resolving as an Aura spell, including the controller's choice
   of what to enchant and the stays-in-its-zone rule when no legal object exists.
   Unreachable while resolution is the only door. *Landed since:* Replenish is a
   second door, so `Event.changeZoneAttaching` now asks CR 303.4f's host choice and
   answers `Nothing` for CR 303.4g's "remains in its current zone". CR 303.4g's
   stack and token branches and CR 303.4i still have no producer (#1734).
3. **CR 702.5c** — multiple `enchant` instances. `Card.enchant` is a `Maybe`, so
   the pool cannot express a second one.
4. **CR 702.5d "enchant player"** — a *modelling* limit, not a missing producer.
   `Object.attachedTo :: Maybe ObjectId` cannot name a player; an enchant-player
   Aura needs the field widened to a player-or-object reference, and CR 704.5m's
   "the player it was attached to has left the game" clause (CR 303.4c) has
   nowhere to be checked. The issue says this explicitly so it does not read as
   "no card wants it yet".
5. **CR 303.4d's chooser** — "if a spell or ability would cause an Aura to become
   attached to more than one object or player, the Aura's controller chooses".
   No effect attaches, so there is nothing to choose between. *Landed since:*
   `Effect.AttachTargetToEach` names a whole destination set and
   `Pawl.Engine.Attach.arbitrate` reduces it, asking the SUBJECT’s controller
   rather than the resolving controller — CR 301.5c’s Equipment sentence at the
   same time. Synthetic Aura Diffusion is the producer, synthetic because the
   Scryfall sweep `Pawl.AuraSpec`’s Arbitration group records found no printing
   that names more than one destination; see #191.
6. **CR 303.4k** — face-down permanents do not exist.
7. **CR 704.5n / 704.5p** — Equipment, Fortification, and attached
   creatures/battles becoming unattached. Out of scope by this unit's scope
   decision; fires with the first Equipment.
8. **CR 613.8b dependency and layer-2 control** — an Aura's control effect
   participating in applies-to dependency ordering. Joins the existing #11 rather
   than opening a competing issue.
9. **The `controllerOf` cycle escape** — joins **#37**, whose visited-set escape
   this reuses verbatim; no new issue.
