# M4.5 P8 — Cost generalization and alternative costs

*Design pass 2026-07-22. The ninth phase of the M4.5 umbrella
(`docs/superpowers/specs/2026-07-20-m4.5-closed-half-gaps-design.md`), closing
the *payment* half of **GAP-Co** — the half P7 explicitly left open. Gates:
**Greed**, **Village Rites** and **Fireblast**. This spec is implementable; a
`writing-plans` plan follows it.*

*CR citations below **were checked against `docs/rules.txt` during this design
pass**: 115.1, 118.1, 118.3, 118.3a, 118.3b, 118.3c, 118.5, 118.5a, 118.6,
118.6a, 118.7, 118.7a, 118.8, 118.8a, 118.8b, 118.8c, 118.9, 118.9a, 118.9b,
118.9c, 118.9d, 118.10, 118.12, 118.13, 118.13a, 119.4, 119.4b, 202.1, 302.6,
601.2, 601.2a, 601.2b, 601.2c, 601.2f, 601.2g, 601.2h, 601.2i, 601.3, 602.1,
602.1a, 605.1a, 701.21, 701.21a, 702.34a, 704.5a. Any number added later and
marked **(verify)** must be checked before it drives code (CLAUDE.md: never
trust recalled Magic rules).*

*Card text and Gatherer rulings for all three gate cards **were verified live
against the Scryfall API during this design pass**, not read from the vendored
MTGJSON dump (`card-data-source`). The oracle text and the one ruling quoted in
§5 are what Scryfall returned on 2026-07-22.*

## 0. Why this phase, and what it proves

CR 118.1 says a cost is "an action or payment necessary to take another action",
and CR 601.2f lists what those actions can be: "paying mana, tapping permanents,
sacrificing permanents, discarding cards, and so on." Pawl can express two of
them, both nullary and both self-referential:

```haskell
-- Pawl.Type.AdditionalCost
data AdditionalCost = TapSelf | SacrificeSelf

-- Pawl.Type.AbilityCost
data AbilityCost = MkAbilityCost { mana :: Maybe ManaCost, additional :: [AdditionalCost] }
```

and a **spell** cannot express even those: a card's whole cost is
`Card.manaCost :: Maybe ManaCost`, which `Cast.costOf` returns and
`Cast.castable` measures affordability against. Three consequences, each a
structural gap rather than missing vocabulary:

1. **`AdditionalCost` grows a constructor per card.** "Pay 2 life", "sacrifice
   two Mountains" and "sacrifice a creature" are not three more nullary
   constructors; they are one *parameterized* one, twice over — an amount, and a
   criterion the paid-with objects must match.
2. **Additional costs cannot reach a spell at all** (CR 118.8), so the
   castability gate cannot see them.
3. **Alternative costs (CR 118.9) have nowhere to live**, and — the falsifier
   that shapes this whole phase — an alternative cost is *not* a different mana
   cost. Fireblast's is entirely non-mana.

P7 closed the *modification* half of GAP-Co: `Pawl.Cost.total` applies CR
601.2f's increases and reductions to a cost. This phase closes the *payment*
half, and in doing so makes `Pawl.Cost` a module about costs rather than a
module about one arithmetic step.

### Gate cards

| Card | Text (Scryfall, 2026-07-22) | What it is here for |
|---|---|---|
| **Greed** | `{3}{B}` Enchantment — "`{B}`, Pay 2 life: Draw a card." | a cost component carrying an **amount**, payable from a life total (CR 119.4), alongside a mana part |
| **Village Rites** | `{B}` Instant — "As an additional cost to cast this spell, sacrifice a creature. Draw two cards." | the **spell-side** additional cost (CR 118.8), and a mandatory one that gates castability |
| **Fireblast** | `{4}{R}{R}` Instant — "You may sacrifice two Mountains rather than pay this spell's mana cost. Fireblast deals 4 damage to any target." | the **alternative-cost seam** (CR 118.9), with an alternative that contains no mana at all |

### The falsifiers, stated up front

- **Greed at 1 life.** CR 118.3 — "A player can't pay a cost without having the
  necessary resources to pay it fully. For example, a player with only 1 life
  can't pay a cost of 2 life" — is the rules' own worked example of this phase's
  gap. A nullary `AdditionalCost` cannot even state the amount, let alone check
  it. And at 2 life the payment is legal and *loses the game* (CR 704.5a), which
  is what proves paying life is a real life-total change and not a bookkeeping
  side effect.
- **Village Rites with no creature.** Its one ruling: *"You must sacrifice
  exactly one creature to cast this spell; you can't cast it without sacrificing
  a creature, and you can't sacrifice additional creatures."* An implementation
  that treats additional costs as something paid *after* announcement will
  happily offer the cast. CR 601.2f puts the additional cost inside the total
  cost, and CR 601.2 makes an unpayable total illegal to announce.
- **Fireblast with two tapped Mountains and an empty mana pool.** Its printed
  `{4}{R}{R}` is unaffordable, yet the spell is castable and deals 4. This kills
  two designs at once: "castability is mana affordability", and "an alternative
  cost is a different `ManaCost`".
- **Fireblast under Thalia.** CR 118.9d: "If an alternative cost is being paid to
  cast a spell, any additional costs, cost increases, and cost reductions that
  affect that spell are applied to that alternative cost." So the alternative's
  *absent* mana component must be a real, taxable `{0}` that Thalia raises to
  `{1}` — which is precisely the distinction §2.3 is about.
- **Ancestral Vision is not Ornithopter.** The trap this spec fell into once
  already, and the reason `Maybe` survives on the new type: a card with **no**
  mana cost has an *unpayable* cost (CR 118.6), while a card with a mana cost of
  `{0}` is castable (CR 118.5a). Scryfall spells the difference exactly —
  Ancestral Vision's `mana_cost` is `''`; Ornithopter's is `'{0}'`.

## 1. Scope

**In scope.** One unified `Cost` type serving spells and activated abilities; a
parameterized `CostComponent` vocabulary with four inhabitants; the CR 118.6
unpayable/`{0}` distinction carried in the type; spell-side additional costs (CR
118.8) and printed alternative costs (CR 118.9) on `Card`; CR 118.9d's
application of adjustments and additional costs to a chosen alternative; a
payability predicate and a prompting payment path in `Pawl.Cost`, which becomes
the sole casing home for the axis; two new prompts with named elisions; one new
`PermanentCriterion` constructor; codec, replay and card-file migration.

**Out of scope, each an issue** (§8): flashback and casting from the graveyard;
optional additional costs and kicker; effect-granted alternative costs; CR
118.10's competition between components for one permanent; CR 601.2h's
player-chosen payment order; CR 118.13's hybrid and Phyrexian symbol choices; CR
118.12's costs paid during resolution; discard- and exile-as-cost components.

**Explicitly not deferred but genuinely absent from the rules:** alternative
costs on *activated abilities*. CR 118.9's first sentence is "Some **spells**
have alternative costs." `alternativeCosts` therefore lives on `Card` and never
on `ActivatedAbility`; this is a rules fact, not an elision, and needs no issue.

## 2. Architecture

### 2.1 `Cost`, one type for spells and abilities

```haskell
-- Pawl.Type.Cost
data Cost = MkCost
  { mana :: Maybe ManaCost,
    components :: [CostComponent]
  }
  deriving (Eq, Ord, Show)
```

`Pawl.Type.AbilityCost` is **retired**, not wrapped: the project has no API
stability obligations, and one axis with two names is exactly the shape the
umbrella's "one structural axis per phase" discipline exists to prevent.
`ActivatedAbility.cost` becomes a `Cost`; a card's costs are *built* as `Cost`s
by `Pawl.Cost.costsFor` (§2.4) rather than stored as one, because a spell has a
list of candidate costs and only one of them is printed in the mana-cost box.

No module cycle appears: `Pawl.Type.Card` imports `Pawl.Type.Cost`, which imports
`Pawl.Type.CostComponent`, which imports `Pawl.Type.PermanentCriterion` and
`Pawl.Type.Subtype`. Nothing in the chain refers back to `Card`, so unlike
`Effect`'s delayed-trigger knot this needs no parametricity trick.

### 2.2 `CostComponent`, the new vocabulary

```haskell
-- Pawl.Type.CostComponent
data CostComponent
  = TapThis                              -- CR 602.1a's {T} symbol
  | SacrificeThis                        -- CR 701.21 (Mindslaver)
  | PayLife Natural                      -- CR 119.4 (Greed)
  | Sacrifice Natural PermanentCriterion -- CR 701.21a (Village Rites, Fireblast)
  deriving (Eq, Ord, Show)
```

`Pawl.Type.AdditionalCost` is retired; `TapThis`/`SacrificeThis` are its two
inhabitants renamed to match the new module's vocabulary (`Self` was
`AdditionalCost`-relative; `This` is the object the cost is on).

**`SacrificeThis` is not `Sacrifice 1 ThisPermanent`.** CR 602.1a's
self-referential cost names one object and offers no choice; folding it into the
criterion form would invent a prompt the rules do not have, and would then need
the criterion language to express "this very object", which is a P9 concern.

`PayLife` takes a `Natural` and not a `Quantity`: no gate card pays a variable
amount of life, and `Quantity`'s evaluation needs a binding environment that a
cost has no access to at CR 601.2f time. A variable life payment is card-driven
(§8).

### 2.3 The unpayable/`{0}` distinction

`Cost.mana` is `Maybe ManaCost`, and the `Maybe` means what CR 118.6 means:

- **`Nothing` = an unpayable cost.** CR 118.6: "Some objects have no mana cost.
  This represents an unpayable cost. An ability can also have an unpayable cost
  if its cost is based on the mana cost of an object with no mana cost." This is
  the same fact `Card.manaCost`'s `Maybe` already carries for CR 202.1, so the
  two fields keep one spelling and `costsFor` passes the card's `Maybe` straight
  through.
- **`Just (MkManaCost [])` = `{0}`.** CR 118.5/118.5a: a cost of `{0}` is real
  and payable — "the action necessary for a player to pay such a cost is the
  player's acknowledgment that they are paying it". `ManaCost` is a list of
  symbols and the empty list already **is** `{0}` (`Pawl.Cost` says so today at
  CR 601.2f's floor).

**This changes the meaning of an existing field.** `AbilityCost.mana`'s `Nothing`
currently means "no mana symbol in the cost" — i.e. `{0}`, payable, which is
every M3e mana ability. Under CR 118.6 that spelling is wrong, so **every
existing ability migrates `Nothing → Just (MkManaCost [])`**, in the library
defaults, in the card JSON, and in the fixtures. The migration is mechanical but
load-bearing: leave a card file's `mana` field absent and its ability silently
becomes unactivatable. `Pawl.ManaSpec`'s Llanowar Elves tests are the tripwire.

Two rules then fall out of the shape instead of needing special cases, both from
CR 118.6a:

- *"If an unpayable cost is increased by an effect or an additional cost is
  imposed, the cost is still unpayable"* — `total` maps over the `Maybe`, so
  `Nothing` stays `Nothing`.
- *"If an alternative cost is applied to an unpayable cost, including an effect
  that allows a player to cast a spell without paying its mana cost, the
  alternative cost may be paid"* — `costsFor` offers each alternative as its own
  `Cost` carrying its own mana component, independent of the printed one. That is
  the seam a suspend or Evermind card lands on later with no rework.

### 2.4 `costsFor`, and where CR 118.9d lands

```haskell
-- Pawl.Cost
costsFor :: ObjectId -> GameState -> [Cost]
```

The candidate costs for casting this object, printed one first:

```haskell
MkCost { mana = Card.manaCost card, components = Card.additionalCosts card }
  : map (\alt -> alt { components = components alt ++ Card.additionalCosts card })
        (Card.alternativeCosts card)
```

This is CR 118.9d in one line: an alternative replaces only the **mana cost**,
and every additional cost still applies to it. CR 601.2f's increases and
reductions apply to whichever candidate is chosen, which is `total`'s job (§2.5),
called on the chosen `Cost` and not on the printed one.

CR 118.9a — "Only one alternative cost can be applied to any one spell as it's
being cast" — is the *list-of-candidates* shape itself: the player picks one
`Cost`, never a combination. `Card.alternativeCosts` is a list because a card may
print more than one (kicker-adjacent cards do), not because more than one may be
paid.

`Card` gains exactly two fields, both read directly and never through the
projection, matching `castingPermissions`' precedent (a cost is consulted while
the object is in hand, where CR 613's layer system does not reach):

```haskell
additionalCosts  :: [CostComponent]  -- CR 118.8
alternativeCosts :: [Cost]           -- CR 118.9
```

### 2.5 `total`, CR 601.2f over the new shape

```haskell
total :: PlayerId -> ObjectId -> Cost -> GameState -> Cost
total pid oid cost gs =
  cost { mana = fmap (applyAdjustments (PlayerEffect.costAdjustments pid oid gs)) (mana cost) }
```

`applyAdjustments` is P7's, unchanged: increases into the generic component, then
reductions off the generic component only (CR 118.7a), floored at `{0}`.
Components are untouched — no P7 vocabulary modifies a non-mana cost, and CR
601.2f's increases and reductions are all mana amounts.

The signature change from `ManaCost -> ManaCost` to `Cost -> Cost` is what makes
the Thalia × Fireblast test possible: the alternative's `Just []` is raised to
`Just [{1}]`, so the alternative cost really is "sacrifice two Mountains **and
pay {1}**".

### 2.6 Payability, CR 118.3

```haskell
canPay          :: PlayerId -> ObjectId -> Cost -> GameState -> Bool
canPayComponent :: PlayerId -> ObjectId -> CostComponent -> GameState -> Bool
```

`canPay` is `Nothing → False` on the mana part (CR 118.6: attempting to pay an
unpayable cost is an illegal action), and otherwise `Mana.canPay` on the mana
part **and** every component payable. Per component:

| Component | Payable when | CR |
|---|---|---|
| `TapThis` | the object is on the battlefield and untapped | 118.3's own example: "a permanent that's already tapped can't be tapped to pay a cost" |
| `SacrificeThis` | the object is a permanent this player controls | 701.21a |
| `PayLife n` | the payer's life total is `>= n`; `n = 0` always | 119.4, 119.4b |
| `Sacrifice n c` | this player controls at least `n` permanents matching `c` | 701.21a |

The mana part is measured against the **current** state, before any component is
paid, and that is CR-correct rather than convenient: CR 601.2g gives the mana
window *before* CR 601.2h's payment, so a Mountain tapped for mana is still on
the battlefield to be sacrificed afterwards. Sacrificing a permanent never
retroactively unmakes the mana it produced.

Criteria are matched **through the projection** (`Projection.subtypesOf`,
`Projection.cardTypesOf`, `Projection.controls`), never against printed
characteristics — the standing house rule, and what makes the Blood Moon test in
§5 pass.

### 2.7 Payment, CR 601.2g/h, and the sacrifice funnel

```haskell
pay :: PlayerId -> ObjectId -> Cost -> Game Payment
data Payment = Paid | Unpaid   -- Pawl.Type.Payment
```

A sum type rather than `Bool`, per the house rule against boolean blindness, and
one convention for the door rather than a fifth one (#79's complaint about
`Pawl.Replacement`'s four).

Order: the mana part first (CR 601.2g's mana window, then the mana half of
601.2h), then components in printed order. CR 601.2h says the player may pay in
any order; fixing the order is an elision, legitimate because no component in
this phase's vocabulary changes another's payability — see §8.

Sacrifice goes through **`Event.sacrifice`**, the CR 701.21 funnel P4 built, not
through a direct zone poke. This is the load-bearing choice of the section: a
cost payment is a game event, so dies-triggers, replacement effects and P4's turn
history all see it. §5's Khabál Ghoul test is the proof.

Paying life is a direct `Player.life` subtraction (CR 119.4's "the payment is
subtracted from their life total; in other words, the player loses that much
life"), and the CR 704.5a state-based action that follows is the existing one in
`Pawl.Sba` — untouched.

### 2.8 Two prompts, and their elisions

```haskell
ChooseCost       :: Decider -> PlayerId -> ObjectId -> [Cost] -> Prompt Cost
ChooseSacrifices :: Decider -> PlayerId -> ObjectId -> [ObjectId] -> Natural -> Prompt (Set ObjectId)
```

`ChooseCost` is issued at CR 601.2b's position — after modes, before X and
targets — and offers exactly the **payable** candidates: each candidate from
`costsFor`, run through `total`, then tested with `canPay`. Because CR 601.2b
puts the cost announcement before the value of X, that test uses the same X=0
floor `castable` uses (a variable cost is payable when it is payable at X=0, and
the player may always choose 0). CR 118.9b makes an alternative cost optional, so
a player with both available is really choosing.

`ChooseSacrifices` is issued during payment, at CR 601.2h, and offers the
permanents matching the component's criterion. It deliberately does **not** reuse
`Prompt.ChooseTargets` or the `TargetSpec` machinery: CR 115.1 makes a target
only what the word "target" names, and conflating the two would let shroud,
hexproof and "becomes the target" triggers observe a sacrifice choice — a rules
bug the type system would stop reporting. Its shape mirrors `ChooseDiscard`
(candidates plus a count); it returns a `Set` because a permanent cannot be
sacrificed twice for one payment.

Both are elided **only when forced**, the `#50`/`#12` pattern: one payable
candidate, or candidates exactly equal to the count. Three payable Mountains and
a count of two is a real choice and is always prompted — Mountains differ in tap
state, counters, and attached auras, so "they are all the same" is not a claim
this engine may make.

Both validate reject-not-repair: an answer outside the offered set makes the
whole cast or activation a no-op. In `Cast.castSpell` payment precedes the zone
change, so rejection needs no restore; in `Activate.activateAbility` the ability
is already on the stack, and the existing full `State.put gs` restore covers it.

### 2.9 The read sites

**`Pawl.Cast`.** `costOf` retires in favour of `Cost.costsFor`. `castable`
becomes "at least one candidate cost is payable" — `any` over
`canPay pid oid (total pid oid (substituteX 0 candidate) gs) gs` — keeping the
CR 601.2b X=0 floor and the CR 601.2f rule that affordability is measured against
the *total*. `castableWhileSearching` gets the same treatment. `castSpell` gains
the `ChooseCost` step and hands the chosen, X-substituted, totalled `Cost` to
`Cost.pay`.

**`Pawl.Activate`.** `canPayAdditional` and `payAdditional` are deleted;
`activatable` calls `Cost.canPay` and `activateAbility` calls `Cost.pay`. Ability
costs route through `Cost.total` too — no P7 vocabulary taxes an activation
today, so nothing changes observably (#90 stays open as a *producer* gap), but
there is now one door rather than two. `tapSicknessOk` stops reading
`AdditionalCost.TapSelf` directly and asks `Cost.requiresTapSymbol`, so CR
302.6's summoning-sickness gate consults a classification and `Pawl.Cost` remains
the single casing home.

### 2.10 `PermanentCriterion` gains one constructor

```haskell
data PermanentCriterion
  = AnyPermanent
  | CreaturePermanent          -- Village Rites reuses this
  | PermanentOfSubtype Subtype -- Fireblast's Mountains
```

One more constructor on the growth path its own module comment already
describes; P9 still merges it with `CardCriterion` into one filter language, and
#38/#39/#40 are unaffected.

### 2.11 Serialization, replay, and the card files

`Pawl.Codec` gains `Cost` and `CostComponent` round-trips, the two new `Card`
fields, and the new `PermanentCriterion` arm; the `AbilityCost`/`AdditionalCost`
codecs are deleted. `Payment` is runtime-only and never serialized.
`Pawl.Replay` gains `Response.ChoseCost` and `Response.ChoseSacrifices` with
their `promptToResponse`/`responseToAnswer` arms and short-transcript fallbacks:
the **first offered cost** (the printed one — the least eventful) and the first
`count` candidates in ascending order (mirroring `ChooseModes`' fallback).

**Every card file with an activated ability changes shape** (`cost.additional` →
`cost.components`, and `cost.mana` becomes `[]` rather than absent — §2.3), and
three new files land: `greed.json`, `fireblast.json`, `village-rites.json`, each
registered in `Pawl.Cards` (a missing registration fails `Pawl.CardSpec`'s
directory-agreement test).

## 3. The two invariants

**The rules core reads a classification, never an effect's identity.**
`CostComponent` is a classification, and `Pawl.Cost` is its sole casing home —
the `Pawl.PlayerEffect` pattern from P7, and the reason `requiresTapSymbol`
exists instead of letting `Pawl.Activate` match a constructor. `Pawl.Cast` and
`Pawl.Activate` learn nothing about which components exist; they ask "can this be
paid" and "pay it".

**The engine makes no choices.** Both new decisions are prompts. `ChooseCost`
exists because CR 118.9b makes the alternative optional; `ChooseSacrifices`
because CR 701.21a lets the player choose which of their permanents dies. Each is
elided only where the rules leave nothing to ask, and each elision is named in §8
with the card that retires it.

## 4. What this phase does **not** touch

- **`Card.manaCost` stays exactly what it is.** CR 118.8d and 118.9c both say
  neither kind of cost changes a spell's mana cost — "Spells and abilities that
  ask for that spell's mana cost still see the original value." Mana value, and
  every future reader of it, is unaffected by anything here.
- **`Pawl.Projection`** is read from and never edited: a cost is not a
  characteristic.
- **`Pawl.Mana`** keeps pools, production and spending unchanged. `payCost` and
  `canPay` are called by `Pawl.Cost` exactly as they are called today; #12's
  mana-source prompt is untouched.
- **`Pawl.Sba`** is untouched; Greed's death-by-payment runs through the existing
  CR 704.5a check.
- **P7's `applyAdjustments` and `costAdjustments`** are untouched; only `total`'s
  wrapper changes shape.

## 5. Cards and tests

Every gate card gets a **gameplay-level** test — cast or activate through the
real engine and assert on game state — per the umbrella's definition-of-done. The
codec round-trip proves only that a card *says* something.

### Rulings discipline (design.md §4)

Scryfall returned no rulings for Fireblast or Greed. Village Rites has exactly
one, and it is an assertion, not colour: *"You must sacrifice exactly one
creature to cast this spell; you can't cast it without sacrificing a creature,
and you can't sacrifice additional creatures."*

### Greed — an amount-bearing component

- `{B}`, Pay 2 life: draw a card. At 20 life with a Swamp: the ability is offered,
  activating it draws one card and the life total is 18.
- **At 1 life the ability is not offered** (CR 118.3's own worked example). This
  is the discriminating test: a payability check that ignores the amount passes
  the first test and fails this one.
- **At 2 life, activating it is legal and loses the game** — life 0, then CR
  704.5a. Proves the payment is a real life-total change, and that a cost may
  legally kill its payer.
- Greed has no `{T}` in its cost, so `requiresTapSymbol` is `False` and CR 302.6
  never applies — the counterpart to Llanowar Elves, whose cost is `Just []` plus
  `TapThis` after the §2.3 migration.

### Village Rites — a spell-side additional cost

- With a Swamp and a creature: castable; the creature is sacrificed and two cards
  are drawn.
- **With a Swamp and no creature: not castable**, per the ruling. A design that
  pays additional costs after announcement offers this cast; CR 601.2f puts the
  additional cost inside the total.
- **Khabál Ghoul counts the sacrificed creature at end of turn.** The cost
  payment went through `Event.sacrifice`, so P4's turn history saw it. A direct
  zone poke passes both tests above and fails this one.
- With two creatures, `ChooseSacrifices` is issued; with one, it is elided.

### Fireblast — the alternative-cost seam

- **Two tapped Mountains, empty mana pool: castable, and it deals 4.** The
  headline test of the phase.
- Six lands including two Mountains and enough red: **both** costs are payable, so
  `ChooseCost` is issued. With only one payable, it is elided.
- Three Mountains: `ChooseSacrifices` is issued. Exactly two: elided.
- **Blood Moon in play makes Evolving Wilds a Mountain**, and it may be
  sacrificed to Fireblast's alternative. Proves `PermanentOfSubtype` reads the
  projection (layer 4) and not the printed type line. (Blood Moon affects only
  *nonbasic* lands, which is why the second permanent is Evolving Wilds and not
  an Island.)
- **Thalia in play raises the alternative cost by `{1}`** (CR 118.9d). With two
  tapped Mountains and nothing else, the alternative is no longer payable and
  Fireblast is not castable; with a third, untapped Mountain it is castable
  again. This is the test that requires `Just []` to be a taxable `{0}`, and it
  is the phase's cross-check against P7.

### Migration coverage

The `Nothing → Just []` change (§2.3) is behaviour-preserving for every existing
ability, and the existing suites are its tripwire: `Pawl.ManaSpec` (Llanowar
Elves), `Pawl.ActivateSpec` (Mindslaver, Prodigal Sorcerer), `Pawl.CodecSpec`
(round-trips), `Pawl.CardSpec` (directory agreement).

### Codec

Round-trip `Cost`, `CostComponent`, the two new `Card` fields and the new
`PermanentCriterion` arm, plus the three new card files through
`Pawl.Cards`.

## 6. Module and type changes (summary)

**New**

| Module | Contents |
|---|---|
| `Pawl.Type.Cost` | `Cost { mana :: Maybe ManaCost, components :: [CostComponent] }` |
| `Pawl.Type.CostComponent` | `TapThis`, `SacrificeThis`, `PayLife`, `Sacrifice` |
| `Pawl.Type.Payment` | `Paid`, `Unpaid` |
| `Pawl.CostSpec` | the phase's tests (near-mirror of `Pawl.Cost`) |

**Retired**

`Pawl.Type.AbilityCost`, `Pawl.Type.AdditionalCost`, and `Pawl.Cast.costOf`.

**Changed**

| Module | Change |
|---|---|
| `Pawl.Cost` | `costsFor`, `total` (now `Cost -> Cost`), `canPay`, `canPayComponent`, `pay`, `requiresTapSymbol`; sole casing home |
| `Pawl.Type.Card` | `+ additionalCosts`, `+ alternativeCosts` |
| `Pawl.Type.ActivatedAbility` | `cost :: Cost` |
| `Pawl.Type.PermanentCriterion` | `+ PermanentOfSubtype Subtype` |
| `Pawl.Type.Prompt` | `+ ChooseCost`, `+ ChooseSacrifices` |
| `Pawl.Type.Response` | `+ ChoseCost`, `+ ChoseSacrifices` |
| `Pawl.Cast` | `castable`, `castableWhileSearching`, `castSpell`'s announcement sequence |
| `Pawl.Activate` | `activatable`, `activateAbility`, `tapSicknessOk`; two helpers deleted |
| `Pawl.Codec` | new types, new fields, deleted `AbilityCost`/`AdditionalCost` codecs |
| `Pawl.Replay` | two prompt arms plus fallbacks |
| `data/cards/*.json` | every ability-bearing file migrated; three new files |
| `Pawl.Cards` (test) | three new printings registered |
| `source/benchmark/Main.hs`, `Pawl.Support` | answer the two new prompts |

## 7. Ordering within the phase (for the plan)

Substrate before consumer; each step one complete commit.

1. `Pawl.Type.Cost` + `Pawl.Type.CostComponent` + codecs, with `AbilityCost` and
   `AdditionalCost` retired and every card file and fixture migrated —
   including `Nothing → Just []` (§2.3). Behaviour-identical; the existing suites
   are the assertion.
2. `Pawl.Cost`'s door: `total` reshaped, `canPay`, `canPayComponent`, `pay`,
   `requiresTapSymbol`, `Pawl.Type.Payment`. `Pawl.Activate` routes through it.
   Still behaviour-identical.
3. `PayLife` + `Greed` → the amount-bearing component, on the ability path.
4. `Sacrifice` + `PermanentOfSubtype` + `ChooseSacrifices` + `Card.additionalCosts`
   + `Village Rites` → the spell-side additional cost and the castability gate,
   with the Khabál Ghoul event-funnel test.
5. `Card.alternativeCosts` + `costsFor` + `ChooseCost` + `Fireblast` → the seam.
6. The two cross-checks: Blood Moon × Fireblast (projection) and Thalia ×
   Fireblast (CR 118.9d).

Fireblast is last for the same reason Silence was last in P7: every other gate
card proves the axis works on the simpler carrier first.

## 8. Deferred, with named expiries

Each becomes a GitHub issue carrying status, rationale and expiry trigger, cited
inline at the code site as `(#N)` with no expiry written into the comment
(CLAUDE.md).

| Deferred | Why it is safe | Expires on |
|---|---|---|
| Flashback, and casting from the graveyard | CR 702.34a is two static abilities, only one of which is a cost: it also needs a `CastFromGraveyard` permission and an "exile it instead of putting it anywhere else it would leave the stack" replacement on P5's machinery | a flashback card (Deep Analysis, Firebolt) — card-driven |
| A stored record of *which* cost was paid | nothing downstream reads it: Fireblast's alternative has no payoff and no replacement | the same flashback card, which needs "if the flashback cost was paid" — card-driven; adjacent to #94 |
| Optional additional costs / kicker (CR 118.8a–b) | the announcement chooses one whole candidate cost, never a subset of optional ones | a kicker or buyback card — card-driven |
| Effect-granted alternative costs ("without paying its mana cost") | `Card.alternativeCosts` is printed-only; the granting effect is a P7-shaped player permission with no producer | a card that grants a free cast (Omniscience-adjacent) — card-driven |
| CR 118.10 competition between components | components are checked for payability independently, so two `Sacrifice` components could count one permanent twice; no card in the pool has two | a cost with two object-consuming components — card-driven |
| CR 601.2h's player-chosen payment order | fixed as mana-then-printed-order; no component in this vocabulary changes another's payability, and CR 601.2g puts the mana window first regardless | a component whose payability depends on another's completion (a `{T}` of another permanent) — card-driven |
| CR 118.13 hybrid and Phyrexian symbol choices | this phase adds no mana symbols; `ManaSymbol` has three inhabitants and none is payable two ways | a hybrid or Phyrexian card — card-driven |
| CR 118.12's "[do something]. If you do" costs paid on resolution | a resolution-time cost is not an announcement cost and needs no seam here | an unless- or if-you-do card — card-driven |
| Discard-as-cost, exile-from-zone components | VOCAB on the axis this phase builds; each is one constructor and one payability arm | the card that prints one — card-driven |
| A variable (`Quantity`) life payment | `PayLife` takes a `Natural`; no gate card pays a variable amount, and CR 601.2f runs before a binding environment exists | a "pay X life" card — card-driven |
| CR 118.6a's "alternative cost applied to an unpayable cost" | the mechanism is built (§2.3) but has no producer: no card in the pool has both no mana cost and an alternative | Ancestral Vision / Evermind — card-driven |
| CR 118.8c's hidden-zone "if able" exception | no effect instructs a player to cast a spell "if able" | such an effect — card-driven |
| `Cast.castSpell` totals against a **hand** projection | unchanged from P7 (#89), now with more to read: a component's criterion is also matched pre-stack | a card whose characteristics differ between hand and stack — card-driven |

Untouched and still open: **#94** (CR 601.2f's "locked in" total is recomputed),
**#91** (CR 118.7b–g non-generic reductions), **#90** (activated-ability cost
modification has no producer — this phase gives it the *door* but not a
producer), **#88** (reductions summed rather than ordered), **#56** (no
mid-announcement rewind), **#12** (mana-source prompt).

## 9. Tracking

- Closes **issue #4** (M4.5 P8) and the *payment* half of GAP-Co, which P7's
  spec (§9) left explicitly open.
- Does **not** retire #38/#39/#40 (P9's filter language); `PermanentCriterion`
  is already on that list and gains one constructor here.
- Does **not** close #90: the ability cost path now runs through `Pawl.Cost.total`,
  but the *producer* — a player effect that taxes an activation — still does not
  exist.
- Updates the umbrella spec: P8's row ticked with its landed gate cards, and §4's
  "P8 and P9 float freely" replaced with P9/P10/P11 remaining.
- Updates `docs/progress.md` with the completion entry and `CLAUDE.md`'s status
  bullet by **replacement**.

## 10. Exit criterion

The cost axis has (a) a classification in the type system — one `Cost` for spells
and abilities, a parameterized `CostComponent`, the CR 118.6 unpayable/`{0}`
distinction carried in the type, printed additional and alternative costs on
`Card`, and one sole casing home — and (b) three real, recognizable gate cards,
each passing a gameplay-level test, covering an amount-bearing component, a
criterion-bearing one with a prompt, a mandatory spell-side additional cost, and
an alternative cost containing no mana at all, cross-checked against the
projection (Blood Moon) and against P7's cost modification (Thalia).

At that point M4.5 has **P9** (filters), **P10** (player counters) and **P11**
(Command zone) remaining, none of which blocks another.
