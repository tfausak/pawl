# One permanent, two cards: meld (#369)

## The problem

CR 701.42a melds two cards into "a single object represented by two cards", and
pawl has no way to say that. `Source.OfCard` names exactly one `PrintingId`;
`Pawl.Engine.Event`'s zone-change funnel is one object in and one object out;
`Pawl.Registry.index` keys every face name of every card and rejects a pool where
two cards claim one name, so the combined back face cannot be a face on both
halves of the pair. Four live comments cite the gap: `Pawl.Types.Layout`,
`Pawl.Engine.Card`, `Pawl.Types.Filter` and `Pawl.TransformSpec`.

The card that drives this is Hanweir Battlements, whose third ability melds it
with Hanweir Garrison -- already in the pool, front face only -- into Hanweir,
the Writhing Township.

## Scope

In: the `Meld` layout, the melded permanent's representation, the `Meld` effect,
the untargeted resolution-time choice its exile needs, CR 202.3c's mana value, CR
701.27g's second exclusion, CR 712.4c/712.9's refusal to transform, and CR
712.21's core -- one permanent leaves the battlefield and two cards arrive in the
new zone.

Out, each with an issue filed and cited at the site: CR 712.21a (the owner
arranges the two cards in a graveyard or library), 712.21b (relative timestamp
order on exile), 712.21d (one replacement effect applying to both cards), 712.21e
(one object, two cards, for anything that counts), CR 903.3b and 903.9c (a melded
commander), CR 123.5a/c (stickers, unmodelled anyway). CR 712.19/201.4e -- a
player choosing the combined back face's name -- needs nothing: see *Where the
combined face lives*.

Mutate (#874) stays out, but the departure machinery below is built for it; see
*What meld shares with merging*.

## The cards

One new file under `data/cards/`, verified against Scryfall on 2026-08-27, and a
one-line change to a card already in the pool.

`hanweir-battlements.json` -- a Land, layout `Meld`, three activated abilities:
`{T}: Add {C}.`; `{R}, {T}: Target creature gains haste until end of turn.`;
`{3}{R}{R}, {T}: If you both own and control this land and a creature named
Hanweir Garrison, exile them, then meld them into Hanweir, the Writhing
Township.`

`hanweir-garrison.json` gains `"layout": "Meld"` and nothing else.

`Pawl.Types.Layout` gains a `Meld` arm, and it marks a COMPONENT meld card
rather than the combined face. That is CR 712.4's own subject -- "meld cards have
a Magic card face on one side and half of an oversized card face on the other" --
and it is what Scryfall reports for both halves of the pair. Two rules need the
classification: CR 701.42b ("cards that aren't meld cards ... can't be melded")
and CR 712.4c ("meld cards cannot be transformed or converted"). Each printed
half carries its front face alone: CR 712.4b makes a meld card's back face
meaningless except as part of a melded permanent, so nothing is lost.

The codec is `Arm.enum`, so the wire format derives and every `case layout of` in
the tree goes red. A `Pawl.CardSpec` lint holds a `Meld` card to exactly one
face.

## Where the combined face lives

Inline in the meld ability, as `Effect.Meld`'s own `Card` payload -- the shape
`Effect.Create` already uses to carry a token's characteristics -- and interned
at resolution with `Game.intern`, exactly as `Pawl.Engine.Event.createTokens`
interns a token card.

Not a card file, and not a registry lookup, because the engine cannot do one: no
`Pawl.Engine` module imports `Pawl.Registry` and `GameState` holds no name-keyed
map, so an `Effect.Meld` naming its result by name would have nothing to resolve
the name against. Every card an effect brings into being in pawl today is either
already an object in the game or is carried inline.

This is also what dissolves the obstacle #369's body calls the first one an
implementer meets: the combined face is never a key in the pool, so it cannot
collide with anything in `Pawl.Registry.index`. CR 712.19 and CR 201.4e survive
it -- `Prompt.ChooseCardName` answers with whatever name the decider names and
never enumerates the pool, so a player may already choose the combined back
face's name.

The pair is named by the ability that melds, which is where the printed text
names it, so no card gains a `components` list. `Pawl.Types.Card` gains no field
either -- one there would be absorbed silently by positional record construction
in the test suite, the trap CLAUDE.md records from PRs #2009 and #2021.

## The melded permanent

```hs
data Source
  = OfCard PrintingId
  | OfMeld MeldSource     -- result, and the cards representing it
  | OfToken PrintingId
  | ...
```

`Pawl.Types.MeldSource` carries `result :: PrintingId` and `components :: NonEmpty
PrintingId` -- `NonEmpty` rather than a pair because CR 712.5 happens to list
seven pairs and nothing in CR 701.42 fixes the number at two.

`Pawl.Engine.Game.cardOfSource` answers with the result card, so every
characteristic read in the engine keeps working unchanged: CR 712.8g gives the
melded permanent "only the characteristics of the combined back face". The
components are read by exactly three rules -- CR 202.3c's mana value, CR 712.21's
departure, and CR 701.27g's exclusion.

A new `Source` arm forces roughly 55 `Source.OfCard` sites and 27 case-on-`Source`
sites red. That is the point: each is read and the PR records why it is right,
which is the coverage `-Werror` cannot give a new field.

Three vacuous rules become real cases, and the comments citing #369 at each are
rewritten in the same change:

- CR 701.27g, second exclusion -- an object represented by more than one card is
  never a transformed permanent. `Pawl.Types.Filter`'s `Transformed` atom and
  `Pawl.Engine.Projection`'s `transformed` field. The comment keeps citing #874
  for merged permanents.
- CR 712.4c / 712.9 -- meld cards cannot transform or convert, and an instruction
  to do so is ignored. `Effect.Transform` aimed at a melded permanent does
  nothing.
- CR 202.3c -- the mana value is "as though it had the combined mana cost of the
  front faces of each card that represents it" (Garrison's {2}{R} plus
  Battlements' none, so 3), and a copy of a melded permanent has mana value 0.
  `Pawl.Engine.Card`'s mana-value and `showsBackFace` neighbourhood.

`Object.owner` for the melded permanent is the shared owner of its components;
the ability that melds requires the activating player to own both, and no other
road to a meld exists in the pool.

## The action, and the choice it needs

`ObjectRef` gains an arm for *one* permanent matching a `Filter`, chosen by the
resolving controller (CR 608.2d) -- a question rather than a read, the sibling of
the existing `AnyNumberMatching`, which is the any-number form of the same
question. It is missing today only because no card in the pool needed the
singular. Hanweir Battlements needs it: with two Hanweir Garrisons on the
battlefield, "a creature named Hanweir Garrison" is a choice, and the two are
distinguishable (counters, Auras, summoning sickness), so eliding it would be an
observable divergence rather than a sound elision.

`Effect.Meld` carries `objects :: ObjectRef` and `result :: card` -- the combined
face inline, per *Where the combined face lives* -- and performs CR 701.42a alone: put those cards onto the battlefield as one permanent
whose source is `OfMeld`. The card is written as the two steps it prints:

```
MoveToZone { objects = [Self, <chosen creature named Hanweir Garrison>]
           , to = Exile, slot = "melding" }
Meld       { objects = InSlot "melding"
           , result  = <the combined face, inline> }
```

`MoveToZone` already binds its destination objects to a slot (CR 400.7j), so this
needs no new plumbing, and it gets CR 701.42c right for free: if the meld cannot
happen -- a token copy, a card that is not the counterpart -- the cards stay where
the exile left them, which is the rule's own Graf Rats example.

`Meld` refuses and leaves the cards alone (CR 701.42b/c) unless every named object
is a card (not a token, not a copy) whose layout is `Meld`, and they share an
owner.

## The departure (CR 712.21)

CR 712.21: "If a melded permanent leaves the battlefield, one permanent leaves
the battlefield and two cards are put into the appropriate zone." One "dies"
trigger, two "card put into a graveyard" triggers.

`Pawl.Engine.Event`'s returning doors -- `changeZoneReturning`,
`changeZoneEntering`, `changeZoneAttaching`, `changeZoneInBatchReturning` -- answer
`Maybe ObjectId` today, one CR 400.7 incarnation. They widen to a sequence of
arriving ids: empty where they answer `Nothing` now, one element for every object
in the pool but a melded permanent, and one per component for that. The `Game ()`
wrappers (`changeZone`, `changeZoneInBatch`) are unchanged, so the blast radius is
the returning doors' callers rather than every zone change in the engine.

Widening rather than special-casing is what makes CR 712.21c reachable now ("if an
effect can find the new object that a melded permanent becomes as it leaves the
battlefield, it finds both cards"), instead of being a deferred consequence of a
single-id return that silently drops the second card.

The split reads a classifier over `Source` -- what components represent this
object -- rather than casing on `OfMeld`, for the reason the next section gives.

## What meld shares with merging

CR 730.3 through 730.3e restate CR 712.21 through 712.21e almost word for word:
one permanent leaves and each component is put into the appropriate zone; the
owner arranges them in a graveyard or library; the exiler fixes their relative
timestamp order; an effect that finds the new object finds all of them; one
replacement effect applies to all components; the commander exemption. The
departure is therefore shared machinery, and the widened funnel serves mutate
(#874) when it lands.

What is not shared is the characteristics half. CR 712.8g reads a melded
permanent off a printing that is not one of its components; CR 730.2a reads a
merged permanent off its topmost component, as a CR 613.2 copiable effect with a
timestamp. Merged components may be tokens (CR 730.2d), face-down independently
(CR 730.2f) and flip cards (CR 730.2h); melded ones are always two face-up cards.

Rejected: one `Source` arm for both, carrying "the printing that gives
characteristics" plus a component list. It would fit meld and merge only until
CR 730.2a's copiable timestamp and CR 730.2d's token components arrive, and with
no mutate card in the pool nothing would catch a shape that is wrong for merge.
Two arms and one classifier keeps the shared code shared and the unshared code
honest.

## Tests

A new `Pawl.MeldSpec`, wired into `Pawl.Test`'s `spec`, covering the CR sections
this change reaches; gameplay-level throughout, per `docs/design.md` section 4.

- Both halves on the battlefield, the ability activated: one permanent named
  Hanweir, the Writhing Township, 7/4, mana value 3, and the two original
  permanents gone (CR 701.42a, 712.8g, 202.3c).
- Two Hanweir Garrisons on the board: the choice is asked, and the Garrison the
  player names is the one exiled (CR 608.2d).
- The melded permanent destroyed: two cards in the graveyard, one "dies" trigger
  (CR 712.21).
- `Effect.Transform` aimed at the melded permanent: nothing happens (CR 712.4c,
  712.9).
- Mutagen Connoisseur beside the melded permanent: it is not a transformed
  permanent (CR 701.27g), which is the assertion `Pawl.TransformSpec` records
  today as unreachable.
- A meld that cannot happen -- the counterpart is a token copy -- leaves the
  exiled cards in exile (CR 701.42b/c).

## What this does not retire

`Pawl.Types.Filter`'s `Transformed` atom keeps a citation for merged permanents
(#874). The deferred rules under *Scope* keep theirs. Six of CR 712.5's seven
meld pairs stay out of the pool; each is ordinary card-driven work, and the
machinery here is what any of them would use.
