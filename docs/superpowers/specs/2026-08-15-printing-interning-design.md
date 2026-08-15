# Objects name a printing instead of carrying one (#1592)

## The problem

`Source.OfCard` holds a `Printing`, a newtype over a whole `Card`. Every object
in the game state therefore embeds a complete card definition -- every face,
every clause, every nested effect -- and every `Eq`/`Ord` on `Object` or
`Source` walks all of it. The engine passes these around by value everywhere.

`Source` also spells "the card behind this" three ways:

```haskell
data Source
  = OfCard  Printing.Printing
  | OfToken Card.Card
  | OfEmblem Card.Card
  | OfAbility ObjectId (ActivatedAbility Card)
  | OfTrigger ObjectId (TriggeredAbility Card)
  | OfInherentTrigger PlayerId (TriggeredAbility Card)
```

`OfCard` carries a `Printing` because a card has one; `OfToken` and `OfEmblem`
carry a bare `Card` because CR 111.6 and CR 114.3 say those objects are not
cards and have no printing. That is a real rules distinction, but it does not
need two payload types to express -- the engine already cases on the
constructor, in `Departure`, `Cost`, `Sba.ceaseToExist` and `Stack`.

## Scope

In: a game-local intern table in `GameState`, a `PrintingId` key, the three
card-shaped `Source` constructors and `Player`'s two `Printing` fields carrying
ids, and `Setup`/`Event` interning at the two places printings enter a game.

Out: growing `Printing` with print-level fields (artist, flavor text). That is
card-driven work, gated on transcribing *Fascist Art Director*; building the
field now would build a capability no card reads (`docs/design.md` section 4).

Out: any codec. #126 is the serialization work and is blocked on this.

## Why printings, not cards

The table interns `Printing`, not `Card`, so that the representation matches
what Magic actually deals in. Paper Magic is played with printings, and un-set
cards read print-level data -- *Fascist Art Director* (Unhinged): "{W}{W}: This
creature gains protection from the artist of your choice until end of turn."
Un-sets are an accepted source, ranked fourth of five in `docs/design.md`
section 6.

A token or an emblem gets an entry whose `Printing` wraps its effect-defined
`Card` and carries no print metadata -- which is exactly what `Source.hs`
anticipates today when it says a future `OfToken Card (Maybe Printing)` would
carry a physical token's metadata. With the table that comment goes away:
`OfToken` names an id like everything else.

Two ids naming the same card is a benign state, not a bug to guard against. The
engine's identity questions are name-keyed -- CR 100.2a's deck limit is by
English name, CR 201.2 matches by name -- and CR 109.3 lists what an object's
characteristics are, closing with "any other information about an object isn't a
characteristic". That is where the illustration (CR 203.1) and the expansion
symbol (CR 206.1) sit; both are stated to have no effect on game play.

## The table

```haskell
-- Pawl.Types.PrintingId
newtype PrintingId = MkPrintingId
  { unwrap :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
```

In `GameState`, beside the counters that already work this way:

```haskell
    printings :: Map.Map PrintingId.PrintingId Printing.Printing,
    nextPrintingId :: PrintingId.PrintingId,
```

`nextObjectId` and `nextTimestamp` are the precedent: a monotonic counter in the
state, bumped at the one place that mints.

**Ids are minted only by insertion.** There is no way to construct a
`PrintingId` that names nothing, so the dangling-reference failure mode does not
exist. This is the reason the table is game-local rather than the registry: a
registry-keyed reference could name a card the registry lacks, and every token
minted mid-game would be exactly that case.

**The registry never enters the monad.** It fills the table at setup and is
done. `Pawl.Types.Game` is unchanged -- no `StateT (GameState, Registry)`, which
would put an append-only structure in a slot that reads as read-only and force
every token creation to reach past `GameState` to touch it.

## Interning

Two call sites, both of which already intern once and create many:

- `Setup.hs:220` iterates `Map.toList (Deck.cards deck)` as `(printing, n)` and
  calls `createCard` n times. One intern per deck entry.
- `Event.hs:2932` resolves `(owner, tokenCard, count)` and builds `count`
  objects from one card. One intern per token-creation event. `Event.hs:522`
  does the same for an emblem.

`Game.intern` therefore does no content-keyed dedup and keeps no reverse
`Map Printing PrintingId` index: a global one would pay the deep `Ord Card` this
change exists to avoid, at every intern, to save entries the engine's call sites
do not generate.

**Deviation, deliberate: `Setup.internDeck` does dedup, deck-locally.** A deck's
distinct printings are interned once into a `Map Printing PrintingId` that
`createDeck` then looks ids out of. `Pawl.Engine.Commander.isCommander` forces
it: that function compares `Player.commander` against the commander object's own
printing, and id equality is stricter than the value equality it replaced --
`Deck.commander` interned separately from `Deck.cards` would no longer match.
The deep `Ord Printing` is paid once per deck at setup and never again, which is
the trade `Game.intern` declines to make globally.

**Deck is unchanged.** `Deck.cards :: Map Printing Natural` stays as it is. A
deck is a pre-game input, constructed before any `GameState` exists, so it
cannot hold game-local ids. `Setup` is the boundary where a `Printing` becomes a
`PrintingId`.

## The engine funnel

`Game.cardOfSource` is the chokepoint -- three call sites, all inside
`Game.hs`, two of which already hold the `GameState`:

```haskell
-- before
cardOfSource :: Maybe Source.Source -> Maybe Card
-- after
cardOfSource :: GameState -> Maybe Source.Source -> Maybe Card
```

`cardOf oid gs` and `cardOfWithLastKnown oid gs` both already take `gs` and pass
it through. Library code has 21 `Printing.` sites total: 7 in `types`, 5 in
`codec`, 9 in `engine`. The 607 test-suite sites are mechanical.

## What this buys, honestly

The headline is representational: the state stops passing whole card
definitions around by value, and `Source`'s three card-shaped constructors
become one payload type carrying three rules meanings.

`Eq`/`Ord` on `Object` and `Source` become `O(1)` in the card, where they used
to walk the whole definition. `Eq`/`Ord` on `GameState` does NOT improve -- it
now contains the table and still walks every printing in it -- and the memory win
was already there without this change, since `Setup` passed one `printing` value
to `createCard` for all four copies of a 4-of and GHC's sharing interned them.

**It costs allocation, and the measurement is the point of this section.**
`PerformanceSpec`'s Prodigal Sorcerer board (256 permanents, the targeting path)
went from 99,317 to 167,512 bytes per permanent -- a 69% regression against a
committed 130,000 ceiling. The cause is one extra `Maybe` per card read:
`Pawl.Engine.Projection.baseCharacteristics` reaches `Game.cardOf` through
`Game.faceOf`, the layer fold runs it per object per candidate, and the
enumeration's quadratic shape multiplies it.

`Game.printingOfObject` recovers most of it by handing back the `Maybe` the
table lookup already built, where `cardOf` fmapped `Printing.card` into a second
one; `faceOf`, `namesOf` and `manaCostFaceOf` unwrap the field themselves. That
is 167,512 -> 130,376, and it improved the Llanowar Elves board too (10,803 ->
10,531, against 10,241 before the change).

What is left -- 130,376 against 99,317, about +31% -- is irreducible. A total
lookup into a `Map` allocates a `Maybe`; the field read it replaced did not.

Four other attempts measured WORSE or made no difference, and are recorded so
they are not retried: sharing `mObj`/`mCard` through a `let` in
`baseCharacteristics` (198,528 -- `PC.names` and `PC.manaValue` are lazy fields
that usually go unforced, so the binding bought a thunk and saved nothing);
routing the three readers through shared given-forms (198,552 -- builds a
`cardOf` thunk even when face-down, where they used to short-circuit); binding
the object by `case` in `faceOf` and passing `Just obj` (155,136 -- GHC's CSE
already shared the lookup, and this allocated a fresh `Maybe`); and `fmap` vs
explicit `case` plus `INLINE` pragmas (byte-identical -- GHC already did both).

## Tests

- Round-trip at the funnel: an object created by `Setup` answers `Game.cardOf`
  with the card its deck entry named.
- A token and an emblem each intern an entry, and `cardOf` reads it back --
  covering the two constructors that carried a bare `Card`.
- Two objects created from one deck entry name the same `PrintingId`.
- An existing gameplay-level test that exercises a token copy still passes
  unchanged, proving the `Source` reshape is behaviour-preserving.

Mutation check: break the intern in `Setup` so it mints a fresh id per object
and confirm the "same `PrintingId`" test fails; break `cardOfSource`'s table
lookup and confirm the round-trip tests fail.

## Verification

- Suite count before -> after, unchanged except for the new tests.
- The rules core does not case on an effect's identity: no. Nothing in this
  change reads an effect at all; it moves a payload behind an index.
- `-Werror` will not catch a `Source` constructor whose payload changed type but
  whose match is `_`. Grep every `Source.Of` site (123 in `engine`) and read
  them, plus the recurring blind spots: `Pawl.Engine.Event`'s `eventBindings`
  fallthrough, `Pawl.TriggerSpec`'s `everyTriggerCondition` and
  `representativeEvents`, and `Pawl.CardSpec`'s filter and keyword traversals.

## Deferred

- The table is append-only and never collects. A token that ceases to exist
  under SBA drops its `Object` but keeps its entry, so the table grows
  monotonically where `objects` does not. Bounded in practice by game length,
  and each entry is one small record. File an issue; do not build a collector
  no measurement asks for.
- `Printing` growing `artist` and flavor text -- card-driven, fires on
  *Fascist Art Director*. No issue yet, by decision: it is far enough out that
  one would be noise.

## Alternatives rejected

**Intern against the registry, keyed by `CardName`.** Only one of the six
`Source` constructors holds a registry-resident card. Tokens and emblems are
effect-defined and never existed in `data/cards/`, so every token creation would
need an inline escape hatch -- and once the hatch exists, `Source` still carries
a `Card` and nothing was gained. This is also the only variant where a dangling
reference is constructible.

**A shared-structure layer in the codec only, leaving `GameState` alone.** Gets
the serialization win without touching the engine, but leaves the
representational problem, which is the actual complaint. Worth noting that #126
remains implementable this way if this change is abandoned.

**`StateT (GameState, Registry) (Program Asked)`.** Rejected above: the table is
appended to mid-game, so it is state, not environment.
