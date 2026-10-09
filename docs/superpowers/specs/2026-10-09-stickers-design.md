# Stickers and ticket counters (#872)

## The problem

Pawl has no stickers, no sticker sheets and no ticket counters. CR 123 makes a
sticker a marker on an object that is not a counter, a token or an object, that
survives a move into a public zone (an exception to CR 400.7) and that is not
copiable. Four kinds do four different things: art (a bare marker), name (a
layer-3 text change), ability (layer 6) and power/toughness (layer 7b). Sheets
are outside the game but are not cards (CR 123.2), so `Player.outsideTheGame`
cannot carry them, and CR 702.33h's sticker kicker waits on all of it.

## Scope

Done means every rule in CR 123 and 107.17 (and 103.2d, 613.7k, 702.33h) is
proven by at least one real card in a gameplay-level test. Every other Unfinity
sticker or ticket card that is expressible afterwards is a plain card add, not
tracked. Four units, landed in order, this spec committed with the first:

1. **Tickets, sheets and art stickers.** `PlayerCounterKind.Ticket`, sheet
   data, CR 103.2d's setup step, `Object.stickers` with CR 123.5 retention and
   CR 613.7k stamps, placement, the stickered filters and the placement event.
2. **Name stickers.** The layer-3 name insertion, the controller's position
   prompt, and the word, letter and unique-vowel quantities.
3. **Ability and P/T stickers, ticket costs.** Layer-6 and layer-7b grants
   across the public zones, CR 123.3c payment, CR 123.7a and 123.8a reads.
4. **Generic producers, sticker kicker, meld and merge.** Every "put a sticker"
   card needs all four kinds; `Keyword.StickerKicker`; CR 123.5a–c. Closes
   #872.

Out, each filed with an issue in the unit named:

- CR 123.2a's "at least ten unique sheets" is deck validation, which pawl does
  not do (related to #4458). Unit 1.
- Whether a Shahrazad subgame (CR 729.2) uses sticker sheets: CR 103.2d is
  silent. Pawl reruns the draw; filed as a rules question. Unit 1.
- An ownership change (ante unit 2's Darkpact, Tempest Efreet) moves a
  stickered card to another owner, and its stickers read as available to the
  original owner again. No rule addresses it. Unit 1.
- CR 123.3d, moving a sticker: no card moves or removes one (Scryfall
  `o:sticker o:move`, `o:sticker o:remove`, both empty 2026-10-09). Unit 1,
  `expires:card-driven`.
- CR 123.6a's blank in a name, and a name sticker on an object with several
  names: both readings below are pawl's, filed as rules questions. Unit 2.
- Ring-4 cards (`docs/design.md` section 6): Animate Object, Art
  Appreciation, Focused Funambulist, Cover the Spot, Scavenger Hunt, Trivia
  Contest, Mobile Clone, Haberthrasher, Juggletron. Astroquarium and Tchotchke
  Elemental land without their physical clauses only if the rest is separable.
- Solaflora, Intergalactic Icon (stickers on it apply to other creatures): filed
  in unit 4 unless it folds in.

## The cards

None is in `data/cards/` (2026-10-09). Every implementer re-verifies Oracle on
Scryfall before writing the JSON; the research notes are not authoritative.

| Unit | Proving cards | What they prove |
|---|---|---|
| 1 | Blorbian Buddy, Ticket Turbotubes | gaining `{TK}` |
| 1 | Proficient Pyrodancer | art placement (CR 123.9); target "with an art sticker"; retention |
| 1 | Wee Champion or Goblin Airbrusher | the placement event |
| 1 | one of Big Winner, Croakid Amphibonaut, Grabby Tabby, Sanguine Sipper, Scared Stiff | "stickered" (CR 123.4) |
| 2 | Baaallerina, Sword-Swallowing Seraph | name placement; "with a name sticker" |
| 2 | Wizards of the _____, Wolf in _____ Clothing | unique vowels on "that sticker"; a blank in the name |
| 2 | Angelic Harold | words in a name |
| 2 | one of Fight the _____ Fight, Last Voyage of the _____, Trespasser | letter counts |
| 2 | Witness Protection or Spy Kit | CR 123.6c ordering against another text change |
| 3 | Lineprancers | P/T placement and "with a P/T sticker" (layer 7b) |
| 3 | Pin Collection | ability placement, ticket-cost waiver, CR 123.7a |
| 3 | Tusk and Whiskers | ability-sticker event |
| 4 | Ticketomaton and the other generic producers | "a sticker" of any kind |
| 4 | Wicker Picker | sticker kicker (CR 702.33h) |

Sheets: 48 in Scryfall set `sunf`, each 3 name, 3 art, 2 ability and 2 P/T
stickers; only ability and P/T stickers carry ticket costs. A unit adds the
sheets its tests need, and only sheets whose ten stickers are all expressible.

## Unit 1: tickets, sheets and art stickers

**Tickets (CR 107.17).** `PlayerCounterKind.Ticket`, between `Energy` and
`Poison` by rule order, gained and spent through `GainPlayerCounters` and
`RemovePlayerCounters`. The type's haddock owes Aetheric Amplifier's card an
entry.

**Sheet data.** `data/sticker-sheets/<slug>.json`, loaded through the registry
library like cards. A sheet holds its number, its three name-sticker words
(hand-split: "Ancestral Hot Dog Minotaur"'s middle sticker is "Hot Dog"), an
art count, two ability stickers (ticket cost and abilities in the card DSL) and
two P/T stickers (ticket cost, power, toughness). A lint ties each sheet's
ability and P/T lines to MTGJSON's text, as `pawl ingest` does for cards.

**Sticker identity (CR 123.3a).** `StickerRef` is (owner, slot among that
player's chosen sheets, index on the sheet), never the sheet's name: limited
play allows duplicate sheets. Availability is computed, never stored: the
player's chosen sheets' stickers minus those on any object they own, in any
zone. A move to a hidden zone frees a sticker with no bookkeeping.

**Sheet choice (CR 123.2, 103.2d).** `Deck.stickerSheets` is the sheets a
player brings. A setup step beside `Setup.anteFromLibraries`, in both
`Setup.newGame` and `Setup.startGameFromCards`, writes `Player.stickerSheets`:
three drawn at random when the player brought more than three, all of them
otherwise. That one rule is exact for constructed's random three and limited's
pre-chosen three or fewer. An empty list means not playing with stickers; no
`GameSettings` flag. The draw needs a random prompt over sheet slots, since a
sheet is neither an object nor a card name. The step reruns on a restart (CR
727) and in a subgame. Sheets stay revealed (CR 123.2c): `Player.stickerSheets`
is public. The `Player.outsideTheGame` elision dies.

**Where stickers live (CR 123.1, 123.5, 613.7k).** `Object.stickers :: Seq
Placement`, each a `StickerRef`, a timestamp and, for a name sticker, its word
position. `Object.newIncarnation` clears it; `Event.changeZoneAttaching`'s
`mkObj` writes it back when the destination is public (`not
(Game.isHiddenZone dest)`), the `paidCosts` route, restamping each placement
right after the new object's timestamp in their old order. A copy (`Resolve`'s
copy construction), a restart rebuild and a cast from outside the game never
write back, so CR 123.1's "not copiable" holds by construction. Phasing and a
type change keep stickers (CR 702.26d, 205.1a) because neither is a zone change.

**Placement (CR 123.3).** One opcode: who places, onto which object, which
kinds are allowed, a ticket-cost cap and whether it is paid (unit 3 uses
the last two), optional or not. It offers the computed available set,
filtered by kind, through a new `Prompt.ChooseSticker` (modelled on
`ChooseDungeon`, with its Replay arms); an answer outside the set falls back as
`Dungeon` does. An object the placer does not own takes nothing (CR 123.3b). A
declined or impossible placement did not happen, so a "when you do" reads it
(`Resolve.Effect.happenedBetween`). A placement emits a `StickerPut` event
carrying the sticker's kind and the object, with a `TriggerCondition`.

**Reading stickers (CR 123.4, 123.9).** `Filter.HasSticker kind` and
`Filter.Stickered`, modelled on `HasCounters` and `HasCountersOfAnyKind`. The
`View` field behind them is filled in every builder: `viewOfCard`,
`viewOfCharacteristics`, `copiableCharacteristics` (empty: not copiable) and
`Count.viewOfSnapshot`. Art stickers carry no data and the engine needs none.

## Unit 2: name stickers

**Projection (CR 123.6, 123.6c, 612, 613.1c).** Each name sticker emits a new
layer-3 `Modification`, "insert these words after word k", at its placement's
timestamp. It folds with `AddNamesMatching` and `HasFullText` in timestamp
order, starting from the copiable values; when the current name has fewer than
k words the words go at the end. CR 123.6c's three examples are the tests:
Fae of Wishes cast as Granted, It That Betrays becoming a copy of Seeker of the
Way, and a later Witness Protection hiding the word. A name sticker stays out of
`copiableCharacteristics` and `Game.copyStampOf`; a Clone of a stickered
permanent is the tripwire.

**Names are not a registry problem.** `CardName` is a bare `Text` and every
reader of a projected name compares by equality; nothing maps a projected name
back to a card. A CR 201.3 "choose a card name" prompt still draws from the
card reference, so it never offers a stickered name, which is right.

**Position (CR 123.6b).** The object's controller, not the placer, chooses
among the word count plus one positions, and the placement records k. The
prompt is skipped only when every position yields the same name, as for an
object with no name, which takes the words as its name.

**Words and blanks (CR 123.6a).** A word is a maximal run of non-space
characters. A blank (`_____`) is not a word: it stays in the name text, does
not count toward k, and a word inserted after word k goes before any blank that
follows it. `_____-o-saurus` is one hyphenated word and counts. Filed as a
rules question; Wolf in _____ Clothing is the test.

**Several names.** An object with more than one name (a split card off the
stack, Spy Kit) gets the words inserted into each. Filed as a rules question.

**Face-down (CR 708, 123.6b–c).** A face-down object has no name, so its name
is its stickers' words in order; turned face up it is printed name plus
stickers. A face-down card exiled is in a public zone and keeps its stickers;
the test says this is the rules-literal reading.

**Quantities (CR 123.6d, 123.6e).** Letters on a name sticker
(case-insensitive), unique vowels (A, E, I, O, U and Y), the number of name
stickers meeting a letter condition, and words in a name: `Quantity` and
`Filter` arms in the open half. "That sticker" is bound for the rest of the
resolution.

## Unit 3: ability and P/T stickers, ticket costs

**Gathering (CR 123.7, 123.8, 613.1f, 613.4b).** A `stickerGathered` beside
`Projection.counterGathered`, joined into `gatherGiven`, that walks every
public zone, not only the battlefield: an ability sticker is
`Modification.GainAbility` in layer 6 and a P/T sticker is
`SetBasePowerToughness` in layer 7b, each at its placement's timestamp. A P/T
sticker applies to a creature permanent or to a creature or Vehicle card off
the battlefield, and does nothing otherwise (CR 208.3a).

**Ticket costs (CR 123.3c).** A sticker whose cost exceeds the object's
owner's ticket counters is not offered; placing it removes that many. Payment
is part of placement, not a `CostComponent`. Pin Collection's "without paying
its ticket cost" is the placement's cap-and-waive parameter.

**Reading sticker data (CR 123.7a, 123.8a).** "The abilities of ability
stickers on X" reads the stickers' printed abilities, not X's projection, so a
`GainAbilitiesOfStickers` source rather than `GainAbilitiesOfSource`. "The
power of a sticker" reads only a P/T sticker's printed numbers.

## Unit 4: generic producers, sticker kicker, meld and merge

**Generic placement.** With all four kinds live, "put a sticker" of any kind
lands, and the generic producers are data. Scampire adds a graveyard target.

**Sticker kicker (CR 702.33h).** `Keyword.StickerKicker (Cost Keyword)` and a
matching `KeywordFamily` constructor. It counts as kicked for generic readers
but does not satisfy a printed kicker's linked "if kicked" (CR 607; Scryfall
ruling on Wicker Picker). Paying it gets `{TK}` and places a sticker on the
spell just before it is considered cast; on a spell the caster does not own it
places nothing (CR 123.3b). Stack to battlefield is public to public, so the
sticker stays. The `Keyword.Kicker` elision dies.

**Meld and merge (CR 123.5a–c).** A melded permanent takes every component's
stickers in timestamp order; a merging object's stickers join the merged
permanent and are restamped (CR 613.7k). When a melded or merged permanent
moves to a public zone, its owner chooses which resulting object keeps all its
stickers: a prompt at the split in `changeZoneAttaching`. That closes the gap
`docs/superpowers/specs/2026-08-27-meld-design.md` notes.

## Testing

Gameplay-level, one concern each:

- Unit 1: a player with more than three sheets has three drawn, with three or
  fewer keeps them all, with none plays without stickers; Pyrodancer's art
  sticker survives a death to the graveyard and is lost on a bounce to hand; an
  opponent's permanent takes no sticker; a used sticker is not offered again
  until its object reaches a hidden zone; a Clone of a stickered permanent is
  not stickered; Pyrodancer's ability targets only a creature with an art
  sticker; a "stickered" lord turns off when the sticker leaves.
- Unit 2: CR 123.6c's three examples; Baaallerina's target restriction; the
  controller, not the placer, picks the position; unique vowels count Y; a
  blank does not count toward k; a face-down stickered permanent's name is its
  words.
- Unit 3: a P/T sticker sets base P/T under a +1/+1 counter; two P/T stickers
  resolve by timestamp; an ability sticker applies in the graveyard; an
  unaffordable sticker is not offered and a paid one removes tickets; Pin
  Collection waives the cost.
- Unit 4: Ticketomaton places any kind; Wicker Picker's sticker reaches the
  battlefield and does not satisfy a printed kicker's "if kicked"; a meld split
  asks the owner which half keeps the stickers.

Every proving test is mutated away per CLAUDE.md, the gameplay assertion named.

## Does the rules core case on an effect's identity?

No. The core cases on a sticker's kind (name, art, ability, P/T), the
CR 123.1 classification, as it cases on a `CounterKind`. It never reads a
particular sticker's text or a particular sheet: an ability sticker's abilities
are open-half values the projection grants without inspecting them, and the
letter, vowel and word quantities are open-half `Quantity` arms.
`PlayerCounterKind.Ticket` is CR 107.17 vocabulary plus CR 123.3c's payment
rule.
