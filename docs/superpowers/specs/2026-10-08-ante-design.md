# Playing for ante (#249)

## The problem

Pawl has no ante zone and no way to change a card's owner. CR 407.3 closes the
category at nine cards; four of them change ownership. `Object.owner` is
written only at construction and never updated, and the engine leans on that:
`Game.removeFromZones` finds a card's library, hand or graveyard by its OWNER,
and "your graveyard" in a `CardLeavesZone` trigger is read as `Filter.OwnedBy`
on the departed card's last known information. CR 108.3 makes ante the only
consumer of legal ownership, so the whole change is rulebook material.

## Scope

Three units, landed in order, this spec committed with the first:

1. **The zone.** `Zone.Ante`, a `GameSettings` ante flag, CR 407.2's setup
   ante, CR 407.4's owner-only ante action, CR 800.4n's departure exception, CR
   407.3's outside-the-game bar. Cards: Contract from Below, Demonic Attorney,
   Rebirth, Jeweled Bird, Amulet of Quoz.
2. **Ownership changes.** Pile membership decoupled from `Object.owner`, a
   set-owner and an exchange-ownership opcode, and a "from anywhere" object
   reference. Cards: Darkpact, Bronze Tablet, Tempest Efreet, Timmerian Fiends.
3. **The ownership report.** CR 407.2's payout to the winner, and a per-game
   report of every card whose owner changed.

Out:

- CR 407.3's deck half (no ante card in a non-ante deck or sideboard) is deck
  validation, which pawl does not do at all (#4458); related to #884.
- A team win's payout (CR 810.8a makes the team win; CR 407.2 names one
  winner). Elided with an issue; the follow-up is a prompt letting the winning
  team assign each card.
- An ownership change to a commander (`Player.commander` is keyed by owner;
  Bronze Tablet would silently strip the designation). Filed, cited at
  `Commander.commanderPrintingOf`.
- "This change in ownership is permanent" beyond the game: pawl models one game,
  never a match (#2997). The report is the whole of it.

## The cards

Oracle verified on Scryfall 2026-10-08; all nine are absent from `data/cards/`.
Each one's first line is "Remove this card from your deck before playing if
you're not playing for ante." Every implementer re-verifies Oracle before
writing the JSON.

| Card | Unit | New beyond the zone |
|---|---|---|
| Contract from Below | 1 | none: discard, ante top of library, draw 7 |
| Demonic Attorney | 1 | none: each player antes top of library |
| Rebirth | 1 | none: "each player may" + `SetLifeTotal` |
| Jeweled Bird | 1 | owner-only ante gating "If you do"; "cards you own from the ante" |
| Amulet of Quoz | 1 | none: opponent may ante, else `FlipCoin` / `LoseGame` |
| Darkpact | 2 | set owner; target card in the ante; exchange it with top of library |
| Bronze Tablet | 2 | set the owner of two exiled cards behind a `PayGate` |
| Tempest Efreet | 2 | exchange ownership; "from anywhere" |
| Timmerian Fiends | 2 | exchange ownership; "from anywhere"; ante as refusal |

Existing pieces they reuse: `MoveToZone`, `ObjectRef.TopOfLibrary`,
`RandomCardInHand` (Merfolk Spy), `PayGate` with a payer (Dash Hopes),
`Optionality` + `Clause.ifTaken` (Eager Construct), `SetLifeTotal`, `FlipCoin`,
`LoseGame`, `PlayerRef.OwnerOfBound`, `Filter.OwnedBy`,
`ObjectRef.EachCardYouOwn`.

## Unit 1: the zone

**Zone.** `Zone.Ante`, public and shared (CR 400.1, 400.2), stored as
`GameState.ante :: Set ObjectId` beside `exile` and `command`. `Codec.Zone` is
`Arm.enum`, so its spelling is derived. Compile-forced sites:
`Game.isHiddenZone`, `Game.zoneMembers`, `Game.insertIntoZone`,
`Codec.InZone.shared`, `Event`'s sacrifice arm, `Reversal.withoutAnnouncement`.
Silent sites, each read and recorded in the PR: `Game.removeFromZones`,
`Setup.startGameFromCards`, `Setup.subgameStateFrom`, `Setup.funnelBack`,
`Setup.gameWith`, `Cast.zoneCandidates`'s `_` arm, `Codec/GameState`'s field
list, `Scenario`'s `Seat`, `ZoneTriggerSpec`'s public-destination rows,
`AbilitySlotLintSpec.isSharedZone`.

**Setting.** `GameSettings.ante :: Bool`, default off (CR 407.1).

**Setup (CR 407.2).** In `Setup.startGameFromCards` and `Setup.newGame`, after
`claimStartingPlayer` and `Companion.reveal`, before `Mulligan.openingHands`:
each player in turn order moves `Game.ask (Prompt.RandomObject library)` into
ante through the zone-change funnel. An empty library antes nothing. Pawl has
already shuffled, so the random card comes from the library rather than the
deck; the two are indistinguishable, and that is an elision with an issue and a
site comment. Living in `startGameFromCards` means a subgame (CR 729.2 follows
rule 103) and a Karn restart (CR 727.1) ante too; `subgameStateFrom` inherits
`settings` already.

**The action (CR 407.4).** A new `Effect.Ante` opcode carrying the anteing
player, since `MoveToZone` names no player and the resolving controller is not
the antee in Demonic Attorney, Rebirth or Amulet of Quoz; a lint bars
`MoveToZone` into ante. The move is refused for an object the anteing player
does not own, and a refused move did not happen, so Jeweled Bird's "If you do"
reads it. Rebirth's "if a player does" is per player, so a seat whose ante is
impossible (empty library) is not offered the choice. Proving board: Jeweled
Bird under Confiscate.

**Departure (CR 800.4n).** `Departure.objectsLeaveWith` excludes
`Object.zone == Zone.Ante`. `Setup.funnelBack` must not take a departed
player's surviving subgame ante card as evidence they are still present.

**Outside the game (CR 407.3).** When `GameSettings.ante` is off, an ante
card (`Face.anteOnly` in its card JSON, tied to its Oracle text by a corpus
lint, never a scan at runtime) is barred beside the CR 315.3 conspiracy
filter in `Event.eligible` and in `Companion.revealable`. Whatever the
setting, `subgameStateFrom.asOutside` does not offer a main-game ante-zone
card: CR 407.3 makes the nine the only cards that remove a card from ante, and
a subgame wish is not one of them. Proving board: Burning Wish in a subgame
started by Shahrazad.

## Unit 2: ownership changes

**Pile membership.** A card's library, hand or graveyard is the pile that
holds it, not its owner's (CR 400.1). `Game.removeFromZones` drops its player
argument and locates the pile by search (`Game.pileHolderOf`), so every caller
that passes `Object.owner` stops passing it: `Event.changeZoneWithCause`,
`Event.unmake`, `Event.forgetObject`, `Sba`'s `ceaseToExist`,
`Departure.objectsLeaveWith` and its cease path, `Setup.applyCrossings` and the
`Setup.createIn*` helpers, `Planechase`'s cease, `Dungeon.remove`,
`Game.withoutBeingCast`. The owner still picks the destination (CR 400.3):
`placeObject (Object.owner obj)` stays, and so does `Resolve/Effect`'s
`Game.sinkInLibrary` call, which reads the library the card has just arrived
in.

**"Your graveyard" reads the pile.** `LastKnown.pile` records the pile holder
the object left. `ZoneChange` does not: every departure reader reaches the
departed id's record, and it has over a hundred positional constructions. A
`CardLeavesZone` gains `whose`, read against `LastKnown.pile`, and the
printings that wrote "your graveyard" or "your library" as a `Filter.OwnedBy`
conjunct move it there; an `OwnedBy` kept for "into your hand" is an arrival
read, which CR 400.3 keeps exact. Before unit 2 the two always agree; between
Tempest Efreet's exchange and its put they do not. Proving board: Kishla
Skimmer under the Efreet's controller, on that player's turn, triggers as the
Efreet leaves their graveyard for the opponent's; one under the opponent, on
the opponent's turn, does not (Kishla reads "during your turn", so the
opponent's must be tested on theirs).

**Opcodes.** `SetOwner` (Darkpact's "You own target card in the ante", and
Bronze Tablet's two writes) and `ExchangeOwnership` of two object references
(Tempest Efreet, Timmerian Fiends), each a write of `Object.owner` on the
current incarnation. Bronze Tablet is two `SetOwner`s and not an exchange: its
2004-10-04 ruling has a stolen Tablet's controller give back only the Tablet
while still taking the other card. `newIncarnation` already carries `owner`
forward. An ownership change is not a zone change and emits no event: unit 3's
report reads `Object.startingOwner`, and the event log is cleared every turn.
Darkpact's "Exchange that card with the top card of your library" is
`ExchangeWithTopOfLibrary`, an exchange of zones (CR 701.12d): the ante card
goes to the top of its (new) owner's library and the top card to the ante, as
one event, and nothing moves unless both can (CR 701.12a). Its target needs
`Pool.CardsInAnte` (CR 115.2).

**"From anywhere."** A new `ObjectRef` that follows a bound object through the
`Moved` log (`ZoneChange.departed` to `ZoneChange.object`) to its current
incarnation, answering nothing once it has left the game. A move with several
arrivals (CR 730.3's merged permanent) ends the chain, an elision with an
issue. Proving board: Timmerian Fiends sacrificed under the artifact owner's
Leyline of the Void, then put from exile into that player's graveyard. Rest
in Peace would exile the final put too, so the Fiends would end in exile
whether or not it was found.

**Commander.** `Commander.commanderPrintingOf` reads the owner's
`Player.commander`; an ownership change to a commander is filed and cited
there, not handled.

**Subgames.** `Setup.funnelBack` rebuilds a departed owner's main-game library
from the parent's copies on the reading that an owner never changes. A card
whose owner changed inside the subgame breaks that for a departed former
owner; telling the copies apart needs the card lineage #4829 needs, so it is
filed and cited there.

## Unit 3: the ownership report

**Starting owner.** `Object.startingOwner`, set at construction to the CR
108.3 owner and carried like `owner` through `newIncarnation`,
`splitComponents`, `funnelBack` and `startGameFromCards`. Tokens are
`Source.OfToken` and outside copies `OfCardCopy`: not cards, never reported.

**Payout (CR 407.2).** When `GameState.result` becomes `Won pid`, every card
in ante has its owner set to `pid` through the unit-2 opcode's write. A draw
pays nothing; every card keeps its current owner. `TeamWon` pays nothing, an
elision with an issue (above). A range-of-influence partial draw is a
departure, and CR 800.4n keeps those players' ante cards in the game for the
eventual winner.

**Report.** `Pawl.Engine.Ownership.changes :: GameState -> Map ObjectId
(PlayerId, PlayerId)`, starting owner to final owner, for every card whose two
differ, read after the payout. `Result` stays `Won | TeamWon | Drawn`: the
subgame readers (`Engine`'s subgame outcome, `Resolve/Effect`'s winner slot)
keep their shape.

**Subgames.** A subgame is a game: its payout runs when its result is set,
before `Setup.funnelBack` returns cards to their owners' main-game libraries
(CR 729.5). Ownership changes made in a subgame persist into the main game,
because CR 729.5 returns each card by owner; CR 729.1b does not stop a card's
owner travelling with it. A Karn restart pays nothing (CR 727.1, no winner),
and CR 727.2 keeps ownership.

## Testing

Gameplay-level, one concern each, in the spec module the implementer's
`docs/adding-a-module.md` reading picks:

- Unit 1: an ante game puts one card from each library into ante before opening
  hands, a non-ante game none; Contract from Below and Demonic Attorney ante the
  top card; Rebirth's decliner keeps their life total; Jeweled Bird under
  Confiscate does not ante and does not draw; Amulet of Quoz's target who antes
  faces no flip; a departed player's ante card stays (CR 800.4n); Burning Wish cannot
  fetch an ante card outside an ante game, nor a main-game ante card from a
  Shahrazad subgame.
- Unit 2: Darkpact's taken card lands on top of its caster's library (a
  zone-only move would send it to its old owner's); the Kishla Skimmer pair
  above; Bronze Tablet's refused payment swaps owners, and a stolen Tablet
  hands back only itself; Timmerian Fiends from exile under Leyline of the
  Void.
- Unit 3: the winner owns every ante card and the report lists them; a draw
  reports none; a Shahrazad subgame's ante goes to the subgame winner's
  main-game library.

Every proving test is mutated away per CLAUDE.md, the gameplay assertion named.

## Does the rules core case on an effect's identity?

No. `Zone.Ante` is CR 400.1's zone list, the setup step and payout are CR
407.2, the owner-only check is CR 407.4 on a zone move, and pile lookup is CR
400.1. The nine cards' effects live in the open half; `SetOwner`,
`ExchangeOwnership` and `ExchangeWithTopOfLibrary` are opcodes the core
classifies, never inspects.
