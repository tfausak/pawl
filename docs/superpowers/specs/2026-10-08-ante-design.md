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
  validation, which pawl does not do at all (#940); related to #884.
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
| Darkpact | 2 | set owner; exchange an ante card with top of library |
| Bronze Tablet | 2 | exchange ownership of two exiled cards behind a `PayGate` |
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

**The action (CR 407.4).** `MoveToZone` to `Zone.Ante` is refused for an
object whose owner is not the instruction's player, and a refused move did not
happen, so Jeweled Bird's "If you do" reads it. Proving board: Jeweled Bird
under Control Magic.

**Departure (CR 800.4n).** `Departure.objectsLeaveWith` excludes
`Object.zone == Zone.Ante`.

**Outside the game (CR 407.3).** When `GameSettings.ante` is off, an ante
card (a flag in its card JSON, never a scan of its
Oracle text) is barred beside the CR 315.3 conspiracy
filter in `Event.eligible` and in `Companion.revealable`. Whatever the
setting, `subgameStateFrom.asOutside` does not offer a main-game ante-zone
card: CR 407.3 makes the nine the only cards that remove a card from ante, and
a subgame wish is not one of them. Proving board: Burning Wish in a subgame
started by Shahrazad.

## Unit 2: ownership changes

**Pile membership.** A card's library, hand or graveyard is the pile that
holds it, not its owner's. `Game.removeFromZones` locates the pile by search
(`Game.pileHolderOf`), and every caller that passes `Object.owner` stops
passing it: `Event.changeZone`, `Event.unmake`, `Event.forgetObject`,
`Sba.ceaseToExist`, `Departure.objectsLeaveWith`'s cease path,
`Setup.applyCrossings`, `Planechase`'s cease, `Dungeon.remove`,
`Game.withoutBeingCast`, `Resolve/Effect`'s `sinkInLibrary`. The owner still
picks the destination (CR 400.3): `placeObject (Object.owner obj)` stays.

**"Your graveyard" reads the pile.** `ZoneChange` and `LastKnown` record the
pile holder the object left, and a `CardLeavesZone` trigger's "your
graveyard" reads it in place of `Filter.OwnedBy`. Before unit 2 the two always
agree; between Tempest Efreet's exchange and its put they do not. Proving
board: Kishla Skimmer under the Efreet's controller triggers as the Efreet
leaves their graveyard for the opponent's; one under the opponent does not.

**Opcodes.** `SetOwner` (Darkpact's "You own target card in the ante") and
`ExchangeOwnership` of two object references (Bronze Tablet, Tempest Efreet,
Timmerian Fiends), each a write of `Object.owner` on the current incarnation.
`newIncarnation` already carries `owner` forward. An ownership change is not a
zone change and emits no `Moved`; it emits a `GameEvent.OwnerChanged` for the
report. Darkpact's "Exchange that card with the top card of your library" is
an exchange of positions: the ante card goes to the top of its (new) owner's
library and the top card to ante, simultaneously.

**"From anywhere."** A new `ObjectRef` that follows a bound object through the
`Moved` log (`ZoneChange.departed` to `ZoneChange.object`) to its current
incarnation, answering nothing once it has left the game. Proving board:
Timmerian Fiends sacrificed under Rest in Peace, then put from exile into the
other player's graveyard.

**Commander.** `Commander.commanderPrintingOf` reads the owner's
`Player.commander`; an ownership change to a commander is filed and cited
there, not handled.

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
  Control Magic does not ante and does not draw; Amulet of Quoz's target who antes
  faces no flip; a departed player's ante card stays (CR 800.4n); Burning Wish cannot
  fetch an ante card outside an ante game, nor a main-game ante card from a
  Shahrazad subgame.
- Unit 2: Darkpact's taken card, later destroyed, goes to the Darkpact
  controller's graveyard (the discriminator against a zone-only move); the
  Kishla Skimmer pair above; Bronze Tablet's refused payment swaps owners;
  Timmerian Fiends from exile under Rest in Peace.
- Unit 3: the winner owns every ante card and the report lists them; a draw
  reports none; a Shahrazad subgame's ante goes to the subgame winner's
  main-game library.

Every proving test is mutated away per CLAUDE.md, the gameplay assertion named.

## Does the rules core case on an effect's identity?

No. `Zone.Ante` is CR 400.1's zone list, the setup step and payout are CR
407.2, the owner-only check is CR 407.4 on a zone move, and pile lookup is CR
400.1. The nine cards' effects live in the open half; `SetOwner` and
`ExchangeOwnership` are opcodes the core classifies, never inspects.
