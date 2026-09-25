# Attractions (#871)

CR 717, 701.51, 701.52, 702.159, 703.4g. Producers: Deadbeat Attendant (open an
Attraction) and Bumper Cars (Visit --- target creature must be blocked this
turn if able).

## Model

- **Lights are printing data.** CR 717.1 gives one name several light
  patterns (Bumper Cars 202a--f), and CR 109.3 excludes them from the
  characteristics, so a copy does not take them. `Printing` gains
  `lights :: Set Natural`, empty for every traditional card. Card files stay
  `Card`s and never carry lights; a deck built in Haskell names the printing.
  `Pawl.Codec.Printing.reference` writes a name only for a printing with no
  lights.
- **The Attraction deck** is `GameState.attractionDecks :: Map PlayerId (Seq
  ObjectId)`: ordered like a library, its objects carrying `Zone.Command` (CR
  717.2) but kept out of `GameState.command`, so that no reader of the command
  zone (emblems, vanguards, conspiracies, commanders) meets a face-down deck
  card. `Deck.attractions` is the multiset a player brings. `Setup.createDeck`
  mints the objects and `newGame` shuffles the deck (CR 103.3a).
- **An Astrotorium back** is classified from the printed card: a `Source.OfCard`
  whose printed faces carry `Subtype.Attraction` (CR 717.1). It is the physical
  card, so the projection is deliberately not read.
- **CR 717.6** is a rules step in `Event.resolveZoneChange`, next to
  `offerCommandZone`: a settled destination other than the battlefield, exile
  or the command zone becomes the command zone. The card lands in
  `GameState.command` face up. That is the junkyard (CR 717.6a), which is not a
  zone, and whose cards do not function (`Vanguard.functionsFromCommandZone`).

## Instructions and events

- `Effect.OpenAttraction` (CR 701.51b): the top of the controller's Attraction
  deck moves through `Event.changeZoneEntering` to the battlefield, so
  replacements and `Moved` behave as for any entry. An empty deck does nothing.
- The roll to visit (CR 703.4g / 717.4 / 701.52a) is a turn-based action in
  `runTurnBasedActions`' precombat-main arm, after the lore counters, for each
  live active-team player who controls an Attraction. It is a real die roll, so
  the CR 706 pipeline in `Effect.RollDie`'s arm (Pixie Guide's extra die,
  Clam-I-Am's reroll, the adjust window) is extracted into one top-level
  function both callers use. It records `DiceRolled`, `DieResultSettled` and a
  new `GameEvent.RolledToVisit (DieResult PlayerId)`.
- `TriggerCondition.Visit` (CR 702.159a) matches `RolledToVisit` when the roller
  controls the bearer, the bearer is an Attraction, and the result is lit on
  the bearer's printing. It triggers from the battlefield only.

## Restart and subgames

- CR 727.2: `startGameFromCards` sends every Astrotorium-back card to its
  owner's Attraction deck rather than the library, and shuffles it.
- CR 729.2a: a subgame takes each player's Attraction deck and shuffles it.
  Face-up Attractions stay in the main game.
- CR 729.5a: at teardown, Attraction cards that began in the deck go back to
  the main-game deck, which is then shuffled.

## Cards and tests

- `data/cards/deadbeat-attendant.json` and `bumper-cars.json`. Bumper Cars'
  effect is `ModifyTarget` granting `Rules {blockRequirements = [AnySubject,
  attacker IsSource]}` until end of turn.
- Gameplay tests: Attendant enters, opens Bumper Cars (202a, lights 2/3/6) from
  a ten-card deck; next precombat main the roll is 3 -> visit triggers and its
  target must be blocked; a roll of 4 -> nothing (the negative, paired board).
  Also: a destroyed Attraction goes to the command zone (CR 717.6), the roll
  honours Pixie Guide (CR 706.6), a restart puts Attractions back in the
  deck, and a subgame takes the deck and gives it back.
