# Scenarios

Each `.json` file here is one gameplay test: a board, a timeline of decisions
and checks, and optional final checks. The test suite runs every one, and
`pawl scenario FILE...` runs any. `pawl schema scenario` prints the schema.

- **Board.** `seats` in turn order, each with a `name`, `life` (default 20)
  and `battlefield`, `hand`, `graveyard` and `library` placements (a library
  top card first). A placement names its `card` and may give a `label`,
  `tapped`, `ready` (CR 302.6), `damage`, `counters`, a `controller` and the
  label of the object or seat it is `attached` to. `active` names a seat;
  `step` is where turn 1 starts; `monarch` optionally names a seat;
  `attackOption` (CR 806.2b) is `MultiplePlayers` unless given, and `null` is
  CR 507.1's choice among every opponent. Setup is a
  state, not a history: nothing placed triggers anything.
- **References.** `"@name"` is a seat or a labelled card. `"Goblin Piker"` is
  the first live object with that name, `"Goblin Piker#2"` the second, in
  creation order across every zone.
- **Timeline.** Each entry is keyed by `turn`, `step` and the `player` who
  decides, and carries `do` (a move) or `check`. Entries sharing a key are
  taken in order. An unscheduled priority prompt passes; any other
  unscheduled prompt fails the scenario, as does an entry whose moment never
  comes. A check runs when its player is next offered priority at its key, so
  put a `"Pass"` between a check with a spell on the stack and one after it
  resolves. `source` picks among prompts sharing a key, such as one combat
  damage assignment per attacker.
- **Checks.** `Life`, `Count` (cards of one name in one of a player's zones;
  the controller's, for the battlefield), `Damage`, `Tapped`, `Counters` (of
  one kind on an object), `Types` (an object's card types, all of them),
  `Attackers` (every attacker and what it attacks), `Blockers` (an attacker's
  blockers, `null` when unblocked), `Defenders` (the defending players, in
  order) and `Monarch` (`null` for nobody). Assert combat at `EndOfCombat` or
  earlier: it is cleared as that step ends.
- **Final.** The run plays whole steps until the timeline is spent, the game
  ends, or the turn passes the last one named; `final` checks that state.

A label names one object, not a card: CR 400.7 makes a card that changes zones
a new object, so a label given in hand goes stale once that card is played.
Name it by card afterwards.
