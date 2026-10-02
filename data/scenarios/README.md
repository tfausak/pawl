# Scenarios

Each `.json` file here, in any subdirectory, is one gameplay test: a board, a
timeline of decisions and checks, and optional final checks. The test suite
runs every one, and `pawl scenario FILE...` runs any (a binary from `cabal
list-bin pawl` needs `pawl_datadir=data` from the repository root). `pawl
schema scenario` prints the schema. A test moved from a Haskell spec goes in a
subdirectory named for that spec; `docs/scenario-burndown.md` lists the ones still to move.

- **Board.** `seats` in turn order, each with a `name`, `life` (default 20),
  player `counters` (`[{ "key": { "type": "Energy" }, "value": 3 }]`), a
  `team` number (CR 808.1), a `range` of influence (CR 801.2a, default
  unlimited), `emperor` (CR 809.2) and
  `battlefield`, `hand`, `graveyard`, `library` and `exile` placements (a
  library top card first; an exiled card face up and linked to nothing). A
  placement names its `card` and may give a `label`, `tapped`, `ready` (CR
  302.6), `damage`, `counters`, `token` (CR 111.1, battlefield only), a
  `controller` and the label of the object or seat it is `attached` to. `active` names a seat;
  `step` is where turn 1 starts; `monarch` optionally names a seat;
  `attackOption` (CR 806.2b) is `MultiplePlayers` unless given, and `null` is
  CR 507.1's choice among every opponent; `brawl` (CR 903.12a),
  `sharedTeamTurns` (CR 805.1) and `deployCreatures` (CR 804.2) are off unless
  given. Setup is a
  state, not a history: nothing placed triggers anything.
- **Note.** An optional `note` says in prose what the scenario rules out and
  why its board is built the way it is. It is free text: nothing reads it, and
  the decoder ignores it like any other unknown key.
- **References.** `"$name"` is a seat or a labelled card; the sigil is not `@`,
  so a label quoted in a PR or issue mentions no GitHub user. `"Goblin Piker"`
  is the first live object with that name, `"Goblin Piker#2"` the second, in
  creation order across every zone.
- **Timeline.** Each entry is keyed by `turn`, `step` and the `player` who
  decides, and carries `do` (a move) or `check`. Entries sharing a key are
  taken in order. An unscheduled priority prompt passes; any other
  unscheduled prompt fails the scenario, as does an entry whose moment never
  comes. A check runs when its player is next offered priority at its key, so
  put a `"Pass"` between a check with a spell on the stack and one after it
  resolves. `source` picks among prompts sharing a key, such as one combat
  damage assignment per attacker. `OrderTimestamps` answers CR 613.7m's
  prompt by naming every object in it, earliest first, and `ChooseOptional`
  answers a "may" with `Exercises` or `Declines`, its `source` the spell
  resolving or the object whose ability is; it answers CR 614.1a's optional
  redirect too, its `source` the object whose replacement it is. `ChooseToPay`
  answers a cost a resolving object offers (CR 118.12a), keyed the same way:
  its `decision` is `Pays` or `Declines`, and paying takes a cast's choices,
  such as `mana`. `ChooseTypeSwap` answers a text change's swap (CR 612.1),
  `{ "from": { "type": "Island" }, "to": { "type": "Swamp" } }`, keyed
  the same way. `ChooseCopyTarget` names what an object entering as a copy
  copies (CR 707.5), `"$bear"`, or `null` to decline its "may", keyed by
  the entering object. `Answer` answers any prompt no move above covers, by
  its kind, `{ "prompt": "ChooseDiscard", "with": ["$card"] }`: an object is
  a reference, a player a seat label, a map an array of `[key, value]` pairs,
  and a recipient or attack target `["Player", "$bob"]`. It is keyed by the
  prompt's decider, or the active player for a prompt nobody decides (a
  shuffle, a die), and answers a prompt raised in the middle of a cast without
  ending it. `ChooseTargets` names each slot's
  targets, `{ "target": ["$moon"] }`, for a target prompt no cast or activation
  of the scenario's own raises (a triggered ability's), keyed the same way; it
  also answers that prompt's announcement of how many. A cast's own `targets`
  likewise announce their count for a slot that takes a variable number. A
  spell or ability with more than one target slot names them with
  `targetsBySlot`, `{ "victim": ["$bear"], "gauge": ["$wall"] }`, in place
  of `targets`.
  `OrderTriggers` lists one player's simultaneous triggers by source, `null`
  for a sourceless one such as the monarch's draw, in the order they go on the
  stack, so the last named resolves first (CR 603.3b).
  An entry carrying `refuse` in place of `do` answers its prompt with a move
  the engine must reverse and ask for again (CR 733.1), such as an illegal
  block declaration; the move standing fails the scenario, and the prompt
  asked again takes the next entry at that key. An illegal `do` is asked
  again too, so turning a `refuse` into `do` proves nothing: a `refuse` is
  proven by changing the board until the move is legal. At priority, a refused
  `Cast`, `PlayLand` or `Activate` the engine does not offer is refused
  already; one it offers is taken, its targets sent to the engine whether
  offered or not, and must leave the stack and the player's hand and
  battlefield as they were.
- **Checks.** `Life`, `Count` (cards of one name in one of a player's zones,
  `Stack` included; the controller's, for the battlefield), `Damage`, `Tapped`, `Counters` (of
  one kind on an object), `Types` (an object's card types, all of them),
  `Attackers` (every attacker and what it attacks), `Blockers` (an attacker's
  blockers, `null` when unblocked), `Defenders` (the defending players, in
  order), `Monarch` (`null` for nobody), `PowerToughness` (an object's
  projected `power` and `toughness`) and `PlayerCounters` (how many of one
  `kind` a `player` has), `Subtypes` (an object's projected subtypes, all of
  them, as bare names) and `Keywords` (how many instances of one `keyword` an
  object has, 0 when it has none), `Names` (an object's projected names) and
  `View`, which renders part of the state as JSON and compares it whole:
  `{ "of": "Stack", "is": ["$bolt"] }`. Its views are `Stack` (top first),
  `Step`, `Offered` (a `player`'s actions at priority, as `"Cast $bolt"`),
  `AttachedTo` and `Controller` (of an `object`), `Result`, `ActivePlayer`,
  `Priority`, `Zone` (a `player`'s `zone`, in order) and `Colors`; an object
  is named as the runner's messages name it, by label while it keeps one.
  Assert combat at `EndOfCombat` or earlier: it is cleared as that step ends.
- **Final.** The run plays whole steps until the timeline is spent, the game
  ends, or the turn passes the last one named; `final` checks that state.

A label names one object, not a card: CR 400.7 makes a card that changes zones
a new object, so a label given in hand goes stale once that card is played.
Name it by card afterwards.
