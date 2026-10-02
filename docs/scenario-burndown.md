# Scenario burndown

The gameplay tests still written in Haskell that `data/scenarios` could hold,
each tagged with what the scenario format lacks to express it. Tracked by
#4420, related to #146.

A unit of work is one tag: give the format that check, move or board feature
(a type, a codec with a round-trip spec, a runner arm, a README line), then
`grep` the tag below and convert the tests it names. `ready` marks a test
that needs nothing new. Remove the tag from each
line it frees; a line with no tags left is a test ready to convert, and a
converted test's line is deleted.

- `check:*` the test's board and moves already replay as a scenario; only
  what it asserts has no `Check`. `check:helper-*` is a spec-local helper to
  map onto a real check.
- `move:*` the test answers a prompt the timeline has no move for.
- `board:*` the test's starting state holds something a board cannot place.

The list comes from a recorded census of `origin/main` @ 1598871. It is a
starting point, not a verdict: a listed test that turns out to be a unit test
of one subsystem stays in Haskell and loses its line. A `board:*` tag was read
off the recorded run's start state alone, so it misses links and statuses on
what it names; triage a tag's tests from their source before building for it. Tests that drive a
subsystem directly rather than playing a game are not listed.

## Tags

In order of how many tests each would free, highest first. Tags naming a
single spec-local helper, and the `other-*` tail, are left out: grep for them.

- `board:continuous-effect`: a continuous effect already in force
- `board:replacement`: a replacement effect already in force
- `check:events`: event history: not state, so a test reading it stays in Haskell unless it can be re-expressed
- `board:combat`: combat already under way
- `check:legal-targets`: the legal targets for a slot
- `board:face`: a face other than the front
- `board:delayed-trigger`: a delayed trigger already armed
- `board:protector`: a battle's protector
- `board:command`: the command zone
- `move:expect-rejected`: a move the engine is expected to refuse, such as an illegal block or an untargetable cast
- `board:objects-ids`
- `check:prompt-offers`: the options a prompt offers
- `board:sickness`: summoning sickness settled for a player other than the controller
- `board:face-down`: a face-down object
- `check:prompt-payload`: what a prompt offers, beyond its candidates
- `board:entered-with`: linked "entered with" records
- `board:outside-the-game`: cards outside the game
- `check:intermediate-state`: an assertion on a state before the end of the run; split the scenario or check at that moment
- `check:designations`: an object's designations and recorded values (monstrous X)
- `board:exile-linked`: an exiled card linked to what exiled it (CR 607.2a, hideaway, "until it leaves")
- `check:delayed-triggers`: the delayed triggers armed
- `board:player-startingdeck`
- `check:mana-pool`: the mana a cost spent, or what floats after it
- `board:source-ofmerge`
- `ref:stack-ability`: an ability on the stack, which no reference names
- `board:object-bindings`
- `board:dungeons`: dungeons
- `board:daytime`
- `board:source-ofmeld`
- `board:object-designations`
- `check:abilities`: an object's abilities
- `board:stack`: something on the stack
- `check:tapped-count`: how many permanents a player has tapped
- `board:object-attachedto`
- `board:player-effect`: a player effect already in force
- `move:action-ActivateManaAbility`: the `action-ActivateManaAbility` prompt
- `check:face-down`: whether an exiled card is face down
- `check:castable`: whether a card in hand can be cast at all
- `board:graveyard`
- `board:player-commander`
- `board:object-chosencolor`
- `move:nested`: the `nested` prompt
- `board:player-speed`
- `board:second-board`: an assertion made on a second, differently built board
- `board:hypothetical`: a state no sequence of moves reaches
- `check:creature-count`: how many creatures a player controls
- `check:combat`: combat state beyond attackers and blockers
- `move:ChooseCost:unmatchable`: the `ChooseCost:unmatchable` prompt
- `board:player-commandercasts`
- `board:object-classlevel`
- `board:control`
- `move:OrderTriggers-departed-source`: a trigger whose source has left the game, which no reference names
- `board:foretold`: a foretold card
- `check:mana-value`: an object's mana value
- `board:battlefield`
- `move:cast-face`: casting a named face
- `board:player-status`: a player who has left the game
- `check:player-effects`: a player effect in force
- `check:supertypes`: an object's supertypes
- `check:prompt-candidates`: which objects or players a prompt offered
- `move:out-of-range-answer`: an answer outside what the prompt offers, which the engine clamps or ignores and the runner refuses up front
- `ready`: nothing is missing: convert by hand
- `board:player-designations`
- `board:object-chosenplayer`
- `board:attackprohibitions`
- `board:object-turnedoverat`
- `board:object-doesnotuntapfor`
- `board:object-chosennames`
- `move:action-Foretell`: the `action-Foretell` prompt
- `move:action-Plot`: the plot special action
- `move:pile-target`: a target naming a face-down pile
- `check:prompt-count`: how many times a prompt is asked
- `check:continuous-effects`: the continuous effects stored
- `board:blockprohibitions`
- `board:object-goadedby`
- `board:zone-phasedout`
- `board:unregeneratables`
- `board:object-unlockedhalves`
- `move:action-Suspend`: the `action-Suspend` prompt
- `board:schemedecks`
- `move:action-TurnFaceUp`: the turn-face-up special action
- `check:mana-types`: the mana types a permanent can produce
- `check:subtype-member`: whether an object has one subtype, among many
- `check:prompt-order`: the order prompts were asked in
- `check:bindings`: an object's linked bindings
- `check:exile-linked`: which object an exiled card is linked to
- `check:phasing`: whether a permanent is phased out, and how
- `board:plotted`: a plotted card
- `board:haunting`: a card haunting a creature
- `board:exiled-until-monarch`: a card exiled until an opponent becomes the monarch
- `check:plotted`: whether a card is plotted
- `check:lands-played`: how many lands a player has played this turn
- `check:power-toughness-absent`: that an object has no power and toughness
- `check:card-types`: an object's card types
- `move:action-UnlockDoor`: the Room unlock special action
- `move:ChooseManaYield`: the `ChooseManaYield` prompt
- `board:last-known`: last-known information edited by hand
- `board:attraction-deck`: an Attraction deck
- `board:lands-played`: lands already played this turn
- `board:events`: this turn's event history
- `check:playable-from-exile`: an exiled card someone may play
- `check:block-requirements`: the block requirements in force
- `check:sickness`: whether a creature is summoning sick
- `board:exile-cast-permission`: an exiled card someone may cast
- `check:face`: which face an object shows
- `check:tokens`: whether an object is a token
- `check:named-copy-choices`: the cards a copy effect has already named
- `check:player-control`: which player controls another player's decisions
- `check:commander-damage`: the combat damage a commander has dealt each player
- `check:text-changes`: the text changes affecting an object

## Tests

### `ActivateSpec`

- CR 101.1 the ChooseX bound is the greatest toughness among creatures you control | `check:prompt-payload`
- CR 113.7 the Aura itself does not have the ability it grants | `check:abilities` `check:helper-activationsOf`
- CR 113.8 a stolen creature's ability, activated by the new controller, resolves under them | `board:sickness`
- CR 113.8 an activated ability resolves under whoever activated it, not a later controller | `check:continuous-effects`
- CR 118.9 a declined first equip still spends the turn's first | `board:player-designations`
- CR 118.9 the next turn has a first equip again | `board:player-designations`
- CR 118.9 whole card: the first equip may be paid with {0}, and the second may not | `board:player-designations`
- CR 118.9b the player may decline the alternative and pay the printed cost | `board:player-designations`
- CR 302.6 a stolen creature's {T} ability is not offered to the thief | `check:helper-isActivate`
- CR 307.5 the Desert's ping is NOT offered in the postcombat main phase | `check:other-Combat.Type.attacked`
- CR 400.3 whole card: the granted {T} bounces a nonland permanent to its OWNER's hand | `board:continuous-effect`
- CR 400.7 / 602.5b the permanent that returns may activate it again | `check:helper-activationsOf` `check:helper-isOnlyOnce`
- CR 506.7b/g the rider opens at the declaration and runs to the end of the combat phase | `board:hypothetical` `check:other-Turn.afterBlockersDeclared`
- CR 513.2 the encore tokens are sacrificed at the beginning of the next end step | `check:delayed-triggers`
- CR 601.2b the ChooseX bound is the energy the player can spend | `check:prompt-payload`
- CR 601.2b/611.2d an activated {X} pump freezes the announced X into the stored effect | `board:face`
- CR 601.2f a matching activation before the Hojo arrived was the first | `board:continuous-effect`
- CR 601.2f an unfloored reduction takes the mana a floored one may not | `board:continuous-effect`
- CR 601.2f the next turn has a first activation again | `board:continuous-effect`
- CR 601.2f the payer picks which reduction applies first | `board:continuous-effect`
- CR 601.2f the reduction reaches only the equip that targets the Mauler | `check:mana-pool`
- CR 602.2 an X past the energy on hand is a no-op | `move:out-of-range-answer`
- CR 602.2 an ability with no timing rider is still offered during combat | `check:helper-isActivate`
- CR 602.2a cycling from hand reveals the Mauler as the ability is announced | `check:helper-revealed` `check:events`
- CR 602.5b the permanent's other ability is untouched | `check:helper-activationsOf` `check:helper-isOnlyOnce`
- CR 608.2h the value is LAST KNOWN, not printed: a pumped Fire-Eater deals 5 | `board:continuous-effect`
- CR 611.2 without the spell the creature has its printed ability alone | `check:abilities` `check:helper-helixSorcerer` `check:helper-helixState`
- CR 612.1 whole card: hacking Arbor Elf moves which land its ability may target | `check:legal-targets`
- CR 612.2a an evolved Presence of Gond's granted ability mints a Goblin Warrior Token | `board:continuous-effect`
- CR 613.1f the enchanted creature has the granted ability ALONGSIDE its printed one | `check:abilities` `check:helper-grantedAbility` `check:helper-theAbility`
- CR 613.7 Humility AFTER the Aura takes the granted ability with the rest | `check:abilities`
- CR 613.7 Humility BEFORE the Aura leaves the granted ability standing | `check:abilities` `check:helper-theAbility`
- CR 701.20a a search that finds nothing reveals nothing | `check:helper-revealsOf` `check:events`
- CR 701.20a basic landcycling reveals the Forest it fetches | `check:helper-revealsOf` `check:events`
- CR 701.20a whole card: Braidwood Sextant fetches a Forest and reveals it | `check:events`
- CR 702.142a the boast ability is offered only once this creature has attacked | `check:other-ActivatedAbility.keyword`
- CR 702.49a the minted cost is the printed one plus the return | `check:other-ActivatedAbility.cost` `check:other-Face.activatedAbilities`
- CR 702.57b whole card: Steeling Stance's forecast pumps once and is refused for the rest of the turn | `check:helper-revealsOf` `check:events`
- CR 702.84a the unearthed permanent that dies is exiled instead | `board:continuous-effect` `board:delayed-trigger` `board:entered-with` `board:graveyard` `board:replacement`

### `ActivationProhibitionSpec`

- CR 400.7 whole cards: a Troll bounced by Unsummon and replayed the same turn is no longer prohibited | `check:other-GameState.activationProhibitions`
- CR 602.2 whole cards: the Troll Deadlock Trap named cannot be activated, and its twin can | `check:other-GameState.activationProhibitions`

### `ArchenemySpec`

- CR 205.4h an ongoing scheme stays face up and its static ability applies | `board:battlefield` `board:objects-ids` `board:player-startingdeck` `board:schemedecks`
- CR 701.33 an ongoing scheme's own trigger abandons it | `board:battlefield` `board:objects-ids` `board:player-startingdeck` `board:schemedecks`
- CR 904.9 the archenemy sets a scheme in motion, and CR 704.6e abandons it once its trigger resolves | `board:objects-ids` `board:player-startingdeck` `board:schemedecks`

### `AttackKeywordTriggerSpec`

- CR 500.5a the stored requirement lasts exactly the combat phase | `check:block-requirements`
- CR 511.2 the delayed ability sacrifices it at end of combat | `check:delayed-triggers`
- CR 702.100a a 0/1 entering ties both axes and evolves nothing | `check:helper-countersOn`
- CR 702.100a an opponent's creature entering is not a trigger at all | `check:helper-countersOn` `check:helper-resolveAll`
- CR 702.116a at four seats picking nobody arms no exile | `check:delayed-triggers`
- CR 702.147a a decayed creature that did not attack is not sacrificed | `check:delayed-triggers`
- CR 702.147a attacking arms one delayed ability | `check:delayed-triggers`
- CR 702.149a a companion whose power is only equal exiles nothing | `board:continuous-effect`
- CR 707.2 a Clone of the Patrol has myriad and mints a copy of the Patrol | `board:sickness`

### `AttractionSpec`

- CR 701.51b Deadbeat Attendant puts the top of the Attraction deck onto the battlefield | `board:attractiondecks` `board:objects-ids`
- CR 701.51c The Most Dangerous Gamer enters, opens an Attraction, and gets a counter | `board:attraction-deck` `board:replacement`
- CR 701.51c an opponent opening an Attraction does not trigger "you" | `board:attractiondecks` `board:objects-ids`
- CR 701.52a the roll to visit is a die roll: Pixie Guide adds a die and CR 706.6 ignores the lower | `board:continuous-effect`
- CR 702.159a a result lit up on the Attraction triggers its visit ability | `board:continuous-effect`
- CR 703.4g the turn machinery itself rolls as the precombat main phase begins | `check:prompt-payload`
- CR 707.2 a copy of an Attraction rolls but lights nothing, and is no Astrotorium card | `check:events`
- CR 717.1 the lights are the printing's | `board:continuous-effect`
- CR 717.2 the Attraction deck is in the command zone, not the library or the starting deck | `check:other-Attraction.deckOf` `check:other-Game.zoneOf` `check:other-GameState.command`

### `AuraSpec`

- CR 109.5: the activator keeps the pay-to-end offer after Confiscate steals the animated Licid | `board:stage-a-placement-cannot-be-attached-to-o6`
- CR 205.2a an Aura on a creature moves only to another creature | `board:object-attachedto`
- CR 205.2a an Aura on a land moves only to another land | `board:object-attachedto`
- CR 301.5d Vulshok Battlemaster takes every Equipment, and bob keeps control of his | `check:helper-hostOf`
- CR 303.4d whole cards: an Aura made a creature unattaches, then is buried | `board:continuous-effect`
- CR 303.4e whole cards: Aura Graft takes bob's Control Magic and the creature it moves onto | `check:other-Object.sickness`
- CR 303.4j whole cards: Crown of the Ages cannot move Setessan Training onto an opponent's creature | `check:other-Object.timestamp`
- CR 608.2h Fumble takes the Auras and Equipment that were attached to the bounced creature | `check:intermediate-state`
- CR 608.2h whole card: an Auratouched Mage killed in response still finds the Aura and reveals it to hand | `board:continuous-effect`
- CR 613.1b the host's controller is the projected one, not its owner | `board:sickness`
- CR 613.1b/704.5m Control Magic keeps a crewed Vehicle and loses it the instant the crew wears off | `board:stage-a-placement-cannot-be-attached-to-o0`
- CR 613.8a/613.8b a permanent already stolen from the enchanted player is not handed over again | `check:other-Combat.canAttack`
- CR 613.8b two Confiscates enchanting each other apply in timestamp order | `board:object-attachedto`
- CR 614.1c whole card: Convincing Mirage makes a Mountain the chosen basic land type | `move:action-ActivateManaAbility`
- CR 701.3a equipping again moves the Equipment off the first creature | `check:helper-attachedTo`
- CR 701.3a whole card: Auratouched Mage finds only an Aura that could enchant it | `check:prompt-offers`
- CR 701.3a whole card: Sovereigns of Lost Alara finds the Aura that could enchant the creature its trigger bound | `move:out-of-range-answer` `check:other-Attach.attachableWithLastKnown`
- CR 701.3b with only its own host available the Aura does not move and is not restamped | `check:helper-attachedTo` `check:other-Object.timestamp`
- CR 701.3c attaching to a different creature restamps; re-attaching to the same one does not | `check:other-Object.timestamp`
- CR 701.3c moving an Aura to a different creature restamps it | `check:helper-attachedTo` `check:other-Object.timestamp`
- CR 702.103f: when its host dies the bestowed Aura stays, unattached, and is a 1/1 Satyr creature again | `board:stage-a-placement-cannot-be-attached-to-o5`
- CR 702.5a whole card: Cloudform becomes an Aura with a granted enchant ability and attaches to what it manifests | `check:other-Card.foldEnchant` `check:other-Projection.enchantOf` `check:other-TargetSlot.required`
- CR 702.5c: a copied trigger's second granted enchant still admits the returned creature | `ref:stack-ability` `check:abilities`
- CR 702.5d whole card: Curse of Death's Hold enters attached to the player it targeted and shrinks that player's creatures | `check:intermediate-state`
- CR 702.6a equipping attaches the Equipment to the target creature | `check:helper-attachedTo`
- CR 704.5m: Cloudform is buried with the creature its granted enchant ability let it hold | `board:stage-a-placement-cannot-be-attached-to-o8`
- with no permanent it can enchant the Aura does not move, and alice still gains it | `check:other-Object.timestamp`

### `BattleSpec`

- CR 310.12b / 118.9 / 712.11a she may then cast it TRANSFORMED and FREE | `board:face` `board:protector`
- CR 310.12b / 704.5v three plus two defeats it, and it is EXILED | `board:face` `board:protector`
- CR 310.12b declining the offer leaves the card in exile | `board:face` `board:protector`
- CR 310.5 / 508.1b a creature is declared as attacking the battle | `board:combat` `board:face` `board:protector`
- CR 310.6 / 510.1b combat damage to a battle removes them too | `board:combat` `board:face` `board:protector`
- CR 310.6 Lightning Bolt takes three defense counters off it | `board:face` `board:protector`
- CR 310.6 the FIRST of those two spells defeats nothing | `board:face` `board:protector`
- CR 310.9b the identical announcement is refused when the protector is not the defending player | `board:combat` `board:face` `board:protector`
- CR 310.9c a creature the protector does not control can't block the battle's attacker | `board:combat` `board:face` `board:protector`
- CR 310.9c and one the protector does control blocks it | `board:combat` `board:face` `board:protector`
- CR 310.9d / 508.5 the defending player of a creature attacking a battle is the battle's protector | `board:combat` `board:face` `board:protector`
- CR 310.9d the battle's CONTROLLER's lands are not the ones read | `board:combat` `board:face` `board:protector`
- CR 310.9d whole card: Synthetic Bulwark Snare reaches a creature attacking a battle you PROTECT | `board:combat` `board:face` `board:protector`
- CR 506.4 a battle that has left the battlefield is assigned no combat damage | `board:combat` `board:face` `board:protector`
- CR 506.4 a battle that stops being a battle and becomes one again stays removed from combat, so the Snare still cannot name the attacker | `board:combat` `board:face` `board:protector`
- CR 506.4 a battle that stops being a battle stops being attacked, so the Snare cannot name the attacker | `board:combat` `board:face` `board:protector`
- CR 506.4 an attacker that leaves the battlefield stops attacking, so CR 704.5x repairs at once | `board:combat` `board:face` `board:protector`
- CR 506.4 whole cards: a Ninja put onto the battlefield attacking the Siege is removed when bob steals it | `board:combat` `board:face` `board:protector`
- CR 506.4 whole cards: a Word of Seizing on the attacked Siege stops it being attacked | `board:combat` `board:face` `board:protector`
- CR 506.4c / 508.5 the same block stays illegal once the Siege has left the battlefield | `board:combat` `board:face` `board:protector`
- CR 506.4c whole cards: a Ninja returning a Piker whose Siege was stolen attacks nothing | `board:combat` `board:face` `board:protector`
- CR 508.3a the same creature declared at the battle's PROTECTOR is silent | `board:combat` `board:face` `board:protector`
- CR 508.3a whole card: Thrashing Frontliner declared at the Siege gets +1/+1 | `board:combat` `board:face` `board:protector`
- CR 508.4 the other road into combat records the same two seats | `board:combat` `board:face` `board:protector`
- CR 508.5 the departed battle's CONTROLLER is not the seat that is read | `board:combat` `board:face` `board:protector`
- CR 508.5 whole cards: a protector who steals the attacked Siege keeps the block | `board:face` `board:protector`
- CR 702.14c and the same three-seat board with the lands swapped leaves the block legal | `board:face` `board:protector`
- CR 702.14c the same attack is blocked normally when the protector's land is an Island | `board:combat` `board:face` `board:protector`
- CR 702.14c the same removal with an ISLAND leaves the block legal | `board:combat` `board:face` `board:protector`
- CR 704.3 and the pass that buries it reports that an action was performed | `board:face` `board:protector`
- CR 704.5v a battle at defense 0 with nothing pending is named by the state-based action | `board:counters` `check:other-Battle.defeated`
- CR 704.5v and is NOT while its defeat ability is still owed a resolution | `board:counters` `check:intermediate-state` `check:other-Battle.defeated` `check:other-Battle.awaitingAbility`
- CR 704.5w a NON-Siege battle at defense 0 is buried anyway | `board:counters` `check:intermediate-state` `check:other-Battle.defeated` `check:other-Battle.awaitingAbility`
- CR 704.5x a battle that IS being attacked keeps a protector who has left | `board:combat` `board:face` `board:protector`
- CR 704.5x the repair reports that an action was performed | `check:other-Sba.performStateBasedActions`
- CR 704.5x the same concession on the same board DOES repair it when nothing attacks | `board:combat` `board:face` `board:protector`
- CR 704.5y whole cards: a protector who steals the battle stops being its protector | `board:face` `board:protector`
- CR 802.2a the departed battle's attacker reads its PROTECTOR, not the first defending player | `board:face` `board:protector`

### `BoardEffectSpec`

- CR 109.2 Corrosive Gale deals X to each creature with flying, and none to the one without | `check:intermediate-state`
- CR 109.2b/701.6a counters every other spell on the stack and draws for what it countered | `board:stack`
- CR 109.5 no other player's library is touched | `check:helper-sortedNames`
- CR 111.3 the Horror is an X/X where X is the number of creatures DESTROYED | `check:helper-horrorPrintedPower`
- CR 113.6g removing the uncounterable spell leaves the count unchanged | `board:stack`
- CR 113.9 an abilities-only sweep counters both opponents' abilities and lets their spell resolve | `board:face-down`
- CR 113.9 with only abilities countered, the Faeries still come and Baral stays silent | `board:stack`
- CR 206.3c destroys the listed nontoken permanents, through a shield, and nothing else | `board:replacement`
- CR 302.6 the newly gained enchantments are re-Sicked and the one alice already controlled is not | `check:other-Object.sickness`
- CR 401.2 an empty library has no top card, so the exile does nothing | `check:helper-namesIn`
- CR 401.2 the top three cards of your library are exiled, and the rest stay put | `check:helper-permissionsIn`
- CR 405.1/701.6a counters an opponent's abilities beside their spells, and makes a Faerie for each | `board:stack`
- CR 514.2 the window closes at the end of the turn it named | `check:other-Object.playableFromExile`
- CR 603.7 the delayed ability sacrifices what came back at the caster's next end step | `board:delayed-trigger` `board:entered-with` `board:graveyard`
- CR 608.2d the delayed +1 untaps the two lands named, of five offered | `check:prompt-payload`
- CR 609.3 X above the library's size exiles what it has | `check:helper-permissionsIn`
- CR 609.3 a library shorter than the depth gives up what it has | `check:helper-permissionsIn`
- CR 609.3 a one-card library gives up its one card | `check:helper-permissionsIn`
- CR 611.2c a creature that becomes attacking after the spell resolves is not in the set | `check:helper-affected`
- CR 611.2c an enchantment that enters after the trigger resolves is not stolen | `move:other-addPermanent`
- CR 611.2c the stored control effect holds the swept ids, not the filter that swept them | `check:helper-affectedSets`
- CR 611.2c the stored effect holds the swept ids, not the filter that swept them | `check:helper-affectedSets`
- CR 701.12b the source and its one target swap controllers | `check:other-Combat.legalAttackers`
- CR 701.12b two creatures of different controllers swap controllers | `check:other-Combat.legalAttackers`
- CR 701.19a a regenerated permanent is not destroyed and not counted | `board:replacement`
- CR 701.21a draws one card for each permanent she named and sacrificed | `check:prompt-options`
- CR 702.26d phasing the source out is not leaving, so nothing returns | `board:exile-linked` `board:zone-phasedout`

### `CardTriggerSpec`

- CR 109.5 you control: an opponent's 2/1 entering gives alice nothing | `check:helper-experienceOf`
- CR 122.1 five experience counters put five, and "another" keeps Ezuri off her own trigger | `check:legal-targets`
- CR 508.3d a declare attackers step with no attackers adds nothing | `check:helper-bobsTurn` `check:helper-sentAt`
- CR 508.3d a declare attackers step with no attackers is not attacking | `check:helper-answering` `check:helper-sentAt`
- CR 508.3e a Seifer the defending player controls is silent | `check:other-Goad.goadedBy`
- CR 508.3e attacking a planeswalker that player controls leaves it silent | `check:other-Goad.goadedBy`
- CR 508.3e the attacked player's 3 follows the announcement | `check:helper-answering` `check:helper-fired` `check:helper-lives` `check:helper-sentAt`
- CR 508.3e the attacker and the attacked player are told apart | `check:helper-answering` `check:helper-fired` `check:helper-lives` `check:helper-sentAt`
- CR 603.4 raid: an attack aimed at a planeswalker by a creature that then died still discards | `check:combat`
- CR 603.4 the clause fails when the declaration went at a third player | `check:events`
- CR 603.7 Ray of Command whole card: the borrowed creature is TAPPED when control reverts at cleanup, and Act of Treason's is not | `board:continuous-effect` `board:delayed-trigger`
- CR 702.170a plotting another card does not fire an exiled Aloe Alchemist | `check:events` `move:action-Plot`
- CR 725.2/109.5 a crown stolen by carol does not fire alice's trigger | `check:creature-count` `check:events` `check:helper-combatDamageTo` `check:helper-targetsPlayer` `check:other-S.soleFaceName`
- CR 725.3 a player who is ALREADY the monarch does not become the monarch, so the Lich's edict stays silent | `check:creature-count` `check:events` `check:helper-targetsPlayer`

### `CaseSpec`

- CR 719.3a the condition gates the solve | `board:replacement`
- CR 719.3a the fourth cast does not solve it by itself | `board:replacement`
- CR 719.3b the Case stays solved into a turn that casts nothing | `board:replacement`
- CR 719.3c the solved ability functions only once the Case is solved | `board:replacement`

### `CastPermissionSpec`

- CR 109.5 one player's Exploration does not raise another's allowance | `check:other-PlayerEffect.landPlaysAllowed`
- CR 109.5 the You scope does not reach bob's graveyard | `check:other-PlayerEffect.mayCastFrom`
- CR 113.9 with Prowling Serpopard alice's activated ability is still counterable | `ref:stack-ability`
- CR 113.9 without Spider-Punk bob's Stifle counters alice's ability | `ref:stack-ability`
- CR 305.1 / 614.1a a graveyard land turned away by its own sacrifice still spends the use | `check:helper-arrivedBetween` `check:helper-namesIn` `check:helper-pbBuried`
- CR 305.1 beside Crucible of Worlds the player chooses whether a graveyard land is played under Serra Paragon | `board:continuous-effect`
- CR 400.1 the grant does not reach the copy in bob's graveyard | `check:other-PlayerEffect.mayCastFrom`
- CR 400.7b / 611.3d the creature cast off the top can attack the turn it resolves | `board:continuous-effect`
- CR 514.2 the cleanup sweep drops an unused WhenUsed grant | `check:other-GameState.playerEffects` `check:other-Expiry.dropAtCleanup`
- CR 514.2 the permission ends at cleanup | `check:other-GameState.playerEffects` `check:other-PlayerEffect.mayCastFrom`
- CR 601.2e a rejected cast leaves the grant standing | `board:player-effect`
- CR 601.2e an announced X that leaves the spell even takes the cast back | `move:expect-rejected`
- CR 601.3 / 118.8 the linked creature is cast by removing three counters | `board:exile-linked`
- CR 601.3 a cast made while searching spends the grant | `ready`
- CR 601.3 the budget comes back at the turn handoff | `board:replacement`
- CR 601.3 the use comes back on her next turn | `board:continuous-effect`
- CR 611.2 resolving stores one MayPlayAsThoughItHadFlash grant expiring on use | `check:other-GameState.playerEffects` `check:other-ActivePlayerEffect.effect` `check:other-ActivePlayerEffect.expiry`
- CR 613.1f The Eighth Doctor's quoted replacement exiles the permanent it was granted to | `board:continuous-effect`
- CR 701.6a / 113.9 with Spider-Punk the ability survives the Stifle and resolves | `ref:stack-ability`
- CR 708.2a a face-down cast spends the grant off the face the gate read | `board:player-effect`

### `CastProhibitionSpec`

- CR 101.4 the active player is asked to name a card first | `ready`
- CR 101.4b the later chooser knows the name the earlier one chose | `ready`
- CR 102.2 an answer naming no opponent falls back to the head of the offer | `ready`
- CR 102.2 at three seats the controller picks which opponent names a card | `ready`
- CR 201.4 a name no card has is refused and the chooser is asked again | `ready`
- CR 201.4 a slug the registry answers to is not a card's name | `ready`
- CR 201.4a a real card the restriction forbids is refused and the chooser is asked again | `ready`
- CR 201.4a the printed restriction reaches both choosers | `ready`
- CR 305.1 a land with the chosen name can't be played, and a basic land still can | `ready`
- CR 305.1 a land with the chosen name can't be played, and an unnamed one still can | `ready`
- CR 514.2 the hexproof outlives the cleanup of the turn it was cast in | `check:player-effects` `move:expect-rejected`
- CR 514.2 the prohibition ends at cleanup | `check:other-GameState.playerEffects`
- CR 514.2 the restriction ends at cleanup | `check:helper-offersCast` `check:other-GameState.playerEffects`
- CR 601.2c the restriction lands on the targeted seat alone | `check:other-ActivePlayerEffect.scope` `check:other-GameState.playerEffects` `check:helper-offersCast`
- CR 601.3 a spell with the chosen name can't be cast, and its neighbour still can | `ready`
- CR 601.3 the opponent's chosen name prohibits the controller too | `ready`
- CR 604.2 destroying the Chamber lifts both prohibitions | `board:object-chosennames`
- CR 611.2a each opponent is barred during their own next turn and no other | `board:player-effect`
- CR 611.2a it survives bob's turn and carol's turn, and ends as alice's next turn begins | `check:player-effects` `move:expect-rejected`
- CR 611.2b a sweep with the Swamp still there changes nothing | `check:helper-conditionalSilenceCasts` `check:other-GameState.playerEffects` `check:other-Expiry.sweepConditional`
- CR 611.2b it is stored while the condition holds, and stops both opponents | `check:other-Expiry.While` `check:other-PlayerEffect.prohibitsCasting` `check:helper-conditionalSilenceCasts`
- CR 611.2b the rewritten clause counts Islands, so losing them ends it | `board:sickness`
- CR 611.2b unhacked, with no Swamp the duration never starts | `check:helper-conditionalSilenceCasts` `check:other-GameState.playerEffects`
- CR 611.2b when the Swamp changes hands the effect is deleted | `board:sickness`
- CR 611.2b with no Swamp the duration never starts and nothing is stored | `check:other-Stack.resolveTop` `check:other-GameState.playerEffects` `check:helper-conditionalSilenceCasts`
- CR 611.2c a spell with the name the resolution chose can't be cast, and its neighbour still can | `ready`
- CR 612.1/612.2 an Artificial Evolution on Liliana moves the -3 onto the new word | `check:player-effects` `move:expect-rejected`
- CR 612.7 / 206.3a a Spy Kit host has the Arabian Nights names of cards the game has never seen, and City in a Bottle sweeps it | `board:object-attachedto`
- CR 612.7 / 702.16e a Spy Kit host has the chosen name of a card in no zone, and its damage is prevented | `board:combat` `board:object-attachedto` `board:object-chosennames`
- CR 612.7 / 709.4a a Spy Kit host has both names of a split card with a creature half | `board:combat` `board:object-attachedto` `board:object-chosennames`
- CR 613.10 both prohibitions reach the opponent, not only the controller | `ready`
- CR 614.1c both the controller and an opponent name a card as it enters | `ready`
- CR 614.1c the controller alone names a card as the Halo enters | `ready`
- CR 702.11c once it resolves, alice has gained 2 and bob's Bolt cannot reach her | `check:player-effects` `move:expect-rejected`
- CR 702.16b the protected player is not a legal target for a spell with the chosen name | `ready`
- CR 702.16c / 704.5m an Aura already enchanting the player is buried once she gains protection from its name | `ready`
- CR 702.16e combat damage from a source with the chosen name is prevented, and the same attacker otherwise connects | `board:combat` `board:object-chosennames`

### `CastRestrictionSpec`

- CR 305.1 alice may play a land out of bob's hand | `board:player-effect`
- CR 601.3 alice casts the Bolt she picked out of the Shell's pile | `board:exile-linked` `check:tapped-count`
- CR 601.3 the same pile casts the Doom Blade when she picks that instead | `board:exile-linked` `check:tapped-count`

### `CastSpec`

- CR 107.3a a granted flashback {X}{R} announces X rather than treating it as 0 | `check:other-Cost.costsFor` `check:other-Cost.Type.mana` `check:helper-theRed`
- CR 201.2 the trigger's slot offers the same-named graveyard cards and no other | `move:ChooseCost-offered-cast`
- CR 302.6 settling does not touch the other player's permanents | `check:helper-sicknessOf` `check:other-GameState.objects` `check:other-Object.sickness`
- CR 302.6 the untap step settles the active player's permanents | `check:helper-sicknessOf`
- CR 400.7g/601.2h Altar of the Lost pays for a granted flashback and not for a hand cast | `board:continuous-effect`
- CR 400.7j Crabomination offers a spell from its own three exiled cards alone | `check:prompt-offers`
- CR 500.5 no mana floats at the end of a game | `board:player-startingdeck` `move:policy-answerer`
- CR 601.2a the spell is already on the stack when CR 601.2c announces its targets | `check:prompt-offers`
- CR 601.2c casting a Bolt stamps the chosen target on the stack object | `check:other-Binding.targetsOf`
- CR 601.2i casting a spell records a SpellCast event for the caster | `check:events`
- CR 603.12/603.3d a reflexive ability's target is chosen as IT goes on the stack, after the payment | `check:delayed-triggers`
- CR 608.2b a spell whose only target came from spliced text does not resolve once it is illegal | `move:other-changeZone` `check:mana-pool`
- CR 608.2h a Scrapshooter killed in response still has the promised opponent draw | `board:continuous-effect`
- CR 612/305.6 a hacked basic Mountain taps for its new color | `check:mana-types`
- CR 702.117a after alice's own Bolt the Salvo is cast for {1}{R}; after nobody's and after bob's, its {4}{R} is unpayable | `move:expect-rejected`
- CR 702.119c an Ashnod's Altar that eats the chosen creature reverses the cast | `move:ChooseCost:unmatchable`
- CR 702.152a blitzed, the Requisitioner attacks, is sacrificed at the next end step and draws a card; hard-cast and bolted it draws nothing | `move:OrderTriggers-departed-source`
- CR 702.173a an Assassin's or a commander's combat damage buys the Vision for {1}{U}; an ordinary creature's does not | `board:player-commander`
- CR 702.180b tapping the chosen creature for mana reverses the cast | `move:ChooseCost:unmatchable`
- CR 702.185c a spell warped this turn makes Insatiable Skittermaw's end step trigger; the same Colossus cast for {9} does not | `move:OrderTriggers-departed-source`
- CR 702.34a a countered flashback spell is exiled too | `move:other-counter`
- CR 702.34a a granted flashback priced at the card's own mana cost is payable for that cost | `check:helper-theRed` `check:other-Cost.Type.mana` `check:other-Cost.costsFor` `check:other-Game.cardOf`
- CR 702.34a the exile replacement is scoped to the spell itself | `move:other-changeZone`
- CR 702.34a the flashback cost exiles the card; the permission's printed cost does not | `board:player-effect`
- CR 702.34a the granted cost, not the printed one, is what a graveyard cast pays | `board:continuous-effect`
- CR 702.34a/113.6f the grant creates the cast-from-graveyard permission, not just a price | `check:other-Cost.costsFor` `board:second-board`
- CR 702.34a/601.2b two flashback abilities offer two costs, and either one exiles the card | `board:continuous-effect`
- CR 702.35a declining the cast puts the exiled card into its owner's graveyard | `check:intermediate-state`
- CR 702.37c a granted permission offers Skirk Marauder face down while searching | `check:prompt-offers`
- CR 702.48a / 118.9d a Patron cast free by cascade is still offered the Goblin sacrifice | `move:ChooseCost-offered-cast`
- CR 702.48a in bob's turn alice casts the Patron by sacrificing a Goblin, reduced by its mana cost | `board:object-bindings`
- CR 707.10 a Double Major copy of a dashed Scout attacks and is returned | `move:OrderTriggers-departed-source`
- CR 707.2 / 400.7 a Clone of a dashed Scout and the Scout flickered neither have haste nor return | `board:delayed-trigger` `board:object-castusing`
- CR 707.2 a graveyard card that is a copy is priced at the copy's mana cost | `check:other-Cost.costsFor` `check:other-Cost.Type.mana`
- a casting game conserves objects | `board:player-startingdeck` `move:policy-answerer`
- a casting game still terminates | `board:player-startingdeck` `move:policy-answerer`
- casting actually happens in a full game | `board:player-startingdeck` `move:policy-answerer`
- resolving an empty stack is a no-op | `check:other-Stack.resolveTop`

### `ClashSpec`

- CR 101.4b the later clashing player knows the earlier one's decision | `check:prompt-payload`
- CR 701.30d the controller's higher mana value wins the clash | `check:events` `check:prompt-payload`

### `ClassSpec`

- CR 604.2 a copy's own as-long-as clause is still gated once the original is exiled | `check:designations`
- CR 707.2a a copy acquires the copied object's player abilities | `check:mana-pool`
- CR 707.2a a copy acquires the copied object's static abilities | `check:designations`
- CR 716.2a / CR 120.1 the level-3 trigger has the spell deal damage by instants and sorceries cast | `board:object-classlevel`
- CR 716.2a / CR 508.3d the level-3 trigger pumps by each OTHER attacker and grants double strike | `board:object-classlevel`
- CR 716.2a / CR 603.10 the level-2 section's trigger fires on the very activation that grants it | `board:object-classlevel`
- CR 716.2a the level-2 trigger does not fire again when the Class goes from 2 to 3 | `board:object-classlevel`
- CR 716.2a the level-3 section is off while the Class is level 2 | `board:object-classlevel`
- CR 716.2b a Class retains its level even if it stops being a Class | `board:object-classlevel`
- CR 716.2b levels are not a copiable characteristic | `check:designations`
- CR 716.2c Sorcerer Class's mana pays to gain a Class level | `board:object-classlevel`

### `CodecIntegrationSpec`

- a cast on an earlier turn round trips | `check:other-GameState.castsBeforeThisTurn` `check:codec-round-trip`

### `CoinSpec`

- CR 705.2 only the flipping player calls, and calls before the coin comes up | `check:prompt-order`

### `ColorSpec`

- CR 105.3 Moonlace makes a blue SPELL colourless, so Red Elemental Blast can no longer counter it | `check:legal-targets`
- CR 113.1c Painter's Servant does not colour an ability on the stack | `board:object-chosencolor`
- CR 114.3 a Koth emblem's damage lands through protection from the colour Painter's Servant named | `board:object-chosencolor`
- CR 115.5 the blast is a blue spell on the stack and still cannot counter itself | `check:legal-targets`
- CR 604.3 Red Elemental Blast counters a devoid SPELL that Painter's Servant has coloured | `check:legal-targets`
- CR 607.2d two Gauntlets of Power each pump the creatures of their OWN chosen colour | `board:object-chosencolor`
- CR 608.2b Doom Blade fizzles when its target becomes black in response | `board:objects-ids`
- CR 611.2a Moonlace states no duration, so a permanent it made colourless stays colourless past cleanup | `check:other-GameState.continuousEffects`
- CR 613.3 devoid beats an OLDER layer-5 'in addition' effect | `board:object-chosencolor`
- CR 613.7a a granted devoid clears an OLDER 'in addition' colour | `move:other-addPermanent`
- Ersatz Gnomes' first ability makes a SPELL colourless, from an activated ability rather than a spell | `check:legal-targets`

### `CombatCostSpec`

- CR 104.3a the offer is the opponents still in the game, and never the controller | `check:prompt-offers`
- CR 506.7b the window opens at the declaration and runs to the end of the combat phase | `check:intermediate-state` `check:other-Turn.afterBlockersDeclared`
- CR 508.1d / 611.2a / 611.2c whole cards: bob's creatures attack alice on his next turn, and only then | `board:attackrequirements`
- CR 508.1d an illegal declaration is rewound and asked again, not replaced by the ceiling's | `check:other-Combat.legalAttackDeclaration`
- CR 508.1j partial payments are not allowed: two Dragons and one land sacrifice nothing | `check:combat` `check:helper-allUntapped` `check:helper-stillThere`
- CR 509.1c a Prized Unicorn does not force a block an Oppressive Rays taxes | `check:other-Combat.legalBlockDeclaration`
- CR 509.1h an attacker whose only blocker returned to hand stays blocked and assigns nothing | `check:combat`
- CR 509.1h whole card: Curtain of Light blocks an unblocked attacker, with nothing blocking it | `check:events`
- CR 509.3c the attacker it is put onto the battlefield blocking becomes blocked | `check:events`
- CR 509.4 / 608.2f whole card: Mirror Match blocks every attacker with its own copy, and exiles them all | `check:events`
- CR 509.4 whole card: Aetherplasm swaps itself out for a creature card from hand, blocking | `check:combat`
- CR 509.4 whole card: Flash Foliage's Saproling blocks the flier it could never have been declared against | `check:events` `check:combat`
- CR 701.43a / 701.43b an exerted attacker misses one untap step, then untaps | `board:combat` `board:object-exertedby`
- CR 701.43a the rider is the EXERTING player's untap step, not the new controller's | `board:sickness`
- CR 733.1 a blocker's toll: the Forests stay tapped and their mana floats | `check:helper-floating`
- CR 733.1 the exert goes back with the declaration and the Drum's tap stands | `move:Answer-ChooseManaYield`

### `CombatEffectSpec`

- CR 509.1h a BLOCKER that stops being a creature leaves the attacker blocked, so nothing is dealt combat damage | `board:combat` `board:object-worldsince`
- CR 702.20b vigilance still attacks | `check:helper-attackersOf`

### `CombatSpec`

- CR 102.1 a player who has left the game is not a candidate | `check:other-Combat.attackableOpponents`
- CR 122.1b two decayed counters, announced as X, stop both creatures blocking | `move:expect-rejected`
- CR 302.6 a creature that just changed control is summoning sick (no haste) | `check:other-Combat.canAttack`
- CR 305.7 setting a suspected land's subtype spares the ability rule 701.60c granted | `check:designations`
- CR 507.1 with no opponents left the action does not happen at all | `check:combat` `check:other-Combat.Type.defenders`
- CR 508.1a a creature that is also a battle is not offered as an attacker | `check:other-Combat.legalAttackers`
- CR 508.1c aimed elsewhere, both of alice's twins attack | `board:attackprohibitions`
- CR 508.1c the creature Netter en-Dal named cannot attack, and its twin still does | `board:attackprohibitions`
- CR 509.1a a Mountain is not a legal blocker, flier or no flier | `check:other-Combat.legalBlockers`
- CR 509.1a a creature that is also a battle is not offered as a blocker either | `check:other-Combat.legalBlockers`
- CR 509.1b Wan Shi Tong's Spirit tokens can't block a non-Spirit, and can block a Spirit | `move:expect-rejected`
- CR 509.1b aimed elsewhere, both of bob's twins block | `board:blockprohibitions`
- CR 509.1b an enchanted creature can't block either, and the Piker beside it still can | `check:other-Combat.canBlock` `check:other-Combat.legalBlockers` `move:expect-rejected`
- CR 509.1b the creature Zirda named cannot block, and its twin still does | `board:blockprohibitions`
- CR 509.1c a required blocker the restriction covers may decline after all | `board:blockprohibitions`
- CR 509.1c a threshold blocking requirement bites only once the gate holds | `check:other-Combat.forcedBlockDeclaration` `move:expect-rejected`
- CR 509.1c two requirements on ONE pair count twice | `check:other-Combat.forcedBlockDeclaration` `move:expect-rejected`
- CR 514.2 the restriction ends at cleanup | `check:other-Combat.canBlock` `check:other-Expiry.dropAtCleanup` `check:other-GameState.blockProhibitions`
- CR 601.2c announcing X as one leaves the second creature blocking | `move:expect-rejected`
- CR 611.2a and cannot attack once that turn has begun | `board:attackprohibitions`
- CR 611.2a the restriction outlasts every other seat's turn and ends as alice's begins | `check:other-GameState.attackProhibitions` `move:expect-rejected`
- CR 611.2a the window names alice's next turn, so bob attacks with the creature on his | `board:attackprohibitions`
- CR 613.11 Tapestry Warden compares power and toughness after characteristic effects | `board:continuous-effect`
- CR 613.7 a Humility older than the suspect leaves rule 701.60c's ability in place | `check:designations`
- CR 613.7 a Humility younger than the suspect removes it | `check:designations`
- CR 702.10b a hasty creature and a sick one, in the same declaration | `check:helper-declaredAttackers`
- CR 702.13b a COLOURLESS creature with intimidate may be blocked only by artifact creatures | `move:expect-rejected`
- CR 702.16k True-Name Nemesis can't be blocked by the chosen player's creature, and can by another's | `board:object-chosenplayer`
- CR 702.19b a trampling Tapestry Warden spills the excess over its toughness | `board:continuous-effect`
- CR 723.1 a controlled active player's choice of defender routes to their controller | `board:control`
- CR 802.3a the announcement, not the creature, is what the row refuses | `check:other-GameState.attackProhibitions` `move:expect-rejected`

### `CommanderSpec`

- CR 616.1 the controller of a stolen commander orders the offer | `board:sickness`
- CR 616.1e declining the offer first lets the redirect exile it | `board:object-bindings` `board:player-commander` `board:player-commandercasts` `board:player-startingdeck`
- CR 616.1e/903.9b the owner may apply the offer before a card's redirect | `board:object-bindings` `board:player-commander` `board:player-commandercasts` `board:player-startingdeck`
- CR 702.124d the other commander ignores this one's casts | `board:command` `board:player-commander` `board:player-commandercasts`
- CR 903.10a damage from two different commanders does not combine | `board:command`
- CR 903.10a fourteen does not | `board:command`
- CR 903.10a only the commander's combat damage is tallied | `board:command`
- CR 903.10a twenty-one combat damage from one commander loses the game | `board:command`
- CR 903.12h twenty-one combat damage from one commander does NOT lose a Brawl game | `board:command` `check:commander-damage`
- CR 903.8 the second cast from the command zone costs {2} more | `board:command` `board:player-commander`
- CR 903.9a a commander that dies is offered back to the command zone | `board:command` `board:player-commander`
- CR 903.9a declining leaves it in the graveyard | `board:command` `board:player-commander`
- CR 903.9b a commander bounced to its owner's hand may go to the command zone | `board:object-bindings` `board:player-commander` `board:player-commandercasts` `board:player-startingdeck`
- CR 903.9b a commander put on top of its owner's library may go to the command zone | `board:object-bindings` `board:player-commander` `board:player-commandercasts` `board:player-startingdeck`
- CR 903.9b an ordinary creature is not offered the command zone | `board:object-bindings` `board:player-commandercasts` `board:player-startingdeck`
- CR 903.9b declining leaves it in her hand | `board:object-bindings` `board:player-commander` `board:player-commandercasts` `board:player-startingdeck`
- CR 903.9b declining leaves it in her library | `board:object-bindings` `board:player-commander` `board:player-commandercasts` `board:player-startingdeck`
- CR 903.9b the offer applies once per commander in the same event | `board:player-commander` `board:player-startingdeck`
- CR 903.9b the owner is asked, not the player who bounced it | `board:object-bindings` `board:player-commander` `board:player-commandercasts` `board:player-startingdeck`

### `ConjureSpec`

- CR 400.7j Calim's Breath cannot exile the discarded Calim as one of the two others | `ready`
- CR 400.7j Calim's Breath returns the Calim its cost discarded, tapped | `ready`
- CR 603.4 conjure behind an intervening if: 4 life gained puts a castable Mox Pearl in hand | `board:stage-no-card-named-mox-pearl`
- CR 702.140e a duplicate of a mutated Headless Skaab owes the Skaab's additional cost | `board:source-ofmerge`
- CR 702.178a at max speed the Smasher returns the duplicate with haste, and the end step sacrifices it | `board:exile-linked` `board:player-speed`
- CR 702.178a one short of max speed, the duplicate stays in exile | `board:exile-linked` `board:player-speed`
- CR 707.2/118.9 a duplicate of a Clone offers the alternative cost of what the Clone copies | `ready`
- CR 707.2/702.37e a duplicate of a Clone of Ainok Tracker is cast face down and turned up for its morph cost | `check:tapped-count`
- CR 707.2/709.5b a duplicate of a copied Room is cast as a door | `move:cast-face` `move:action-UnlockDoor`
- CR 707.2/715.2b a duplicate of a Clone of Flaxen Intruder is cast as Welcome Home | `move:cast-face` `check:tapped-count`
- CR 715.3a a duplicate of Flaxen Intruder cast as Welcome Home costs Welcome Home's cost | `move:expect-rejected`
- CR 730.2/730.3 a duplicate of a Clone that merged as the spell is the Cubwarden again | `board:source-ofmerge`
- CR 730.3/707.2 a duplicate of a Clone split out of a merge is the Piker again | `board:source-ofmerge`
- Gate to Seatower's seek puts the nonland card randomness named into the hand, leaving the library's order | `board:stage-no-card-named-gate-to-seatower`
- Kari Zev's Ragavan attacks without being declared and goes home at the next end step | `board:stage-no-card-named-ragavan-nimble-pilferer`
- a conjure from the file registry's reference never conjures a synthetic card | `ready`
- a printed spellbook is offered whole, and the card randomness named is the one conjured | `check:prompt-offers`
- a printed spellbook picked by choice is offered whole, and the card its controller named is the one conjured | `check:prompt-offers`
- conjure into exile puts the duplicate in the conjurer's exile, exiled with the conjuring creature | `check:exile-linked`

### `CopySpec`

- CR 305.7 Blood Moon strips the abilities Vesuva copied from another land | `board:object-bindings`
- CR 400.11 casting the copy moves nothing out of a graveyard, so Kishla Skimmer does not trigger | `ready`
- CR 400.11 the copy is cast under Grafdigger's Cage and Aven Interrupter, and not under Drannith Magistrate | `ready`
- CR 702.103e a bestowed copy whose host died resolves as a token creature | `check:helper-rollickersOn` `check:other-Game.isToken`
- CR 702.128a embalm exiles the card for a white Zombie token copy with no mana cost, and a Clone of it keeps all three | `check:mana-value`
- CR 702.129a eternalize makes a black 4/4 Zombie token copy, and neither it nor a Clone of it is a Doom Blade target | `check:mana-value`
- CR 702.99c a flickered Piker is no longer encoded and offers nothing | `board:sickness`
- CR 707.10 a copied activated ability was not activated, so no mana was spent to activate it | `ref:stack-ability`
- CR 707.10 a triggered ability is copied, and its copy takes a new target | `ref:stack-ability`
- CR 707.10b a copied activated ability keeps its source, and the copy is not activated | `ref:stack-ability`
- CR 707.10b a copy of a triggered ability still counts once the original has been countered | `ref:stack-ability`
- CR 707.10b a copy of an activated ability still counts once the original has been countered | `ref:stack-ability`
- CR 707.10c a card the copied X does not reach is not offered | `check:legal-targets` `move:out-of-range-answer`
- CR 707.10c a copied trigger's slot is baked, so the offer is a real choice | `ref:stack-ability`
- CR 707.10c new targets are chosen for a copied activated ability | `ref:stack-ability`
- CR 707.10e no copy is created where Ivy is not a legal target | `board:continuous-effect`
- CR 707.13 Garth One-Eye's cast copy of Black Lotus has Black Lotus's characteristics and resolves as a token | `ready`
- CR 707.13 a declined copy leaves nothing behind, and a name the reference does not know makes no copy | `ready`
- CR 707.13 a second activation of the same Garth does not offer Black Lotus again | `check:named-copy-choices`
- CR 707.14 a Clone of the face-down 3/3 connects and creates no copy | `check:abilities`
- CR 707.14 the face-down Divination connects and alice casts a copy of Divination for free | `check:outside-the-game`
- CR 707.2 / 707.10c Transcantation's Bolt resolves at the target its controller chooses anew | `ready`
- CR 707.2 Transcantation's Bolt left without a new target hits nobody | `ready`
- CR 707.2 an Adventure that becomes a copy of a Bolt goes to the graveyard | `move:cast-face`
- CR 707.2a a copy of Blood Moon goes on setting land subtypes once the original is exiled | `check:mana-types` `check:abilities`
- CR 708.2 a face-down copy of Silent Arbiter no longer bounds the attack | `check:face-down` `check:bindings`
- CR 708.2 the face-down permanent is a nameless 3/3 creature with the two listed abilities | `check:abilities`
- Multiversal Recruitment's token copy is not legendary, and neither is a copy of it (CR 707.9b) | `check:supertypes`
- Omni-Changeling's copy is every creature type, and so is a token copy of it (CR 604.3a) | `check:subtype-member`
- Unstable Shapeshifter becomes a copy and keeps a noncopy effect (CR 707.4) | `board:continuous-effect`
- a token copy of the Shapeshifter carries the ability it kept (CR 707.2) | `board:object-bindings`
- two Sakashimas copying different creatures are one legend rule apart (CR 707.9b) | `check:supertypes`

### `CostSpec`

- CR 101.1 an announced waterbend X of 0 is refused, and 1 is not | `move:ChooseX-out-of-bounds`
- CR 101.4b the helper is asked on the board the caster's answer left | `check:intermediate-state` `check:other-GameState.lastChoice`
- CR 107.3a/601.2h the announced X is divided among creatures and the target gets -X/-X | `check:prompt-payload`
- CR 118.1 / 613.4c the cost removes the +1/+1 counter, so the 3/3 becomes a 2/2 that cannot pay again | `check:helper-counterRemovalsOf` `check:other-Event.removeCounters`
- CR 118.3 counters exactly covering the count are one division, so nothing is asked | `check:prompt-offers`
- CR 118.3 no untapped creature means no payment | `check:helper-isTapped` `check:helper-pooledFrom` `check:other-Cost.canPayComponent`
- CR 118.3 the Altar eats the Ignus before its own return is paid | `move:ActivateManaAbility`
- CR 118.3 the ChooseX bound is the counters the creatures carry between them | `check:prompt-payload`
- CR 118.6 paying an unpayable cost changes nothing | `check:tapped-count`
- CR 118.6 the pay-energy ability is payable at two energy, not at one, and grows the Cub | `move:expect-rejected`
- CR 118.8b declining to behold gains nothing | `move:ChooseCost:unmatchable`
- CR 118.8d a creature cast with its additional waterbend {5} enters with mana value 2 | `check:mana-value`
- CR 122.1 / 601.2h the payer divides the three counters by creature and by kind | `check:prompt-payload`
- CR 122.1 / 601.2h the payer picks the kind of the one counter | `check:prompt-payload`
- CR 400.3 a land its payer does not own goes back to its owner's hand | `board:sickness`
- CR 601.2f Bury in Books aimed at the attacking Giant is cast off three Islands | `check:mana-pool`
- CR 601.2f a different tapped creature is a different amount of damage | `check:helper-isTapped`
- CR 601.2f on one board, aiming at the Giant at home costs all five Islands and the attacker three | `check:mana-pool`
- CR 601.2f one opponent's two spells take {U} off; two opponents' one each do not | `board:replacement`
- CR 601.2f the perpetually discounted Sliver is cast off four Islands and enters a 5/5 | `board:continuous-effect`
- CR 601.2g the mana window opens before the payer says how much of a Siege Wurm's cost is convoked | `check:prompt-order`
- CR 601.2h an answer of the wrong size pays nothing at all | `move:action-ActivateManaAbility` `move:Answer-ChooseManaYield`
- CR 601.2h the payer chooses how many counters come off, and the token is that big | `check:prompt-payload`
- CR 601.2h the payer chooses which creature the +1/+1 counter comes off, and the ability then draws | `check:events` `check:prompt-payload`
- CR 601.2h the payer divides the two counters among creatures, one off each of two | `check:prompt-payload`
- CR 608.2h the ability reads the power of the creature its own cost tapped | `check:helper-isTapped`
- CR 609.7a the evidence the Inspector collected is not a source it refers to | `move:ChooseCost:unmatchable`
- CR 701.17a a card milled to pay a cost was milled this turn | `check:other-Filter.MilledThisTurn`
- CR 701.4a a Dragon card in hand pays the cost | `check:events`
- CR 701.4a a Dragon on the battlefield pays the cost | `check:events`
- CR 701.4a the payer is asked across both zones at once | `check:prompt-payload`
- CR 701.4b beholding a Dragon card in hand gains the 2 life too | `move:ChooseCost:unmatchable`
- CR 701.4b beholding a Dragon permanent gains the 2 life | `move:ChooseCost:unmatchable`
- CR 701.59a Evidence Examiner investigates when the Inspector's cost collects evidence | `move:ChooseCost:unmatchable`
- CR 701.59a one card of mana value 3 pays it, and the rest stays put | `check:prompt-payload`
- CR 701.59b a graveyard totalling 2 cannot collect evidence 3 | `check:helper-collectsEvidenceFrom`
- CR 701.59c Vitu-Ghazi Inspector's enters ability reads the evidence its spell collected | `move:ChooseCost:unmatchable`
- CR 704.5a paying the last 2 life is legal and loses the game | `check:other-GameState.players` `check:other-Player.status`
- CR 733.1 a nested window's mana abilities are the payer's to keep | `move:ActivateManaAbility`
- CR 733.1 a refused assisted cast asks the helper too, and each keeps only their own | `check:prompt-order`
- CR 733.1 an activator who keeps the mana abilities keeps both lands tapped and both mana floating | `check:other-GameState.activatedThisTurn`
- CR 733.1 reversing the payment does not undo the Tomb's own shuffle | `check:mana-pool`
- CR 733.1 the payer may keep the mana ability they activated while making the illegal play | `check:mana-pool`
- CR 733.1 the payer who reverses it gets the mana, the tap and the damage back | `check:mana-pool`

### `CountSpec`

- CR 108.3 a spell still on the stack is read off the object, not off a record | `board:exile-cast-permission` `board:stack`
- CR 110.2 X counts the one seat with more lands than alice and the one with fewer | `check:events`
- CR 110.2 a seat TIED with alice controls no more lands than she does | `check:events`
- CR 111.3 the 3/3 is fixed at creation: it survives the turn handoff that clears the log | `board:replacement`
- CR 202.3 X is the GREATEST mana value among the instants and sorceries ALICE cast | `board:replacement`
- CR 400.3 a card put into bob's graveyard is not put into yours | `board:replacement`
- CR 601.2a a spell an OPPONENT cast neither triggers nor counts | `board:replacement`
- CR 601.2a the second, cast from the LIBRARY, does not win | `board:library-order`
- CR 608.2i a creature that died is not a card put there from anywhere other than the battlefield | `board:replacement`
- CR 608.2i the count is THIS turn's: it resets at the handoff | `board:replacement`
- CR 608.2i three casts in one turn gain 1, then 2, then 3 | `board:replacement`
- CR 608.2i three of alice's own spells reach her graveyard from the stack, so she draws | `board:replacement`
- CR 613.1b a Wall STOLEN from bob counts carol's hand, who controls it | `board:sickness`
- CR 800.4a a player who has conceded is not one of alice's opponents | `board:player-status`
- CR 800.4a neither EachPlayer nor Opponent names a player who has left the game | `check:other-Count.playersFor` `check:other-Filter.contextFor` `check:other-S.countOf` `check:other-S.stubView` `check:other-Teams.none`

### `CounterKeywordTriggerSpec`

- CR 107.14 two {E} makes it a real choice, and the Aetherjet is a 2/2 | `check:prompt-count`
- CR 111.3 the Aetherjet's X/X is the energy PAID, not the energy held | `check:power-toughness-absent`
- CR 603.2c two artifact creatures connecting in one step give {E}{E}, not {E}{E}{E}{E} | `check:events`
- CR 603.4 a wurm with no time counters neither triggers nor is sacrificed | `check:helper-times`
- CR 611.2d the +1/+1 outlives the combat record it was computed from | `check:other-Combat.Type.declaredAttacked`
- CR 701.37a a second monstrosity marks nothing, so nothing triggers | `board:object-designations`
- CR 701.37a a suspected Colossus is still not monstrous | `check:designations`
- CR 701.37b a second monstrosity announcing 3 makes nothing and leaves the recorded X at 2 | `check:designations`
- CR 701.37c monstrosity 2 makes TWO 2/2 Hydras | `check:designations`
- CR 702.112b the designation does not survive CR 400.7 | `check:other-Object.newIncarnation`
- CR 702.112b whole card: a renowned creature's counters double when it connects | `board:combat` `board:object-designations`

### `CounterspellSpec`

- CR 107.3a the announced X is what the targeted spell's controller pays | `check:mana-pool`
- CR 113.3b Squelch counters the activated ability and leaves the trigger, which still kills the Piker | `ref:stack-ability`
- CR 113.7 the trigger reaches the artifact's ability and not the creature's, and the artifact is destroyed | `ref:stack-ability`
- CR 113.7a the ability of an artifact sacrificed to activate it is still from an artifact source, and nothing is destroyed | `ref:stack-ability`
- CR 114.2 an unevolved Ajani's emblem mints three Cat Tokens | `board:command`
- CR 118.12 a graveyard one card smaller demands one mana less | `check:mana-pool`
- CR 118.12 the offer is {1} for each card in the RESOLVING controller's graveyard | `check:mana-pool`
- CR 118.12 whole card: an unhacked Lithophage's gate demands the Mountain | `check:helper-namesIn` `check:helper-payResponses`
- CR 118.12a a creature card in the graveyard pays it and the Vultures survive | `check:helper-isExileResponse` `check:helper-namesIn` `check:helper-payResponses` `check:other-Replay.record` `check:other-Stack.resolveTop`
- CR 118.12a declining the {B} sacrifices the Zombie to its owner's graveyard | `check:creature-count` `check:helper-payResponses` `check:tapped-count`
- CR 118.12a paying the {B} leaves the Zombie on the battlefield | `check:helper-payResponses` `check:other-Replay.record` `check:other-Stack.resolveTop` `check:tapped-count`
- CR 118.3 an empty graveyard cannot pay, so the Vultures are sacrificed | `check:helper-payResponses` `check:other-Replay.record` `check:other-Stack.resolveTop`
- CR 404.2 with two creature cards it is the TOP one that is exiled | `check:helper-isExileResponse` `check:helper-namesIn` `check:other-Replay.record` `check:other-Stack.resolveTop`
- CR 601.2f a cost increase taxes the CAST and not the resolution payment | `check:mana-pool`
- CR 601.2f a noncreature card in the graveyard cannot pay it either | `check:helper-namesIn` `check:other-Replay.record` `check:other-Stack.resolveTop`
- CR 612 the Evolution's own restriction reaches the player being asked | `check:prompt-payload`
- CR 612.1 an Evolution on an Evolution rewrites the restriction itself | `check:prompt-payload`
- CR 612.1 an evolved Ajani's emblem mints Wurms rather than Cats | `board:command`
- CR 612.1 an evolved Moonmist's shield spares Goblins instead, so the Werewolf's damage is the prevented one | `check:events` `check:other-Damage.applyDamage` `check:other-GameState.replacements`
- CR 612.1 hacked Zombie -> Goblin, PutCounters puts a hexproof from Goblins counter on | `check:legal-targets`
- CR 612.1 hacked Zombie -> Goblin, the entry rider's counter is a hexproof from Goblins counter | `check:legal-targets`
- CR 612.1 whole card: hacking Lithophage moves which land its CR 118.12 gate demands | `board:continuous-effect`
- CR 612.2 an Evolution naming a word the emblem lacks leaves the Cats alone | `board:command`
- CR 612.2a an evolved Bitterblossom's second name word moves too | `board:continuous-effect`
- CR 612.2a whole card: an evolved Bitterblossom's trigger mints an Elf Rogue Token | `board:continuous-effect`
- CR 613.7 whole card: two Evolutions on the Turn to Frog spell compose | `check:text-changes`
- CR 702.135b an evolved Ministrant plus a granted afterlife leaves two Elves and a Spirit | `move:OrderTriggers-departed-source`
- CR 704.5g an indestructible creature survives lethal marked damage | `check:creature-count`
- CR 704.5h an indestructible creature survives deathtouch | `check:creature-count`

### `CrewSpec`

- CR 514.2 the Vehicle stops being a creature at cleanup | `check:power-toughness-none`
- CR 702.122b a countered crew ability still crewed the Vehicle | `ref:stack-ability`
- CR 702.122c a creature whose crew ability was countered still crewed it | `ref:stack-ability`
- CR 702.122d a creature Revoke Privileges enchants is not offered to pay | `check:prompt-offers`
- CR 702.122e crewed by exactly two creatures grants the ability | `check:abilities` `check:intermediate-state`

### `DamageReplacementSpec`

- CR 120.4 a redirect onto the recipient it was already aimed at deals one event, not two | `check:events` `check:helper-countedRedirectRows`
- CR 120.4 one sentence naming alice twice deals her ONE event, of 6 | `board:command`
- CR 608.2f an ability's two halves are one batch | `board:replacement`
- CR 608.2f creatures and players in a won flip are one batch | `board:replacement`
- CR 608.2f creatures and players in one sentence are one batch | `board:replacement`
- CR 608.2f one sentence's differing amounts are one batch | `board:replacement`
- CR 609.7a the redirection moves the chosen source's damage and no other source's | `board:replacement`
- CR 609.7a the same board answering the OTHER source moves that one's damage instead | `board:replacement`
- CR 609.7a the unchosen source's damage to alice stays where it was aimed | `check:helper-countedRedirectRows`
- CR 609.7b the recheck reads the RECORD, not the print: a Fire-Eater made green before it left deals its 2 | `board:continuous-effect` `board:replacement`
- CR 609.7b the shield rechecks the source: a Piker made a Wolf deals its 2 | `board:continuous-effect` `board:replacement`
- CR 609.7b the shield rechecks the source: a Piker made green deals its 2 | `board:continuous-effect` `board:replacement`
- CR 611.2c a permanent alice controls is covered by the same countdown | `ready`
- CR 611.2c the chosen source's damage to bob's own creature is not covered | `ready`
- CR 614.3 the row covers the next damage event and no later one | `board:objects-ids`
- CR 614.9 Lava Burst's damage to a creature is not redirected, and another source's still is | `check:helper-redirectSources`
- CR 614.9 a 1-damage event moves whole, and the row is spent by it | `check:events` `check:other-Damage.applyDamage` `check:other-GameState.replacements`
- CR 614.9 guard: a destination that left the battlefield makes the effect do nothing | `check:helper-redirectRows`
- CR 614.9 the redirection covers the creature the spell named and not the other | `check:other-Damage.applyDamage` `check:other-GameState.replacements`
- CR 615.10 the emblem floors damage to alice and to her other planeswalker at 1, and reaches nothing else | `board:command`
- CR 615.12 the same shield still prevents Lava Burst's 3 dealt to a player | `board:objects-ids`
- CR 615.12 the shield prevents none of Lava Burst's 3 and is not reduced by it | `board:objects-ids`
- CR 615.12 the unreduced shield still covers the next 2 once Spider-Punk is gone | `board:replacement`
- CR 615.12 unpreventable damage is still moved, and still contended for | `board:replacement`
- CR 615.13 a trigger scoped to one recipient reads its own share of the application | `board:replacement`
- CR 615.13 another card's prevention of the same 4, on the same creature, is not 'this way' | `board:replacement`
- CR 615.13 the other disjunct: a red source gains the life too | `ready`
- CR 615.5 the prevented three come off as three +1/+1 counters | `check:helper-aimCreature` `check:helper-countersOn`
- CR 615.7 NONcombat damage from that same creature is prevented | `check:helper-amounts` `check:helper-shieldsLeft`
- CR 615.7 a simultaneous batch contends for the 2, and alice divides it | `board:replacement`
- CR 615.7 alice redirects 1 from each of two simultaneous hits, and both creatures live | `board:replacement`
- CR 615.7 combat damage from a creature the Beast's controller does NOT control is prevented | `check:helper-amounts` `check:helper-shieldsLeft`
- CR 615.7 once spent, the next event stays whole | `check:other-Damage.applyDamage`
- CR 615.7 one shield over you AND your permanents is a single shared pool | `board:replacement`
- CR 615.7 the chosen source's 5 to alice: 2 is dealt to bob and 3 to alice | `check:events` `check:continuous-effects`
- CR 615.7 the next 1 of a 3-damage event moves and the other 2 stay where they were aimed | `check:events` `check:other-Damage.applyDamage` `check:other-GameState.replacements`
- CR 615.7 the shield still prevents the Goblin Piker's 3 whole | `check:helper-amounts` `check:helper-shieldsLeft`
- CR 615.7 without Spider-Punk the shield prevents the whole 3 | `check:helper-amounts` `check:helper-shieldsLeft`
- CR 615.7's allocation lands on the events it was asked about, after the sort | `board:replacement`
- CR 615.7's order sits INSIDE one chooser's APNAP turn, not across choosers | `board:objects-ids`
- CR 615.8 / 609.7a the shield eats one whole instance from the source it named, and the reflection hits that source's controller | `check:continuous-effects`
- CR 615.9 / 615.13 samite ministration gains life from a black source it named, and none from a green one | `board:delayed-trigger` `board:replacement`
- CR 616.1 the shielded creature's controller picks which of two simultaneous events the row replaces | `board:replacement`
- CR 616.1 two players choosing for one batch are asked in APNAP order | `board:objects-ids`
- CR 701.14a the fight's replacement runs before its own resolution ends | `check:intermediate-state`
- CR 701.26b an untapped permanent never becomes untapped, so its stun counter is not spent | `check:helper-countersOn` `check:helper-tapStateOf`
- CR 702.16e protection's prevention is not 'this way', and the same board's printed one is | `board:continuous-effect`
- CR 702.64a an event at the ceiling never happens | `check:events` `check:other-DamageEvent.amount`
- a shield over a PLAYER runs CR 615.5's rider, scaled by the amount | `board:replacement`
- the combat-only shield leaves noncombat damage alone, rider and all (CR 608) | `check:helper-preventAllRows`

### `DamageSpec`

- CR 120.1/120.2b the event credits the targeted creature, and CR 120.3f pays ITS controller | `check:events`
- CR 120.3f a Goblin Piker dealing the same two gains nobody anything | `check:events`
- CR 120.4a Flame Spill's excess goes to the creature's controller | `check:helper-subtract`
- CR 120.4a the excess is the greatest across the card types the permanent has | `check:intermediate-state`
- CR 120.4a/120.6 the bar is lethal damage, not toughness | `check:helper-subtract`
- CR 120.6 a regenerated creature carries no marked damage and is still a legal target | `board:replacement`
- CR 701.14c a self-fight is ONE damage event, so one shield counter answers it | `board:face` `board:object-turnedoverat` `board:replacement`
- CR 701.14c one shield counter answers the whole self-fight | `board:face` `board:object-turnedoverat` `board:replacement`
- CR 701.14c the one blow is TWICE its power | `board:continuous-effect` `board:face` `board:object-turnedoverat` `board:replacement`
- CR 701.14d fight damage is not combat damage, so a combat-only prevention misses it | `board:replacement`
- CR 701.19a a blocker that regenerates is removed from combat, and the attacker is STILL blocked | `board:replacement`
- CR 702.15b/613.1b a stolen Fire-Eater's lifelink pays the THIEF, not its owner | `board:sickness`
- CR 702.164b two Aspirant's Ascents make Branchblight Stalker toxic 4 | `board:continuous-effect`
- CR 702.16k bob's damage is prevented when bob was chosen and marked when carol was | `board:object-chosenplayer`
- CR 702.16k/113.7a a stolen Fire-Eater sacrificed at a Nemesis naming its owner still deals its damage | `board:sickness`
- CR 702.2b Llanowar Elves' one damage leaves the Wall standing | `check:events`
- CR 702.2b a Typhoid Rats' one damage destroys the 0/8 Wall | `check:events` `check:intermediate-state`
- CR 702.2c a deathtouch-granted trampler needs only 1 on the blocker, spilling the rest | `board:continuous-effect`
- CR 702.4b/120.3g a double-striking Branchblight Stalker poisons twice | `board:continuous-effect`
- CR 704.5j a Thalia and an Urborg coexist under one controller | `check:helper-inPlay`
- CR 704.5j two copies of a NON-legendary creature both survive | `check:helper-inPlay`
- CR 704.5j two players may each control a Thalia | `check:helper-inPlay`
- CR 704.5k a lone world permanent survives | `check:helper-inPlay`
- CR 704.5k whole cards: resolving Living Plane buries the Concordant Crossroads already out | `board:object-worldsince`
- life <= 0 loses | `check:other-GameState.players` `check:other-Player.commander` `check:other-Player.commanderCasts` `check:other-Player.commanderDamage` `check:other-Player.companion` `check:other-Player.companionTaken` `check:other-Player.completedDungeonNames` `check:other-Player.completedDungeons` `check:other-Player.counters` `check:other-Player.designations` `check:other-Player.dungeons` `check:other-Player.outsideTheGame` `check:other-Player.ringTemptations` `check:other-Player.speed` `check:other-Player.startingDeck` `check:other-Player.status`

### `DaytimeSpec`

- CR 502.2 neither day nor night: the check does not happen | `check:other-GameState.daytime`
- CR 502.2 night and only one spell last turn stays night | `board:daytime`
- CR 502.2/702.145f night and two spells last turn becomes day | `board:daytime`
- CR 603.4 two Wolves are not three, so it stays day | `board:daytime`
- CR 608.2d an answer naming what was never offered turns nothing over | `board:daytime`
- CR 608.2d naming nothing is a legal answer and turns nothing over | `board:daytime`
- CR 608.2d the chooser names the Human Werewolf and it turns over | `board:daytime`
- CR 702.145b by day the same spell enters front face up | `board:daytime`
- CR 702.145d controlling a daybound permanent makes it day | `check:helper-faceNameOf` `check:helper-frontName` `check:other-GameState.daytime`
- CR 712.13 by day that spell enters front face up | `board:daytime`
- CR 731.1 a game with no daybound permanent stays neither day nor night | `check:other-GameState.daytime`
- CR 731.1/702.145c Tovolar's upkeep trigger makes it night and transforms him | `board:daytime`
- CR 731.2 the handoff records what the previous turn's active player cast | `check:other-GameState.spellsCastLastTurn` `check:events`

### `DepartureSpec`

- CR 104.3a a conceding player leaves immediately, with Conceded as the reason | `check:helper-statusOf`
- CR 603.3a/800.4d a borrowed permanent's trigger is the departing player's, so it is never put on the stack | `board:sickness`
- CR 725.4 the monarch departs on someone else's turn: the active player takes the crown | `check:helper-crownings`
- CR 725.4 the monarch departs on their own turn: the next seat in turn order takes the crown | `check:helper-crownings`
- CR 800.1 two seats: the same concede leaves the Song on the battlefield, so nothing is handed over | `check:card-types` `check:other-Departure.continuesAfterDeparture` `check:other-GameState.continuousEffects`
- CR 800.1/104.2a two seats: the same departure ends the game instead of running CR 800.4a | `check:other-Departure.continuesAfterDeparture`
- CR 800.4a/603.6c the exile is a zone change, so a bystander's leaves-the-battlefield trigger fires | `board:combat` `board:entered-with`
- CR 800.4a/800.4m turnOrder is the SEATING roster: a departure does not shorten it | `check:other-GameState.turnOrder`
- CR 800.4c a permanent lent to a surviving player is exiled when the loan ends and its default controller has left | `board:combat` `board:entered-with`
- stillPlaying omits a departed player | `ready`

### `DetainSpec`

- CR 508.1c the detained creature does not end up among the attackers | `board:combat` `board:object-detaineduntil`

### `DiceSpec`

- CR 108.3 a creature reanimated under another player's control still makes its OWNER lose the game | `board:delayed-trigger` `board:entered-with` `board:graveyard` `board:object-bindings`
- CR 614.5 a second Guide adds a second die and a second ignore | `check:prompt-payload`
- CR 706.1 a modified roll is still one roll of the printed die | `check:prompt-payload`
- CR 706.1 the Guide adds a die to the instruction | `check:prompt-payload`
- CR 706.1 the count is how many dice the instruction offers | `check:prompt-payload`
- CR 706.1 the engine offers the die and never rolls it | `check:prompt-payload`
- CR 706.2 a rerolled die that repeats the number is offered again | `check:prompt-payload`
- CR 706.2 an increase from another source is part of the result | `check:prompt-payload`
- CR 706.2 the offer is gated on the number the card names | `check:prompt-payload`
- CR 706.2a each costed modifier is its own offer | `check:prompt-payload`
- CR 706.2a the modifier is taken only once each turn | `board:battlefield` `board:objects-ids`
- CR 706.2a two free offers to the same player are one question | `check:prompt-payload`
- CR 706.2b Goblin Bookie rerolls another player's die | `check:events`
- CR 706.2b a reroll is a roll by the player who throws it | `check:events`
- CR 706.2b rerolls come before increases and decreases | `check:prompt-payload`
- CR 706.4 the Hydra's counters are the total of X dice | `check:prompt-payload`
- CR 706.4 two equal results are not a choice | `check:prompt-payload`
- CR 706.6 rolls tied for the lowest leave nothing to ask | `check:prompt-payload`

### `DungeonSpec`

- CR 118.12a Veils of Fear offers each player the discard and only the seats that declined lose the life | `board:dungeons`
- CR 309.2 a deck's dungeon cards are recorded on its player and minted into no zone | `check:other-Game.printingOf` `check:other-GameState.objects` `check:other-GameState.players` `check:other-Player.dungeons`
- CR 309.2a a player owning two dungeons chooses which one to enter | `board:dungeons`
- CR 309.4c / 701.22a Cave Entrance's Scry 1 puts the top card where its controller says | `board:dungeons`
- CR 309.5a a room with two arrows asks which one to follow, and the answer decides the room | `board:dungeons`
- CR 309.7 / 603.4 Acererak asks WHICH dungeon was completed, not how many | `board:dungeons`
- CR 309.7 / 702.4b completing a dungeon is remembered, and Gloom Stalker's double strike reads it | `board:dungeons`
- CR 309.7 completing a dungeon triggers Dungeon Crawler out of the graveyard | `board:dungeons`
- CR 701.24a Throne of the Dead Three's "then shuffle" randomises the library and moves nothing | `board:dungeons`
- CR 701.49a the first venture puts the dungeon in the command zone with the marker on the topmost room | `board:dungeons`
- CR 701.49b / 309.3 venturing again advances the one dungeon instead of entering another | `board:dungeons`
- CR 701.49d venturing into Undercity enters the dungeon carrying that quality, where a plain venture enters the other | `board:dungeons`
- CR 704.5t / 701.49a the bottommost room ends the dungeon, and venturing again starts a new one | `board:dungeons`

### `EarthbendSpec`

- CR 110.2a the land returns under the earthbender's control, not its owner's | `board:sickness`
- CR 701.66a the land that dies comes back tapped under its controller's control | `board:continuous-effect` `board:delayed-trigger`
- CR 702.10b the earthbent land can attack the turn it was animated | `check:other-Combat.legalAttackDeclarationAs`

### `EmperorSpec`

- CR 809.5c the game is a draw for a team if it is a draw for its emperor | `check:other-Player.status,` `check:helper-stillPlaying`

### `EntryReplacementSpec`

- CR 120.1 the Doll's own {T} ability feeds its own trigger | `board:object-chosenplayer`
- CR 614.10a spending Brine's row lets Savor's expire with its own turn | `move:cast-face` `check:continuous-effects`
- CR 614.10a spending Savor's row leaves Brine's to take the following untap step | `move:cast-face` `check:continuous-effects`
- CR 614.1b Brine Elemental arms one skip per opponent, and Savor's turn one more | `move:cast-face` `check:continuous-effects`
- CR 616.1 two entry replacements of ONE source are distinct entries | `check:prompt-payload`
- CR 616.1 two skips alike in effect but not in lifetime raise a choice | `move:cast-face`
- CR 702.10b the goblin that took haste attacks the turn it entered | `board:continuous-effect`
- CR 702.136b riot twice is asked twice, and both counters land | `board:twin-run`
- CR 702.38a the Ogre Sentry in the same hand is not offered | `move:answer-all-offered`
- CR 702.38a two Beast cards revealed are four +1/+1 counters | `check:events`
- CR 702.98a a +1/+1 counter arriving later shuts blocking off too | `check:other-Combat.canBlock` `board:mid-test-counter-fixture`
- CR 702.98a declining leaves a 2/1 that can block | `check:other-Combat.canBlock` `check:other-Combat.legalBlockers`
- CR 702.98a taking the counter makes it bigger and stops it blocking | `check:other-Combat.canBlock` `check:other-Combat.legalBlockers`

### `EventSpec`

- CR 101.4b Fleshbag Marauder's later victim knows the earlier victim's pick | `check:prompt-payload`
- CR 800.4b no token is created under the control of a player who has left the game | `check:other-Game.objectCount`

### `EventTriggerSpec`

- CR 112.2 Kambal's 'that player' is the opponent who cast it | `check:helper-lives`
- CR 113.6k Desolation Twin's cast trigger fires from the stack | `check:helper-eldraziOf`
- CR 113.9 the same Baral: a countered SPELL fires it, a countered ABILITY does not | `ref:stack-ability`
- CR 505.1b an extra main phase makes the postcombat main the third, and it does not trigger | `check:other-GameState.triggeredThisGame`
- CR 601.2i Brineborn Cutthroat counts only the casts on another player's turn | `board:replacement`
- CR 601.2i casting an instant fires Young Pyromancer | `check:helper-elementalsOf`
- CR 601.2i the turn's SECOND cast fires Clarion Spirit, and no other | `check:helper-spiritsOf`
- CR 701.66b the Adept fires as rule 701.66a's delayed ability is created, not when it returns the land | `board:continuous-effect` `board:delayed-trigger`
- a creature spell neither fires the ability nor spends its rider | `check:helper-spiritsOf`

### `ExileSpec`

- CR 305.9 a hidden Forest is played as the turn's land and never cast for free | `board:exile-linked` `board:face-down` `check:lands-played`
- CR 406.3 the look bob had while he controlled the land survives losing it, and a land that exiled nothing gives him none | `check:legal-targets`
- CR 406.3 the player the exiling instruction let look names both cards, and the owner who was shown nothing gets their pile | `check:helper-offerTo` `check:helper-pilesIn` `check:helper-faceDownExiled` `check:other-Object.owner`
- CR 406.3 the same two cards exiled face up leave the flashback one targetable, and it returns to her hand | `check:legal-targets`
- CR 406.3a a Grist card exiled FACE DOWN has no characteristics to function from | `check:power-toughness-absent` `board:second-game`
- CR 406.3a the flashback card exiled face down has no flashback to be targeted by, so the casting is reversed | `check:face-down` `check:legal-targets` `move:expect-rejected`
- CR 406.3a the foretold card is turned face up as it is cast, so the noncreature tax passes it by | `board:foretold`
- CR 406.4 a draw answered with a card outside the named pile falls back to a card in it | `check:face-down` `move:pile-target`
- CR 406.4 a pile of one card is drawn from without asking, and a pile of two is asked about | `move:pile-target`
- CR 406.4 choosing the pile shuffles the card the random draw named, not the other one | `check:face-down` `move:pile-target`
- CR 406.4 the owner of a foretold card shuffles it out of exile and an opponent aiming at it by name gets the face-up one | `board:foretold`
- CR 406.4 two castings of one spell make two piles, and the draw comes out of the pile that was named | `check:face-down` `check:legal-targets` `move:pile-target`
- CR 603.7 at the next end step the exiled cards return to hand and alice draws | `check:delayed-triggers` `check:face-down`
- CR 607.2a the play ability names only what THIS permanent exiled, and a copy names what the copy exiled | `board:exile-linked` `board:face-down`
- CR 608.2h the play ability reads the turn's declarations as it resolves, so two attackers is one short and three is not | `board:exile-linked` `board:face-down`
- CR 702.75a hideaway 4 hides the card its controller named and bottoms the other three, and only she may name it afterwards | `check:helper-offerTo` `check:helper-pilesIn`
- CR 702.75a the look follows control of the land that exiled the card, and CR 406.3's does not | `board:exile-linked` `board:face-down` `check:legal-targets`
- CR 707.2 the look follows the copy that exiled the card, not the Windbrisk Heights it copied | `board:exile-linked` `board:face-down` `check:legal-targets`

### `ExpirySpec`

- Blighted Nightmare's perpetual +1/+1 applies in the graveyard and follows the card out | `check:legal-targets`
- CR 500.5 an end-of-combat retention effect expires BEFORE the pool empties | `board:combat` `board:player-effect`
- CR 500.5a whole card: live throughout the end of combat step, gone once the phase ends | `check:other-GameState.continuousEffects`
- CR 500.5a whole card: the animation outlives the step it was made in | `check:other-ContinuousEffect.expiry` `check:other-GameState.continuousEffects`
- CR 500.5a whole card: the permission ends as alice's next combat phase ends | `check:helper-permissionOn`
- CR 604.2 an ordinary static ability does NOT linger past its permanent | `check:other-GameState.continuousEffects`
- CR 611.2a / 514.2 it ends as that turn ends, and no later turn of theirs can play the card | `move:expect-rejected`
- CR 611.2a whole card: the animation outlives Titania's Song and ends at cleanup | `check:abilities`
- CR 611.2b chapter I grants hexproof for the same duration, and it ends with the Saga | `board:continuous-effect`
- CR 611.2b it ends when the crown moves to the ability's controller | `board:continuous-effect`
- CR 611.2b it ends when the crown moves to the third player | `board:continuous-effect`
- CR 707.2a a copy of Titania's Song hands over its own effect | `check:continuous-effects`
- Pearl Collector's perpetual lifelink survives the graveyard, where an indefinite grant does not | `board:continuous-effect`

### `FaceDownSpec`

- CR 109.5 a Pine Walker bob controls does not untap alice's permanent | `board:face-down`
- CR 109.5 alice's own face-down creature is not a legal target | `board:face-down`
- CR 110.5 Break Open turns the opponent's face-down creature face up | `board:face-down`
- CR 110.5 a face-up creature the opponent controls is not a legal target | `board:face-down`
- CR 601.2c / 708.2a Weaver of Lies turns both announced creatures face down at once | `check:face-down` `move:action-TurnFaceUp` `move:cast-face`
- CR 601.2c the offer is every OTHER morph creature, and all three turn over | `check:face-down` `check:prompt-offers` `move:action-TurnFaceUp` `move:cast-face`
- CR 613.4c turning one face up drops the Whisperer by two | `board:face-down`
- CR 613.7f turning face down restamps the permanent after a removal that had wiped its grant | `board:object-designations`
- CR 613.7f turning face up restamps the permanent after a removal that had wiped its grant | `board:object-designations`
- CR 701.20b a face-down permanent revealed in place draws nothing | `board:objects-ids`
- CR 701.40b the manifest procedure pays the mana cost, CR 702.37e the morph cost | `board:entered-with` `board:face-down`
- CR 701.40d a manifested disguise card is offered both procedures, at two prices | `board:entered-with` `board:face-down`
- CR 701.40g a manifested SORCERY stays face down, and still deals a 2/2's damage | `board:entered-with` `board:face-down`
- CR 701.40g a turn-face-up trigger does not fire for a manifested sorcery | `board:entered-with` `board:face-down`
- CR 702.37b the manifest procedure pays no megamorph cost, so no counter lands | `board:entered-with` `board:face-down`
- CR 708 a manifested CREATURE turns face up, and deals its printed damage | `board:entered-with` `board:face-down`
- CR 708.12 a manifested land card is not a creature card, whatever CR 708.2a made the permanent | `board:face-down`
- CR 708.12 the printed card is read, not the continuous effect on the permanent | `board:face-down`
- CR 708.12 with the continuous effect elsewhere the same card turns face up | `board:face-down`
- CR 708.2a Yedora returns the dead creature as the Forest land it listed | `check:creature-count` `check:face-down` `check:mana-types`
- CR 708.3 / 708.7 entering face down and turning face up draw nothing | `move:cast-face` `move:action-TurnFaceUp`
- CR 708.7 Pine Walker does not untap a creature cast face up | `move:cast-face`
- CR 708.7 Pine Walker does not untap an Aura turned face up | `board:face-down`
- CR 708.7 Pine Walker untaps the permanent that turned face up, not itself | `board:face-down`

### `FlipSpec`

- CR 603.2 Akki flips on the NONCOMBAT damage Soul's Fire makes it deal to bob | `check:events`
- CR 603.2 Harm's Way sends Akki's combat damage to alice, and Akki does not flip | `check:supertypes` `check:face`
- CR 603.2 the same noncombat damage aimed at alice does not flip Akki | `check:events`
- CR 707.2 a Clone of a flipped Tok-Tok is an unflipped Akki Lavarunner | `check:supertypes`
- CR 707.3 a Clone of Akki Lavarunner flips into Tok-Tok, and a Clone of that is Akki | `check:supertypes` `check:mana-value`
- CR 707.9b a Sakashima that copied Akki flips into Tok-Tok named Sakashima | `check:supertypes`
- CR 710.2 Akki flips into Tok-Tok, keeping CR 710.1c's mana value and colour | `check:helper-costReadings` `check:helper-isLegendary`

### `ForageSpec`

- CR 601.2h / 602.1a a forage paid as an activation cost exiles the three cards the forager chose | `move:action-ActivateManaAbility`
- CR 608.2d a forager who can do neither half is not offered the forage | `check:helper-names` `check:helper-namesIn`

### `GameSpec`

- CR 104.2c gameplay: a Shahrazad subgame won by a team excludes every player on it | `move:nested`
- CR 305.2 at most one land per turn | `board:player-startingdeck`
- CR 400.7/727.2 gameplay: a restart puts Painter's Servant into a library with its chosen colour forgotten | `board:object-chosencolor`
- CR 514.3 a cleanup step with nothing waiting grants no priority | `check:helper-runCountingActions`
- CR 514.3a a state-based action alone fires the exception | `board:continuous-effect`
- CR 514.3a the exception grants a real priority round | `check:prompt-count`
- CR 514.3a the second cleanup step finds nothing waiting and ends the turn | `check:prompt-count`
- CR 603.4/100.6a gameplay: Shahrazad and Sindbad does not trigger once the match has had a subgame | `board:subgamesthismatch`
- CR 605.3a an untapped land's own mana ability is that optional action | `check:other-GameState.lastChoice`
- CR 607.2a: Karn's -3 files the permanent it exiles against Karn | `check:exile-linked`
- CR 723.1/723.3 gameplay: Mindslaver hands alice bob's whole turn, then control lapses | `board:control`
- CR 723.1a gameplay: Word of Command over a Mindslaver hands bob back to alice, not to himself | `board:control`
- CR 723.3/723.5: alice decides for bob, but bob's resources move | `board:control`
- CR 723.5 combat: alice declares bob's attackers, so alice takes the hit | `board:control`
- CR 723.5a: the controller spends only the controlled player's resources | `board:control`
- CR 723.6 a controlled player concedes themselves; their controller cannot do it for them | `board:control`
- CR 727.5/727.4 gameplay: Karn's ultimate leaves its own exiles in exile and then plays them | `board:exile-linked`
- CR 729.1a #153: a question names the game it came from, so a subgame's is not a main-game one | `move:nested`
- CR 729.1a/729.1b #137 gameplay: a TRIGGERED ability plays the subgame, and CR 729.1b's winner is bound | `move:nested` `check:other-GameState.subgamesThisMatch`
- CR 729.1b gameplay: Shahrazad's non-winners each lose half their own life, rounded up | `move:nested`
- CR 729.1b gameplay: a DRAWN Shahrazad subgame is won by nobody, so every player pays | `move:nested`
- CR 729.1b/729.3 gameplay: alice casts a subgame spell, bob decks, bob loses 3 | `move:nested`
- CR 729.5/729.4b gameplay: cards funnel back, main-game board survives, main-game counters untouched | `move:nested`
- CR 800.4a a player who departs paying a cost is not asked again | `check:prompt-count` `check:other-Player.status`
- CR 800.4a/117.4 priority after a concede goes to the next seat, and the pass cycle restarts | `check:other-GameState.players` `check:other-Player.status`
- CR 800.4j after a resolution, priority returns to the next seat, not the departed active player | `board:stack`
- CR 800.4j the active player having left does not stop the turn: priority starts at the next seat | `check:other-Engine.priorityHolder`
- CR 800.4j/703.4i the declare-attackers guard is load-bearing at two seats, where the game loop cannot reach this state | `check:combat` `check:other-Projection.controls`
- CR 800.4k a departed player's turn does not begin | `check:other-GameState.turnNumber` `check:other-GameState.turnOrder`
- M5.6a gate: a three-player game survives a concede, and the departed seat still ends its durations | `board:player-effect`
- M5.6d gate: attacking the monarch takes the crown and frees Palace Jailer's prisoner; attacking the other opponent does neither | `board:exiled-until-monarch`
- a priority round whose only action is Pass leaves it alone | `check:other-GameState.lastChoice`
- land play conserves cards | `board:matchup` `check:object-count`
- no player receives priority after the restart resolves | `board:stack`
- one event short of the limit is not a draw | `ready`
- playing lands fills the battlefield | `board:matchup`
- the conceding player departs as Conceded, not Lost | `check:other-GameState.players` `check:other-Player.status`
- the limit is a draw | `ready`
- the next step runs the rebuilt turn 1's untap step | `board:stack`
- the seat walk terminates when every seat has departed | `check:other-GameState.turnNumber`
- the step the restart fired in does not advance past turn 1's untap step | `board:stack`

### `GoadSpec`

- CR 701.15b whole cards: the goaded creature is sent at carol, not at carol's Jace | `board:combat` `board:object-goadedby`

### `HarnessSpec`

- CR 702.186b the infinity ability exists only once CR 701.64a has harnessed the Stone | `board:object-designations`

### `HealSpec`

- CR 614.1a / 701.69a whole card: Pyramids' shield heals the land instead of letting it be destroyed | `board:object-attachedto`
- CR 701.19c can't be regenerated stops Death Ward's shield and not Pyramids' | `board:object-attachedto`
- CR 701.8a Pyramids' first mode destroys the Aura on a land | `board:object-attachedto`

### `InitiativeSpec`

- CR 726.2 a trampler that trades with its blocker still hands the initiative over | `board:combat` `board:dungeons` `board:initiative`

### `InvestigateSpec`

- CR 609.3 an empty hand reveals nothing and casts nothing | `check:helper-board` `check:helper-bobsHand` `check:helper-revealed` `check:helper-rolling`
- CR 701.20a a random reveal shows the card randomness named, not the first in hand | `check:events`
- CR 701.20a and the middle card, which no fixed reading of the hand reaches | `check:events`
- CR 701.20a the same board with a different roll reveals a different card | `check:events`
- CR 701.60a a spell ends the designation, and CR 701.60c's menace and can't-block end with it | `board:object-designations` `check:designations` `move:expect-rejected`
- CR 701.60b the cost takes the suspected creature, and the menace one is not a candidate | `check:designations`

### `KeywordTriggerSpec`

- CR 509.3e a black creature joining a block a black creature is already in is +2/+0 once | `check:other-Combat.blockersOf`
- CR 509.3e whole card: a Saproling put onto the battlefield blocking pushes an already-blocked attacker over the floor | `board:combat` `board:object-goadedby`
- CR 509.3e whole card: two Saprolings arriving at once cross the floor together | `board:combat` `board:object-goadedby`
- CR 603.3b two DIFFERENT abilities of one source are distinguishable entries | `check:prompt-payload`
- CR 603.6a two triggers of the SAME ability stay indistinguishable | `check:prompt-payload`
- CR 613.1f Glittering Lion losing its shield keeps the restriction backup granted it | `board:continuous-effect`
- CR 613.1f Glittering Lion losing its shield keeps the static ability backup granted it | `board:continuous-effect`
- CR 613.1f only a removal later than the grant takes the granted restriction away | `move:expect-rejected`
- CR 613.1f only a removal later than the grant takes the granted static ability away | `check:abilities`
- CR 613.7 two Evolutions on Wanderwine Prophets champion a Merfolk | `board:objects-ids`
- CR 702.124j the entry trigger searches the TARGET player's library | `check:events`
- CR 702.165a the backed-up creature can't be blocked by a creature with power 2 or less | `board:continuous-effect`
- CR 702.30a control coming back re-opens the window it closed | `board:continuous-effect`

### `LearnSpec`

- CR 118.12 Library of Leng does not reach learn's discard | `board:outside-the-game`
- CR 701.48a a learner may decline both branches | `board:outside-the-game`
- CR 701.48a the first branch discards a card and then draws one | `board:outside-the-game`
- CR 701.48a the second branch takes the Lesson from outside the game, and leaves the non-Lesson there | `board:outside-the-game`

### `LeavesTriggerSpec`

- CR 109.5 an opponent's spell naming ANOTHER player fires nothing | `check:helper-payResponses`
- CR 110.2a a stolen Young Wolf comes back under its OWNER's control | `ref:stack-ability`
- CR 113.3c a TRIGGERED ability naming the same creature draws nothing | `check:designations`
- CR 113.6k the same ability does not fire from a graveyard | `board:exiledwith` `board:haunting`
- CR 113.6m an ability whose delayed trigger returns it from the graveyard functions there | `board:delayed-trigger` `board:entered-with`
- CR 113.6m the same ability does not function from the battlefield | `board:entered-with` `board:graveyard`
- CR 305.7 under Blood Moon the same entry triggers nothing | `check:other-Mana.manaTypesOf`
- CR 400.3 a card leaving bob's graveyard on alice's turn draws nothing | `board:entered-with` `board:graveyard`
- CR 603.10 a card that reaches a graveyard mid-batch is no witness to an earlier event | `check:other-Game.faceOf`
- CR 603.10a a card leaving alice's graveyard on her own turn draws her a card | `board:entered-with` `board:graveyard`
- CR 603.10a the same departure on bob's turn draws nothing | `board:entered-with` `board:graveyard`
- CR 603.2 whole card: Unsummon on bob's Piker makes bob discard | `board:objects-ids`
- CR 603.6a two tokens enter together and each trigger names its own | `check:events` `check:other-DamageEvent.amount`
- CR 603.6a whole card: a Goblin Piker enters and Aether Flash's 2 damage kills it (CR 704.5g) | `check:events` `check:other-DamageEvent.amount`
- CR 608.2a the intervening if is checked AGAIN as the ability resolves | `board:last-known`
- CR 608.2h a second Aether Flash resolves with the entrant already dead, and deals nothing | `check:events`
- CR 614.1 Vizier of Remedies takes persist's counter to zero, so the Goblin returns bare and persists again | `check:helper-countersOn` `check:helper-inGraveyard` `check:helper-named`
- CR 702.55b/702.55c the haunted creature dying fires the card's rider | `board:haunting`
- CR 730.3 a merged token put into a library still puts its Cubwarden card there, and the Seeker grows | `ready`
- Professor Hojo sacrificed to the cost still triggers | `board:object-designations`
- Professor Hojo sees a target its own cost sacrificed | `board:object-designations`
- a card moved from a library into a library is not put into one | `check:other-Event.changeZone`

### `LibraryOrderSpec`

- CR 114.2 CreateEmblem puts an emblem in the command zone under the resolver | `check:other-GameState.command` `check:other-Object.owner` `check:other-Object.zone`
- CR 302.6 GainControl does NOT re-Sick a permanent its controller already controlled | `check:other-Object.sickness`
- CR 603.5 whole card: Deadly Complication's optional clause is asked about while its target lives | `board:object-designations` `check:designations`
- CR 609.3 an empty library looks at nothing and does nothing | `check:helper-zoneNames`
- CR 609.3 an empty library raises no question | `check:helper-asks`
- CR 701.22b scry 0 raises no prompt and moves nothing | `check:helper-scryLibrary`
- CR 701.25a an empty library raises no surveil prompt | `check:helper-asks`
- CR 701.25c Enhanced Surveillance does not turn surveil 0 into a surveil | `check:helper-surveilGraveyard`
- CR 701.25c surveil 0 raises no prompt and moves nothing | `check:helper-asks` `check:helper-surveilGraveyard`
- CR 701.29a the fatesealer is asked, about the chosen opponent's top cards | `check:prompt-payload`
- CR 701.37b the designation leaves with the permanent | `board:object-designations`
- CR 701.44a a revealed land card goes to hand, with no counter and no question | `check:helper-revealedNames` `check:events`
- CR 701.44a a revealed nonland card grows the explorer, and the choice bins it | `check:helper-revealedNames` `check:events`
- CR 701.44b an empty library still grows the explorer | `check:helper-revealedNames` `check:events`
- CR 725 BecomeMonarch TheController makes the resolver the monarch | `check:events`
- CR 725 a crown that goes to an opponent and back inside one resolution still frees the prisoner | `board:exiled-until-monarch`
- CR 725.1/725.3 the crown goes to the TARGETED player, not the controller and not the damage's target | `check:prompt-payload`
- a looked-at nonland card raises no question | `check:helper-asks`

### `LifeReplacementSpec`

- CR 109.5 the row stays with the player who activated it, not the enchantment | `board:replacement`
- CR 109.5 the row watches its controller's draws and nobody else's | `board:replacement`
- CR 121.4 with no row the same empty library records the failed draw | `check:other-GameState.drewFromEmpty`
- CR 614.11 the life a draw replacement substitutes is a gain the row resizes | `board:replacement`

### `LifeTriggerSpec`

- CR 101.4 arming the other way round changes nothing | `board:delayed-trigger`
- CR 101.4 the active player is asked first even though the other seat armed first | `board:delayed-trigger`
- CR 101.4c one entry has nothing to order, so nobody is asked to | `board:delayed-trigger`
- CR 101.4c the entry alice names first is asked first | `board:delayed-trigger`
- CR 101.4c the other order swaps which seat pays which amount | `board:delayed-trigger`
- CR 102.1/603.2 bob gains 2 from his own Radiant Fountain and loses 4 | `board:delayed-trigger`
- CR 111.3 / 601.2b cast for X=2, it mints two 2/2 red Human Knights with trample and haste and arms one entry | `check:delayed-triggers`
- CR 119.9 one lifelink source damaging two blockers at once is one life gain event | `check:events`
- CR 603.10 stripping the PRIDEMATE takes the second gain from it and not the first | `check:other-Projection.triggeredAbilitiesOf`
- CR 603.2c a fourth seat in the batch is a fourth firing | `board:delayed-trigger`
- CR 603.2c three seats gain in one batch, so the entry fires three times | `board:delayed-trigger`
- CR 603.2c two Knights connecting in one step are one trigger event, so the entry fires once | `check:delayed-triggers` `check:events`
- CR 603.7b / 514.2 the same Knights connecting on alice's next turn crown nobody | `check:delayed-triggers`
- CR 603.7b a batch occurred once, so its controller is asked nothing | `board:delayed-trigger`
- CR 603.7b a fourth seat in the batch is still not a question | `board:delayed-trigger`
- CR 603.7b a fourth seat is a fourth candidate | `board:delayed-trigger`
- CR 603.7b a per-occurrence entry on the same batch is asked once | `board:delayed-trigger`
- CR 603.7b one occurrence is not a choice, so bob pays for his own gain | `board:delayed-trigger`
- CR 603.7b the controller names carol's gain out of three simultaneous ones | `board:delayed-trigger`
- CR 603.7b the same batch answered differently drains bob instead | `board:delayed-trigger`
- CR 603.7b the same entry fires again for alice, whose gain is 1 | `board:delayed-trigger`
- CR 702.15e two lifelink attackers connecting at once are two life gain events | `check:events`

### `ManaSourceSpec`

- CR 106.4 the retained mana pays for two activations in a LATER step | `board:combat`
- CR 106.4 the three green land in the UPKEEP player's pool, not the controller's | `check:helper-poolOf` `check:helper-retainedGreen`
- CR 106.6 the mana pays an equip cost and casts no spell | `check:other-Object.manaSpent`
- CR 500.5 a skipped end of combat step still takes the retained mana | `board:combat`
- CR 603.2b on the controller's own upkeep the same trigger pays the controller | `check:helper-poolOf` `check:helper-retainedGreen`
- CR 605.1b the enters trigger resolves off the stack and adds {R}{G} | `check:helper-plainGreen` `check:helper-plainRed` `check:helper-poolUnits`
- CR 605.3a the mana window reaches the Sol Ring and never Millikin | `move:action-ActivateManaAbility`
- CR 605.3a the mana window reaches the Star and never the Sphere | `move:action-ActivateManaAbility`
- CR 724.1d ending the turn during combat takes the retained mana before cleanup | `board:combat`
- CR 724.2d a combat phase ended part-way through takes the retained mana | `board:combat`

### `ManaSpec`

- CR 106.12a a basic land tapped for the CHOSEN colour adds the Gauntlet's additional mana | `board:object-chosencolor`
- CR 106.13 the pool crosses whole, and its production tags with it | `move:Answer-ChooseManaYield`
- CR 106.13 the targeted player loses life for the mana Drain Power takes | `move:Answer-ChooseManaYield`
- CR 117.3c the activator receives priority again afterward | `move:action-ActivateManaAbility`
- CR 118.3 what the activation may yield is gated by its own cost | `move:action-ActivateManaAbility`
- CR 302.6 a stolen Llanowar Elves is not a mana source for the thief | `check:other-Cost.manaActivations` `check:other-Mana.manaSources` `check:other-Projection.controls`
- CR 500.1 the priority window offers the source only inside the rider's step | `move:action-ActivateManaAbility`
- CR 500.5/613.4c whole card: the green Omnath keeps is the green that keeps it big | `check:helper-poolSize`
- CR 601.2a no cast is offered off the sorcery-speed source, the proposal itself closing CR 307.5's window | `move:action-ActivateManaAbility`
- CR 605.1b a land's ability adding the chosen colour with no tap adds Caged Sun's additional mana | `board:object-chosencolor`
- CR 605.3a a mana ability whose cost holds mana may be activated inside the window | `move:action-ActivateManaAbility`
- CR 605.3a a player with priority may fill their pool with nothing to pay for | `move:action-ActivateManaAbility`
- CR 605.3a the order the batch is activated in is the targeted player's | `check:mana-pool` `move:ChooseManaYield`
- CR 605.3a the priority window offers her mana ability on either opponent's turn and not on hers | `move:action-ActivateManaAbility`
- CR 605.5a mana a land's ability adds as it RESOLVES fires Caged Sun, whose trigger then uses the stack | `board:object-chosencolor`
- CR 607.2d a Coldsteel Heart placed with no colour chosen produces nothing | `check:helper-tappedFor` `check:other-Cost.manaActivations` `check:other-Mana.canPay` `check:other-Mana.manaTypesOf` `check:other-Object.chosenColor`
- CR 607.2d a Coldsteel Heart that chose blue offers blue and nothing else | `board:object-chosencolor`

### `ManaSymbolSpec`

- CR 107.4e each symbol picks its own route: {R}{R}{2} and {R}{4} | `check:other-Mana.canPay`
- CR 107.4h whole card: Berg Strider's victim does not untap when snow mana paid for it, and does when it did not | `board:object-doesnotuntapfor`
- CR 601.2b whichever half of {G/U} is announced, the OTHER floats | `check:mana-pool`

### `MassEffectSpec`

- CR 601.2b a player leaving in response does not shrink an X already announced | `ready`
- CR 608.2d Carth the Lion's reveal is not offered without a planeswalker among them | `check:helper-namesIn` `check:helper-revealed`
- CR 608.2d the second choice is made among the cards the first left | `check:prompt-candidates`
- CR 608.2d two cards are taken from among the seven, both of them chosen | `check:prompt-candidates`
- CR 701.17c the return chooses among the milled cards, not among the graveyard | `check:prompt-candidates`
- CR 701.19a a regeneration shield saves its creature from Day of Judgment | `board:replacement`
- CR 701.20e the choice ranges over the matching revealed cards, not over all five | `check:prompt-candidates`

### `MeldSpec`

- CR 202.3c a copy of a melded permanent has mana value 0 | `board:source-ofmeld`
- CR 202.3c a melded permanent's mana value is its components' front faces combined | `check:mana-value` `check:can-block`
- CR 608.2c the ability does nothing without a Hanweir Garrison you both own and control | `board:sickness`
- CR 608.2f the pair leaves the battlefield in one event | `check:events`
- CR 612.7 / 701.42c a Spy Kit host is a Hanweir Garrison the game never saw, and both it and the land stay exiled | `board:object-attachedto`
- CR 701.42a a one-or-more-cards-leave-exile trigger counts both melded cards | `check:events` `check:other-Binding.eventAmount` `check:other-Binding.toAmount` `check:other-Event.eventBindings` `check:other-LoggedEvent.event` `check:other-Moved.change` `check:other-Moved.departures` `check:other-ZoneChange.from` `check:other-ZoneChange.object` `check:other-ZoneChange.to`
- CR 701.42a the melding ability exiles the pair and puts one permanent onto the battlefield | `check:helper-componentPrintings` `check:helper-townshipName` `check:other-Game.printingOf` `check:other-Object.owner` `check:other-Object.source` `check:other-Printing.card`
- CR 701.42b/701.42c a token counterpart melds nothing, and the land stays exiled | `check:intermediate-state`
- CR 712.21 a melded permanent dies as one permanent and arrives as two cards | `ready`
- CR 712.21 a melded permanent's death fires a dies trigger once and a card-arrival trigger twice | `ready`
- CR 712.21a the owner arranges the two cards her melded permanent becomes on top of her library | `board:source-ofmeld`
- CR 712.21c a perpetual grant on a melded permanent follows both cards it becomes | `board:source-ofmeld`
- CR 712.21c a trigger that exiles what the melded permanent became exiles both cards | `ready`
- CR 712.21e a melded permanent's death is two cards put into a graveyard and one object that moved | `board:source-ofmeld`
- CR 712.21e each card of a melded permanent is counted as its own card type | `board:source-ofmeld`
- CR 712.4c/712.9 the melded permanent ignores an instruction to transform | `check:helper-townshipName` `check:other-PC.subtypes` `check:other-Projection.namesOf` `check:other-Projection.project`
- CR 903.10a a melded permanent's combat damage is tallied against its owner's commander | `board:source-ofmeld`
- CR 903.9a a melded commander's own card goes to the command zone, whichever half melded first | `board:source-ofmeld`
- CR 903.9c a melded commander sent to the command zone from a hand leaves its other half in the hand | `board:source-ofmeld`
- CR 903.9c a melded commander sent to the command zone from a library leaves its other half in the library | `board:source-ofmeld`
- CR 903.9c the component split into the command zone is not a card put into the library | `board:source-ofmeld`
- a perpetual grant naming a meld component follows onto the permanent the pair melds into | `board:continuous-effect`

### `ModalDoubleFacedSpec`

- CR 712.12 playing the land face puts THAT face onto the battlefield | `check:abilities` `check:other-GameState.landsPlayed`

### `ModalSpec`

- CR 601.2b all four modes fillable: the prompt is issued and the chosen two resolve | `check:prompt-offers`
- CR 601.2b the mode prompt offers all four and asks for exactly two | `check:prompt-offers`
- CR 608.2b the damage mode fizzles when its only target leaves before resolution | `check:events`
- casting the damage mode binds the 'damaged' slot, never 'wall' | `check:bindings`
- no legal mode removes the trigger from the stack (CR 603.3c) | `check:helper-nothing`

### `MoveCounterSpec`

- a board whose creatures bear no +1/+1 counter moves nothing and still asks nothing | `check:helper-onStack` `check:helper-pairOn`

### `MulliganSpec`

- CR 800.1: a game that BEGAN with three players keeps its free mulligan after a departure | `check:other-Mulligan.freeMulligans`

### `MutateSpec`

- CR 702.140a a stolen mutate spell may target the owner's creature and not the caster's | `board:exile-linked` `move:expect-rejected`
- CR 702.140b a mutating creature spell whose target became illegal resolves as an ordinary creature spell | `board:continuous-effect`
- CR 702.140c a merge asks for its side exactly once, whatever represents the target | `board:source-ofmeld`
- CR 702.140e a Dormant Gomazoa under a Cubwarden still does not untap | `board:source-ofmerge`
- CR 702.140e a Prized Unicorn under a Cubwarden still makes bob block it | `board:source-ofmerge`
- CR 702.140e an Exalted Dragon under a Cubwarden still charges a land to attack | `board:source-ofmerge`
- CR 730.2/707.10 a copy of a mutating creature spell merges, as a copy and not as a card | `board:objects-ids`
- CR 730.2a/613.7 a merge outranks Mirrorweave's copy at once and is recomputed when it ends | `board:copyeffects`
- CR 730.2a/712.8g Cubwarden merges with a melded permanent, over and under | `board:source-ofmeld`
- CR 730.2a/730.2e a face-down merged permanent turned face up shows the topmost component's own face | `board:battlefield` `board:objects-ids`
- CR 730.2d a merged permanent is a token only if its topmost component is | `board:source-ofmerge`
- CR 730.2e a merge over a face-down permanent leaves the topmost component's face-up status | `board:battlefield` `board:objects-ids`
- CR 730.2g a face-down merged permanent with a sorcery card in it stays face down | `board:entered-with` `board:face-down`
- CR 730.2h a flipped merged permanent uses its flip component's alternative characteristics | `board:source-ofmerge`
- CR 730.2h a merged permanent flips for a flip component that is not its topmost one | `board:source-ofmerge`
- CR 730.2i a merged permanent transforms by turning its double-faced component over | `board:source-ofmerge`
- CR 730.2i/702.145c a Cubwarden over a nightbound Werewolf turns with the day and back with the night | `board:daytime` `board:face`
- CR 730.2i/707.2 a Clone of a transformed merged permanent copies the turned-over reading | `board:source-ofmerge`
- CR 730.2j Cyber Conversion does nothing to a merged permanent with a double-faced component | `board:source-ofmerge`
- CR 730.3/111.7 a merged permanent's token component ceases to exist while its card is put into the graveyard | `board:source-ofmerge`
- CR 730.3/603.6c a merged permanent's component fires its own put-into-a-graveyard trigger from either arrangement | `board:source-ofmerge`

### `OutsideTheGameSpec`

- CR 100.4a two copies of one printing are two cards: the second wish finds the second copy | `board:outside-the-game`
- CR 103.2a a deck's sideboard is recorded on its player and minted into no zone | `check:helper-poolOf` `check:other-Game.printingOf` `check:other-GameState.objects`
- CR 307.1 with a spell on the stack the cycle's instant is castable and its sorcery is not | `board:outside-the-game`
- CR 315.3 a conspiracy outside the game cannot be brought in | `board:outside-the-game`
- CR 400.11b the pool is spent: a second Burning Wish finds nothing | `board:outside-the-game`
- CR 400.11c Burning Wish puts the sorcery it revealed from outside the game into her hand | `board:outside-the-game`
- CR 400.11c a draw replaced by a wish brings the card in from outside the game | `board:outside-the-game`
- CR 400.11c a pool holding no sorcery yields nothing | `board:outside-the-game`
- CR 400.11c two eligible cards are a choice, and the answer decides which arrives | `board:outside-the-game`
- CR 400.2 declining outside the game too, the graveyard's match is found | `board:outside-the-game`
- CR 604.2/729.4a gameplay: Death Wish takes Titania's Song out of the main game and its effect goes on applying there | `move:nested`
- CR 608.2d declining Invocation of the Founders' may copies nothing | `board:face`
- CR 701.20a Death Wish brings the card in without revealing it, where Burning Wish reveals | `board:outside-the-game`
- CR 701.23a/701.23j a library find fills the one count, and the library is shuffled | `board:outside-the-game`
- CR 701.23j Invasion of Arcavios brings an instant in from outside the game, and a search that skipped her library shuffles nothing | `board:outside-the-game`
- CR 701.23j a card taken from outside the game fills the count, sparing the graveyard's match | `board:outside-the-game`
- CR 701.23j outside the game alone may find nothing | `board:outside-the-game`
- CR 707.10 Invocation of the Founders copies an instant she casts from her hand | `board:face`
- CR 708.2/729.4 a manifested main-game sorcery is offered to a subgame's wish as a creature and not as a sorcery | `board:objects-ids` `board:outsideobjects`
- CR 729.4/729.4a/729.5 gameplay: Living Wish takes two main-game creatures out of a Shahrazad subgame, and the triggers wait for the main game | `move:nested`
- CR 729.5 gameplay: a wish that takes the resolving Shahrazad itself still finishes resolving with the winner it bound | `move:nested`

### `PhasingSpec`

- CR 702.103g a bestowed Aura that phases in unattached is a creature again | `board:zone-phasedout`
- CR 702.26a a permanent without phasing does not phase out | `check:helper-onBattlefield` `check:other-Phasing.isPhasedOut`
- CR 702.26a another player's untap step phases nothing out | `check:helper-onBattlefield`
- CR 702.26a it phases back in at its own controller's next untap step | `board:zone-phasedout`
- CR 702.26a the cycle repeats every untap step | `check:helper-onBattlefield`
- CR 702.26a/702.26c a phased-out permanent phases in at its controller's next untap step | `check:creature-count` `check:helper-onBattlefield` `check:other-Phasing.isPhasedOut`
- CR 702.26b Reality Ripple phases out a creature with no phasing | `check:other-Phasing.phasedOutStatus` `check:helper-zoneOf`
- CR 702.26f a for-as-long-as duration ends when its permanent phases out | `board:continuous-effect`
- CR 702.26g the Aura phases in with its host, still attached | `check:helper-attachedHostOf` `check:helper-onBattlefield` `check:other-Phasing.isPhasedOut`
- CR 702.26h an object named AND dragged phases out indirectly | `check:phasing`
- CR 702.26i an Aura attached to a PLAYER phases in still attached | `check:phasing`
- CR 702.26j/702.26d neither transition emits an event | `check:events`

### `PlanechaseSpec`

- CR 311.5 a departing active player's plane is replaced from the next seat's planar deck | `board:objects-ids` `board:planardecks` `board:player-startingdeck`
- CR 901.10a a plane leaving the game ends a pending planeswalking ability | `board:objects-ids` `board:planardecks` `board:player-startingdeck`
- CR 901.6 the chaos ability's you is the planar controller | `board:command`

### `PlaneswalkerCombatSpec`

- CR 110.2 a Towershell its attacker does not own returns under the ATTACKER's control | `board:sickness`
- CR 506.4 a planeswalker that stops being a planeswalker stops being attacked, so Soul Snare cannot name the attacker | `board:combat` `board:continuous-effect`
- CR 506.4 a planeswalker whose CONTROLLER changes stops being attacked, so the new controller's Soul Snare cannot name the attacker | `board:combat` `board:continuous-effect`
- CR 506.4 a planeswalker whose controller changes and changes BACK stays removed from combat, so Soul Snare still cannot name the attacker | `board:combat` `board:continuous-effect`
- CR 506.4d whole cards: a blocking Jace that stops being BOTH card types is removed from combat | `board:continuous-effect`
- CR 506.4d whole cards: a blocking Jace that stops being a planeswalker is still a blocking creature | `board:continuous-effect`
- CR 506.4e / 704.5x whole cards: becoming a battle mid-combat, he gets no protector, so bob's Snare cannot name the Elf | `board:continuous-effect`
- CR 506.4e whole card: a creature attacking the planeswalker is attacking a battle bob protects, so his Snare exiles it | `board:continuous-effect` `board:protector`
- CR 506.4e whole cards: attacked as a battle, it stops being one and is still a planeswalker that's being attacked | `board:continuous-effect` `board:protector`
- CR 506.4e whole cards: attacked as a planeswalker, it stops being one and is still a battle that's being attacked | `board:continuous-effect` `board:protector`
- CR 506.4e whole cards: becoming a battle mid-combat, then no planeswalker, he is still a battle that's being attacked | `board:continuous-effect`
- CR 508.1b the prompt is asked once per attacker, over the defending player and their planeswalker | `check:prompt-candidates`
- CR 508.3a the tokens are attacking, and the attack trigger fired only for the Garrison | `check:events`
- CR 508.4 whole card: Hanweir Garrison's two Humans enter tapped and attacking | `check:sickness`
- CR 702.19c an unblocked 7/7 pays Jace's 3 loyalty and sends the other 4 at bob | `check:helper-assignmentLog`
- CR 702.19c the whole 7 may stay on Jace instead | `check:helper-assignmentLog`

### `PlaneswalkerSpec`

- CR 107.1b / 704.5i announced at X=0 she enters with no loyalty and is buried | `check:helper-graveyardCount`
- CR 111.6 a TOKEN put into exile is not a card, so the intervening if fails | `check:events`
- CR 111.6 a card put into exile from the battlefield satisfies the token's intervening if | `check:events`
- CR 205.1b the -6 untaps two lands and makes them 5/5 Elemental creature lands with flying and haste | `check:tapped-count`
- CR 306.8 Lightning Bolt's 3 damage removes all three loyalty counters, and CR 704.5i buries Jace | `check:intermediate-state`
- CR 603.12 / 701.8a the -2's reflexive trigger destroys the planeswalker it targets, and no land is offered | `check:legal-targets`
- CR 603.4 a card put into exile from a hand satisfies the token's intervening if | `check:events`
- CR 603.4 with nothing exiled this turn the token's ability never triggers | `check:events`
- CR 606.5 / 704.5i paying the combined -9 spends all nine counters and buries Jace | `check:intermediate-state`
- CR 606.5 Carth's added +1 makes Jace's -10 activatable at 9 loyalty | `check:helper-activation`
- CR 606.6 without Carth the same Jace at 9 loyalty is not offered its -10 | `check:helper-activation`

### `PlayerDesignationSpec`

- CR 702.131a Secrets of the Golden City blesses its controller and then draws three | `check:helper-marksOf`
- CR 702.131b nine permanents grant nothing | `check:helper-flies` `check:helper-marksOf`
- CR 702.131b ten permanents grant the city's blessing and Skymarcher Aspirant flies | `check:helper-flies` `check:helper-marksOf`
- CR 702.195a an artifact, a Saga and a legend grant an enduring story | `check:helper-hasKeyword` `check:helper-marksOf`
- CR 702.195a two qualifying permanents grant nothing | `check:helper-hasKeyword` `check:helper-marksOf`

### `PlayerEffectSpec`

- CR 119.3 with no restriction Renewed Faith's 6 lands | `check:events` `check:helper-lifeGainsOf`
- CR 119.7 bob's Giant Cindermaw stops alice gaining anything | `check:events` `check:helper-lifeGainsOf`
- CR 119.8 alice's Platinum Emperion takes the 3 damage without the 3 life | `check:events` `check:helper-lifeLossesOf` `check:other-DamageEvent.amount` `check:other-DamageEvent.target`
- CR 120.3a with no restriction the Bolt's 3 damage costs 3 life | `check:events` `check:helper-lifeLossesOf`
- CR 601.2f payment spends the total cost | `check:tapped-count`
- CR 601.2f whole cards: the rewritten filter decides a real cast | `move:expect-rejected`
- CR 612.1 an evolved Edgewalker discounts Zombies and no longer discounts Clerics | `check:helper-textChangesAffecting` `check:helper-totalManaCost`
- CR 612.2 a land-type pair leaves the creature-type filter alone | `check:helper-textChangesAffecting` `check:helper-totalManaCost`
- CR 612.5 the evolved discount moves to the Piker with the text box | `board:continuous-effect`
- CR 613.11 an increase after Reliquary Tower still leaves no maximum | `check:other-PlayerEffect.maximumHandSize`
- CR 613.11 the Cindermaw's controller can't gain either | `check:events` `check:helper-lifeGainsOf`
- CR 613.8b an Evolution on the Piker waits for the Edgewalker's text | `board:continuous-effect`
- without the reducer the same spell is full price | `check:helper-green` `check:helper-totalManaCost`

### `PopulateSpec`

- CR 701.36a the creature token is copied, and neither the nontoken beside it nor the opponent's token is | `check:tokens`

### `PowerToughnessSpec`

- CR 119.3/608.2i two gains in one turn add up: +2/+2 then +4/+4 | `board:events`
- CR 608.2i last turn's life gain does not count | `board:continuous-effect`
- CR 613.1d a Convincing Mirage'd Forest stops being one, and the pump stops with it | `board:hypothetical`

### `PreparationSpec`

- CR 722.2b a Clone of the Aviator becomes prepared and mints a Jump copy | `check:designations`

### `PreventionSpec`

- CR 400.7c the shield follows the chosen permanent spell onto the battlefield | `check:prompt-offers`
- CR 601.2c the shield's description names the object the spell targeted | `board:replacement`
- CR 603.7 the creature the resolution named is exiled when it dies, and no other is | `board:delayed-trigger` `board:player-effect` `board:unregeneratables`
- CR 603.7b the entry is gone by the next turn, so the same death exiles nothing | `board:delayed-trigger` `board:player-effect` `board:unregeneratables`
- CR 608.2h a departed source is narrowed by its last known information, so a Human that has left is choosable | `check:prompt-offers`
- CR 609.7a a slot the installing resolution bound but the row never names is not offered | `board:replacement`
- CR 609.7a a source only a waiting ability still refers to is offered, and shields the damage it deals | `ref:departed-object`
- CR 609.7a a source only a waiting delayed trigger still refers to is offered | `board:delayed-trigger` `board:entered-with` `board:graveyard`
- CR 609.7a a source only a waiting row's captured slot still names is offered | `board:replacement`
- CR 609.7a a source only a waiting shield's baked field still names is offered | `board:replacement`
- CR 609.7a an activated ability a waiting delayed trigger still names is not offered as a source | `board:delayed-trigger`
- CR 609.7a an activated ability on the stack is not a spell, so it is not offered as a source | `check:prompt-offers`
- CR 609.7a an object bound by an unrelated clause of the row's own resolution is not offered | `board:replacement`
- CR 609.7a the same board answering the OTHER source shields that one instead | `board:replacement`
- CR 609.7a the shield watches the source its controller chose and no other | `board:replacement`
- CR 609.7a the unbounded shield watches the source its controller chose and no other | `board:replacement`
- CR 609.7b the shield covering a player watches every source with the printed properties, and no other | `board:replacement`
- CR 611.2c the clause is gone on the next turn, so a fresh shield prevents the same damage | `board:replacement`
- CR 611.2c the shield covers whatever matches its description when the damage would happen | `board:combat` `board:replacement`
- CR 612.1 the swap reaches the word the redirection's CHOSEN SOURCE names | `board:continuous-effect` `board:replacement`
- CR 612.1 the swap reaches the word the shield's RECIPIENT predicate names | `board:combat` `board:continuous-effect` `board:replacement`
- CR 612.1 the swap reaches the word the shield's SOURCE predicate names | `board:continuous-effect` `board:replacement`
- CR 614.10a one Fatigue skips one draw step, and the next one draws | `board:replacement`
- CR 614.10a two Fatigues skip two draw steps, not one | `board:replacement`
- CR 614.1b Eon Hub replaces the upkeep step with nothing | `check:helper-began` `check:other-GameState.remaining`
- CR 614.1b a Fatigue aimed at bob leaves alice's draw step alone | `board:replacement`
- CR 614.9 the named creature's damage cannot be redirected away from it either | `board:replacement`
- CR 615.1 a shield naming only its source covers every recipient | `board:replacement`
- CR 615.1 the shield covers only the recipients the card describes | `board:replacement`
- CR 615.1 the shield covers the player it names and no other | `board:replacement`
- CR 615.12 the clause reaches the creature the resolution named, and no other | `board:replacement`
- CR 615.5 the counters are on before CR 704.5g asks whether the attacker died | `board:replacement`
- CR 615.7 a shield naming NO source watches every source, and asks nothing | `check:helper-answersFor` `check:helper-chosenSourcesIn` `check:other-Stack.resolveTop`
- CR 615.7 a shield that covers the whole batch asks nothing | `check:helper-amounts` `check:helper-answersFor` `check:helper-wasAskedToAllocateDamage` `check:other-Damage.applyDamage` `check:other-GameState.replacements`
- CR 615.7 bob splits the shield between the two events, which no order can do | `ready`
- CR 615.7 the shielded PLAYER chooses which of two simultaneous damages the shield prevents | `board:replacement`
- CR 615.9 only a source with the printed properties can be chosen | `board:replacement`
- CR 700.4 the same creature exiled from the battlefield instead fires nothing | `board:delayed-trigger` `board:player-effect` `board:unregeneratables`
- CR 800.4a a departing player's shield is not a control effect, so it stays | `board:replacement`
- the same shield does prevent combat damage, 1 of it (CR 615.7) | `board:replacement`

### `ProjectionSpec`

- CR 113.6b a graveyard grant's protection still bars the artifact blocker | `check:other-GameState.continuousEffects` `check:other-GameState.command` `move:expect-rejected`
- CR 113.6p an emblem's own replacement row floors the life total from the command zone | `board:command`
- CR 208.1/208.2b Imperial Recruiter's search offers the */* card and the 2-power creature, not the 3-power one | `check:prompt-offers` `check:shuffled-set`
- CR 208.2a Tarmogoyf is a power-0 search candidate with every graveyard empty | `check:prompt-offers`
- CR 208.2a Tarmogoyf is a search candidate at power 2, off a land and an instant in a graveyard | `check:prompt-offers`
- CR 208.2a Tarmogoyf is no search candidate at power 3, a sorcery added to the graveyard | `check:prompt-offers`
- CR 514.2 Turn to Frog wears off at cleanup and the Wraith is a Wraith again | `check:other-GameState.continuousEffects`
- CR 514.2 an until-end-of-turn effect wears off at cleanup | `check:other-GameState.continuousEffects`
- CR 607.2d two Obelisks of Urd each pump the creatures of their OWN chosen type | `board:object-chosensubtype`
- CR 608.2h/611.2d Untamed Might's X is frozen at resolution, not re-read | `board:continuous-effect`
- CR 611 Giant Growth stores a +3/+3 effect; the Piker is 5/4 | `check:other-GameState.continuousEffects`
- CR 611.2a a stored grant's protection still prevents the artifact's damage | `board:continuous-effect`
- CR 612.5 a Hack made before the exchange moves with Glacial Crasher's restriction | `board:continuous-effect`
- CR 612.5 a Hack made before the exchange moves with Kird Ape's text | `board:continuous-effect`
- CR 612.6 two Hacks rewrite the Shapeshifter's Glacial Crasher gate once | `board:combat` `board:objects-ids`
- CR 613.1d Maskwood Nexus makes a library's Giant a Goblin, and Goblin Matron's search offers it | `check:prompt-offers`
- CR 613.3 Turn to Frog beats a changeling's CDA: a Frog and nothing else | `check:subtype-member`
- CR 613.7 an earlier Magical Hack is part of the text box that moves | `board:continuous-effect`
- CR 613.7 two Hacks on Kird Ape compose | `board:objects-ids`
- CR 613.7a a granted changeling beats an OLDER Turn to Frog, where a printed one loses to it | `board:continuous-effect`
- CR 613.7a a levelled Student's text box applies after the Wings on its new host | `board:continuous-effect`
- CR 613.7a without the grant the same Piker is a Frog and nothing else | `board:continuous-effect`
- CR 613.8b Armored Galleon's Hack and the exchange loop back to timestamp order | `board:continuous-effect`
- CR 613.8b Kird Ape's Hack and the exchange loop back to timestamp order | `board:continuous-effect`
- CR 613.8b a Hack on the Piker waits for Kird Ape's text | `board:continuous-effect`
- CR 613.8b a later Hack on one Kird Ape moves with the exchange | `board:continuous-effect`
- CR 613.8b a swap waits for the later one that makes its word | `board:objects-ids`
- CR 613.8b a text change waits for the exchange that gives it a word | `board:continuous-effect`
- CR 613.8b an earlier text change waits for the later one it depends on | `board:objects-ids`
- CR 701.23a without the Nexus, Goblin Matron's search offers the printed Goblin alone | `check:prompt-offers`
- CR 702.16e an emblem's granted protection still prevents the damage | `board:command`
- CR 702.73a changeling granted by a RESOLUTION makes the creature every creature type | `check:subtype-member`

### `PrototypeSpec`

- CR 718.3b cast prototyped the Assembler is a white 2/2 with mana value 2; cast normally a colourless 4/5 with mana value 5 | `check:mana-value`
- CR 718.5 the prototyped permanent keeps vigilance and its activated ability | `board:object-castusing` `board:object-prototyped`

### `RadSpec`

- CR 701.17a it takes the card the turn's mill binned, as a 3/3 green Mutant | `check:legal-targets`

### `RangeOfInfluenceSpec`

- CR 801.10 proliferate offers only players within its controller's range | `check:prompt-offers`
- CR 801.2c a seat emptied mid-turn closes up only when the next turn begins | `check:legal-targets`
- CR 801.4 a target opponent slot does not offer an opponent outside the controller's range | `check:legal-targets`
- CR 801.5a a choice of opponent offers only opponents within the chooser's range | `check:prompt-offers`
- CR 801.5a a choice of player offers only players within the chooser's range | `check:prompt-offers`

### `RemoveCounterSpec`

- CR 122.1 / 608.2d alice removes counters of several kinds from among every permanent, then draws and loses that many | `check:prompt-payload`
- CR 608.2d up to three stun counters from among all permanents, and a card for each | `check:prompt-payload`

### `ReplacementSpec`

- CR 119.4 at 2 life the payment is ILLEGAL, so it enters tapped with no life paid | `check:helper-lostLife` `check:helper-warriorOut` `check:other-Engine.priorityLoop`
- CR 614.1 a row installed while its clause was false applies once the clause turns true | `board:replacement`
- CR 614.12a the copy choice is locked in BEFORE the enters event exists | `check:intermediate-state`
- CR 614.12a/701.21a Shimatsu is not among the permanents it may sacrifice | `check:prompt-offers`
- CR 614.15 with two artifacts metalcraft is off, so the Blast deals its printed 2 | `check:other-GameState.replacements`
- CR 614.1c DECLINING with a Kithkin card in hand still enters tapped | `check:helper-revealsOf`
- CR 614.1c Rustic Clachan REVEALING a Kithkin card enters untapped | `check:events` `check:helper-revealsOf`
- CR 614.1c one +1/+1 counter per creature card in EVERY graveyard | `check:other-S.addGraveyardCard`
- CR 614.1c the Blade enters attached to the creature its controller chose | `check:events`
- CR 614.1c with NO Kithkin card in hand it enters tapped, unasked | `check:events` `check:helper-revealsOf`
- CR 614.1d Zof Bloodbog's own text makes it enter TAPPED | `board:face`
- CR 614.7 an Aura the same pass buries is never offered to a regeneration shield | `board:continuous-effect`
- CR 615.10 Fog prevents both attackers' damage in one batch | `check:events`
- CR 616.1b before CR 616.1c: the NEW controller chooses the copy | `board:replacement`
- CR 616.1b three seats: carol is asked WHICH Gather Specimens takes her creature | `board:replacement`
- CR 616.1d the back-face bucket outranks Kismet's, so no order is asked | `board:daytime`
- CR 701.19a Uses=Once: the first destruction is replaced, the second is not | `board:combat` `board:replacement`
- CR 701.19c whole cards: Terror kills an Uthden Troll that just regenerated | `board:replacement`
- CR 702.82a devour 3 gives three +1/+1 counters per creature sacrificed | `check:prompt-offers`
- CR 702.82c devour artifact 1 offers the artifacts and not the creatures | `check:prompt-offers`
- CR 800.4a a control-on-entry row ends when its controller leaves the game | `board:replacement`

### `ResolveSpec`

- CR 101.4b Jungle Wayfinder's later seats know the earlier seats' answers | `check:prompt-payload`
- CR 101.4b a later payer is told what the payers before it answered | `check:prompt-payload`
- CR 101.4b a later seat knows what the seats before it answered for that land | `check:prompt-payload`
- CR 109.5 the offer is every player, alice included, and not only her opponents | `check:prompt-offers`
- CR 110.5b whole card: Nature's Lore puts the Forest it finds onto the battlefield UNTAPPED | `check:events`
- CR 113.6m / 602.1b Grim Reminder's return is offered from the graveyard during its controller's upkeep only | `board:replacement`
- CR 118.12a Killing Wave asks each creature's controller, and a paid creature alone survives | `check:prompt-order`
- CR 118.12a each land is its own offer, and any player's payment saves only that land | `check:prompt-payload`
- CR 118.3 a seat who cannot pay the sacrifice is not offered it | `check:helper-lands` `check:helper-lives` `check:helper-wormsStands`
- CR 201.4 Petra Sphinx sends a top card its target did not name to the graveyard | `ready`
- CR 201.4 the same board with the other card on top draws one instead | `ready`
- CR 201.4/608.2c a second activation of Petra Sphinx forgets the name the first one chose | `ready`
- CR 201.4/608.2c whole card: Ancient Vendetta's search reads the name its own first clause chose | `ready`
- CR 201.4/608.2c whole card: Predict's mill tally reads the name its own first clause chose | `ready`
- CR 201.4/701.20a whole card: Petra Sphinx matches the name its target chose against the card it revealed | `ready`
- CR 607.2a: killing the OTHER Dragon returns the OTHER card | `board:exile-linked`
- CR 607.2a: the dead Dragon returns the card IT exiled, not the other Dragon's | `board:exile-linked`
- CR 608.2b a Bolt whose only target died fizzles | `check:events`
- CR 608.2h a modification that cannot be frozen is not stored at all | `check:other-GameState.continuousEffects`
- CR 608.2h/611.2d Rush of Blood's X is the power of the creature in its own target slot | `check:continuous-effects`
- CR 615 Fog prevents combat damage but not spell damage (the gate) | `board:replacement`
- CR 701.17d Bruvac doubles the mill, and Predict is answered by the second milled card too | `ready`
- CR 701.20b whole card: Grim Reminder's revealed card stays in the library and costs only the opponent who cast its name | `board:replacement`
- CR 701.23a/701.23e whole card: Hoarding Dragon exiles the artifact it finds, unrevealed | `check:events`
- CR 701.55a the limb bob takes may be the one that serves alice | `check:prompt-payload`
- CR 701.55b Great Intelligence's Plan still offers the discard to an empty-handed opponent | `check:prompt-payload`
- a departed player is not a legal target | `check:legal-targets` `check:other-TargetSlot.required`
- an opponent's creature: that opponent loses the life | `check:creature-count`

### `RestampSpec`

- CR 613.7m / 608.2f the controller orders every card one conjure loop made | `ready`
- CR 614.12 / 608.2f a card one conjure loop made later enters beside the earlier ones, not after them | `ready`

### `ReversalSpec`

- CR 733.1 a window that wrote nothing undoes the announcement | `check:other-Reversal.withoutAnnouncement` `check:events` `check:other-GameState.nextEventGroup` `check:other-GameState.lastChoice`
- CR 733.1 an announcement that wrote nothing keeps the window whole | `check:other-Reversal.withoutAnnouncement`

### `RingSpec`

- CR 701.54 Birthday Escape draws, mints the emblem, and designates the creature its controller chose | `check:helper-temptationsOf`
- CR 701.54a another player gaining control ends the designation, and it does not return | `board:command`
- CR 701.54a two players each keep their own Ring-bearer | `board:command`
- CR 701.54b a Clone of the Ring-bearer is not a Ring-bearer | `check:designations`
- CR 701.54c a bigger creature can't block the Ring-bearer, an equal or smaller one can | `board:command`
- CR 701.54c four temptations drain each opponent when the Ring-bearer connects | `board:command`
- CR 701.54c one temptation does not | `board:command`
- CR 701.54c the Ring-bearer connects past a bigger untapped creature, in a real combat | `board:command`
- CR 701.54c the Ring-bearer is legendary, which CR 205.4e sees | `check:helper-isLegendary`
- CR 701.54c the clause reaches the Ring-bearer alone | `board:command`
- CR 701.54c the drain reaches the Ring-bearer alone | `board:command`
- CR 701.54c the loot reaches the Ring-bearer alone | `board:command`
- CR 701.54c the powers compared are the PROJECTED ones | `board:command`
- CR 701.54c three temptations do not | `board:command`
- CR 701.54c three temptations sacrifice the creature that blocked the Ring-bearer | `board:command`
- CR 701.54c two temptations loot when the Ring-bearer attacks | `board:command`
- CR 701.54d a player with no creatures is still tempted | `check:helper-temptationsOf` `check:events`
- CR 701.54e a stolen Ring-bearer is not legendary while its mark survives | `board:command`

### `RoomSpec`

- CR 709.5f the other branch unlocks a door instead | `move:cast-face`
- CR 709.5g locking a door takes its designation back away | `board:object-unlockedhalves`
- CR 709.5g locking each door takes both designations back away | `board:object-unlockedhalves`
- CR 709.5i 'you' is the player who unlocked, not the Room's controller | `board:object-unlockedhalves`

### `SacrificeRestrictionSpec`

- CR 101.2 whole cards: Village Rites eats the Piker alice owns, never the two she stole | `board:sickness`

### `SagaSpec`

- CR 101.2 a finished Saga that cannot be sacrificed is left where it stands | `board:sickness`

### `SetupSpec`

- CR 727.1 / 103.7 a restarted Planechase game sets a new starting plane | `board:command`
- CR 727.2 / 103.3a a restart puts a face-up scheme back into the shuffled scheme deck | `board:command`
- CR 729.2a the planar deck plays the subgame and comes back | `board:command`
- CR 729.2a the scheme deck plays the subgame and comes back | `board:command`
- CR 729.5/712.21 a subgame ending with a melded permanent returns both of its cards to the main-game library | `check:helper-componentsOn` `check:helper-sourcesOf` `check:other-Game.componentsOf` `check:other-GameState.objects` `check:other-GameState.nextObjectId`
- CR 903.6/903.9c a restarted Commander game puts the melded commander's own card into the command zone and its partner into the deck | `board:source-ofmeld`

### `ShieldCounterSpec`

- CR 122.1c a rule's destruction is not replaced, though a counter is still there | `check:intermediate-state`
- CR 122.1c the counter does not save the bird from a rule's destruction | `ready`
- CR 514.2 the prohibition lasts exactly the turn, and the same shield saves the same creature next turn | `board:replacement`
- CR 701.19c / 704.5g the prohibited creature's shield does not save it from lethal damage | `board:replacement`

### `SpecialActionSpec`

- CR 107.3d the announced X is both the time counters and the mana | `move:action-Suspend`
- CR 116.2d paying the cost lets that player, and only that player, search | `move:action-Ignore`
- CR 116.3 the player receives priority again afterward | `board:objects-ids`
- CR 514.2 the ignore ends at cleanup | `move:action-Ignore`
- CR 603.4 the free play is removed if the card has left exile by the time it resolves | `move:action-Suspend`
- CR 701.9a taking it discards the card without using the stack | `board:objects-ids`
- CR 702.143a casting it costs the foretell cost | `board:foretold`
- CR 702.143a the foretold card is cast for its own mana cost reduced by {2}, the Devourer gone | `move:action-Foretell`
- CR 702.143a the foretold card is castable only by its owner, only later, and only for the foretell cost | `move:action-Foretell`
- CR 702.143b taking it exiles the card face down without using the stack | `move:action-Foretell`
- CR 702.143c a spell cast from a foretold card was foretold, and one cast from hand was not | `board:foretold`
- CR 702.143c foretelling a card triggers its controller's Devourer and no one else's | `move:action-Foretell`
- CR 702.143d a card an effect makes foretold was not foretold by anyone | `check:foretold`
- CR 702.143d an effect makes an exiled card foretold and gives it a foretell cost | `board:hypothetical` `check:foretold` `check:face-down`
- CR 702.143d casting it costs the foretell cost the effect gave it | `board:face-down` `board:foretold`
- CR 702.143d the granted cost is the mana cost of the face being cast | `board:hypothetical` `check:castable`
- CR 702.170b taking it exiles the card without using the stack | `move:action-Plot`
- CR 702.170d casting it costs nothing | `board:plotted`
- CR 702.170d the plotted card is castable only by its owner, and only later | `move:action-Plot`
- CR 702.170f plotting the top card exiles it from the library, and it is cast free later | `check:plotted` `check:tapped-count` `move:action-Plot`
- CR 702.62 the card is exiled with a time counter, ticks down at the next upkeep, and is cast free | `move:action-Suspend`
- CR 707.10 a copy of a foretold spell was not foretold | `board:face-down` `board:foretold`
- CR 707.2a a copy of the Arbiter carries the offer with the ban | `board:object-bindings`
- CR 712.11b casting the back face pays the back face's granted cost | `board:face-down` `board:foretold` `check:face` `move:cast-face`

### `SpeedSpec`

- CR 119.2 damage to an opponent raises your speed too | `check:helper-speedOf` `check:helper-subtract`
- CR 305.7 a Blood Moon'd Raceway starts nobody's engines | `check:helper-speedOf`
- CR 604.1 the grant is re-asked, not latched | `check:abilities`
- CR 702.178a past 4 the Raceway keeps its max speed ability | `board:player-speed`
- CR 702.178b whole card: activating it exiles the Surveyor and draws | `board:player-speed`
- CR 702.179c a card's own text raises the speed a player already has | `check:speed` `check:inherent-triggers-spent`
- CR 702.179c a player with no speed instructed to increase becomes that value | `check:speed`
- CR 702.179d a life loss on somebody else's turn raises nothing | `board:player-speed`
- CR 702.179d a new turn restores the once-each-turn allowance | `board:player-speed`
- CR 702.179d an opponent losing life on your turn raises your speed | `check:helper-speedOf` `check:helper-subtract`
- CR 702.179d speed stops at 4 | `board:player-speed`
- CR 702.179d the increase happens only once each turn | `board:player-speed`
- CR 702.179d your own life loss raises nothing | `check:helper-speedOf` `check:helper-subtract`
- CR 704.5aa a player controlling Muraganda Raceway with no speed gets speed 1 | `check:helper-speedOf`
- CR 704.5aa does not fire again once speed exists | `check:speed`

### `TargetPerPlayerSpec`

- CR 601.2c Afterlife from the Loam takes one creature card from each player's graveyard as Zombies | `check:legal-targets`
- CR 601.2c Dismantling Wave destroys up to one artifact or enchantment of each opponent's | `check:legal-targets`
- CR 602.2b The Theorist, Jace Beleren's -2 returns each opponent's chosen artifact or creature | `check:legal-targets`

### `TargetSpec`

- CR 113.3b Squelch's pool holds the activated ability alone where Stifle's holds both | `check:legal-targets`
- CR 113.9 Stifle's pool holds only the ability and Cancel's only the spell | `check:legal-targets`
- CR 115.7d / 702.21a an unchanged target already illegal may stay, and the new one draws ward | `ref:departed-object,` `check:bindings`
- CR 115.7d a new target that makes an unchanged target illegal is refused | `check:spell-targets`
- CR 601.2c Bioshift's second slot cannot be a creature its first slot's controller does not control | `check:legal-targets`
- CR 601.2c Dwell on the Past's card slot is scoped to the player its other slot targets | `check:other-Target.legalSets`
- CR 601.2c Fall of the Hammer's victim slot cannot be the creature its dealer slot names | `check:legal-targets` `move:expect-rejected`
- CR 601.2c the joint check re-derives a jointly judged slot against the announced X | `check:legal-targets` `move:expect-rejected`
- CR 601.2c the slot admits exactly the creature with a counter on it | `check:helper-abolisherOf` `check:legal-targets`
- CR 601.2c whole card: hexproof from black leaves alice's Doom Blade no legal target, hexproof from white leaves it one | `board:continuous-effect`
- CR 602.2a/115.5 Adric's ability is not offered its own object among the abilities it may counter | `check:legal-targets` `check:events`
- CR 608.2b Doom Blade fizzles when its target gains shroud in response | `board:continuous-effect`
- CR 700.2d a repeated mode's computed bound measures its own occurrence's sibling slot | `check:legal-targets` `move:expect-rejected`
- CR 700.2d a repeated mode's filter reads its own occurrence's sibling slot, not the first's | `check:legal-targets` `move:expect-rejected`
- CR 701.24c Dwell on the Past shuffles the targeted player's library even when both targeted cards have left the graveyard | `move:other-changeZone`
- CR 702.16k a Saltfield Recluse stolen in response still weakens the Nemesis that chose the thief | `board:object-chosenplayer`
- CR 725.5 Dawnglade Regent's hexproof does nothing until its own trigger crowns alice | `check:legal-targets`

### `TeamSpec`

- CR 102.3 a target opponent slot does not offer the teammate | `check:legal-targets`
- CR 502.2a the handoff records what the previous active team cast | `check:events` `check:other-GameState.spellsCastLastTurn`
- CR 702.179d each active teammate's speed rises once | `board:player-speed`
- CR 725.4 the active team's primary player names the new monarch | `check:prompt-offers`
- CR 805.2 a departed active player's teammate declares the attack | `board:player-status`
- CR 805.5 a teammate who passed is asked again before the team passes | `check:prompt-order`
- CR 805.5b a departed active player's teammate receives priority | `board:player-status`
- CR 805.8 controlling a player controls their team | `check:player-control`
- CR 805.8 one effect skips a team's step once | `board:face-down` `move:action-TurnFaceUp`

### `TimeTravelSpec`

- CR 701.56a the choice is offered over the whole candidate set, and over nothing else | `check:prompt-offers`

### `TransformSpec`

- CR 202.3 the bound moves with the life gained, not with the card | `check:legal-targets`
- CR 603.12 / 202.3 taking the may converts Ratchet and returns an in-range artifact tapped | `check:legal-targets`
- CR 603.4 the upkeep triggers read what each player cast last turn | `board:replacement`
- CR 613.7g transforming restamps the permanent past a removal that had wiped its grant | `board:object-designations`
- CR 700.4 the same ability's other limb fires when it dies | `move:other-applyEffect`
- CR 701.27a transforming twice returns the permanent to its front face | `check:helper-faceReadings`
- CR 701.27f a delayed transform is measured from when the ability was created | `board:delayed-trigger` `board:face` `board:object-turnedoverat` `board:replacement`
- CR 701.27f a turn that was ignored triggers nothing | `board:hypothetical`
- CR 701.27g a permanent with its back face up is a transformed permanent | `board:daytime`
- CR 701.27g a permanent with its front face up is not one | `check:helper-faceNameOf` `check:other-GameState.daytime`
- CR 701.27g not one even if it had its back face up previously | `board:daytime`
- CR 702.161a the front face, which has no living metal, is a creature on either turn | `check:helper-faceNameOf`
- CR 712.13a/701.27e the same trigger's enters limb fires on a Piper cast at night | `check:intermediate-state`

### `TriggerSpec`

- CR 101.4/603.3b the active player's trigger is placed first (bottom of stack) | `check:other-Object.owner`
- CR 111.1 "those tokens" names every minted token, so all three are sacrificed | `check:delayed-triggers`
- CR 113.7 a placed trigger binds its source into the reserved self slot | `check:other-Binding.targetsOf` `check:other-Binding.triggerSource` `check:other-Object.bindings`
- CR 116.2c paying the stated cost stops the delayed ability from triggering | `board:delayed-trigger`
- CR 303.4b a red creature the Aura does NOT enchant decides nothing | `check:helper-entering` `check:helper-tapStateOf`
- CR 406.2 "exile them" moves all three, and only them | `check:delayed-triggers`
- CR 514.2 cleanup drops a stated-duration delayed ability | `check:delayed-triggers` `board:hypothetical`
- CR 603.2b running a step records that it began, on the active player's turn | `check:events`
- CR 603.2b two StepBegins triggers from one event emit in ascending ObjectId order | `check:helper-gatheredIn` `check:other-EventGroup.first` `check:other-LoggedEvent.event` `check:other-LoggedEvent.group` `check:other-PendingTrigger.source`
- CR 603.3b one trigger is elided by count alone (Soul Warden, one token) | `check:other-S.tokensOf`
- CR 603.3b the Vigil's entry fires off chapter III triggering | `board:delayed-trigger`
- CR 603.3b the ability triggered by another ability's triggering is placed SECOND, so it resolves first | `check:helper-chaptersOnStackFrom` `check:helper-triggerSourcesOnStack`
- CR 603.3b two triggers under one controller ask for an order, exactly once | `move:OrderTriggers-departed-source`
- CR 603.4 a false intervening "if" leaves the delayed ability armed for the next end step | `board:delayed-trigger`
- CR 603.4 ten cards in hand is not fewer than ten, so nothing triggers | `check:helper-beginEndStep` `check:helper-board` `check:helper-resolveAll`
- CR 603.4 the clause fails on a WHITE host, so nothing triggers | `check:helper-entering` `check:helper-tapStateOf`
- CR 603.6a a SelfEnters trigger does not fire on another object's entry | `check:helper-gathered`
- CR 603.6a a SelfEnters trigger still fires on its own entry | `check:other-PendingTrigger.controller` `check:other-PendingTrigger.source`
- CR 603.6a a noncreature permanent entering fires nothing | `check:helper-sourcesOf`
- CR 603.6a one Soul Warden fires once per entering creature | `check:helper-sourcesOf`
- CR 603.6a two SelfEnters triggers emit in ascending ObjectId order | `check:helper-gathered` `check:other-PendingTrigger.source`
- CR 603.6a whole cards: a second Soul Warden entering gains alice exactly 1 life | `check:intermediate-state`
- CR 603.7 the token is sacrificed at the beginning of the next end step | `check:delayed-triggers`
- CR 603.7b a non-final chapter triggering leaves the one-shot entry armed | `board:delayed-trigger`
- CR 603.7b the delayed ability sacrifices the token at alice's end step | `check:helper-thopters` `check:other-GameState.delayedTriggers`
- CR 603.7c one token already gone leaves the rest sacrificed | `check:delayed-triggers`
- CR 603.7c placement-time's own chosen mode wins a collision with the captured environment | `check:other-Binding.fromChoices` `check:other-Binding.modesOf` `check:other-Object.bindings`
- CR 603.7c with the token already gone the ability does nothing and is consumed | `board:delayed-trigger`
- CR 603.8 a true state condition puts EXACTLY ONE instance on the stack | `check:helper-triggerIds`
- CR 603.8 re-settling while the instance is on the stack adds no second copy | `check:helper-settle` `check:helper-triggerIds`
- CR 603.8 the condition being FALSE means no trigger at all | `check:helper-triggerIds`
- CR 603.8 whole card: an unhacked Outcast asks about SWAMPS and stays | `check:helper-resolveTop` `check:helper-triggerIds`
- CR 608.2h counting first means the token is still alive and is not counted | `move:OrderTriggers-departed-source`
- CR 608.2h sacrificing first makes the Ghoul count the token | `move:OrderTriggers-departed-source`
- CR 707.2 a COPY of the Saga answers with the copy's chapters | `check:intermediate-state`
- CR 800.4d a departed player's delayed ability triggers, is consumed, and is not put on the stack | `board:delayed-trigger` `board:graveyard` `board:player-status`
- a graveyard-bound event yields no enters trigger | `check:helper-gathered`
- advance settles before handing off, so no unscanned event is discarded | `check:events` `check:other-Object.source`

### `TurnSpec`

- CR 500.5a an until-end-of-combat effect expires though the end of combat step never ran | `board:combat` `board:continuous-effect` `board:replacement`
- CR 500.7 damage can't be prevented during the Gambit's own extra turn, and on no other | `check:intermediate-state` `check:player-effects`
- CR 500.7 several extra turns from one effect are added in APNAP order | `check:extra-turns`
- CR 500.7 the skip stays on Savor's own extra turn, not on a later-created one | `board:extraturns` `board:extraturnunderway` `board:turnanchor`
- CR 500.8 Aurelia's added combat phase goes AFTER this one, not inside it | `check:other-GameState.remaining` `check:other-Turn.expandExtraPhase`
- CR 506.1 the control combat phase runs the rest of its steps | `board:combat` `board:continuous-effect`
- CR 508.8 a creature put onto the battlefield attacking keeps the two steps | `check:combat` `check:other-Combat.skipEmptyCombat` `check:other-GameState.remaining`
- CR 511.3 combat is emptied though the end of combat step never ran | `board:combat` `board:continuous-effect` `board:replacement`
- CR 511.3 the control end of combat step runs, and the phase's end sweeps anyway | `check:other-Turn.afterBlockersDeclared`
- CR 601.3 Mandate of Peace is castable only during a combat phase | `board:combat` `board:continuous-effect`
- CR 603.7a uncleaved, alice loses at the end step of the unpreventable extra turn | `check:intermediate-state`
- CR 611.1 the until-end-of-turn prohibition outlives the phase it ended | `board:combat` `board:continuous-effect`
- CR 614.10a the end of combat step never begins, and the phase ends anyway | `check:helper-began` `check:other-GameState.remaining`
- CR 724.1d ending the turn during combat expires that phase's effects | `board:combat` `board:continuous-effect`
- CR 724.1d ending the turn skips straight to the cleanup step | `check:step`
- CR 724.1f no player gets priority once the turn has ended | `check:prompt-count`
- CR 724.2b it exiles the whole stack, the resolving spell included | `board:combat` `board:continuous-effect`
- CR 724.2d an until-end-of-combat effect expires though the end of combat step never ran | `board:combat` `board:continuous-effect`
- CR 724.2d ending the combat phase skips straight to the next phase | `board:combat` `board:continuous-effect`
- CR 724.2d it removes every creature from combat | `board:combat` `board:continuous-effect`
- CR 724.2f no player gets priority once the combat phase has ended | `board:combat` `board:continuous-effect`
- CR 724.2g ending the combat phase outside one does nothing | `board:combat` `board:continuous-effect`
- advance on an empty schedule hands off the turn | `check:other-GameState.remaining` `check:other-GameState.turnNumber` `check:other-Turn.firstPhase` `check:other-Turn.laterPhases`
- advance pops the schedule head into the current phase | `check:other-GameState.remaining`

### `UntapRestrictionSpec`

- CR 502.3 a control change between the steps: the new controller's untap step is the second one skipped | `board:object-doesnotuntapfor` `board:replacement`
- CR 502.3/611.2a whole card: Telekinesis' target stays tapped through its controller's next two untap steps and untaps at the third | `board:object-doesnotuntapfor` `board:replacement`
- CR 611.2a Elvish Hunter's one step on top of Telekinesis' two still ends at the second step | `board:object-doesnotuntapfor` `board:replacement`

### `VanguardSpec`

- CR 902.7 a vanguard's activated ability may be activated from the command zone | `board:command`
- CR 902.7 a vanguard's triggered ability triggers from the command zone | `board:command`

### `VariableEffectSpec`

- CR 115.6 Rat Out aimed at a creature shrinks it and still makes the Rat | `check:intermediate-state`
- CR 601.2c X=1 announcing three targets destroys only one | `move:out-of-range-answer`
- CR 603.2c two counters on one creature at once is one trigger | `check:events`
- CR 608.2c two clauses of one resolution are two trigger events | `check:helper-minusCountersOn` `check:helper-placementGroups`
- CR 608.2f each victim takes the mana value of the card exiled FOR IT, in APNAP order | `check:playable-from-exile`
- CR 701.41a support on a permanent cannot choose that permanent | `check:legal-targets` `move:expect-rejected`
- CR 701.47a a lone Army raises no prompt | `board:token` `board:controller`
- CR 701.47a amass counters the Army its controller chose | `board:token` `board:controller`
- CR 701.47a the same board answered the other way counters the other Army | `board:token` `board:controller`
- CR 701.47c a later clause reads the amassed Army she named | `board:token` `board:controller`
- CR 701.47c the same board answered the other way mills the other Army's power | `board:token` `board:controller`

### `VoteSpec`

- CR 701.38a the permanent with the most votes is exiled and the rest are not | `check:prompt-offers` `check:helper-askedPlayers`
- CR 701.38b a clause gated on a word's tally happens and the clause gated on the other does not | `check:prompt-offers` `check:helper-askedPlayers`
- CR 701.38d an additional vote is a second ballot for that seat, taken before the next seat votes | `check:helper-askedPlayers`

### `ZoneChangeSpec`

- #222 an answer naming a player who is not in the game is refused | `move:out-of-range-answer` `check:events`
- CR 104.3b the control: one creature is one life, and carol stays in the game | `check:events` `check:helper-lifeGains` `check:helper-lifeLosses`
- CR 110.2 the control: a creature carol owns but bob controls is charged to BOB | `board:sickness`
- CR 110.2 the control: a land carol owns but bob controls is charged to BOB | `board:sickness`
- CR 118.12a carol pays 5 life, so the discarded creature stays in the graveyard | `check:helper-battlefieldNames` `check:helper-namesIn` `check:helper-payResponses`
- CR 119.3 Sign in Blood makes the player it targets draw two and lose two life | `check:events` `check:helper-subtract`
- CR 119.3 Stronghold Discipline charges each player for their OWN creatures | `check:helper-lifeLosses`
- CR 119.5 Arbiter of Knollridge raises every seat to the HIGHEST total, gaining only where the total moves | `check:helper-lifeGains` `check:helper-lifeLosses`
- CR 119.5 Biorhythm sets EACH seat to its OWN creature count | `check:events` `check:helper-lifeGains` `check:helper-lifeLosses`
- CR 119.7 an exchange that would raise a player who can't gain life doesn't happen at all | `check:helper-lifeGains` `check:helper-lifeLosses`
- CR 119.9 an exchange between equal totals logs no life event | `check:helper-lifeGains` `check:helper-lifeLosses`
- CR 120.3a Acidic Soil deals each player their OWN land count | `check:helper-damages`
- CR 121.2c Vision Skeins draws for the active player first, then in turn order | `check:helper-drawersOf`
- CR 121.3 drawing from an empty library records the failed draw | `check:other-GameState.drewFromEmpty`
- CR 202.3b a Clone copying a TRANSFORMED Stonewing Antagonizer has mana value 0, not the front face's 1 | `check:mana-value` `board:face`
- CR 303.4b / 614.1a Wheel of Sun and Moon reroutes only the enchanted player's cards | `check:helper-revealsOf` `check:events`
- CR 401.2 each attacking creature's OWNER picks the end, not the resolving controller | `board:sickness`
- CR 401.7 Oust into a one-card library puts the creature on the bottom | `board:sickness`
- CR 401.7 Oust: second from the top, and its CONTROLLER gains 3 life | `board:sickness`
- CR 401.7 Temporal Cleansing: the OWNER picks second from the top, and it lands under the top card | `board:sickness`
- CR 401.7 Temporal Cleansing: the owner picks the bottom instead | `board:sickness`
- CR 401.7 Unexpectedly Absent with X = 0 puts it on top | `board:sickness`
- CR 401.7 Unexpectedly Absent with X = 2 puts it just beneath the top two cards | `board:sickness`
- CR 603.4 Psychic Theft's card, once alice casts it, triggers nothing at her end step | `board:delayed-trigger`
- CR 608.2c Make a Wish returns two DISTINCT cards, the same answer given twice | `check:prompt-offers`
- CR 701.10d doubling a life total leaves the target at twice its OWN total, as a life gain | `check:helper-lifeGains` `check:helper-lifeLosses`
- CR 701.10d the control: the same card aimed at another seat doubles THAT seat's total | `check:helper-lifeGains`
- CR 701.12a an exchange left with one side does nothing at all | `check:helper-lifeGains` `check:helper-lifeLosses`
- CR 701.12c Soul Conduit exchanges the totals of two players, neither of them its controller | `check:helper-lifeGains` `check:helper-lifeLosses`
- CR 701.12c whole card: Mirror Universe swaps its controller's total with the target's | `check:helper-lifeGains` `check:helper-lifeLosses` `check:other-Target.legalSets`
- CR 701.19a Murder is replaced by regeneration | `board:replacement`
- CR 701.19a regeneration does not save a bounced creature | `board:replacement`
- CR 701.9 / 101.4 Tinybones Joins Up has every targeted player discard, in APNAP order | `check:prompt-order`
- CR 701.9a Aether Rift returns the creature card it discarded when nobody pays | `check:helper-battlefieldNames` `check:helper-namesIn` `check:helper-payResponses`
- CR 701.9a Dream Salvage draws as many cards as the target discarded this turn | `board:battlefield` `board:objects-ids`
- CR 701.9a a noncreature card discarded by Aether Rift stays put and nobody is asked to pay | `check:helper-battlefieldNames` `check:helper-namesIn` `check:helper-payResponses`
- CR 701.9b Duress discards the card its caster chose from the revealed hand | `check:other-prompt-offer` `check:helper-revealsOf`
- CR 701.9c a creature card Aether Rift discarded into exile is not returned | `check:helper-battlefieldNames` `check:helper-namesIn` `check:helper-payResponses`
- CR 701.9c a land discarded into a library unrevealed does not return Psychic Miasma | `check:helper-revealsOf`
- CR 707.2 a Clone copying the UNTRANSFORMED Thraben Gargoyle keeps that face's mana value | `check:mana-value`
- CR 724.1e Psychic Theft's card cast before a Time Stop triggers nothing at the next turn's end step | `board:delayed-trigger`
- CR 800.4a Vision Skeins does not draw for a player who has left the game | `check:helper-drawersOf`
- Reverse the Sands whole card: the controller's permutation is what happens, seat by seat | `check:helper-lifeGains` `check:helper-lifeLosses`
- a rotation moves every seat, and in the direction the answer names | `check:helper-lifeGains` `check:helper-lifeLosses`
- one remaining player leaves only the identity, so no prompt is raised | `board:player-status`
- redistributing among nobody is a legal answer and moves nothing | `check:helper-lifeGains` `check:helper-lifeLosses`
- the same board answered differently redistributes differently at every seat | `check:helper-lifeGains` `check:helper-lifeLosses`

### `ZoneReplacementSpec`

- CR 613.1f Can't Stay Away's quoted replacement exiles the returned creature and nothing else | `board:continuous-effect` `board:entered-with`

### `ZoneTriggerSpec`

- CR 113.6k a battlefield-only trigger on a card that arrived in a graveyard and left it is not offered | `board:delayed-trigger` `board:entered-with`
- CR 113.6k the trigger carries the whole printed ability | `check:helper-beginUpkeep` `check:helper-gathered` `check:other-Face.triggeredAbilities` `check:other-PendingTrigger.ability` `check:other-S.combinedFace` `check:other-TriggeredAbility.condition`
- CR 113.6m Squee's upkeep trigger is gathered from the graveyard, on its effect's word alone | `check:helper-gathered` `check:other-PendingTrigger.source`
- CR 113.6m the same card on the battlefield triggers for nobody | `check:helper-gathered` `check:other-PendingTrigger.source`
- CR 113.6m the upkeep half does not trigger from the battlefield | `check:helper-beginUpkeep` `check:helper-gathered` `check:other-PendingTrigger.source`
- CR 114.2 another seat's end step fires nothing | `board:command`
- CR 114.4 the emblem's trigger is gathered from the command zone | `board:command`
- CR 114.4 whole card: three Cat tokens arrive at its controller's end step | `board:command`
- CR 400.7e a bounce to a HIDDEN zone binds no became slot, though the source slot is still stamped | `check:other-Binding.became` `check:other-Binding.triggerSource`
- CR 400.7e a death to a PUBLIC zone does bind became, for the same condition | `check:other-Binding.became` `check:other-Binding.triggerSource`
- CR 603.10 a permanent that arrived after a death and left again does not witness it | `check:events` `check:intermediate-state`
- CR 603.10a Oglor's perpetual grant fires as the milled card leaves the graveyard | `board:continuous-effect`
- CR 603.10a a Meren who died earlier in the batch sees neither token buried later in it | `check:events`
- CR 603.6a whole card: casting Thragtusk gains 5 life, and its leaves trigger stays silent on the way in | `check:helper-beastsOf`
- CR 608.2c two clauses of one resolution are two trigger events | `check:events`
- CR 608.2f four Goblins swept into graveyards give the Townsfolk one counter | `check:helper-deathGroups`
- CR 704.3 two death groups in one trigger scan are two trigger events | `check:events`
