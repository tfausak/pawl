# Hard cards

Cards that are especially hard for a rules engine, one per mechanism. Each
entry names what it stresses, where that is proven, or what is missing. A card
earns a row only for an axis no other row covers; a second card that stresses
the same thing doesn't. Paper cards only: Alchemy, Un-cards and playtest cards
are out of scope here even where they are hard.

A card need not be in `data/cards/` to be listed. A missing capability gets an
issue; an expressible card not yet in the pool is a plain card add.

| Card | What it stresses | Status |
|---|---|---|
| Opalescence + Humility | Layer 4 animating a layer 6 ability remover; timestamp order within layer 7b (CR 613.6, 613.7, 613.8a) | Proven: `ProjectionSpec` "CR 613.7 Humility + Opalescence" cases |
| Blood Moon | Setting a basic land type strips abilities; dependency overrides timestamps (CR 305.7, 613.8) | Proven: `ProjectionSpec` Urborg and Life and Limb cases, `ManaSpec` |
| Dryad Arbor | One object that is a land and a creature; played, never cast (CR 305.9, 302.6) | Proven: `CastPermissionSpec`, `ManaSpec` Humility case |
| Painter's Servant | A battlefield static that colours cards in every zone, hidden ones included (CR 611.3a, 613.1e) | Partial: spells and emblems proven in `ColorSpec`; hand and library untested (#4784) |
| Vesuvan Doppelganger | Copy with an exception and a self-reproducing added ability (CR 707.9a, 707.9c) | Proven: `CopySpec` "CR 707.9c" cases; forgetting linked choices on re-copy open (#4729) |
| Volrath's Shapeshifter | Graveyard order driving a full-text change (CR 612.6, 404.3) | Proven: `ProjectionSpec` "CR 612.6" cases, `ZoneChangeSpec` |
| Magical Hack | Swapping a land-type word wherever the text uses it as one: filters, landwalk, mana, counters, shields, durations (CR 612.1, 612.2) | Proven: `CastSpec` `magicalHackSpec`, `CounterspellSpec` `magicalHackTimingSpec`, `data/scenarios/projection/` |
| Artificial Evolution | A creature-type swap that renames the tokens the text creates, under a word the card forbids (CR 612.2a) | Proven: `data/scenarios/counterspell/cr-612-2a-*`; the forbidden word is not enforced (#641) |
| Sleight of Mind | Swapping a colour word in a spell's or permanent's text (CR 612.2) | Gap (#4819) |
| Ixidron | Mass face-down on entry; a count of face-down creatures as P/T (CR 708.2, 614.1c, 614.12) | Proven: `FaceDownSpec` |
| Hanweir Battlements + Hanweir Garrison | Meld: two cards, one permanent, leaving as two (CR 701.42, 712.4, 712.21) | Proven: `MeldSpec` |
| Teferi's Protection | Player protection from everything, a life lock and mass phasing, all until your next turn (CR 702.16j, 119.7, 119.8, 702.26) | Expressible, not in pool |
| Karn Liberated | Restarting the game, sparing linked exiles (CR 727, 607.2a) | Proven: `GameSpec` "CR 727.5/727.4 gameplay" |
| Shahrazad | A subgame inside a resolution (CR 729) | Proven: `GameSpec` "CR 729" gameplay cases |
| Burning Wish | Cards from outside the game, including the main game from inside a subgame (CR 400.11, 729.4a) | Proven: `OutsideTheGameSpec`, with meld and merge via Living Wish |
| Word of Command | Controlling another player for one resolution, mid-spell (CR 723.2, 723.7) | Proven: `data/scenarios/game/cr-723-*` |
| Time Stop | Ending the turn: exiling the stack, skipping to cleanup (CR 724.1) | Proven: `TurnSpec` `endTurnSpec`; a trigger during the process is untested (#4785) |
| Sphinx of the Second Sun | An extra beginning phase: a second untap, upkeep and draw in one turn (CR 500.8, 302.6, 305.2) | Gap (#4782) |
| Gemstone Caverns | A pregame action from the opening hand, gated on the starting player (CR 103.6) | Proven: `MulliganSpec`, `ManaSpec` |
| Serum Powder | An action taken inside the mulligan procedure, any time a player could mulligan (CR 103.5b) | Proven: `MulliganSpec` CR 103.5b cases |
| Tovolar, Dire Overlord | Day and night: a game designation switched at untap, forced by a card, transforming daybound permanents (CR 731, 702.145) | Proven: `DaytimeSpec`, `data/scenarios/daytime/` |
| Palace Jailer | The monarch, and a duration that waits for an opponent to be crowned, outliving its source (CR 725, 610.3d) | Proven: `LibraryOrderSpec` "CR 725" cases, `GameSpec` M5.6d gate |
| Acererak the Archlich | Dungeons: command-zone cards, completion read by name behind an intervening "if" (CR 309.7, 701.49, 603.4) | Proven: `DungeonSpec` "Acererak asks WHICH dungeon"; its attack trigger is untested (#4788) |
| Melira, Sylvok Outcast | Poison: a "can't" overriding infect's results while the damage is still dealt (CR 101.2, 702.90, 702.15b) | Partial: `CounterRestrictionSpec` with spell-placed counters; infect damage untested (#4789) |
| Worldgorger Dragon + Animate Dead | A mandatory loop making new objects each pass; a draw only with no optional action (CR 104.4b, 732.4, 303.4f) | Expressible, Dragon not in pool; the axis is proven by `GameSpec` `mandatoryLoopBoardSpec` with Sporemound |
| Lich's Mirror | Replacing a loss of the game (CR 614.1a, 104.3, 104.4a) | Gap (#4783) |
| Panglacial Wurm | Casting from a hidden zone mid-resolution, while searching (CR 601.3, 608.2g) | Proven: `CastSpec`, `data/scenarios/cast/cr-601-3-*` |
| Stifle | Countering an ability on the stack (CR 701.6a, 113.9) | Proven: `CounterspellSpec` `stifleSpec` |
| Chains of Mephistopheles | A draw replacement whose replacement draws again; per-draw-step exception (CR 614.5, 616.1f) | Gap (#4781) |
| Knowledge Pool | A cast trigger exiling the spell into a linked pool, then the caster casting another pooled card mid-resolution (CR 601.2i, 607.2a, 608.2g, 118.12) | Expressible, not in pool |
| Doubling Season | Effect-only replacements on tokens and counters, ordered by the affected player against others; costs untouched (CR 614.16, 616.1) | Proven: `data/scenarios/replacement/cr-616-1-*` with Hardened Scales, planeswalker loyalty scenarios |
| Wear // Tear | Split-card characteristics varying by zone; fuse casting both halves from hand (CR 709.4, 702.102) | Proven: `CastRestrictionSpec` `wearTearSpec`; hand-only gate untested (#4791) |
| Nacatl War-Pride | Blocking requirements and a per-attacker restriction maximised across token copies (CR 509.1b, 509.1c) | Gap (#4790) |
| Deflection | Changing a spell's single target (CR 115.7a, 601.2c) | Proven: `TargetSpec` `deflectionSpec` |
| Fork | Copying a spell with a colour exception and new targets (CR 707.10, 707.9b) | Expressible, not in pool |
| Isochron Scepter | Linked imprint; casting a copy of an exiled card, repeatably (CR 607.2a, 707.12) | Expressible, not in pool |

## Rules written for one card

The comprehensive rules name some cards outright: "One card (X) ...". Each is
machinery the rules carve out for a single printing. Find them with `grep -nE
'(One|Two) cards? \(' docs/rules.txt`. Rules naming a card already in the table
above (Karn Liberated, Volrath's Shapeshifter, Shahrazad, Word of Command) are
not repeated.

| Rule | Card | Status |
|---|---|---|
| CR 103.1c | Power Play | Tested in `ConspiracySpec` |
| CR 106.13 | Drain Power | Tested in `ManaSpec`, `data/scenarios/mana/cr-106-13-*` |
| CR 116.2e | Circling Vultures | Tested in `CostSpec`, `CounterspellSpec`, `SplitSecondSpec` |
| CR 206.3a-c | City in a Bottle, Golgothian Sylex, Apocalypse Chime | Tested in `FilterSpec`, `BoardEffectSpec`, `data/scenarios/board-effect/` |
| CR 207.5 | Cryptic Spires | Gap (#884) |
| CR 612.5, 701.12h | Exchange of Words | Tested in `ProjectionSpec`, `data/scenarios/projection/` |
| CR 612.7 | Spy Kit | Tested in `CastProhibitionSpec`, `DamageSpec` |
| CR 702.158 | Space Beleren | Gap (#3522) |
| CR 706.5 | Celebr-8000 | Gap (#3243) |
| CR 706.8 | Centaur of Attention | Tested in `data/scenarios/dice/cr-706-8-*` |
| CR 707.13 | Garth One-Eye | Tested in `CopySpec`, `data/scenarios/copy/cr-400-11-*` |
| CR 707.14 | Magar of the Magic Strings | Tested in `CopySpec`, `data/scenarios/copy/` |
| CR 723.2 | Opposition Agent | Gap (#3348) |
| CR 724.2 | Mandate of Peace | Tested in `TurnSpec`, `data/scenarios/turn/cr-724-2*` |

## Considered and left out

- Emrakul, the Promised End and Mindslaver: Word of Command is the harder form
  of controlling a player.
- Mutate: meld covers several cards as one permanent.
- Life and Limb: Dryad Arbor covers the land creature.
- Banding: it moves who assigns combat damage, not a new mechanism.
- Flip cards, modal double-faced cards and the other layouts: once one
  alternate layout works, the rest follow from it.
- Fiend Hunter: linked abilities split across two triggers are too narrow.
- Dexterity (Chaos Orb): out of scope; see `docs/design.md` section 6.
- Ante: out of scope for the same reason as Un-cards, until Pawl implements it.
