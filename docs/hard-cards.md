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
| Worldgorger Dragon + Animate Dead | A mandatory loop making new objects each pass; a draw only with no optional action (CR 104.4b, 732.4, 303.4f) | Expressible, Dragon not in pool; the axis is proven by `GameSpec` `mandatoryLoopBoardSpec` with Sporemound |
| Lich's Mirror | Replacing a loss of the game (CR 614.1a, 104.3, 104.4a) | Gap (#4783) |
| Panglacial Wurm | Casting from a hidden zone mid-resolution, while searching (CR 601.3, 608.2g) | Proven: `CastSpec`, `data/scenarios/cast/cr-601-3-*` |
| Stifle | Countering an ability on the stack (CR 701.6a, 113.9) | Proven: `CounterspellSpec` `stifleSpec` |
| Chains of Mephistopheles | A draw replacement whose replacement draws again; per-draw-step exception (CR 614.5, 616.1f) | Gap (#4781) |
| Deflection | Changing a spell's single target (CR 115.7a, 601.2c) | Proven: `TargetSpec` `deflectionSpec` |
| Fork | Copying a spell with a colour exception and new targets (CR 707.10, 707.9b) | Expressible, not in pool |
| Isochron Scepter | Linked imprint; casting a copy of an exiled card, repeatably (CR 607.2a, 707.12) | Expressible, not in pool |

## Considered and left out

- Emrakul, the Promised End and Mindslaver: Word of Command is the harder form
  of controlling a player.
- Mutate: meld covers several cards as one permanent.
- Life and Limb: Dryad Arbor covers the land creature.
- Banding: it moves who assigns combat damage, not a new mechanism.
- Chaos Orb: dexterity is outside the engine, not hard within it.
