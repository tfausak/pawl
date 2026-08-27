# Subgames and cards outside the game --- implementation plan

> **For agentic workers:** REQUIRED SUB-SKILL: use superpowers:subagent-driven-development
> (recommended) or superpowers:executing-plans. Steps use checkbox (`- [ ]`) syntax.

**Goal:** a wish cast inside a subgame reaches the main game's cards, and a
main-game ability that triggers on the card leaving its zone waits until the main
game resumes (CR 729.4 / 729.4a / 729.5). Closes #152.

**Architecture:** the subgame never holds, reads or mutates the parent state.
`Setup.subgameStateFrom` hands the subgame a SNAPSHOT --- `GameState.outsideObjects`,
one entry per main-game card, carrying owner and printing and keyed by the id that
card has OUT THERE. Taking one removes its entry and appends the outer id to
`GameState.broughtIn`. When `Engine.playSubgame` returns, that frame --- the only
one holding both games --- walks `broughtIn` in order and applies each departure to
the parent: file CR 608.2h last known information, record `GameEvent.LeftTheGame`,
delete the object. The parent's next settle, which is the one after the subgame's
spell finishes resolving, puts the triggers on the stack; that is CR 729.5's last
sentence with no queue to build.

**Why the snapshot is exact and not an approximation.** The main game is
discontinued for the whole subgame (CR 729.1a): no priority, no state-based
actions, no continuous-effect recomputation. So a snapshot taken at the subgame's
start equals the main game's state at every instant of the subgame, and applying
the departures at the end --- in `broughtIn` order --- gives the same events, the
same last known information and the same trigger order as applying them at the
instant each card crossed. The alternative that was rejected: a recursive
`GameState.outerGame :: Maybe GameState`, which would let the subgame mutate the
main game and would put a `GameState` inside its own wire codec.

**Why the filter reads the printed face.** CR 729.1b: "no effects or definitions
created in either the main game or the subgame have any meaning in the other". So
a main-game object is read from the subgame as its card --- printed face plus CR
604.3's characteristic-defining abilities --- which is exactly
`Projection.viewOfCard`, the view the sideboard pool already uses. No projected
characteristics are snapshotted, and the subgame cannot see a main-game
continuous effect.

**Spec:** this document; issue #152 and its comments carry the case.

## Global constraints

- CR is ground truth: `docs/rules.txt`, grepped by number. 729.1a, 729.1b, 729.4,
  729.4a, 729.5, 400.11, 400.11b, 400.11c, 603.10a, 608.2h.
- The rules core must never case on an effect's IDENTITY. Nothing in this plan
  adds such a case; the wish reaches `Pawl.Engine.OutsideTheGame` through the
  existing `Effect.RevealFromOutsideTheGame` arm, which passes a `Filter` through.
- One type per module under `Pawl.Types.<TypeName>`; `Mk` prefix for a
  single-constructor record; bare constructors for a sum.
- Adding a module means staging `pawl.cabal` and running `cabal-gild pawl.cabal`
  directly --- `hooky fix` has skipped that case.
- STAGE, then `hooky fix`, then `git add` again.
- After each task: `cabal test`. Before the PR: mutate each new assertion away and
  record which assertion reddened.

## File map

| File | Responsibility |
|---|---|
| `source/libraries/types/Pawl/Types/OutsideObject.hs` | new: one main-game card as the subgame sees it |
| `source/libraries/types/Pawl/Types/OutsideCard.hs` | new: which card a wish reaches --- pool entry or outer object |
| `source/libraries/types/Pawl/Types/GameState.hs` | two new fields: `outsideObjects`, `broughtIn` |
| `source/libraries/types/Pawl/Types/Prompt.hs` | `ChooseFromOutsideTheGame` now offers `OutsideCard` |
| `source/libraries/types/Pawl/Types/Response.hs` | `ChoseFromOutsideTheGame` carries `OutsideCard` |
| `source/libraries/codec/Pawl/Codec/OutsideObject.hs` | new codec |
| `source/libraries/codec/Pawl/Codec/OutsideCard.hs` | new codec |
| `source/libraries/codec/Pawl/Codec/GameState.hs` | the two new fields on the wire |
| `source/libraries/engine/Pawl/Engine/Setup.hs` | fill the snapshot; funnel the subgame's pools back |
| `source/libraries/engine/Pawl/Engine/OutsideTheGame.hs` | eligibility over both sources; the crossing |
| `source/libraries/engine/Pawl/Engine/Engine.hs` | `playSubgame` applies the departures |
| `source/libraries/engine/Pawl/Engine/Replay.hs` | the prompt's transcript arms |
| `data/cards/living-wish.json` | new card |
| `source/libraries/test/Pawl/OutsideTheGameSpec.hs` | the unit cases and the gameplay case |

---

### Task 1: the snapshot's type and the two state fields

**Files:**
- Create: `source/libraries/types/Pawl/Types/OutsideObject.hs`
- Create: `source/libraries/codec/Pawl/Codec/OutsideObject.hs`, `.../OutsideObjectSpec.hs`
- Modify: `source/libraries/types/Pawl/Types/GameState.hs`, `source/libraries/codec/Pawl/Codec/GameState.hs`, `source/libraries/engine/Pawl/Engine/Setup.hs`, `pawl.cabal`

**Produces:** `OutsideObject.MkOutsideObject {owner :: PlayerId, printing :: PrintingId}`;
`GameState.outsideObjects :: Map ObjectId OutsideObject`;
`GameState.broughtIn :: Seq ObjectId`.

- [ ] **Step 1: write the type**

```haskell
module Pawl.Types.OutsideObject where

import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PrintingId as PrintingId

-- | CR 729.4: one card in a game that is on hold, as the game being played sees
-- it -- "all objects in the main game ... are considered outside the subgame".
-- Keyed in GameState.outsideObjects by the id the card has OUT THERE, which is
-- the only handle the outer frame needs to apply the departure when this game
-- ends.
--
-- Two fields and no characteristics: CR 729.1b gives a main-game effect no
-- meaning here, so the card is read from its PRINTED FACE
-- (Pawl.Engine.Projection.viewOfCard), exactly as CR 103.2a's sideboard pool is.
-- The owner is carried because CR 108.3b scopes every reach outside the game to
-- the acting player's OWN cards.
data OutsideObject = MkOutsideObject
  { owner :: PlayerId.PlayerId,
    printing :: PrintingId.PrintingId
  }
  deriving (Eq, Ord, Show)
```

- [ ] **Step 2: add the two GameState fields**

Next to `players` in the record, with this comment on the first:

```haskell
    -- | CR 729.4: the cards outside THIS game that sit in a game on hold --
    -- empty for a game nobody is nested inside. Filled by
    -- Pawl.Engine.Setup.subgameStateFrom from its parent's card objects (and
    -- from the parent's own copy of this field, which is CR 729.6's nesting),
    -- and spent by Pawl.Engine.OutsideTheGame.
    outsideObjects :: Map.Map ObjectId.ObjectId OutsideObject.OutsideObject,
    -- | CR 729.4a: the outer ids this game has brought in, in the order they
    -- crossed. Read once, by Pawl.Engine.Engine.playSubgame, which applies each
    -- departure to the game it holds; an id that is not one of that game's own
    -- objects came from further out and is passed outward one level (CR 729.6).
    broughtIn :: Seq.Seq ObjectId.ObjectId,
```

- [ ] **Step 3: write the codec and its spec**

`Codec/OutsideObject.hs` mirrors `Codec/PhasedOut.hs`'s shape (a two-field
`Fields.object`). In `Codec/GameState.hs`, add in record order:

```haskell
  outsideObjects <- Fields.required "outsideObjects" (Common.naturalMap ObjectId.codec OutsideObject.codec) GameState.outsideObjects
  broughtIn <- Fields.required "broughtIn" (Common.seq ObjectId.codec) GameState.broughtIn
```

and the matching two lines in the constructor block.

- [ ] **Step 4: set them empty at every game-building site**

`Setup.emptyGame`, `Setup.restartGame`'s record update and `Setup.subgameStateFrom`
all list their fields. `emptyGame` gets `Map.empty` / `Seq.empty`. `restartGame`
KEEPS both --- CR 727.1 restarts the game, and a card outside it is still outside
it; write that reason at the site. `subgameStateFrom` gets `broughtIn = Seq.empty`
and the real snapshot in Task 2.

- [ ] **Step 5: `cabal-gild pawl.cabal`, then `cabal test`**

Expected: green, and `Pawl.Codec.GameStateSpec`'s round trip covers the new fields.

- [ ] **Step 6: commit** stage the files you touched BY PATH (another session may share this checkout, so never `git add -A`), then `hooky fix`, `git add` again, and commit.

---

### Task 2: fill the snapshot from the parent

**Files:** Modify `source/libraries/engine/Pawl/Engine/Setup.hs`,
`source/libraries/test/Pawl/GameSpec.hs`

**Consumes:** Task 1's fields. **Produces:** a populated `outsideObjects` in a subgame state.

- [ ] **Step 1: write the failing test** (in `GameSpec`, beside the other
      `subgameStateFrom` cases)

```haskell
  Spec.it s "CR 729.4: the subgame sees the main game's cards as outside it, minus the ones it took" $ do
    mountain <- S.printingOf s registry "Mountain"
    bear <- S.printingOf s registry "Grizzly Bears"
    let g0 = poolToLibraryG S.alice (addManyG mountain 3 S.alice (Setup.emptyGame S.bothPlayers))
        (bearId, g1) = S.addCreature bear S.alice g0
        sub = Setup.subgameStateFrom S.alice g1
    Spec.assertEqWith s "the battlefield card is outside the subgame" (Map.member bearId (GameState.outsideObjects sub)) True
    Spec.assertEqWith s "CR 729.2: the library cards moved INTO the subgame, so they are not outside it" (length (GameState.outsideObjects sub)) 1
    Spec.assertEqWith s "and it is hers" (fmap OutsideObject.owner (Map.lookup bearId (GameState.outsideObjects sub))) (Just S.alice)
```

`S.addCreature :: Printing -> PlayerId -> GameState -> (ObjectId, GameState)` is
the helper (`Pawl.Support:357`); `S.countOnBattlefieldByName` and
`S.creaturesInPlay` are the readers the later cases want.

- [ ] **Step 2: run it and watch it fail** --- `cabal test` reports the map empty.

- [ ] **Step 3: implement.** In `subgameStateFrom`, beside `movedObjects`:

```haskell
      -- CR 729.4: "all objects in the main game and all cards outside the main
      -- game are considered outside the subgame (except those specifically
      -- brought into the subgame)". The exception is `movedObjects` -- CR 729.2's
      -- libraries and CR 729.2c's commanders -- which are IN the subgame and so
      -- are excluded here. Only Source.OfCard objects: CR 400.11c's road brings
      -- in a CARD, and a token or an emblem is neither.
      --
      -- The parent's OWN outsideObjects ride along, which is CR 729.6's nesting:
      -- a card two games out is outside this one too, and playSubgame passes an
      -- unclaimed departure outward one level at a time.
      outside =
        Map.union
          (Map.mapMaybe asOutside (Map.withoutKeys (GameState.objects parent) (Set.union libIds cmdIds)))
          (GameState.outsideObjects parent)
      asOutside obj = case Object.source obj of
        Source.OfCard printingId -> Just (OutsideObject.MkOutsideObject (Object.owner obj) printingId)
        _ -> Nothing
```

and write `GameState.outsideObjects = outside` into the record update.

- [ ] **Step 4: `cabal test`** --- green.

- [ ] **Step 5: mutate.** Change `Map.withoutKeys` to `id` and confirm the count
      assertion reddens (the library cards would be offered twice over). Put it back.

- [ ] **Step 6: commit.**

---

### Task 3: one choice type for both sources

**Files:** Create `source/libraries/types/Pawl/Types/OutsideCard.hs`,
`source/libraries/codec/Pawl/Codec/OutsideCard.hs` + spec; modify
`Pawl/Types/Prompt.hs`, `Pawl/Types/Response.hs`,
`Pawl/Engine/Replay.hs`, `Pawl/Engine/OutsideTheGame.hs`,
`source/libraries/test/Pawl/OutsideTheGameSpec.hs`, `pawl.cabal`

**Produces:** `data OutsideCard = InPool PrintingId | InAnotherGame ObjectId`;
`ChooseFromOutsideTheGame :: Decider -> PlayerId -> NonEmpty OutsideCard -> Prompt OutsideCard`.

Why a new type rather than reusing `PrintingId`: the two are NOT
indistinguishable. Which zone the card leaves decides which main-game abilities
trigger (CR 729.4a), so a player holding a Grizzly Bears in the pool and another
on the main-game battlefield is making a real choice, and the engine may not
make it for them.

- [ ] **Step 1: write the type**, with that paragraph as its header comment and
      `Ord` derived (the prompt's `NonEmpty` is built in interning order).

- [ ] **Step 2: rewire the prompt, response and replay.** `Prompt.hs` line ~377
      and `Response.hs` line ~233 change their payload type; `Replay.hs`'s three
      arms (82, 261, 588) follow. `Replay.hs:588`'s default answer stays
      `NonEmpty.head candidates`.

- [ ] **Step 3: rewire `OutsideTheGame.eligible`/`reveal`** to build and match
      `OutsideCard.InPool` --- behaviour unchanged this task, pool only.

- [ ] **Step 4: `cabal test`.** `OutsideTheGameSpec`'s existing prompt case needs
      its expectation rewritten to `OutsideCard.InPool`; nothing else should move.

- [ ] **Step 5: commit.**

---

### Task 4: eligibility over both sources, and the crossing

**Files:** Modify `source/libraries/engine/Pawl/Engine/OutsideTheGame.hs`,
`source/libraries/test/Pawl/OutsideTheGameSpec.hs`

**Produces:** `eligible :: Filter Keyword -> ObjectId -> PlayerId -> GameState -> [OutsideCard]`;
`mint :: PlayerId -> PrintingId -> GameState -> (ObjectId, GameState)` (the half
`bringIn` and the crossing share).

- [ ] **Step 1: write the failing test.** A subgame state built by
      `Setup.subgameStateFrom` from a parent holding alice's Grizzly Bears on the
      battlefield; alice casts Living Wish in the SUBGAME (unit level: call
      `OutsideTheGame.reveal` directly with `Or [HasCardType Creature, HasCardType Land]`)
      and answers `InAnotherGame bearId`.

```haskell
    Spec.assertEqWith s "CR 729.4/400.11c: the main-game creature arrives in her subgame hand" (printingsIn Zone.Hand S.alice after) [bear]
    Spec.assertEqWith s "CR 729.4a: the crossing is recorded for the outer frame to apply" (Foldable.toList (GameState.broughtIn after)) [bearId]
    Spec.assertEqWith s "and it is no longer offered" (Map.member bearId (GameState.outsideObjects after)) False
```

- [ ] **Step 2: run it and watch it fail** --- `eligible` offers pool entries only.

- [ ] **Step 3: implement.** `eligible` unions the pool list with

```haskell
      outer =
        [ OutsideCard.InAnotherGame oid
          | (oid, entry) <- Map.toAscList (GameState.outsideObjects gs),
            OutsideObject.owner entry == pid,
            admits (OutsideObject.printing entry)
        ]
```

reusing the existing `admits`, which is `Projection.viewOfCard` over the printed
face --- CR 729.1b, per the header. Split `bringIn` into `mint` (the object
construction, unchanged) plus the two spends, and add

```haskell
-- CR 729.4a: bring in a card from a game that is on hold. The entry is dropped
-- and the OUTER id is appended to GameState.broughtIn, which is the whole record
-- the outer frame needs: this game cannot reach that game's state, and must not.
bringInFrom :: PlayerId -> ObjectId -> GameState -> (Maybe ObjectId, GameState)
```

- [ ] **Step 4: `cabal test`** --- green.

- [ ] **Step 5: mutate.** Drop the `OutsideObject.owner entry == pid` guard and
      confirm a second case --- bob's card is not offered to alice (CR 108.3b) ---
      reddens. Write that case if it is not already there.

- [ ] **Step 6: commit.**

---

### Task 5: apply the departures to the parent

**Files:** Modify `source/libraries/engine/Pawl/Engine/Engine.hs` (`playSubgame`),
`source/libraries/engine/Pawl/Engine/Setup.hs` (`funnelBack`),
`source/libraries/test/Pawl/GameSpec.hs`

**Consumes:** `GameState.broughtIn` from Task 4.

- [ ] **Step 1: write the failing test.** A parent with alice's Grizzly Bears on
      the battlefield; run `Engine.playSubgame` under an answer that takes the
      wish (the subgame fixture from Task 7's shape, or a hand-built `finalSub`
      passed straight to the new pure function). Assert: the bear is gone from the
      parent's battlefield, a `GameEvent.LeftTheGame` naming its id is in the
      parent's unscanned events, and `GameState.lastKnown` has its record.

- [ ] **Step 2: run it and watch it fail.**

- [ ] **Step 3: implement.** A pure function beside `funnelBack`, mirroring
      `Departure.objectsLeaveWith` --- file CR 608.2h last known information from
      the board BEFORE any of them left, remove from zones with
      `Game.removeFromZones`, delete the object, record
      `GameEvent.LeftTheGame`:

```haskell
-- CR 729.4a: the cards the subgame brought in have left the main game. Applied
-- HERE, when the subgame ends, and that is exact rather than late: the main game
-- was discontinued throughout (CR 729.1a), so no state-based action, priority or
-- continuous effect could have read it in between, and the events land in the
-- same order they crossed.
--
-- NOT wrapped in Event.simultaneouslyPure, where CR 800.4a's departure is: those
-- objects leave at one instant and these crossed at different moments of the
-- subgame, so each gets its own event group and CR 603.10a reads them as a
-- sequence.
--
-- An id that is not one of this game's objects came from further out (CR 729.6):
-- its entry is dropped from this game's own outsideObjects and appended to this
-- game's broughtIn, so the frame one level out applies it.
applyCrossings :: GameState -> GameState -> GameState
```

Call it from `playSubgame` BEFORE `Setup.funnelBack`, so `funnelBack`'s
`keptParentObjects` cannot resurrect a card that left.

- [ ] **Step 4: carry the pool spend back.** `funnelBack` currently keeps the
      PARENT's `players`, so a sideboard card a wish spent inside the subgame is
      both in the main-game library (via `returned`) and still in the pool. Take
      each seated player's `Player.outsideTheGame` from `finalSub` --- CR 400.11b
      keeps a card brought in "in the game until the game ends", and CR 729.5 puts
      it in the main-game library. Every other `Player` field stays the parent's
      (CR 729.1b). Write a case for it.

- [ ] **Step 5: `cabal test`; mutate** the `applyCrossings` call out of
      `playSubgame` and confirm the battlefield assertion reddens (not the event
      one --- if the event assertion reddens first, reorder so the gameplay-level
      claim is the one that catches it).

- [ ] **Step 6: commit.**

---

### Task 6: Living Wish

**Files:** Create `data/cards/living-wish.json`; modify `source/libraries/test/Pawl/CardSpec.hs` if its
traversals need it.

Oracle, verified on Scryfall 2026-08-27 (A25 179): `{1}{G}` Sorcery --- "You may
reveal a creature or land card you own from outside the game and put it into your
hand. Exile Living Wish."

- [ ] **Step 1: write the card**, `data/cards/burning-wish.json` with the mana cost
      changed to `{1}{G}` and the filter changed to

```json
{"type":"Or","value":[{"type":"HasCardType","value":{"type":"Creature"}},{"type":"HasCardType","value":{"type":"Land"}}]}
```

- [ ] **Step 2: `cabal test`** --- the card lints and round-trips through
      `Pawl.CardSpec` with no engine change.

- [ ] **Step 3: commit.**

---

### Task 7: the gameplay case --- Shahrazad, Living Wish, Super Shredder

**Files:** Modify `source/libraries/test/Pawl/OutsideTheGameSpec.hs`

This is the case that proves #152. All three cards are printed and in the pool
after Task 6.

**Fixture.** Main game: alice has Shahrazad in hand and two Plains untapped; her
Grizzly Bears and bob's Super Shredder ("whenever another permanent leaves the
battlefield, put a +1/+1 counter on Super Shredder") are on the battlefield.
alice's main-game library is Living Wish plus enough Forests that she reaches two
lands inside the subgame before bob decks; bob's library is sized so his deck-out
(CR 704.5b) ends the subgame after that. Drive the main game with
`Engine.priorityLoop`; answer with `S.castAnswer` plus these overrides:

- `Prompt.ChooseOptional {}` -> `OptionalDecision.Exercises` (Living Wish's printed "may")
- `Prompt.ChooseFromOutsideTheGame _ _ offered` -> the `InAnotherGame` entry in `offered`
- `Prompt.RandomFirstPlayer` -> alice

- [ ] **Step 1: write the failing test**, asserting in this order:

```haskell
    Spec.assertEqWith s "CR 729.4: the main-game creature left the main game for the subgame" (nameOnBattlefield "Grizzly Bears" after) False
    Spec.assertEqWith s "CR 729.4a/729.5: Super Shredder's leaves-the-battlefield trigger resolved once the main game resumed" (countersOn shredderId after) 1
    Spec.assertEqWith s "CR 729.5: it went on the stack AFTER Shahrazad finished resolving" (counterEventIndex after > lifeLossEventIndex after) True
    Spec.assertEqWith s "CR 729.5: the card the wish took comes back to her main-game library" (namesInLibrary S.alice after) ["Grizzly Bears", ...]
    Spec.assertEqWith s "the main game did not end" (GameState.result after) Nothing
```

The third is the one CR 729.5's last sentence is about: Shahrazad's own
`LoseLife` clause is recorded during its resolution, and the trigger's counter
only after. Read both out of `GameState.events`.

- [ ] **Step 2: run it and watch it fail.**

- [ ] **Step 3: no implementation** --- Tasks 1-6 are the implementation. If it
      fails for any reason other than a fixture-sizing mistake, that is a real
      defect: fix it in the task it belongs to.

- [ ] **Step 4: mutate.** Delete the `GameEvent.LeftTheGame` record from
      `applyCrossings` (keeping the removal) and confirm the COUNTER assertion
      reddens, not the battlefield one. Then restore, and delete the removal
      (keeping the event) and confirm the battlefield assertion reddens. Record
      both in the PR body by name.

- [ ] **Step 5: commit.**

---

### Task 8: the paperwork

- [ ] **Step 1: rewrite `playSubgame`'s elision paragraph.** It currently reads
      "Not implemented: cards brought into a subgame from the main game, and the
      main-game triggers their removal queues (#152)". That is now implemented ---
      delete the sentence and leave the CR 729.4a note that says where the
      departures are applied and why the timing is exact.

- [ ] **Step 2: `grep -rn '#152' source/ docs/ data/`** and rewrite every citation.

- [ ] **Step 3: file the deferrals as issues, cite each inline.** At least:
      a face-down main-game card is offered by its printed face, CR 708 status not
      consulted; only leaves-the-BATTLEFIELD conditions exist, so a card crossing
      out of a main-game hand, graveyard or exile fires nothing (Burning Wish's
      own case); Cunning Wish and Ring of Ma'ruf are unwritten.

- [ ] **Step 4: read #875/#876/#877 for a census row citing #152** and edit it in
      this PR.

- [ ] **Step 5: check what #152 unblocks** ---
      `gh api repos/tfausak/pawl/issues/152/dependencies/blocking` --- and say so
      in the PR body.

- [ ] **Step 6: self-review the branch.** Re-check every CR citation against
      `docs/rules.txt`; re-read every comment the change touched; grep every
      construction site of `GameState` for the two new fields (CLAUDE.md's
      new-field trap, PRs #2009 and #2021); check `Pawl.Engine.Event`'s
      `eventBindings` fallthrough and `Pawl.Engine.Filter`'s `boundSlots` for arms
      the new prompt type should have.

- [ ] **Step 7: stage, `hooky fix`, `git add` again, push, open the draft PR**
      with `Closes #152`, the citations, the design calls (snapshot over recursive
      parent; departures applied at the subgame's end, with the CR 729.1a
      argument), the two mutations by the assertion each reddened, an explicit
      "no" on whether the rules core cases on an effect's identity, and what was
      deferred.
