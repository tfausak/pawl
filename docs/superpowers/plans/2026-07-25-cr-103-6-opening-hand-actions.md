# CR 103.6a Opening-Hand Actions Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Implement CR 103.6a — "you may begin the game with this card on the battlefield" — so Leyline of the Void works in a main game, a restart (CR 727), and a subgame (CR 729).

**Architecture:** A third phase in `Mulligan.openingHands`, after the CR 103.5 process completes, offering each player in turn order the actions their opening hand grants, looping until they decline. The action's payload rides a new `Card.openingHandAction :: [Effect Card]` field, and needs **no new opcode**: `MoveToZone Binding.triggerSource Zone.Battlefield` is how this codebase already says "this permanent", so the performer just binds the granting card into the reserved `self` slot. Leyline's own ability is Rest in Peace's `ZoneChangeR` shape with a new `ControllerRelation.Opponents`, whose zone-change test is split out **owner-based** per CR 400.3.

**Tech Stack:** Haskell 2010 (GHC 9.14.1), `tasty`/`tasty-hunit`, the `StateT GameState (Program Prompt)` suspension monad, the hand-written JSON codec in `Pawl.Codec`.

## Global Constraints

- **Haskell 2010, no language extensions** unless unavoidable. The shared window loop takes the prompt constructor as an ordinary function argument (its result type is the fixed `Prompt (Maybe ObjectId)`), so no `RankNTypes` is needed in the library.
- **Warning-clean under `+pedantic` (`-Werror`).** A new `Prompt` constructor breaks every exhaustive answerer and a new `ControllerRelation` constructor breaks all four of its consumers; that is the safety net for Tasks 1 and 2. Run `cabal build all --enable-tests --enable-benchmarks`; `cabal clean` first for a definitive warning check. **Never run two builds concurrently.**
- **Adding or deleting a library module requires a direct `cabal-gild --io pawl.cabal`.** `hooky fix` only runs cabal-gild when `pawl.cabal` itself has a staged change, so a new or renamed module is otherwise missing from the discovered list and the build fails on `-Wmissing-home-modules`. Task 4 renames a module and hits this.
- **No partial functions**, no boolean blindness, `Mk` constructor prefix, derive at least `Eq` and `Show`, `Text` not `String`, no list comprehensions, `let` over `where`, `case` over point-free.
- **One type per module** under `Pawl.Type.<Name>`; qualified imports aliased to the last component; no explicit export lists; a module never imports its parents.
- **The two invariants outrank this plan:** the engine never cases on a card's identity (only classifications), and never makes a player's choice. `Pawl.Mulligan` may *mention* `Effect` but must never `case` on one.
- **TDD non-negotiable:** write each failing test, run it, watch it fail, then implement. Tick each `- [ ]` as you finish it.
- **Every rules claim cites CR 103.6** (or the specific sub-rule) and was checked against `docs/rules.txt`. Never write an expiry into a code comment — file an issue and cite `(#N)`.
- **Commit style:** commit directly to `main`, one small complete commit per task, with the two `CLAUDE.md` trailers.
- **After each task:** `git add -A`, `hooky fix`, `git add -A`, `hooky run`.

**Spec:** `docs/superpowers/specs/2026-07-25-cr-103-6-opening-hand-actions-design.md`.

---

## File Structure

- **Create** `data/cards/leyline-of-the-void.json` — the gate card.
- **Rename** `source/library/Pawl/Type/MulliganPerformer.hs` → `source/library/Pawl/Type/HandActionPerformer.hs`.
- **Modify** `source/library/Pawl/Type/Prompt.hs` — `+OpeningHandAction`.
- **Modify** `source/library/Pawl/Type/Response.hs` — `+TookOpeningHandAction`.
- **Modify** `source/library/Pawl/Type/ControllerRelation.hs` — `+Opponents`.
- **Modify** `source/library/Pawl/Type/Card.hs` — `+openingHandAction`.
- **Modify** `source/library/Pawl/Replay.hs` — three arms.
- **Modify** `source/library/Pawl/Codec.hs` — the card field, the relation's two arms.
- **Modify** `source/library/Pawl/Replacement.hs` — the owner-based zone-change subject test, plus `Opponents` arms.
- **Modify** `source/library/Pawl/Resolve.hs` — the performer's rename and its reserved-slot bindings.
- **Modify** `source/library/Pawl/Mulligan.hs` — `actionsFor`'s selector, the shared `handWindow`, `openingHandActions`, the third phase.
- **Modify** `source/library/Pawl/Setup.hs`, `source/library/Pawl/Engine.hs` — the renamed type and performer.
- **Modify** the exhaustive `Prompt` answerers: `Support.hs` (5), `benchmark/Main.hs` (3), `CastSpec.hs` (3), `GameSpec.hs` (2), and the non-wildcard `MulliganSpec` locals.
- **Modify** the four `Card.Type.MkCard` literals: `CardSpec.hs`, `ResolveSpec.hs` (×3).
- **Modify** `MulliganSpec.hs`, `CodecSpec.hs`, `CardsSpec.hs`, `ReplacementSpec.hs`, `Support.hs`.
- **Modify** `pawl.cabal` — via a direct `cabal-gild --io pawl.cabal` after the Task 4 rename.

---

## Task 1: The `OpeningHandAction` prompt channel

The wire only. One commit, because a new GADT constructor breaks every exhaustive `Prompt` match at once.

**Files:**
- Modify: `source/library/Pawl/Type/Prompt.hs`, `source/library/Pawl/Type/Response.hs`, `source/library/Pawl/Replay.hs`
- Modify (answerers): `Support.hs`, `benchmark/Main.hs`, `CastSpec.hs`, `GameSpec.hs`, `MulliganSpec.hs`
- Test: `source/test-suite/Pawl/ReplaySpec.hs`

**Interfaces:**
- Produces: `Prompt.OpeningHandAction :: Decider -> PlayerId -> [ObjectId] -> Prompt (Maybe ObjectId)`; `Response.TookOpeningHandAction (Maybe ObjectId)`.

- [x] **Step 1: Write the failing test.** In `source/test-suite/Pawl/ReplaySpec.hs`, add this case immediately after the existing `"MulliganAction records and replays a Maybe ObjectId"` case:

```haskell
          HU.testCase "OpeningHandAction records and replays a Maybe ObjectId" $
            let p = Prompt.OpeningHandAction decider S.alice [ObjectId.MkObjectId 7, ObjectId.MkObjectId 8]
                answer = Just (ObjectId.MkObjectId 7)
             in HU.assertEqual "round trip" (Just answer) (Replay.decode p (Replay.encode p answer)),
```

- [x] **Step 2: Run the test to verify it fails to COMPILE.**

Run: `cabal build pawl-test-suite --enable-tests 2>&1 | tail -20`
Expected: compile error — `Data constructor not in scope: Prompt.OpeningHandAction`.

- [x] **Step 3: Add the `Prompt` constructor.** In `source/library/Pawl/Type/Prompt.hs`, append after `MulliganAction`:

```haskell
  -- CR 103.6: an action a card in this player's opening hand lets them take once
  -- the mulligan process is complete -- "begin the game with it on the
  -- battlefield" (CR 103.6a). The [ObjectId] is the cards in hand offering one;
  -- the answer is which to take, or Nothing to decline.
  --
  -- Offered in turn order, starting player first (CR 103.6), and repeatedly to
  -- the same player until they decline: CR 103.6 lets a player take "any such
  -- actions in any order", so both which and how many are theirs to choose.
  --
  -- A SEPARATE channel from MulliganAction, not a reuse of it: that window sits
  -- AT a mulligan declaration and this one opens once the whole process is
  -- complete, so an interpreter that could not tell them apart could not answer
  -- either well. Not asked when the list is empty; where the rules leave nothing
  -- to ask, don't prompt.
  OpeningHandAction :: Decider -> PlayerId -> [ObjectId] -> Prompt (Maybe ObjectId)
```

- [x] **Step 4: Add the `Response` constructor.** In `source/library/Pawl/Type/Response.hs`, append after `TookMulliganAction`:

```haskell
  | -- CR 103.6: the hand card whose opening-hand action a player took (Nothing =
    -- declined), serialized so a DecisionLog replays it. Its own constructor
    -- rather than a reuse of TookMulliganAction, for the reason ChoseDefender
    -- records: decode must return Nothing for a response that does not match the
    -- prompt being asked, and two prompts sharing a constructor cannot do that.
    TookOpeningHandAction (Maybe ObjectId)
```

- [x] **Step 5: Wire `Pawl.Replay`.** Three edits, each directly after the matching `MulliganAction` arm:

```haskell
  Prompt.OpeningHandAction {} -> Response.TookOpeningHandAction answer
```

```haskell
  Prompt.OpeningHandAction {} -> case response of
    Response.TookOpeningHandAction moid -> Just moid
    _ -> Nothing
```

```haskell
  -- CR 103.6: declining is always legal and the least-eventful fallback when a
  -- transcript runs short (mirrors MulliganAction -> Nothing).
  Prompt.OpeningHandAction {} -> Nothing
```

- [x] **Step 6: Update every exhaustive answerer.** Each already has a `Prompt.MulliganAction {} -> …` arm; add a sibling directly after it.

Run: `grep -rn "Prompt.MulliganAction {}" source/test-suite source/benchmark`
Expected: 13 hits. Pure answerers get `Prompt.OpeningHandAction {} -> Nothing`; the two monadic ones (`Support.hs`'s last interpreter and `GameSpec.hs`'s) get `Prompt.OpeningHandAction {} -> pure Nothing`.

`MulliganSpec`'s local answerers end in `_ -> S.identityAnswer p` and need no arm.

- [x] **Step 7: Build and test.**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -20 && cabal test 2>&1 | tail -5`
Expected: warning-free; the suite passes including the new round-trip.

- [x] **Step 8: Format, lint, and commit.**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "feat(prompt): add the CR 103.6 OpeningHandAction channel (#149)"
```

---

## Task 2: `ControllerRelation.Opponents`, and the owner-based zone-change test

Leyline's second ability needs a relation the type cannot express, and the shared matcher answers the wrong question for zone changes. Both land together: the new constructor is what forces the split to be written correctly.

**Files:**
- Modify: `source/library/Pawl/Type/ControllerRelation.hs`, `source/library/Pawl/Replacement.hs`, `source/library/Pawl/Codec.hs`
- Test: `source/test-suite/Pawl/ReplacementSpec.hs`, `source/test-suite/Pawl/CodecSpec.hs`

**Interfaces:**
- Produces: `ControllerRelation.Opponents`; `Replacement.matchesZoneOwner :: GameState -> ObjectId -> ControllerRelation -> ObjectId -> Bool`.

- [x] **Step 1: Write the failing tests.** In `source/test-suite/Pawl/ReplacementSpec.hs`, add these two cases to the top-level list. They build a Rest-in-Peace-shaped redirect with the new relation and check both halves of §3.5's claim. Add whatever imports the file lacks (`ReplacementEffect`, `ZoneChangePattern`, `ControllerRelation`, `Zone`, `Event`, `Game`, `Departure` are the likely ones — follow the file's existing aliases).

```haskell
      HU.testCase "CR 400.3: an Opponents zone-change redirect exiles an opponent's card, not your own" $ do
        -- The Leyline shape without the Leyline: a floating redirect whose source
        -- alice controls. Bob's card is exiled on the way to his graveyard;
        -- alice's own goes to her graveyard untouched.
        piker <- Registry.printing registry "Goblin Piker"
        let g0 = Setup.emptyGame S.bothPlayers
            (mine, g1) = S.addCreature piker S.alice g0
            (theirs, g2) = S.addCreature piker S.bob g1
            g3 = S.addReplacement (leylineShape S.alice) g2
            after = S.runPure S.identityAnswer g3 (Event.changeZone mine Zone.Graveyard >> Event.changeZone theirs Zone.Graveyard)
        HU.assertEqual "alice's own card reaches her graveyard" 1 (length (Game.zoneMembers Zone.Graveyard S.alice after))
        HU.assertEqual "bob's is exiled instead" 0 (length (Game.zoneMembers Zone.Graveyard S.bob after))
        HU.assertEqual "and it is in exile" 1 (length (Game.zoneMembers Zone.Exile S.bob after)),
      HU.testCase "CR 400.3: the test is the card's OWNER, not its controller" $ do
        -- A card alice OWNS but bob CONTROLS still dies to alice's graveyard, so
        -- alice's own Leyline must not exile it. A controller-based test would.
        piker <- Registry.printing registry "Goblin Piker"
        let g0 = Setup.emptyGame S.bothPlayers
            (oid, g1) = S.addCreature piker S.alice g0
            g2 = S.addReplacement (leylineShape S.alice) g1
            g3 = S.runPure S.identityAnswer g2 (Resolve.applyEffect S.noSource S.bob Map.empty (Map.singleton slot True) (Map.singleton slot (Recipient.ToObject oid)) (Effect.GainControl Duration.Indefinite slot))
            slot = SlotName.MkSlotName (Text.pack "target")
            after = S.runPure S.identityAnswer g3 (Event.changeZone oid Zone.Graveyard)
        HU.assertEqual "it reaches its OWNER's graveyard, unexiled" 1 (length (Game.zoneMembers Zone.Graveyard S.alice after))
        HU.assertEqual "nothing was exiled" 0 (length (Game.zoneMembers Zone.Exile S.alice after)),
```

with this helper beside the file's other fixtures — mirror the shape `S.addReplacement` expects (an `ActiveReplacement`; copy the construction from the file's existing `S.addRegenShield` or `addReplacement` callers, substituting the effect below):

```haskell
-- Leyline of the Void's redirect, as data: any card headed for an OPPONENT's
-- graveyard is exiled instead (CR 400.3 makes that graveyard its owner's).
leylineEffect :: ReplacementEffect.ReplacementEffect
leylineEffect =
  ReplacementEffect.ZoneChangeR
    (ZoneChangePattern.MkZoneChangePattern Zone.Graveyard ControllerRelation.Opponents)
    Zone.Exile
```

In `source/test-suite/Pawl/CodecSpec.hs`, extend the existing `ControllerRelation` coverage by adding a round-trip through the zone-change pattern with the new relation, beside the `ZoneChangePattern.whoseObject = ControllerRelation.Anyones` case near line 386:

```haskell
          HU.testCase "a ZoneChangeR carrying Opponents round-trips" $
            roundTrip
              "leyline"
              Codec.replacementEffectToJson
              Codec.jsonToReplacementEffect
              ( ReplacementEffect.ZoneChangeR
                  (ZoneChangePattern.MkZoneChangePattern Zone.Graveyard ControllerRelation.Opponents)
                  Zone.Exile
              ),
```

- [x] **Step 2: Run the tests to verify they fail to COMPILE.**

Run: `cabal build pawl-test-suite --enable-tests 2>&1 | tail -20`
Expected: `Data constructor not in scope: ControllerRelation.Opponents`.

- [x] **Step 3: Add the constructor.** In `source/library/Pawl/Type/ControllerRelation.hs`:

```haskell
data ControllerRelation
  = Yours
  | Anyones
  | -- CR 102.1: "an opponent" -- any other player. Leyline of the Void's "an
    -- opponent's graveyard". Read against the effect SOURCE's controller, like
    -- its siblings, but see Pawl.Replacement: for a ZONE CHANGE the subject is
    -- the object's OWNER (CR 400.3), because the destination zone is theirs.
    --
    -- "Any other player" is CR 806.1's free-for-all reading, the /= test
    -- Count.playersFor and Filter.matches already use. Teams (CR 102.3) would
    -- make it wrong and have no representation (#175).
    Opponents
  deriving (Eq, Ord, Show)
```

- [x] **Step 4: Split the zone-change subject test.** In `source/library/Pawl/Replacement.hs`, change the `ZoneChangeR` arm of `applies` to call the new function:

```haskell
        (ReplacementEffect.ZoneChangeR pat _, ProposedEvent.WouldChangeZone zc) ->
          ZoneChange.to zc == ZoneChangePattern.whenDestination pat
            && matchesZoneOwner gs src (ZoneChangePattern.whoseObject pat) (ZoneChange.object zc)
```

and add the function beside `matchesController`:

```haskell
-- CR 614.1: does this zone change's object satisfy the pattern's relation?
--
-- The subject is the object's OWNER, not its controller, and that is a rules
-- fact rather than a convenience: CR 400.3 ("if an object would go to any
-- library, graveyard, or hand other than its owner's, it goes to its owner's
-- corresponding zone") and CR 404.1 make the destination zone the owner's, so
-- "an opponent's graveyard" asks who OWNS the card. A creature its controller
-- stole with Act of Treason still dies to its owner's graveyard, which a
-- controller-based test would get backwards.
--
-- Split out of matchesController, which stays controller-based for CR 109.5's
-- "you" on a counter or token pattern. No committed card yet pairs a ZoneChangeR
-- with anything but Anyones, so the split changes no behavior in the pool.
matchesZoneOwner :: GameState -> ObjectId -> ControllerRelation -> ObjectId -> Bool
matchesZoneOwner gs src rel oid =
  let ownerOf o = fmap Object.owner (Game.lookupObject o gs)
   in case rel of
        ControllerRelation.Anyones -> True
        ControllerRelation.Yours -> ownerOf oid == Projection.controllerOf src gs
        ControllerRelation.Opponents -> case (ownerOf oid, Projection.controllerOf src gs) of
          (Just owner, Just you) -> owner /= you
          -- An unknown owner or a sourceless effect admits nothing, rather than
          -- everything: a redirect with no controller has no opponents.
          _ -> False
```

- [x] **Step 5: Add the two remaining `Opponents` arms.** `-Werror` will name both. In `matchesController`:

```haskell
  -- CR 102.1: no producer today -- a counter pattern scoped to an opponent's
  -- permanents. Controller-based, unlike the zone-change test above.
  ControllerRelation.Opponents -> case (Projection.controllerOf oid gs, Projection.controllerOf src gs) of
    (Just theirs, Just yours) -> theirs /= yours
    _ -> False
```

In the `TokenR` arm's inline `case` (around line 231):

```haskell
            -- CR 102.1: no producer today -- tokens created under an opponent's
            -- control.
            ControllerRelation.Opponents -> case Projection.controllerOf src gs of
              Just you -> pid /= you
              Nothing -> False
```

- [x] **Step 6: Add the codec arms.** In `source/library/Pawl/Codec.hs`:

```haskell
  ControllerRelation.Opponents -> "Opponents"
```

```haskell
      (Text.pack "Opponents", ControllerRelation.Opponents)
```

- [x] **Step 7: Build and test.**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -20 && cabal test 2>&1 | tail -5`
Expected: warning-free; all four new cases pass. **If the owner-vs-controller case passes even with a controller-based test, the fixture is wrong, not the claim** — verify by temporarily pointing the `ZoneChangeR` arm back at `matchesController` and watching that one case fail.

- [x] **Step 8: Format, lint, and commit.**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "feat(replacement): add ControllerRelation.Opponents, owner-based for zone changes (#149)"
```

---

## Task 3: The `Card.openingHandAction` carrier

**Files:**
- Modify: `source/library/Pawl/Type/Card.hs`, `source/library/Pawl/Codec.hs`, `source/library/Pawl/Mulligan.hs`
- Modify (record literals): `source/test-suite/Pawl/CardSpec.hs`, `source/test-suite/Pawl/ResolveSpec.hs` (×3)
- Test: `source/test-suite/Pawl/CodecSpec.hs`

**Interfaces:**
- Produces: `CardT.openingHandAction :: Card -> [Effect Card]`; `Mulligan.actionsFor :: (Card.Card -> [Effect Card.Card]) -> PlayerId -> GameState -> [(ObjectId, [Effect Card.Card])]`.

- [x] **Step 1: Write the failing tests.** In `source/test-suite/Pawl/CodecSpec.hs`, beside the two `mulliganAction` cases:

```haskell
          HU.testCase "a Card carrying a CR 103.6 opening-hand action round-trips" $ do
            bloodMoon <- Registry.printing registry "Blood Moon"
            let base = Printing.card bloodMoon
                c = base {CardT.openingHandAction = [Effect.MoveToZone Binding.triggerSource Zone.Battlefield]}
            roundTrip "card" Codec.cardToJson Codec.jsonToCard c,
          HU.testCase "an empty openingHandAction list is omitted from the JSON" $ do
            bloodMoon <- Registry.printing registry "Blood Moon"
            let base = Printing.card bloodMoon
            HU.assertEqual "the fixture really has none" [] (CardT.openingHandAction base)
            case J.asObject (Codec.cardToJson base) of
              Left err -> HU.assertFailure (Text.unpack err)
              Right pairs -> HU.assertBool "key absent" (notElem (Text.pack "openingHandAction") (fmap fst pairs)),
```

Add `import qualified Pawl.Binding as Binding` and `import qualified Pawl.Type.Zone as Zone` to `CodecSpec` if absent.

- [x] **Step 2: Run the tests to verify they fail to COMPILE.**

Run: `cabal build pawl-test-suite --enable-tests 2>&1 | tail -20`
Expected: `CardT.openingHandAction` is not a field of `Card`.

- [x] **Step 3: Add the field.** In `source/library/Pawl/Type/Card.hs`, after `mulliganAction`:

```haskell
    -- CR 103.6: the effects of this card's opening-hand action, in written
    -- order -- what "you may begin the game with it on the battlefield" (CR
    -- 103.6a) does when the player takes it. Empty for every printing but
    -- Leyline of the Void.
    --
    -- Read DIRECTLY from the card and never through the projection, the
    -- mulliganAction / castingPermissions precedent: the ability functions in
    -- the HAND (CR 113.6), where the CR 613 layer system does not reach.
    --
    -- The SIBLING of mulliganAction, not a reuse: the two windows are at
    -- different times (CR 103.5b sits AT a declaration, CR 103.6 opens once the
    -- whole mulligan process is complete), and a card that acts at one must not
    -- be offered at the other.
    --
    -- One action per card, the same shape and the same caveat as mulliganAction
    -- (#183).
    openingHandAction :: [Effect Card]
```

- [x] **Step 4: Wire the codec.** In `cardToJson`, another optional-key block after the `mulliganAction` one:

```haskell
        <> ( if null (CardT.openingHandAction c)
               then []
               else [(Text.pack "openingHandAction", listTo effectToJson (CardT.openingHandAction c))]
           )
```

In `jsonToCard`, the binding and the record field:

```haskell
  openingHandAction <- listFromDefault jsonToEffect (getOpt (Text.pack "openingHandAction") ps)
```

```haskell
        CardT.openingHandAction = openingHandAction
```

- [x] **Step 5: Add the field to the four `MkCard` literals.**

Run: `grep -n "Card.Type.mulliganAction" source/test-suite/Pawl/CardSpec.hs source/test-suite/Pawl/ResolveSpec.hs`
Expected: 4 hits. Each gains a sibling `Card.Type.openingHandAction = [],`.

- [x] **Step 6: Generalize `actionsFor` to take the field selector.** In `source/library/Pawl/Mulligan.hs`:

```haskell
-- CR 103.5b / CR 103.6: the cards in this player's hand that grant an action
-- from the window `field` names, each paired with the effects that action
-- performs. A CLASSIFICATION, not an identity test: this asks whether the card
-- declares an action, never which card it is.
--
-- Read straight off the card (Game.cardOf) and never through the projection --
-- the Card.castingPermissions precedent: these abilities function in the HAND
-- (CR 113.6), where the CR 613 layer system does not reach.
actionsFor :: (Card.Card -> [Effect Card.Card]) -> PlayerId -> GameState.GameState -> [(ObjectId, [Effect Card.Card])]
actionsFor field pid gs =
  let withAction oid = case Game.cardOf oid gs of
        Nothing -> Nothing
        Just card -> case field card of
          [] -> Nothing
          effects -> Just (oid, effects)
   in Maybe.mapMaybe withAction (Game.zoneMembers Zone.Hand pid gs)
```

Its one existing caller, `mulliganWindow`, becomes `State.gets (actionsFor Card.mulliganAction pid)`.

- [x] **Step 7: Build and test.**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -20 && cabal test 2>&1 | tail -5`
Expected: warning-free; the two new codec cases pass and every committed card file still round-trips byte-identically.

- [x] **Step 8: Format, lint, and commit.**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "feat(card): carry a CR 103.6 opening-hand action on Card (#149)"
```

---

## Task 4: The performer serves both windows

Rename the type and its implementation, and bind the granting card into the reserved `self` slot so `MoveToZone Binding.triggerSource _` resolves. No behavior changes for CR 103.5b; the binding is what makes Task 6's action possible.

**Files:**
- Rename: `source/library/Pawl/Type/MulliganPerformer.hs` → `source/library/Pawl/Type/HandActionPerformer.hs`
- Modify: `source/library/Pawl/Resolve.hs`, `source/library/Pawl/Mulligan.hs`, `source/library/Pawl/Setup.hs`, `source/test-suite/Pawl/Support.hs`
- Modify: `pawl.cabal` (via a direct `cabal-gild --io pawl.cabal`)
- Test: `source/test-suite/Pawl/MulliganSpec.hs`

**Interfaces:**
- Produces: `HandActionPerformer.HandActionPerformer`; `Resolve.performHandAction :: HandActionPerformer`.

- [x] **Step 1: Write the failing test.** The observable change is that an effect naming the reserved `self` slot now resolves. Put this in `MulliganSpec` beside the CR 103.5b cases — it drives the *mulligan* window, so it proves the binding independently of Task 6:

```haskell
      HU.testCase "a hand action's effects can name their own card through the reserved self slot" $ do
        -- Resolve.performHandAction binds the granting card into
        -- Binding.triggerSource, which is how "this card" is expressible without
        -- a self-variant opcode (Effect.Sacrifice's own comment). Proved on the
        -- CR 103.5b window so it does not depend on CR 103.6 existing yet.
        powder <- Registry.printing registry "Serum Powder"
        mountain <- Registry.printing registry "Mountain"
        let gs0 = powderGame powder mountain 20
            selfExile oid = S.runPure S.identityAnswer gs0 (Resolve.performHandAction oid S.alice [Effect.MoveToZone Binding.triggerSource Zone.Exile])
            handIds = Game.zoneMembers Zone.Hand S.alice (S.runPure S.identityAnswer gs0 (pure ()))
        case handIds of
          [] -> HU.assertFailure "expected a drawn opening hand to exile from"
          oid : _ -> HU.assertEqual "the named card left the hand" 1 (length (Game.zoneMembers Zone.Exile S.alice (selfExile oid)))
```

Note `gs0` here has **not** had opening hands drawn, so `handIds` is empty and the case fails on `assertFailure` until you draw. Replace `S.runPure S.identityAnswer gs0 (pure ())` with a state that has hands: run `Mulligan.openingHands S.performer [S.alice, S.bob]` under `keepAnswer` first and take its hand. Write it that way; the placeholder above is what NOT to ship.

- [x] **Step 2: Run the test to verify it fails.**

Run: `cabal test 2>&1 | tail -30`
Expected: FAIL — `Resolve.performHandAction` is not in scope (the rename has not happened), or, once renamed but before the binding is added, the exile does not happen because the slot is unbound.

- [x] **Step 3: Rename the type module.**

```bash
git mv source/library/Pawl/Type/MulliganPerformer.hs source/library/Pawl/Type/HandActionPerformer.hs
```

Then in the file, rename the module and the synonym, and rewrite the doc comment's first paragraph:

```haskell
module Pawl.Type.HandActionPerformer where
```

```haskell
-- CR 103.5b / CR 103.6: how the closed half performs the effects of an action a
-- card grants from a player's HAND before the game begins -- the card that
-- granted it, the player taking it, and the effects themselves. Both windows use
-- it: the mulligan-declaration window (CR 103.5b) and the opening-hand window
-- (CR 103.6), the second of which is explicitly not a mulligan, which is why the
-- name is no longer MulliganPerformer.
type HandActionPerformer = ObjectId -> PlayerId -> [Effect Card] -> Game ()
```

Keep the rest of the comment (the cycle argument and the no-default argument) unchanged.

- [x] **Step 4: Regenerate the cabal module list.** `hooky fix` will NOT do this — the rename leaves `pawl.cabal` with no staged change of its own.

```bash
cabal-gild --io pawl.cabal && grep -n "HandActionPerformer\|MulliganPerformer" pawl.cabal
```
Expected: `Pawl.Type.HandActionPerformer` present, `Pawl.Type.MulliganPerformer` gone.

- [x] **Step 5: Rename the implementation and add the bindings.** In `source/library/Pawl/Resolve.hs`, rename `performMulliganAction` to `performHandAction`, update its import and signature, and replace its three empty maps:

```haskell
performHandAction :: HandActionPerformer.HandActionPerformer
performHandAction source player =
  Monad.mapM_
    ( applyEffect
        source
        player
        Map.empty
        -- CR 115.1: the reserved self slot is NOT a target, so there is no CR
        -- 608.2b legality question to answer -- the card is in the acting
        -- player's hand by construction. Binding it is how "this card" is
        -- expressible with no self-variant opcode (see Effect.Sacrifice's
        -- comment, and Engine.placeOne, which binds a trigger's source the same
        -- way).
        (Map.singleton Binding.triggerSource True)
        (Map.singleton Binding.triggerSource (Recipient.ToObject source))
    )
```

Update the `RestartGame` arm to `Setup.restartGame performHandAction controller`.

- [x] **Step 6: Follow the rename through.** `-Werror` names every site.

Run: `grep -rn "MulliganPerformer\|performMulliganAction" source/`
Expected: after editing, 0 hits. The sites are `Mulligan.hs` (import + three signatures), `Setup.hs` (import + three signatures), `Support.hs` (import + `performer`'s type). `S.performer` keeps its name.

- [x] **Step 7: Build and test.**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -20 && cabal test 2>&1 | tail -5`
Expected: warning-free; the new self-slot case passes and every CR 103.5b case still does.

- [x] **Step 8: Format, lint, and commit.**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "refactor(resolve): the hand-action performer serves both windows and binds self (#149)"
```

---

## Task 5: Leyline of the Void

**Files:**
- Create: `data/cards/leyline-of-the-void.json`
- Test: `source/test-suite/Pawl/CardsSpec.hs`

**Interfaces:**
- Consumes: `ControllerRelation.Opponents` (Task 2), `CardT.openingHandAction` (Task 3).
- Produces: the printing `"Leyline of the Void"`.

- [x] **Step 1: Write the failing test.** In `source/test-suite/Pawl/CardsSpec.hs`, beside the Serum Powder case:

```haskell
      HU.testCase "leyline-of-the-void.json loads with a CR 103.6a action and an Opponents redirect" $ do
        c <- Registry.card registry "Leyline of the Void"
        HU.assertEqual "name" (Text.pack "Leyline of the Void") (CardT.name c)
        HU.assertEqual
          "the CR 103.6a action puts itself onto the battlefield"
          [Effect.MoveToZone Binding.triggerSource Zone.Battlefield]
          (CardT.openingHandAction c)
        HU.assertEqual
          "and the redirect is scoped to an opponent's graveyard"
          [ ReplacementEffect.ZoneChangeR
              (ZoneChangePattern.MkZoneChangePattern Zone.Graveyard ControllerRelation.Opponents)
              Zone.Exile
          ]
          (CardT.replacementEffects c)
```

Add the imports the file lacks (`Binding`, `Zone`, `ZoneChangePattern`, `ControllerRelation`).

- [x] **Step 2: Run the test to verify it fails.**

Run: `cabal test 2>&1 | tail -30`
Expected: FAIL — `MkUnknownCard {slug = UnsafeSlug "leyline-of-the-void", …}`.

- [x] **Step 3: Write the card.** Oracle text (Scryfall, verified 2026-07-25): `{2}{B}{B}` Enchantment. "If this card is in your opening hand, you may begin the game with it on the battlefield. / If a card would be put into an opponent's graveyard from anywhere, exile it instead."

```json
{
  "activatedAbilities": [],
  "castingPermissions": [],
  "keywords": [],
  "manaCost": [
    {
      "type": "Generic",
      "value": 2
    },
    {
      "type": "OfType",
      "value": {
        "type": "Colored",
        "value": {
          "type": "Black"
        }
      }
    },
    {
      "type": "OfType",
      "value": {
        "type": "Colored",
        "value": {
          "type": "Black"
        }
      }
    }
  ],
  "name": "Leyline of the Void",
  "openingHandAction": [
    {
      "type": "MoveToZone",
      "value": [
        "self",
        {
          "type": "Battlefield"
        }
      ]
    }
  ],
  "power": null,
  "replacementEffects": [
    {
      "type": "ZoneChangeR",
      "value": [
        {
          "whenDestination": {
            "type": "Graveyard"
          },
          "whoseObject": {
            "type": "Opponents"
          }
        },
        {
          "type": "Exile"
        }
      ]
    }
  ],
  "spell": {
    "modes": [
      {
        "effects": [],
        "targetSpecs": []
      }
    ],
    "selection": {
      "type": "ChooseExactly",
      "value": 1
    }
  },
  "staticAbilities": [],
  "toughness": null,
  "triggeredAbilities": [],
  "typeLine": {
    "subtypes": [],
    "supertypes": [],
    "types": [
      {
        "type": "Enchantment"
      }
    ]
  }
}
```

The `SlotName` encoding above was checked against `Codec.slotNameToJson` (`Json.jText`, a bare string — **not** a tagged object) and against a committed card that already uses the opcode: `data/cards/aether-channeler.json` encodes `MoveToZone` as `["permanent", {"type": "Hand"}]`.

- [x] **Step 4: Run the tests to verify they pass.**

Run: `cabal test 2>&1 | tail -5`
Expected: PASS, including `CardsSpec`'s whole-directory re-parse and slug sweeps.

- [x] **Step 5: Format, lint, and commit.**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "feat(cards): add Leyline of the Void, the CR 103.6a gate card (#149)"
```

---

## Task 6: The CR 103.6 window

**Files:**
- Modify: `source/library/Pawl/Mulligan.hs`
- Test: `source/test-suite/Pawl/MulliganSpec.hs`

**Interfaces:**
- Consumes: everything from Tasks 1–5.
- Produces: `Mulligan.handWindow`, `Mulligan.openingHandActions :: HandActionPerformer -> [PlayerId] -> Game ()`.

- [x] **Step 1: Write the failing tests.** In `MulliganSpec`, add these fixtures beside the Serum Powder ones:

```haskell
-- alice's library with a Leyline of the Void on top and `n` Mountains under it;
-- bob's is uniform Mountains. The Leyline is drawn into her opening hand, which
-- is where CR 103.6 reads from.
leylineGame :: Printing.Printing -> Printing.Printing -> Int -> GameState.GameState
leylineGame leyline mountain n =
  let g0 = Setup.emptyGame S.bothPlayers
      addMany p pid k g = List.foldl' (\h _ -> snd (S.addCreature p pid h)) g (replicate k ())
      withAlice = addMany mountain S.alice n (addMany leyline S.alice 1 g0)
   in poolToLibrary S.bob (poolToLibrary S.alice (addMany mountain S.bob n withAlice))

-- Both players open with a Leyline on top, so the test can watch turn order.
leylineBothGame :: Printing.Printing -> Printing.Printing -> Int -> GameState.GameState
leylineBothGame leyline mountain n =
  let g0 = Setup.emptyGame S.bothPlayers
      addMany p pid k g = List.foldl' (\h _ -> snd (S.addCreature p pid h)) g (replicate k ())
      withAlice = addMany mountain S.alice n (addMany leyline S.alice 1 g0)
      withBoth = addMany mountain S.bob n (addMany leyline S.bob 1 withAlice)
   in poolToLibrary S.bob (poolToLibrary S.alice withBoth)

-- Takes every offered CR 103.6 action; keeps every hand; declines CR 103.5b.
useOpeningAction :: Prompt.Prompt r -> r
useOpeningAction p = case p of
  Prompt.OpeningHandAction _ _ candidates -> Maybe.listToMaybe candidates
  Prompt.DeclareMulligan {} -> MulliganDecision.Keep
  _ -> S.identityAnswer p

-- Declines every CR 103.6 action; keeps every hand.
declineOpeningAction :: Prompt.Prompt r -> r
declineOpeningAction p = case p of
  Prompt.OpeningHandAction {} -> Nothing
  Prompt.DeclareMulligan {} -> MulliganDecision.Keep
  _ -> S.identityAnswer p

-- Records the prompt stream as tags, so a test can prove CR 103.6's window opens
-- only once the whole CR 103.5 process is complete, and in turn order.
recordOpeningOrder :: Prompt.Prompt r -> State.State [(Text.Text, PlayerId)] r
recordOpeningOrder p = case p of
  Prompt.DeclareMulligan _ pid _ -> do
    State.modify' ((Text.pack "declare", pid) :)
    pure MulliganDecision.Keep
  Prompt.OpeningHandAction _ pid _ -> do
    State.modify' ((Text.pack "opening", pid) :)
    pure Nothing
  _ -> pure (S.identityAnswer p)
```

and these cases to the `tests` list (`Text` needs importing into `MulliganSpec`):

```haskell
      HU.testCase "CR 103.6: the window opens only once the mulligan process is complete" $ do
        leyline <- Registry.printing registry "Leyline of the Void"
        mountain <- Registry.printing registry "Mountain"
        let gs0 = leylineGame leyline mountain 20
            (_, tags) = State.runState (Program.foldProgramM recordOpeningOrder (State.runStateT (Mulligan.openingHands S.performer [S.alice, S.bob]) gs0)) []
            ordered = reverse tags
        HU.assertEqual
          "both declarations, then alice's opening-hand window"
          [(Text.pack "declare", S.alice), (Text.pack "declare", S.bob), (Text.pack "opening", S.alice)]
          ordered,
      HU.testCase "CR 103.6a: taking the action puts the card onto the battlefield" $ do
        leyline <- Registry.printing registry "Leyline of the Void"
        mountain <- Registry.printing registry "Mountain"
        let after = run useOpeningAction (leylineGame leyline mountain 20)
        HU.assertEqual "alice's hand is one smaller" 6 (S.handSize S.alice after)
        HU.assertEqual "and the Leyline is on the battlefield" 1 (length (Game.zoneMembers Zone.Battlefield S.alice after)),
      HU.testCase "CR 103.6: declining leaves the card in hand" $ do
        leyline <- Registry.printing registry "Leyline of the Void"
        mountain <- Registry.printing registry "Mountain"
        let after = run declineOpeningAction (leylineGame leyline mountain 20)
        HU.assertEqual "a full opening hand" 7 (S.handSize S.alice after)
        HU.assertEqual "and nothing on the battlefield" 0 (length (Game.zoneMembers Zone.Battlefield S.alice after)),
      HU.testCase "CR 103.6: the starting player's window comes before the other player's" $ do
        leyline <- Registry.printing registry "Leyline of the Void"
        mountain <- Registry.printing registry "Mountain"
        let gs0 = leylineBothGame leyline mountain 20
            (_, tags) = State.runState (Program.foldProgramM recordOpeningOrder (State.runStateT (Mulligan.openingHands S.performer [S.alice, S.bob]) gs0)) []
            openings = fmap snd (filter (\(tag, _) -> tag == Text.pack "opening") (reverse tags))
        HU.assertEqual "alice first, then bob" [S.alice, S.bob] openings,
      HU.testCase "CR 103.6: no granting card means no prompt" $ do
        mountain <- Registry.printing registry "Mountain"
        let gs0 = libraryGame mountain 20
            (_, tags) = State.runState (Program.foldProgramM recordOpeningOrder (State.runStateT (Mulligan.openingHands S.performer [S.alice, S.bob]) gs0)) []
        HU.assertEqual "no opening-hand prompt at all" [] (filter (\(tag, _) -> tag == Text.pack "opening") tags),
      HU.testCase "CR 103.6: a game with an opening-hand action replays deterministically" $ do
        leyline <- Registry.printing registry "Leyline of the Void"
        mountain <- Registry.printing registry "Mountain"
        let gs0 = leylineGame leyline mountain 20
            ((_, recorded), responses) = Replay.record useOpeningAction gs0 (Mulligan.openingHands S.performer [S.alice, S.bob])
            (_, replayed) = Replay.replay responses gs0 (Mulligan.openingHands S.performer [S.alice, S.bob])
        HU.assertEqual "battlefield matches" (Game.zoneMembers Zone.Battlefield S.alice recorded) (Game.zoneMembers Zone.Battlefield S.alice replayed)
        HU.assertEqual "hand matches" (S.handSize S.alice recorded) (S.handSize S.alice replayed),
```

**A two-Leyline case is deliberately omitted here** and added in Step 5 once the loop exists, because it is the one case that distinguishes a loop from a single ask.

- [x] **Step 2: Run the tests to verify they fail.**

Run: `cabal test 2>&1 | tail -30`
Expected: compile error on `Prompt.OpeningHandAction` in the local answerers is impossible (Task 1 added it), so these fail at runtime: no `opening` tag is ever recorded, the battlefield stays empty, and the ordering assertions get `[]`.

- [x] **Step 3: Write the shared window loop.** In `source/library/Pawl/Mulligan.hs`, replace `mulliganWindow` with the parameterized `handWindow` and keep a thin CR 103.5b caller:

```haskell
-- The shared CR 103.5b / CR 103.6 loop: offer this player every action their
-- hand grants through `field`, on the `ask` channel, until they decline or none
-- is left. Performing one is never a mulligan and never a cost -- it is the
-- action itself (CR 103.5b, CR 103.6).
--
-- Terminates even against an interpreter that never declines: every action in
-- the pool moves its card out of the hand (CR 103.6a puts it onto the
-- battlefield; CR 103.5b's exiles it), so the candidate list strictly shrinks.
handWindow ::
  (Card.Card -> [Effect Card.Card]) ->
  (Decider.Decider -> PlayerId -> [ObjectId] -> Prompt.Prompt (Maybe ObjectId)) ->
  HandActionPerformer ->
  PlayerId ->
  Game ()
handWindow field ask perform pid = do
  candidates <- State.gets (actionsFor field pid)
  case candidates of
    -- Where the rules leave nothing to ask, don't prompt.
    [] -> pure ()
    _ -> do
      decider <- State.gets (Decide.deciderFor pid)
      answer <- Trans.lift (Program.prompt (ask decider pid (fmap fst candidates)))
      case answer of
        Nothing -> pure ()
        Just oid -> case lookup oid candidates of
          -- An id that was not offered: validated by MEMBERSHIP, the
          -- Action.Activate posture, which keeps this total with no partial
          -- lookup and no way for an interpreter to conjure an action.
          Nothing -> pure ()
          Just effects -> do
            perform oid pid effects
            handWindow field ask perform pid
```

The CR 103.5b call site in `mulliganRounds` becomes:

```haskell
    handWindow Card.mulliganAction Prompt.MulliganAction perform pid
```

`Pawl.Mulligan` needs `import qualified Pawl.Type.Decider as Decider`. Keep `mulliganWindow`'s existing doc comment on the CR 103.5b call site as a short note; the general argument now lives on `handWindow`.

- [x] **Step 4: Add the CR 103.6 phase.**

```haskell
-- CR 103.6: "Once the mulligan process (see rule 103.5) is complete, the
-- starting player may take any such actions in any order. Then each other player
-- in turn order may do the same." `owners` is already in turn order with the
-- starting player first, which is exactly that order.
--
-- A player who has left the game is not here to act: the rebuild paths derive
-- `owners` from Departure.stillPlayingInOrder, so they get no window, exactly as
-- they get no opening hand.
openingHandActions :: HandActionPerformer -> [PlayerId] -> Game ()
openingHandActions perform owners =
  Monad.forM_ owners (handWindow Card.openingHandAction Prompt.OpeningHandAction perform)
```

and the third phase in `openingHands`:

```haskell
  mulliganRounds perform Map.empty owners
  -- CR 103.6: the opening-hand window, after the whole CR 103.5 process.
  openingHandActions perform owners
```

- [x] **Step 5: Add the loop-distinguishing case.** Now that the loop exists, prove it is a loop. Add this fixture and case:

```haskell
-- alice opens with TWO Leylines on top of her library, so CR 103.6's "any such
-- actions in any order" is observable: one ask cannot place both.
twoLeylineGame :: Printing.Printing -> Printing.Printing -> Int -> GameState.GameState
twoLeylineGame leyline mountain n =
  let g0 = Setup.emptyGame S.bothPlayers
      addMany p pid k g = List.foldl' (\h _ -> snd (S.addCreature p pid h)) g (replicate k ())
      withAlice = addMany mountain S.alice n (addMany leyline S.alice 2 g0)
   in poolToLibrary S.bob (poolToLibrary S.alice (addMany mountain S.bob n withAlice))
```

```haskell
      HU.testCase "CR 103.6: 'any such actions in any order' -- the window re-offers until declined" $ do
        leyline <- Registry.printing registry "Leyline of the Void"
        mountain <- Registry.printing registry "Mountain"
        let after = run useOpeningAction (twoLeylineGame leyline mountain 20)
        HU.assertEqual "both Leylines are on the battlefield" 2 (length (Game.zoneMembers Zone.Battlefield S.alice after))
        HU.assertEqual "and the hand is two smaller" 5 (S.handSize S.alice after),
```

- [x] **Step 6: Build and test.**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -20 && cabal test 2>&1 | tail -5`
Expected: warning-free; all seven new cases pass and the whole existing suite still does.

- [x] **Step 7: Verify the tests have teeth.** They passed only after the implementation, but confirm the ordering case is real: temporarily move `openingHandActions` ABOVE `mulliganRounds` in `openingHands` and check the "window opens only once the mulligan process is complete" case fails. Restore afterwards.

- [x] **Step 8: Format, lint, and commit.**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "feat(mulligan): open the CR 103.6 opening-hand window after the mulligan process (#149)"
```

---

## Task 7: Close out

**Files:**
- Modify: `source/library/Pawl/Type/Card.hs` (the `(#N)` placeholder), `docs/progress.md`, `CLAUDE.md`

- [x] **Step 1: File the two deferral issues.**

```bash
gh issue create --title "CR 103.6b: revealing a card from the opening hand (the Chancellor cycle)" \
  --label gap --label expires:card-driven \
  --body "CR 103.6b: \"If a card allows a player to reveal it from their opening hand, the player taking this action does so. The card remains revealed until the first turn begins. Each card may be revealed this way only once.\" Needs two pieces of state nothing else wants yet: a per-object revealed flag cleared when the first turn begins, and once-only tracking. CR 103.6a needs neither -- putting the card onto the battlefield removes it from the hand, which is its own once-only guard. The CR 103.6 window (Mulligan.openingHandActions) is where this plugs in; it would read a second Card field alongside openingHandAction. Spec: docs/superpowers/specs/2026-07-25-cr-103-6-opening-hand-actions-design.md section 2."

gh issue create --title "Gemstone Caverns (CR 103.6a with a rider)" \
  --label gap --label expires:card-driven \
  --body "A second CR 103.6a card, deferred with the rule's window built (#149). \"If Gemstone Caverns is in your opening hand and you're not the starting player, you may begin the game with Gemstone Caverns on the battlefield with a luck counter on it. If you do, exile a card from your hand.\" Needs a luck CounterKind (CR 122.1b-i are all future), an exile-from-hand choice, and a CONDITION on the action itself (\"you're not the starting player\") -- Card.openingHandAction is an unconditional effect list today. Spec: docs/superpowers/specs/2026-07-25-cr-103-6-opening-hand-actions-design.md section 2."
```

- [x] **Step 2: Replace the `(#N)` placeholder** in `Card.openingHandAction`'s comment with #183's number (the same one-action-per-card caveat `mulliganAction` cites), and comment on #184 so it covers the new field and the reserved-slot trap:

```bash
gh issue comment 184 --body "This now applies to \`Card.openingHandAction\` too (#149), and that field makes the trap concrete: Leyline of the Void's action is \`MoveToZone Binding.triggerSource Zone.Battlefield\`, and \`Resolve.slotsOf\` DOES return the reserved \`self\` slot for it. So an equality-style D4 lint widened to these fields must subtract the reserved slot names (self, variableX, chosenModes, copySource, you) from the read-slots side before comparing, exactly as Pawl.Binding's comment warns -- otherwise it demands a targetSpecs entry the reserved-slot rule forbids, and becomes unsatisfiable."
```

- [x] **Step 3: Verify the whole build and suite from clean.**

Run: `cabal clean && cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -20 && cabal test 2>&1 | tail -5`
Expected: warning-free; the whole suite passes.

- [x] **Step 4: Add the `docs/progress.md` entry**, after the #176 entry, matching the surrounding style: what it establishes (the CR 103.6 window and its position after the whole 103.5 process), the two design findings (no new opcode — `MoveToZone` plus the reserved self slot, per `Effect.Sacrifice`'s own comment; and the owner-based zone-change subject test per CR 400.3, which also corrects `Yours` for that event class), the gate card, what was added, the rename of `MulliganPerformer` → `HandActionPerformer` and why, the deferrals with their issue numbers, and the spec/plan paths.

- [x] **Step 5: Update the `CLAUDE.md` status bullet** — **replace**, never append. Fold CR 103.6a in beside the CR 103.5b sentence; leave the M5.6 summary and the "M6 is next" pointer intact.

- [x] **Step 6: Verify the plan is complete and close the issue.**

```bash
grep -c -- '- \[ \] \*\*Step' docs/superpowers/plans/2026-07-25-cr-103-6-opening-hand-actions.md
```
Expected: `0`.

```bash
gh issue close 149 --comment "CR 103.6a is implemented: the opening-hand window opens once the mulligan process completes, in turn order from the starting player, re-offering until each player declines. Leyline of the Void is in data/cards. CR 103.6b (reveal) and Gemstone Caverns are filed as #N and #N; CR 103.6c belongs to #175."
```

- [x] **Step 7: Format, lint, and commit.**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "docs(cr-103.6): record the opening-hand action gap closure (#149)"
```

---

## Self-Review Notes

**Spec coverage.** §2 scope → Tasks 5 and 7; §3.1 window placement → Task 6; §3.2 carrier + selector → Task 3; §3.3 no new opcode + self binding → Tasks 4 and 5; §3.4 rename → Task 4; §3.5 Opponents + owner test → Task 2; §4 prompt → Task 1; §5 window → Task 6; §6 testing → Tasks 2, 5, 6; §7 blast radius → File Structure; §8 done → Task 7; §9 deferrals → Task 7.

**One step deliberately carries a warning rather than finished code.** Task 4 Step 1's test sketch is written the wrong way on purpose, with the fix spelled out — the fixture needs a state that has *drawn* opening hands, and the naive version silently asserts nothing. Do not ship it as written.

**One trap already sprung.** Task 5's JSON originally encoded the `self` slot as a tagged object; `Codec.slotNameToJson` is `Json.jText`, a bare string, and `data/cards/aether-channeler.json` confirms the committed shape. Fixed in the block above before this plan was committed — the check that caught it is worth keeping for the next card.

**Ordering constraints.** Task 5 needs Tasks 2 and 3 (the relation and the field its JSON names). Task 6 needs all of 1–5. Task 4 must precede Task 6: without the reserved-slot binding, Leyline's action resolves to a no-op and Task 6's battlefield assertions would fail for the wrong reason.

**Where the loop-vs-single-ask distinction is proved.** Task 6 Step 5, not Step 1 — the two-Leyline case is the only one that fails if `handWindow` asks once instead of looping, so it is written after the loop exists and immediately re-run.
