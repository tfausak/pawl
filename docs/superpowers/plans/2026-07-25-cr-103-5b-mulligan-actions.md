# CR 103.5b Mulligan-Window Actions Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Implement CR 103.5b — an effect that lets a player act "any time [that player] could mulligan" — so Serum Powder works in a main game, a restart (CR 727), and a subgame (CR 729).

**Architecture:** A new `Prompt.MulliganAction` channel is offered inside `Mulligan.mulliganRounds`, immediately before each still-deciding player's `DeclareMulligan`, looping until they decline. The action's payload rides a new `Card.mulliganAction :: [Effect Card]` field read straight off the card (the ability functions in the hand, CR 113.6), and is performed by one new targetless opcode, `Effect.ExileHandThenDraw`. Because `Pawl.Resolve` sits *above* `Pawl.Mulligan` in the module graph (`Effect.RestartGame` → `Setup.restartGame` → `startGameFromCards` → `openingHands`), the effect performer is a **parameter** — `Pawl.Type.MulliganPerformer` — threaded through the four setup entry points, the `resolveSpellWith runSubgame` precedent.

**Tech Stack:** Haskell 2010 (GHC 9.14.1), `tasty`/`tasty-hunit`, the `StateT GameState (Program Prompt)` suspension monad, the hand-written JSON codec in `Pawl.Codec`.

## Global Constraints

- **Haskell 2010, no language extensions** unless unavoidable. `Pawl.Type.Prompt` already has `{-# LANGUAGE GADTs #-}`; `Pawl.MulliganSpec` already has `GADTs` + `RankNTypes`. No new extensions are needed.
- **Warning-clean under `+pedantic` (`-Werror`).** Every exhaustive `Prompt` and `Effect` match must be updated or the build fails — that is the safety net for the mechanical arms in Tasks 1 and 2. Run `cabal build all --enable-tests --enable-benchmarks`; `cabal clean` first when a definitive warning check is needed. **Never run two builds concurrently.**
- **No partial functions**, no boolean blindness, `Mk` constructor prefix, derive at least `Eq` and `Show`, `Text` not `String`, no list comprehensions, `let` over `where`, `case` over point-free.
- **One type per module** under `Pawl.Type.<Name>`; qualified imports aliased to the last component; no explicit export lists; a module never imports its parents.
- **The two invariants outrank this plan:** the engine never cases on a card's identity (only classifications), and never makes a player's choice. `Pawl.Mulligan` may *mention* `Effect` but must never `case` on one — the only module that may is `Pawl.Resolve`.
- **TDD non-negotiable:** write each failing test, run it, watch it fail, then implement. Tick each `- [ ]` as you finish it.
- **Every rules claim cites CR 103.5b** (or the specific sub-rule) and was checked against `docs/rules.txt`. Never write an expiry (milestone name) into a code comment — file an issue and cite `(#N)`.
- **Commit style:** commit directly to `main`, one small complete commit per task. End commit messages with the two trailers from `CLAUDE.md` (Co-Authored-By + Claude-Session).
- **After each task:** `git add -A`, `hooky fix`, `git add -A`, `hooky run` (both act on **staged** files only).

**Spec:** `docs/superpowers/specs/2026-07-25-cr-103-5b-mulligan-actions-design.md`.

---

## File Structure

- **Create** `source/library/Pawl/Type/MulliganPerformer.hs` — the `ObjectId -> PlayerId -> [Effect Card] -> Game ()` synonym (the `Pawl.Type.Game` synonym-module precedent).
- **Create** `data/cards/serum-powder.json` — the gate card.
- **Modify** `source/library/Pawl/Type/Prompt.hs` — `+MulliganAction`.
- **Modify** `source/library/Pawl/Type/Response.hs` — `+TookMulliganAction`.
- **Modify** `source/library/Pawl/Type/Effect.hs` — `+ExileHandThenDraw`.
- **Modify** `source/library/Pawl/Type/Card.hs` — `+mulliganAction`.
- **Modify** `source/library/Pawl/Replay.hs` — `encode`/`decode`/`defaultAnswer` arms.
- **Modify** `source/library/Pawl/Codec.hs` — the effect arms (2) and the card arms (encode-if-non-empty, decode-with-default, record field).
- **Modify** `source/library/Pawl/Resolve.hs` — six exhaustive `Effect` arms, the opcode implementation, `performMulliganAction`, and the `RestartGame` arm.
- **Modify** `source/library/Pawl/Mulligan.hs` — `actionsFor`, `mulliganWindow`, the performer parameter on `openingHands`/`mulliganRounds`.
- **Modify** `source/library/Pawl/Setup.hs` — the performer parameter on `newGame`, `startGameFromCards`, `restartGame`.
- **Modify** `source/library/Pawl/Engine.hs` — two call sites pass `Resolve.performMulliganAction`.
- **Modify** the 13 exhaustive `Prompt` answerers: `source/test-suite/Pawl/Support.hs` (5), `source/benchmark/Main.hs` (3), `source/test-suite/Pawl/CastSpec.hs` (3), `source/test-suite/Pawl/GameSpec.hs` (2).
- **Modify** `source/test-suite/Pawl/Support.hs` — also `+performer`, the one `MulliganPerformer` the tests pass.
- **Modify** the four `Card.Type.MkCard` literals: `source/test-suite/Pawl/CardSpec.hs:203`, `source/test-suite/Pawl/ResolveSpec.hs:440,761,814`.
- **Modify** `source/test-suite/Pawl/{ReplaySpec,CodecSpec,CardSpec,CardsSpec,ResolveSpec,MulliganSpec,SetupSpec,GameSpec}.hs` — new tests and re-signatured call sites.
- **Modify** `pawl.cabal` — regenerated by `cabal-gild` (via `hooky fix`) for the new library module; do not hand-edit the discovered fields.

---

## Task 1: The `MulliganAction` prompt channel

The wire only, no behavior. One commit, because a new GADT constructor breaks every exhaustive `Prompt` match until all 13 are updated.

**Files:**
- Modify: `source/library/Pawl/Type/Prompt.hs`, `source/library/Pawl/Type/Response.hs`, `source/library/Pawl/Replay.hs`
- Modify (answerers): `source/test-suite/Pawl/Support.hs`, `source/benchmark/Main.hs`, `source/test-suite/Pawl/CastSpec.hs`, `source/test-suite/Pawl/GameSpec.hs`
- Test: `source/test-suite/Pawl/ReplaySpec.hs`

**Interfaces:**
- Produces: `Prompt.MulliganAction :: Decider -> PlayerId -> [ObjectId] -> Prompt (Maybe ObjectId)`; `Response.TookMulliganAction (Maybe ObjectId)`.

- [ ] **Step 1: Write the failing test.** In `source/test-suite/Pawl/ReplaySpec.hs`, add this case immediately after the existing `"Bottom records and replays an ordered [ObjectId]"` case (around line 91), inside the same list:

```haskell
          HU.testCase "MulliganAction records and replays a Maybe ObjectId" $
            let p = Prompt.MulliganAction decider S.alice [ObjectId.MkObjectId 7, ObjectId.MkObjectId 8]
                answer = Just (ObjectId.MkObjectId 8)
             in HU.assertEqual "round trip" (Just answer) (Replay.decode p (Replay.encode p answer)),
```

- [ ] **Step 2: Run the test to verify it fails to COMPILE** (the constructor does not exist yet).

Run: `cabal build pawl-test-suite --enable-tests 2>&1 | tail -20`
Expected: compile error — `Data constructor not in scope: Prompt.MulliganAction`.

- [ ] **Step 3: Add the `Prompt` constructor.** In `source/library/Pawl/Type/Prompt.hs`, append after `Bottom` (the last constructor):

```haskell
  -- CR 103.5b: "If an effect allows a player to perform an action 'any time
  -- [that player] could mulligan,' the player may perform that action at a time
  -- they would declare whether they will take a mulligan." The [ObjectId] is the
  -- cards in this player's hand that grant such an action; the answer is which
  -- one to use, or Nothing to decline.
  --
  -- Offered immediately BEFORE each DeclareMulligan, and again after each action
  -- taken -- CR 103.5b's last sentence ("If the player performs the action, they
  -- then declare whether they will take a mulligan") makes the declaration
  -- follow, and nothing in the rule limits a player to one action. Offered in
  -- EVERY round, not just the first ("This need not be in the first round of
  -- mulligans"), and only to a player who has not yet kept -- a player who kept
  -- never declares again, so there is no time at which they could act.
  --
  -- Performing the action is NOT taking a mulligan: nothing is shuffled or
  -- bottomed and the CR 103.5 mulligan count is untouched, so it feeds neither
  -- the bottom count nor CR 103.5c's free allowance.
  --
  -- Not asked when the list is empty; where the rules leave nothing to ask,
  -- don't prompt. Carries a Decider like every other player-facing prompt.
  MulliganAction :: Decider -> PlayerId -> [ObjectId] -> Prompt (Maybe ObjectId)
```

- [ ] **Step 4: Add the `Response` constructor.** In `source/library/Pawl/Type/Response.hs`, append inside the `data Response` sum, after `PutOnBottom [ObjectId]`:

```haskell
  | -- CR 103.5b: the hand card whose mulligan-window action a player took
    -- (Nothing = declined), serialized so a DecisionLog replays it. Its own
    -- constructor rather than a reuse of Searched / CastWhileSearched, for the
    -- reason ChoseDefender records: decode's job is to return Nothing for a
    -- response that does not match the prompt being asked, and two prompts
    -- sharing a constructor cannot do that.
    TookMulliganAction (Maybe ObjectId)
```

- [ ] **Step 5: Wire `Pawl.Replay`.** Three edits in `source/library/Pawl/Replay.hs`:

In `encode`, after the `Prompt.Bottom {}` arm:

```haskell
  Prompt.MulliganAction {} -> Response.TookMulliganAction answer
```

In `decode`, after the `Prompt.Bottom {}` arm:

```haskell
  Prompt.MulliganAction {} -> case response of
    Response.TookMulliganAction moid -> Just moid
    _ -> Nothing
```

In `defaultAnswer`, after the `Prompt.Bottom` arm:

```haskell
  -- CR 103.5b: declining is always legal and the least-eventful fallback when a
  -- transcript runs short (mirrors DeclareMulligan -> Keep).
  Prompt.MulliganAction {} -> Nothing
```

- [ ] **Step 6: Update the 13 exhaustive answerers.** Each already has a `Prompt.Bottom _ _ hand count -> …` arm; add the new arm directly after it. **Pure** answerers (the arm is `Prompt.MulliganAction {} -> Nothing`): `Support.hs:220,261,295,348`; `benchmark/Main.hs:88,119,151`; `CastSpec.hs:430,634,676`; `GameSpec.hs:1369`. **Monadic** answerers (the arm is `Prompt.MulliganAction {} -> pure Nothing`): `Support.hs:434`; `GameSpec.hs:350`.

Line numbers shift as you edit — find them by searching for `Prompt.Bottom`:

Run: `grep -n "Prompt.Bottom" source/test-suite/Pawl/Support.hs source/benchmark/Main.hs source/test-suite/Pawl/CastSpec.hs source/test-suite/Pawl/GameSpec.hs`
Expected: 13 hits. Every one gets a sibling arm.

- [ ] **Step 7: Run the build and the test to verify they pass.**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -20 && cabal test 2>&1 | tail -20`
Expected: warning-free build; the suite passes, including `MulliganAction records and replays a Maybe ObjectId`.

- [ ] **Step 8: Format, lint, and commit.**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "feat(prompt): add the CR 103.5b MulliganAction channel (#182)"
```

---

## Task 2: The `ExileHandThenDraw` opcode

Serum Powder's payload. One commit, because a new `Effect` constructor breaks all nine exhaustive matches.

**Files:**
- Modify: `source/library/Pawl/Type/Effect.hs`, `source/library/Pawl/Codec.hs`, `source/library/Pawl/Resolve.hs`
- Modify (exhaustive match): `source/test-suite/Pawl/CardSpec.hs` (`effectCounts`, around line 293)
- Test: `source/test-suite/Pawl/ResolveSpec.hs`, `source/test-suite/Pawl/CodecSpec.hs`

**Interfaces:**
- Consumes: nothing from Task 1.
- Produces: `Effect.ExileHandThenDraw :: Effect card` (nullary).

- [ ] **Step 1: Write the failing tests.** In `source/test-suite/Pawl/ResolveSpec.hs`, add this case to the top-level test list (anywhere in it; next to the Rest in Peace case near line 621 is a natural home):

```haskell
      HU.testCase "CR 103.5b ExileHandThenDraw exiles the whole hand, then draws that many" $ do
        mountain <- Registry.printing registry "Mountain"
        swamp <- Registry.printing registry "Swamp"
        let g0 = Setup.emptyGame S.bothPlayers
            (_, g1) = S.addHandCard mountain S.alice g0
            (_, g2) = S.addHandCard swamp S.alice g1
            g3 = List.foldl' (\g _ -> snd (S.addLibraryCard mountain S.alice g)) g2 (replicate 5 ())
            after =
              S.runPure S.identityAnswer g3 $
                Resolve.applyEffect S.noSource S.alice Map.empty Map.empty Map.empty Effect.ExileHandThenDraw
        HU.assertEqual "the hand is refilled to the size it had" 2 (S.handSize S.alice after)
        HU.assertEqual "both old cards went to exile" 2 (length (Game.zoneMembers Zone.Exile S.alice after))
        HU.assertEqual "and the library is two shorter" 3 (length (Game.zoneMembers Zone.Library S.alice after)),
```

In `source/test-suite/Pawl/CodecSpec.hs`, add this case next to the existing `"ExileAllGraveyards"` round-trip (around line 177):

```haskell
          HU.testCase "ExileHandThenDraw" $
            roundTrip "e-powder" Codec.effectToJson Codec.jsonToEffect Effect.ExileHandThenDraw,
```

- [ ] **Step 2: Run the tests to verify they fail to COMPILE.**

Run: `cabal build pawl-test-suite --enable-tests 2>&1 | tail -20`
Expected: compile error — `Data constructor not in scope: Effect.ExileHandThenDraw`.

- [ ] **Step 3: Add the opcode.** In `source/library/Pawl/Type/Effect.hs`, append after `PlaySubgame SlotName`:

```haskell
  | -- CR 103.5b (Serum Powder): exile every card in the resolving controller's
    -- hand, then draw that many cards. Targetless and controller-scoped, the
    -- ExileAllGraveyards / Draw shape.
    --
    -- ONE opcode rather than an exile composed with a Draw: "that many" is the
    -- hand size BEFORE the exile, so a following Draw would read a hand that is
    -- already empty. Splitting it needs a Count that reads a value produced
    -- earlier in the same resolution, which nothing else wants.
    --
    -- The card granting the action is itself in the hand and is exiled with the
    -- rest: CR 103.5b's action is not a cost, and nothing sets it aside.
    ExileHandThenDraw
```

- [ ] **Step 4: Add the eight classification arms.** Each is a one-liner beside the module's existing `Effect.ExileAllGraveyards` arm.

`source/library/Pawl/Codec.hs` — in `effectToJson` (beside line 1199) and `jsonToEffect` (beside line 1236):

```haskell
  Effect.ExileHandThenDraw -> nullary (Text.pack "ExileHandThenDraw")
```

```haskell
    "ExileHandThenDraw" -> Right Effect.ExileHandThenDraw
```

`source/library/Pawl/Resolve.hs` — five classification arms (`slotsOf` ~78, `readsX` ~119, `manaProduced` ~152, `searchesLibrary` ~185, `rewriteEffect` ~259):

```haskell
  Effect.ExileHandThenDraw -> Set.empty      -- slotsOf: targetless
      Effect.ExileHandThenDraw -> False      -- readsX
  Effect.ExileHandThenDraw -> Nothing        -- manaProduced: not a mana ability
  Effect.ExileHandThenDraw -> False          -- searchesLibrary
  Effect.ExileHandThenDraw -> effect         -- rewriteEffect: no text to change
```

`source/test-suite/Pawl/CardSpec.hs` — in `effectCounts` (beside line 293):

```haskell
  Effect.ExileHandThenDraw -> []
```

- [ ] **Step 5: Implement the opcode.** In `source/library/Pawl/Resolve.hs`'s `applyEffectWith`, add an arm beside `Effect.ExileAllGraveyards` (around line 555):

```haskell
  -- CR 103.5b (Serum Powder): "exile all the cards from your hand, then draw
  -- that many cards." The count is the hand size BEFORE the exile, which is why
  -- this is one opcode and not an exile followed by a Draw.
  --
  -- Both halves go through the usual funnels: Event.changeZone mints each exiled
  -- card a fresh incarnation (CR 400.7), and Event.drawCard flags a draw from an
  -- empty library, so a short deck still loses at the first upkeep (CR 727.3 /
  -- 729.3) exactly as the mulligan redraw already does.
  Effect.ExileHandThenDraw -> do
    gs <- State.get
    let handIds = Game.zoneMembers Zone.Hand controller gs
    Monad.mapM_ (\oid -> Event.changeZone oid Zone.Exile) handIds
    Monad.replicateM_ (length handIds) (Event.drawCard controller)
```

- [ ] **Step 6: Run the tests to verify they pass.**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -20 && cabal test 2>&1 | tail -20`
Expected: warning-free build; both new cases pass.

- [ ] **Step 7: Format, lint, and commit.**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "feat(effect): add ExileHandThenDraw, Serum Powder's CR 103.5b payload (#182)"
```

---

## Task 3: The `Card.mulliganAction` carrier

**Files:**
- Modify: `source/library/Pawl/Type/Card.hs`, `source/library/Pawl/Codec.hs`
- Modify (record literals): `source/test-suite/Pawl/CardSpec.hs:203`, `source/test-suite/Pawl/ResolveSpec.hs:440,761,814`
- Test: `source/test-suite/Pawl/CodecSpec.hs`

**Interfaces:**
- Consumes: `Effect.ExileHandThenDraw` (Task 2).
- Produces: `CardT.mulliganAction :: Card -> [Effect Card]`.

- [ ] **Step 1: Write the failing tests.** In `source/test-suite/Pawl/CodecSpec.hs`, add both cases next to the existing `"a Card carrying player abilities round-trips"` / `"an empty playerAbilities list is omitted from the JSON"` pair (around lines 233–248):

```haskell
          HU.testCase "a Card carrying a CR 103.5b mulligan action round-trips" $ do
            bloodMoon <- Registry.printing registry "Blood Moon"
            let base = Printing.card bloodMoon
                c = base {CardT.mulliganAction = [Effect.ExileHandThenDraw]}
            roundTrip "card" Codec.cardToJson Codec.jsonToCard c,
          -- Byte-stability: an empty list must not appear in the rendered JSON,
          -- or every committed card file changes. The same posture
          -- playerAbilities and additionalCosts already take.
          HU.testCase "an empty mulliganAction list is omitted from the JSON" $ do
            bloodMoon <- Registry.printing registry "Blood Moon"
            let base = Printing.card bloodMoon
            HU.assertEqual "the fixture really has none" [] (CardT.mulliganAction base)
            case J.asObject (Codec.cardToJson base) of
              Left err -> HU.assertFailure (Text.unpack err)
              Right pairs -> HU.assertBool "key absent" (notElem (Text.pack "mulliganAction") (fmap fst pairs)),
```

`CodecSpec` may not yet import `Pawl.Type.Effect`; if not, add `import qualified Pawl.Type.Effect as Effect` to its import block.

- [ ] **Step 2: Run the tests to verify they fail to COMPILE.**

Run: `cabal build pawl-test-suite --enable-tests 2>&1 | tail -20`
Expected: compile error — `CardT.mulliganAction` is not a field of `Card`.

- [ ] **Step 3: Add the field.** In `source/library/Pawl/Type/Card.hs`, append inside `MkCard` after `playerAbilities`:

```haskell
    -- CR 103.5b: the effects of this card's "any time you could mulligan"
    -- action, in written order. Empty for every printing but Serum Powder.
    --
    -- Read DIRECTLY from the card and never through the projection -- the
    -- castingPermissions / additionalCosts precedent: the ability functions in
    -- the HAND (CR 113.6), where the CR 613 layer system does not reach.
    --
    -- An empty list means NO action, not an action that does nothing: the two
    -- are indistinguishable in play, so the ambiguity costs nothing.
    --
    -- One action per card. A printing declaring two is unrepresentable (#N).
    mulliganAction :: [Effect Card]
```

Note: `#N` is a placeholder replaced in Task 7 with the real issue number — do not leave it as `#N` at the end of the plan. `Pawl.Type.Card` already imports `Effect` for other fields? It does **not** — add `import Pawl.Type.Effect (Effect)` to its import block.

- [ ] **Step 4: Wire the codec.** In `source/library/Pawl/Codec.hs`:

`cardToJson` — append another optional-key block after the `alternativeCosts` one:

```haskell
        <> ( if null (CardT.mulliganAction c)
               then []
               else [(Text.pack "mulliganAction", listTo effectToJson (CardT.mulliganAction c))]
           )
```

`jsonToCard` — add the binding after `alternativeCosts`, and the record field after `CardT.alternativeCosts = alternativeCosts,`:

```haskell
  mulliganAction <- listFromDefault jsonToEffect (getOpt (Text.pack "mulliganAction") ps)
```

```haskell
        CardT.mulliganAction = mulliganAction
```

- [ ] **Step 5: Update the four `MkCard` record literals.** Each needs the new field; all four fixtures declare no mulligan action:

```haskell
                  Card.Type.mulliganAction = [],
```

Run: `grep -n "MkCard" source/test-suite/Pawl/CardSpec.hs source/test-suite/Pawl/ResolveSpec.hs`
Expected: 4 hits (CardSpec 1, ResolveSpec 3). Each literal gets the field.

- [ ] **Step 6: Run the tests to verify they pass.**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -20 && cabal test 2>&1 | tail -20`
Expected: warning-free build; both new codec cases pass, and every existing card file still round-trips byte-identically (`Pawl.CardsSpec`'s whole-directory sweep proves the omit-when-empty half).

- [ ] **Step 7: Format, lint, and commit.**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "feat(card): carry a CR 103.5b mulligan action on Card (#182)"
```

---

## Task 4: Serum Powder

**Files:**
- Create: `data/cards/serum-powder.json`
- Test: `source/test-suite/Pawl/CardsSpec.hs`

**Interfaces:**
- Consumes: `Effect.ExileHandThenDraw` (Task 2), `CardT.mulliganAction` (Task 3).
- Produces: the printing `"Serum Powder"` in the registry.

- [ ] **Step 1: Write the failing test.** In `source/test-suite/Pawl/CardsSpec.hs`, add this case next to the `clone.json` case in the same list:

```haskell
      HU.testCase "serum-powder.json loads as a {3} artifact with a CR 103.5b mulligan action" $ do
        c <- Registry.card registry "Serum Powder"
        HU.assertEqual "name" (Text.pack "Serum Powder") (CardT.name c)
        HU.assertEqual "the CR 103.5b action" [Effect.ExileHandThenDraw] (CardT.mulliganAction c)
        HU.assertEqual "one activated ability, the {T}: Add {C} mana ability" 1 (length (CardT.activatedAbilities c))
```

`CardsSpec` may not yet import `Pawl.Type.Effect`; if not, add `import qualified Pawl.Type.Effect as Effect`.

- [ ] **Step 2: Run the test to verify it fails.**

Run: `cabal test 2>&1 | tail -30`
Expected: FAIL — the registry throws `UnknownCard` for `"Serum Powder"`.

- [ ] **Step 3: Write the card.** Create `data/cards/serum-powder.json`. Oracle text (Scryfall `ima/228`): `{3}` Artifact; `{T}: Add {C}.`; "Any time you could mulligan and this card is in your hand, you may exile all the cards from your hand, then draw that many cards."

```json
{
  "activatedAbilities": [
    {
      "cost": {
        "components": [
          {
            "type": "TapThis"
          }
        ],
        "mana": []
      },
      "modal": {
        "modes": [
          {
            "effects": [
              {
                "type": "AddMana",
                "value": {
                  "type": "Colorless"
                }
              }
            ],
            "targetSpecs": []
          }
        ],
        "selection": {
          "type": "ChooseExactly",
          "value": 1
        }
      }
    }
  ],
  "castingPermissions": [],
  "keywords": [],
  "manaCost": [
    {
      "type": "Generic",
      "value": 3
    }
  ],
  "mulliganAction": [
    {
      "type": "ExileHandThenDraw"
    }
  ],
  "name": "Serum Powder",
  "power": null,
  "replacementEffects": [],
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
        "type": "Artifact"
      }
    ]
  }
}
```

- [ ] **Step 4: Run the tests to verify they pass.**

Run: `cabal test 2>&1 | tail -20`
Expected: PASS — including `Pawl.CardsSpec`'s "each committed file re-parses to its compiled card" and "every file name in data/cards is already a slug" sweeps, which now cover the new file.

- [ ] **Step 5: Format, lint, and commit.**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "feat(cards): add Serum Powder, the CR 103.5b gate card (#182)"
```

---

## Task 5: The CR 103.5b window

The behavior: the performer parameter, the classification, and the window loop. One commit — the parameter and the loop are one deliverable, and the parameter alone changes nothing observable.

**Files:**
- Create: `source/library/Pawl/Type/MulliganPerformer.hs`
- Modify: `source/library/Pawl/Resolve.hs`, `source/library/Pawl/Mulligan.hs`, `source/library/Pawl/Setup.hs`, `source/library/Pawl/Engine.hs`
- Modify (call sites): `source/test-suite/Pawl/Support.hs`, `source/test-suite/Pawl/SetupSpec.hs`, `source/test-suite/Pawl/GameSpec.hs`, `source/test-suite/Pawl/CastSpec.hs`, `source/test-suite/Pawl/MulliganSpec.hs`
- Test: `source/test-suite/Pawl/MulliganSpec.hs`

**Interfaces:**
- Consumes: `Prompt.MulliganAction` (Task 1), `Effect.ExileHandThenDraw` (Task 2), `CardT.mulliganAction` (Task 3), the `"Serum Powder"` printing (Task 4).
- Produces: `MulliganPerformer.MulliganPerformer = ObjectId -> PlayerId -> [Effect Card] -> Game ()`; `Resolve.performMulliganAction :: MulliganPerformer`; `Mulligan.actionsFor :: PlayerId -> GameState -> [(ObjectId, [Effect Card])]`; `Mulligan.openingHands :: MulliganPerformer -> [PlayerId] -> Game ()`; `Setup.newGame :: MulliganPerformer -> NonEmpty (PlayerId, Deck) -> Game ()`; `Setup.startGameFromCards :: MulliganPerformer -> Game ()`; `Setup.restartGame :: MulliganPerformer -> PlayerId -> Game ()`; `S.performer :: MulliganPerformer`.

- [ ] **Step 1: Write the failing tests.** In `source/test-suite/Pawl/MulliganSpec.hs`, add these helpers after `bottomReversedAnswer`, and the two cases to the `tests` list. Exactly one new import is needed — `import qualified Data.Maybe as Maybe`; everything else these use (`State`, `Program`, `Prompt`, `ObjectId`, `Printing`, `Registry`, `Replay`, `Set`, `GameState`, `Game`, `Zone`, `PlayerId`) is already imported there.

```haskell
-- alice's library: a Serum Powder on top, then `n` Mountains; bob's is uniform
-- Mountains. poolToLibrary orders a library by ascending ObjectId, which is
-- insertion order, so the Powder added first is the top card and is drawn into
-- her opening hand -- CR 103.5b's window reads the HAND, so it has to be drawn
-- and not merely owned.
powderGame :: Printing.Printing -> Printing.Printing -> Int -> GameState.GameState
powderGame powder mountain n =
  let g0 = Setup.emptyGame S.bothPlayers
      addMany p pid k g = List.foldl' (\h _ -> snd (S.addCreature p pid h)) g (replicate k ())
      withAlice = addMany mountain S.alice n (addMany powder S.alice 1 g0)
   in poolToLibrary S.bob (poolToLibrary S.alice (addMany mountain S.bob n withAlice))

-- Records every CR 103.5b offer's candidate list, declines it, and keeps.
recordWindow :: Prompt.Prompt r -> State.State [[ObjectId.ObjectId]] r
recordWindow p = case p of
  Prompt.MulliganAction _ _ candidates -> do
    State.modify' (candidates :)
    pure Nothing
  Prompt.DeclareMulligan {} -> pure MulliganDecision.Keep
  _ -> pure (S.identityAnswer p)

-- Takes the first offered CR 103.5b action whenever one is offered, and always
-- keeps. With a single Powder in the deck the window loop ends by itself: the
-- action exiles the Powder along with the rest of the hand, so the redrawn hand
-- offers nothing.
usePowder :: Prompt.Prompt r -> r
usePowder p = case p of
  Prompt.MulliganAction _ _ candidates -> Maybe.listToMaybe candidates
  Prompt.DeclareMulligan {} -> MulliganDecision.Keep
  _ -> S.identityAnswer p
```

```haskell
      HU.testCase "CR 103.5b: a hand card granting an action is offered at the declaration" $ do
        powder <- Registry.printing registry "Serum Powder"
        mountain <- Registry.printing registry "Mountain"
        let gs0 = powderGame powder mountain 20
            ((_, _after), offered) = State.runState (Program.foldProgramM recordWindow (State.runStateT (Mulligan.openingHands S.performer [S.alice, S.bob]) gs0)) []
        HU.assertEqual "exactly one offer -- alice's, in the one round she declares" 1 (length offered)
        HU.assertEqual "and it offered exactly her Powder" [1] (fmap length offered),
      HU.testCase "CR 103.5b: taking the action exiles the whole hand and redraws that many" $ do
        powder <- Registry.printing registry "Serum Powder"
        mountain <- Registry.printing registry "Mountain"
        let after = run usePowder (powderGame powder mountain 20)
        HU.assertEqual "alice's hand is a full seven again" 7 (S.handSize S.alice after)
        HU.assertEqual "her first seven are exiled" 7 (length (Game.zoneMembers Zone.Exile S.alice after))
        HU.assertEqual "and her library is seven shorter than after the opening draw" 7 (libSize S.alice after)
        HU.assertEqual "bob, with no Powder, is untouched" 7 (S.handSize S.bob after)
        HU.assertEqual "and exiles nothing" 0 (length (Game.zoneMembers Zone.Exile S.bob after)),
```

The `libSize` arithmetic: alice's library is 21 (one Powder + 20 Mountains). The opening draw takes 7, leaving 14; the action exiles those 7 and draws 7 more, leaving 7.

- [ ] **Step 2: Run the tests to verify they fail to COMPILE** (`Mulligan.openingHands` takes one argument today, and `S.performer` does not exist).

Run: `cabal build pawl-test-suite --enable-tests 2>&1 | tail -20`
Expected: compile error — `Couldn't match expected type` on `Mulligan.openingHands`, and `S.performer` not in scope.

- [ ] **Step 3: Create the performer type.** Write `source/library/Pawl/Type/MulliganPerformer.hs`:

```haskell
module Pawl.Type.MulliganPerformer where

import Pawl.Type.Card (Card)
import Pawl.Type.Effect (Effect)
import Pawl.Type.Game (Game)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)

-- CR 103.5b: how the closed half performs a mulligan-window action's effects --
-- the hand card that granted the action, the player taking it, and the effects
-- themselves.
--
-- A PARAMETER rather than an import because Pawl.Resolve sits ABOVE
-- Pawl.Mulligan in the module graph (Effect.RestartGame -> Setup.restartGame ->
-- startGameFromCards -> openingHands), so Pawl.Mulligan importing Pawl.Resolve
-- would close a cycle. That cycle is a fact about the RULES, not the layout: an
-- opcode restarts a game, a game start draws opening hands, and drawing opening
-- hands performs opcodes. The precedent is Resolve.resolveSpellWith, which takes
-- its subgame runner the same way for the same reason.
--
-- Deliberately has NO default: "no subgame runner" is a real state of the world
-- (Resolve.noSubgame), but "no mulligan performer" is not -- it would silently
-- disable every CR 103.5b card at whichever call site forgot one.
type MulliganPerformer = ObjectId -> PlayerId -> [Effect Card] -> Game ()
```

- [ ] **Step 4: Implement the performer in `Pawl.Resolve`.** Add `import qualified Pawl.Type.MulliganPerformer as MulliganPerformer` to `source/library/Pawl/Resolve.hs`, then add this definition next to `applyEffect` (around line 910):

```haskell
-- CR 103.5b: perform the effects of a mulligan-window action. Pawl.Mulligan's
-- window loop reaches this through the MulliganPerformer parameter it is handed
-- (see Pawl.Type.MulliganPerformer for why it is a parameter).
--
-- The action does not use the stack -- CR 103.5b says the player PERFORMS it,
-- not that they cast or activate anything -- so there is nothing to put on the
-- stack, no targets to choose and no modes to bind: the empty binding, legality
-- and chosen maps are the whole context. The CHOICE of whether to act was
-- already routed through Decide.deciderFor at the prompt (CR 723).
--
-- Stands on the noSubgame floor (applyEffect), exactly as the RestartGame arm
-- does: no mulligan-window action starts a subgame.
performMulliganAction :: MulliganPerformer.MulliganPerformer
performMulliganAction source player =
  Monad.mapM_ (applyEffect source player Map.empty Map.empty Map.empty)
```

- [ ] **Step 5: Add the window to `Pawl.Mulligan`.** In `source/library/Pawl/Mulligan.hs`, add the imports `import qualified Data.Maybe as Maybe`, `import qualified Pawl.Type.Card as Card`, `import Pawl.Type.Effect (Effect)`, `import Pawl.Type.MulliganPerformer (MulliganPerformer)`, `import Pawl.Type.ObjectId (ObjectId)`, then add these two definitions (put them just before `mulliganRounds`):

```haskell
-- CR 103.5b: the cards in this player's hand that grant an action they may take
-- at their mulligan declaration, each paired with the effects that action
-- performs. A CLASSIFICATION, not an identity test: this asks whether the card
-- declares an action, never which card it is.
--
-- Read straight off the card (Game.cardOf) and never through the projection --
-- the Card.castingPermissions precedent: the ability functions in the HAND (CR
-- 113.6), where the CR 613 layer system does not reach.
actionsFor :: PlayerId -> GameState.GameState -> [(ObjectId, [Effect Card.Card])]
actionsFor pid gs =
  let withAction oid = case Game.cardOf oid gs of
        Nothing -> Nothing
        Just card -> case Card.mulliganAction card of
          [] -> Nothing
          effects -> Just (oid, effects)
   in Maybe.mapMaybe withAction (Game.zoneMembers Zone.Hand pid gs)

-- CR 103.5b: offer this player every action their hand grants "any time [they]
-- could mulligan", repeatedly, until they decline or none is left. Performing
-- one is NOT taking a mulligan -- nothing is shuffled or bottomed and `counts`
-- is untouched -- so the caller's declaration still follows (CR 103.5b's last
-- sentence). Nothing in CR 103.5b or on the card limits a player to one action,
-- and a hand that redraws into a second granting card may use it here too.
--
-- Terminates even against an interpreter that never declines: each action moves
-- at least one card out of the hand for the rest of the game, so the deck
-- strictly shrinks, and an empty library redraws nothing -- leaving a hand with
-- no candidate, which ends the loop.
mulliganWindow :: MulliganPerformer -> PlayerId -> Game ()
mulliganWindow perform pid = do
  candidates <- State.gets (actionsFor pid)
  case candidates of
    -- Where the rules leave nothing to ask, don't prompt.
    [] -> pure ()
    _ -> do
      decider <- State.gets (Decide.deciderFor pid)
      answer <- Trans.lift (Program.prompt (Prompt.MulliganAction decider pid (fmap fst candidates)))
      case answer of
        Nothing -> pure ()
        Just oid -> case lookup oid candidates of
          -- An id that was not offered: validated by MEMBERSHIP, the
          -- Action.Activate posture, which keeps this total with no partial
          -- lookup and no way for an interpreter to conjure an action.
          Nothing -> pure ()
          Just effects -> do
            perform oid pid effects
            mulliganWindow perform pid
```

- [ ] **Step 6: Thread the parameter through `Pawl.Mulligan`.** Change the two signatures and call the window. `openingHands`:

```haskell
openingHands :: MulliganPerformer -> [PlayerId] -> Game ()
openingHands perform owners = do
  Monad.forM_ owners (Monad.replicateM_ openingHand . Event.drawCard)
  mulliganRounds perform Map.empty owners
```

`mulliganRounds` — the signature gains the parameter, the declaration sub-pass opens the window **before** reading the hand size, and the recursive call passes it along:

```haskell
mulliganRounds :: MulliganPerformer -> Map.Map PlayerId Numeric.Natural.Natural -> [PlayerId] -> Game ()
mulliganRounds perform counts deciding = do
  decisions <- Monad.forM deciding $ \pid -> do
    -- CR 103.5b: the window comes FIRST -- the action is taken "at a time they
    -- would declare", and the declaration follows it. Reading the hand size
    -- after it is load-bearing: an action that empties the hand makes this a
    -- forced keep under CR 103.5's final sentence.
    mulliganWindow perform pid
    handSize <- State.gets (length . Game.zoneMembers Zone.Hand pid)
    if handSize <= 0
      then pure (pid, MulliganDecision.Keep)
      else do
        decider <- State.gets (Decide.deciderFor pid)
        let taken = Map.findWithDefault 0 pid counts
        decision <- Trans.lift (Program.prompt (Prompt.DeclareMulligan decider pid taken))
        pure (pid, decision)
  let mulliganers = fmap fst (filter (\(_, d) -> d == MulliganDecision.Mulligan) decisions)
  case mulliganers of
    [] -> pure ()
    _ -> do
      counts' <- Monad.foldM takeMulligan counts mulliganers
      mulliganRounds perform counts' mulliganers
```

Keep the existing comments on `mulliganRounds` and its `case`; only the marked lines change.

- [ ] **Step 7: Thread the parameter through `Pawl.Setup` and `Pawl.Engine`.** In `source/library/Pawl/Setup.hs`, add `import Pawl.Type.MulliganPerformer (MulliganPerformer)` and change three definitions (bodies otherwise unchanged):

```haskell
newGame :: MulliganPerformer -> NonEmpty.NonEmpty (PlayerId, Deck.Deck) -> Game ()
newGame perform matchup = do
  …
  Mulligan.openingHands perform (fmap fst (NonEmpty.toList matchup))

startGameFromCards :: MulliganPerformer -> Game ()
startGameFromCards perform = do
  …
  Mulligan.openingHands perform owners

restartGame :: MulliganPerformer -> PlayerId -> Game ()
restartGame perform starter = do
  …
  startGameFromCards perform
```

In `source/library/Pawl/Resolve.hs`, the `RestartGame` arm passes the one implementation:

```haskell
  Effect.RestartGame -> Setup.restartGame performMulliganAction controller
```

In `source/library/Pawl/Engine.hs`, two call sites (around lines 858 and 870):

```haskell
  (result, finalSub) <- Trans.lift (State.runStateT (Setup.startGameFromCards Resolve.performMulliganAction >> playGame) sub0)
```

```haskell
  Setup.newGame Resolve.performMulliganAction matchup
```

- [ ] **Step 8: Add `S.performer` and fix every test call site.** In `source/test-suite/Pawl/Support.hs`, add `import qualified Pawl.Resolve as Resolve` and `import qualified Pawl.Type.MulliganPerformer as MulliganPerformer`, then:

```haskell
-- The one CR 103.5b performer (Pawl.Resolve.performMulliganAction), so a test
-- that only wants a game set up does not have to reach into Pawl.Resolve for it.
performer :: MulliganPerformer.MulliganPerformer
performer = Resolve.performMulliganAction
```

Then pass `S.performer` at every call site of the four re-signatured functions:

Run: `grep -rn "Setup.newGame\|Setup.startGameFromCards\|Setup.restartGame\|Mulligan.openingHands" source/test-suite`
Expected: ~22 hits across `SetupSpec`, `GameSpec`, `CastSpec`, `MulliganSpec`. Each becomes `Setup.newGame S.performer matchup`, `Setup.startGameFromCards S.performer`, `Setup.restartGame S.performer S.alice`, `Mulligan.openingHands S.performer [S.alice, S.bob]` and so on. Comment-only mentions do not change.

- [ ] **Step 9: Run the tests to verify they pass.**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -20 && cabal test 2>&1 | tail -20`
Expected: warning-free build; both new cases pass and the whole existing suite still passes — the window is a no-op for every deck without a granting card.

- [ ] **Step 10: Format, lint, and commit.**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "feat(mulligan): offer CR 103.5b actions at the mulligan declaration (#182)"
```

---

## Task 6: CR 103.5b fidelity

The seven remaining behaviors from the spec's §6. Each is a separate assertion about a rule, so they land together as the rule's test suite.

**Files:**
- Test: `source/test-suite/Pawl/MulliganSpec.hs`

**Interfaces:**
- Consumes: everything from Tasks 1–5 (`S.performer`, `powderGame`, `recordWindow`, `usePowder`).
- Produces: nothing.

- [ ] **Step 1: Write the failing tests.** Add these helpers after `usePowder`:

```haskell
-- alice's library: `above` Mountains, then a Serum Powder, then 20 more; bob's
-- is uniform. With `above` = 7 the Powder is NOT in the opening hand and is
-- drawn only after a mulligan, which is how CR 103.5b's "This need not be in
-- the first round of mulligans" becomes observable.
powderUnder :: Printing.Printing -> Printing.Printing -> Int -> GameState.GameState
powderUnder powder mountain above =
  let g0 = Setup.emptyGame S.bothPlayers
      addMany p pid k g = List.foldl' (\h _ -> snd (S.addCreature p pid h)) g (replicate k ())
      withAlice = addMany mountain S.alice 20 (addMany powder S.alice 1 (addMany mountain S.alice above g0))
   in poolToLibrary S.bob (poolToLibrary S.alice (addMany mountain S.bob 20 withAlice))

-- alice's library: a Serum Powder, six Mountains, a SECOND Serum Powder, then
-- 20 Mountains. Her opening hand is the first Powder plus six Mountains; using
-- it exiles that hand and draws the second Powder, which is what makes CR
-- 103.5b's repeated action observable.
chainGame :: Printing.Printing -> Printing.Printing -> GameState.GameState
chainGame powder mountain =
  let g0 = Setup.emptyGame S.bothPlayers
      addMany p pid k g = List.foldl' (\h _ -> snd (S.addCreature p pid h)) g (replicate k ())
      withAlice =
        addMany mountain S.alice 20 (addMany powder S.alice 1 (addMany mountain S.alice 6 (addMany powder S.alice 1 g0)))
   in poolToLibrary S.bob (poolToLibrary S.alice (addMany mountain S.bob 20 withAlice))

-- alice's whole library is a Serum Powder and six Mountains -- exactly one
-- opening hand, nothing left to redraw.
shortPowderGame :: Printing.Printing -> Printing.Printing -> GameState.GameState
shortPowderGame powder mountain =
  let g0 = Setup.emptyGame S.bothPlayers
      addMany p pid k g = List.foldl' (\h _ -> snd (S.addCreature p pid h)) g (replicate k ())
      withAlice = addMany mountain S.alice 6 (addMany powder S.alice 1 g0)
   in poolToLibrary S.bob (poolToLibrary S.alice (addMany mountain S.bob 20 withAlice))

-- Takes the first offered action, and mulligans exactly once (alice only), so a
-- test can prove the action did NOT count toward CR 103.5's bottom count.
powderThenMulliganOnce :: Prompt.Prompt r -> r
powderThenMulliganOnce p = case p of
  Prompt.MulliganAction _ _ candidates -> Maybe.listToMaybe candidates
  Prompt.DeclareMulligan _ pid taken ->
    if pid == S.alice && taken < 1 then MulliganDecision.Mulligan else MulliganDecision.Keep
  _ -> S.identityAnswer p

-- Declines every window and mulligans once (alice only), recording how many
-- DECLARATIONS had already happened at each offer. An offer recorded after two
-- declarations is a second-round offer: round 1 is alice's declaration then
-- bob's.
recordWindowRound :: Prompt.Prompt r -> State.State (Int, [Int]) r
recordWindowRound p = case p of
  Prompt.MulliganAction {} -> do
    State.modify' (\(n, seen) -> (n, n : seen))
    pure Nothing
  Prompt.DeclareMulligan _ pid taken -> do
    State.modify' (\(n, seen) -> (n + 1, seen))
    pure (if pid == S.alice && taken < 1 then MulliganDecision.Mulligan else MulliganDecision.Keep)
  -- Bottoms the LAST card, not the first. S.identityAnswer bottoms the first,
  -- which here is the Powder the mulligan just drew -- it would land at the
  -- library bottom and round 2 would have nothing to offer, so the test would
  -- pass or fail on the fixture's draw order rather than on CR 103.5b.
  Prompt.Bottom _ _ hand count -> pure (reverse (take (fromIntegral count) (reverse hand)))
  _ -> pure (S.identityAnswer p)

-- alice keeps at once; bob mulligans twice then keeps. Every window is
-- declined, and each offer's player is recorded, so a test can prove a player
-- who has kept is never offered again.
recordWindowPlayers :: Prompt.Prompt r -> State.State [PlayerId] r
recordWindowPlayers p = case p of
  Prompt.MulliganAction _ pid _ -> do
    State.modify' (pid :)
    pure Nothing
  Prompt.DeclareMulligan _ pid taken ->
    pure (if pid == S.bob && taken < 2 then MulliganDecision.Mulligan else MulliganDecision.Keep)
  _ -> pure (S.identityAnswer p)
```

Then add these seven cases to the `tests` list:

```haskell
      HU.testCase "CR 103.5b: the action is not a mulligan -- it does not add to the bottom count" $ do
        -- alice takes the action and then mulligans ONCE. CR 103.5 bottoms a
        -- number equal to the mulligans she has taken, which is one -- so her
        -- opening hand is six. A five would mean the action had been counted.
        powder <- Registry.printing registry "Serum Powder"
        mountain <- Registry.printing registry "Mountain"
        let after = run powderThenMulliganOnce (powderGame powder mountain 20)
        HU.assertEqual "one mulligan bottoms exactly one card" 6 (S.handSize S.alice after),
      HU.testCase "CR 103.5b: the window is offered in a later round, not just the first" $ do
        -- The Powder sits under alice's opening seven, so round 1 offers her
        -- nothing; she mulligans, redraws into it, and round 2 offers it. The
        -- recorded value is how many declarations preceded the offer: 2 (hers
        -- and bob's, both in round 1).
        powder <- Registry.printing registry "Serum Powder"
        mountain <- Registry.printing registry "Mountain"
        let gs0 = powderUnder powder mountain 7
            ((_, _after), (_, offers)) = State.runState (Program.foldProgramM recordWindowRound (State.runStateT (Mulligan.openingHands S.performer [S.alice, S.bob]) gs0)) (0, [])
        HU.assertEqual "exactly one offer, and it came after both first-round declarations" [2] offers,
      HU.testCase "CR 103.5b: the action may be taken more than once in one window" $ do
        -- Using the first Powder draws the second; the loop offers again and
        -- ends only when the redrawn hand holds none. Fourteen cards exiled is
        -- two uses.
        powder <- Registry.printing registry "Serum Powder"
        mountain <- Registry.printing registry "Mountain"
        let after = run usePowder (chainGame powder mountain)
        HU.assertEqual "two hands exiled" 14 (length (Game.zoneMembers Zone.Exile S.alice after))
        HU.assertEqual "and the third hand is a full seven" 7 (S.handSize S.alice after),
      HU.testCase "CR 103.5b: a hand with no granting card is not asked" $ do
        -- Where the rules leave nothing to ask, don't prompt.
        mountain <- Registry.printing registry "Mountain"
        let gs0 = libraryGame mountain 20
            ((_, _after), offered) = State.runState (Program.foldProgramM recordWindow (State.runStateT (Mulligan.openingHands S.performer [S.alice, S.bob]) gs0)) []
        HU.assertEqual "no window prompt at all" [] offered,
      HU.testCase "CR 103.5b: a player who has kept is never offered the window again" $ do
        -- alice keeps in round 1 and leaves the pool; bob keeps the loop alive
        -- for two more rounds. She declares once, so she is offered once --
        -- CR 103.5b's window exists only "at a time they would declare".
        powder <- Registry.printing registry "Serum Powder"
        mountain <- Registry.printing registry "Mountain"
        let gs0 = powderGame powder mountain 20
            ((_, _after), offers) = State.runState (Program.foldProgramM recordWindowPlayers (State.runStateT (Mulligan.openingHands S.performer [S.alice, S.bob]) gs0)) []
        HU.assertEqual "offered in her one declaration round and never again" 1 (length (filter (== S.alice) offers)),
      HU.testCase "CR 103.5b: a game with a mulligan action replays deterministically" $ do
        powder <- Registry.printing registry "Serum Powder"
        mountain <- Registry.printing registry "Mountain"
        let gs0 = powderGame powder mountain 20
            ((_, recorded), responses) = Replay.record usePowder gs0 (Mulligan.openingHands S.performer [S.alice, S.bob])
            (_, replayed) = Replay.replay responses gs0 (Mulligan.openingHands S.performer [S.alice, S.bob])
        HU.assertEqual "alice hand matches" (S.handSize S.alice recorded) (S.handSize S.alice replayed)
        HU.assertEqual "alice exile matches" (Game.zoneMembers Zone.Exile S.alice recorded) (Game.zoneMembers Zone.Exile S.alice replayed)
        HU.assertEqual
          "alice library ORDER matches"
          (Game.zoneMembers Zone.Library S.alice recorded)
          (Game.zoneMembers Zone.Library S.alice replayed),
      HU.testCase "CR 727.3/729.3: a short deck still flags drewFromEmpty through the CR 103.5b action" $ do
        -- alice's whole library is one opening hand. Taking the action exiles
        -- all seven and redraws from an empty library, which flags the failed
        -- draw and leaves her with nothing -- a forced keep under CR 103.5's
        -- final sentence, so she is not asked to declare.
        powder <- Registry.printing registry "Serum Powder"
        mountain <- Registry.printing registry "Mountain"
        let after = run usePowder (shortPowderGame powder mountain)
        HU.assertEqual "her hand is empty" 0 (S.handSize S.alice after)
        HU.assertBool "and she drew from an empty library" (Set.member S.alice (GameState.drewFromEmpty after)),
```

- [ ] **Step 2: Run the tests to verify they fail.**

Run: `cabal test 2>&1 | tail -40`
Expected: compile errors first (the new helpers reference nothing new, so this should compile) and then FAILs only if the Task 5 implementation is wrong. **If every case passes on the first run, that is the expected outcome for tests 1, 4, 5, 6 and 7** — they pin behavior Task 5 already implements. Tests 2 (later round) and 3 (repeats) are the ones most likely to expose a bug; if any case fails, fix `Pawl.Mulligan`, never the assertion.

- [ ] **Step 3: Run the whole suite.**

Run: `cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -20 && cabal test 2>&1 | tail -20`
Expected: warning-free build; all cases pass.

- [ ] **Step 4: Format, lint, and commit.**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "test(mulligan): pin the seven CR 103.5b fidelity behaviors (#182)"
```

---

## Task 7: Close out

**Files:**
- Modify: `source/library/Pawl/Type/Card.hs` (the `(#N)` placeholder), `source/library/Pawl/Card.hs` (the lint citation)
- Modify: `docs/progress.md`, `CLAUDE.md`

- [ ] **Step 1: File the two deferral issues.**

```bash
gh issue create --title "A card declaring two CR 103.5b mulligan actions is unrepresentable" \
  --label gap --label expires:card-driven \
  --body "\`Card.mulliganAction\` is one action's effects, and \`Prompt.MulliganAction\` identifies an action by its source object alone, so a printing declaring two would put two different actions on the wire as one candidate. No printing declares two. A second one needs a discriminator alongside the source, the caveat #61 records for \`OrderTriggers\`. Spec: docs/superpowers/specs/2026-07-25-cr-103-5b-mulligan-actions-design.md section 3.1."

gh issue create --title "The CardSpec lint family does not range over Card.mulliganAction" \
  --label gap --label expires:card-driven \
  --body "\`Pawl.Card.allEffects\` is the spell's effects only -- it serves the D4 slot lint and the CR 612 text-change scan, both of which range over a spell's text. A slot named inside a \`Card.mulliganAction\` would therefore go unlinted. Nothing can name one today: \`Effect.ExileHandThenDraw\` is targetless and slotless. The lint should grow this with the first slotted mulligan action, not before. Spec: docs/superpowers/specs/2026-07-25-cr-103-5b-mulligan-actions-design.md section 3.1."
```

- [ ] **Step 2: Replace the `(#N)` placeholder** in `source/library/Pawl/Type/Card.hs` with the first issue's real number, and add a citation to `Pawl.Card.allEffects` for the second:

```haskell
-- Every effect across all of a card's modes, in printed (mode, then written)
-- order. CR 608.2c/700.2: the card's whole text spans its modes; the D4 lint
-- and the text-change scan (M3d) range over all of them regardless of what is
-- chosen.
--
-- Card.mulliganAction is deliberately NOT included: it is not part of the
-- spell, and CR 103.5b's action is performed from the hand rather than cast
-- (#N).
```

- [ ] **Step 3: Verify the whole build and suite once more, from clean.**

Run: `cabal clean && cabal build all --enable-tests --enable-benchmarks 2>&1 | tail -20 && cabal test 2>&1 | tail -20`
Expected: warning-free (a clean build is the only one that shows warnings from unchanged modules); the whole suite passes.

- [ ] **Step 4: Add the `docs/progress.md` entry.** Append after the M5.6 entry, matching the surrounding style (a bolded lead, what it establishes, what it added, what it deferred, and the spec/plan paths):

```markdown
- **CR 103.5b (mulligan-window actions) is implemented** (issue-driven gap
  closure #182, not a milestone phase; surfaced by the CR 103.5 spec's own
  section 3.2, which named Serum Powder as the card that would want a mulligan
  window). **Gate: Serum Powder.** **What it establishes:** the CR 103.5
  declaration pass now opens a window before each still-deciding player's
  declaration, looping until they decline — CR 103.5b's action may be taken in
  any round ("This need not be in the first round"), more than once, and by a
  player who has not yet kept. Performing one is **not** taking a mulligan:
  nothing is shuffled or bottomed and the count is untouched, so it feeds
  neither CR 103.5's bottom count nor CR 103.5c's free allowance, and the
  declaration still follows it. **The structural finding:** `Pawl.Resolve` sits
  ABOVE `Pawl.Mulligan` (`Effect.RestartGame` → `Setup.restartGame` →
  `startGameFromCards` → `openingHands`), and that cycle is a fact about the
  rules rather than the layout — an opcode restarts a game, a game start draws
  opening hands, and drawing opening hands performs opcodes. The effect
  performer is therefore a **parameter**, `Pawl.Type.MulliganPerformer`, threaded
  through the four setup entry points; the `resolveSpellWith runSubgame`
  precedent, with no default, because "no mulligan performer" is not a real
  state of the world. **Added:** `Effect.ExileHandThenDraw` (one fused opcode —
  "that many" is the hand size BEFORE the exile, so a following `Draw` would
  read an empty hand); `Card.mulliganAction :: [Effect Card]`, read straight off
  the card and never through the projection (the ability functions in the HAND,
  CR 113.6 — the `castingPermissions` precedent), with the omit-when-empty codec
  treatment so no committed card file changed; `Prompt.MulliganAction` +
  `Response.TookMulliganAction` + their `Replay` arms; `Mulligan.actionsFor` and
  `Mulligan.mulliganWindow`; `data/cards/serum-powder.json`; nine
  `Pawl.MulliganSpec` cases. **Deferred:** a printing declaring two CR 103.5b
  actions (#N, card-driven); the `CardSpec` lint family's coverage of the new
  field (#N, card-driven). CR 103.6's opening-hand actions (#149) are a
  different mechanism at a different time — after the whole mulligan process —
  and are untouched. Spec and plan:
  `docs/superpowers/specs/2026-07-25-cr-103-5b-mulligan-actions-design.md` and
  `docs/superpowers/plans/2026-07-25-cr-103-5b-mulligan-actions.md`.
```

Replace both `#N` placeholders with the real issue numbers from Step 1.

- [ ] **Step 5: Update the `CLAUDE.md` status bullet** — **replace**, never append (the milestone history lives in `progress.md`). Amend the first sentence of the status bullet to record that CR 103.5b landed after M5.6 and that M6 (#9) is still next; leave the rest of the bullet's M5.6 summary intact.

- [ ] **Step 6: Verify the plan is complete and close the issue.**

```bash
grep -c -- '- \[ \] \*\*Step' docs/superpowers/plans/2026-07-25-cr-103-5b-mulligan-actions.md
```
Expected: `0`.

```bash
gh issue close 182 --comment "CR 103.5b is implemented: the mulligan-declaration window offers every action a hand card grants, looping until declined, in every round and only to players who have not yet kept. Serum Powder is in data/cards. Deferrals filed as #N and #N."
```

- [ ] **Step 7: Format, lint, and commit.**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "docs(cr-103.5b): record the mulligan-window action gap closure (#182)"
```

---

## Self-Review Notes

**Spec coverage.** §3.1 carrier → Task 3; §3.2 opcode → Task 2; §3.3 cycle/performer → Task 5; §3.4 window → Task 5; §4 prompt → Task 1; §5 card data → Task 4; §6 testing → Tasks 2–6 (all nine cases); §7 blast radius → the File Structure section; §8 definition of done → Tasks 5–7; §9 deferrals → Task 7.

**One correction folded back into the spec.** §4 originally listed the `MulliganSpec` locals among the answerers gaining the new `Prompt` arm; they do not need one — every `MulliganSpec` answerer ends in `_ -> S.identityAnswer p`. The spec now names the 13 genuinely exhaustive answerers, and Task 1 Step 6 lists them file by file.

**One fixture trap, already handled.** `S.identityAnswer` answers `Prompt.Bottom` by bottoming the *first* `count` cards of the redrawn hand. In Task 6's "offered in a later round" case, that first card is the Serum Powder the mulligan just drew — bottoming it would leave round 2 with nothing to offer and the test would be measuring the fixture's draw order rather than CR 103.5b. `recordWindowRound` therefore carries its own `Prompt.Bottom` arm that bottoms the last card instead. Do not simplify it away.

**Ordering constraint.** Task 4 (the card) must follow Tasks 2 and 3 (the opcode and the field its JSON names). Task 5 must follow all of 1–4: its first test loads Serum Powder and answers a `MulliganAction` prompt.
