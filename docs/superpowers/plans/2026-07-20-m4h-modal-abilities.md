# M4h Modal Abilities Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Give `ActivatedAbility` and `TriggeredAbility` a `Modal card` payload and wire the mode choice into the activation path (CR 602.2b) and the trigger-placement path (CR 700.2b/603.3c/603.3d), reusing M4g's mode machinery.

**Architecture:** M4g built `Mode`/`Modal`/`ModeSelection`/`ModeIndex` Card-free and parametric so both ability types can adopt them with no cycle. The reshape collapses each type's flat `effects`/`targetSpecs` into one `modal :: Modal card` (retiring M4g's interim `[Effect card]` divergence, since `Mode.effects` is a `Seq`). The reshape is behavior-preserving — every existing ability becomes one forced mode — and is verified against the existing suite; the new *prompting* and *removal* behaviors land as separate TDD tasks. The gate is **Aether Channeler** (`{2}{U}` modal ETB trigger), composing three existing verbs (`Create`/`MoveToZone Hand`/`Draw`) under a modal wrapper, zero opcodes.

**Tech Stack:** Haskell 2010 (GHC 9.14.1, no extensions beyond `GADTs`/`RankNTypes`/`NamedFieldPuns`), `containers` (`Seq`/`Set`/`Map`), `tasty` (`tasty-hunit` + `tasty-quickcheck`). Cards are `data/cards/*.json` (hand-rolled codec, honesty round-trip).

## Global Constraints

- **Warning-clean build (`+pedantic` = `-Werror`).** `cabal build all --enable-tests --enable-benchmarks` must pass with zero warnings. Incremental builds hide warnings from unchanged modules; when in doubt `cabal clean` first. Remove now-unused imports after each reshape.
- **Two invariants outrank everything.** The rules core never cases on a card's identity, only classifications; the engine never makes a player's choice — elide a prompt only when forced (one legal mode / non-modal), and CR 603.3c *removes* rather than silently defaulting when no mode is legal.
- **Every rules claim carries its CR number** in a code comment, checked against `docs/rules.txt` (never recalled).
- **Code conventions:** no language extensions unless forced; no explicit export lists; one type per module under `Pawl.Type.<Name>`; qualified imports aliased to the last component, operators unqualified; no partial functions; `newtype` + `Mk`-prefixed non-punning constructors; `case` over point-free; `Text` not `String`; `Seq`/`Set` over lists for new collection fields; derive `Eq`+`Show`.
- **`hooky fix` then `hooky run` on staged files** before each commit (`git add -A` first, then `hooky fix`, then `git add -A` again, then `hooky run`).
- **Commit directly to `main`**, one small complete commit per task. End commit messages with the `Co-Authored-By`/`Claude-Session` trailers already configured for this repo.
- **Stage explicit paths**, not `git add -A` blindly, if foreign files appear in `git status` (concurrent sessions share the worktree).
- **Definitive-check commands:** full build `cabal build all --enable-tests --enable-benchmarks`; full suite `cabal test`; one test group `cabal test --test-options='-p "<pattern>"'`.

---

## File Structure

**Library (`source/library/Pawl/`):**
- `Type/TargetSpec.hs` — add `NonlandPermanentTarget` (Task 1).
- `Target.hs` — `NonlandPermanentTarget` legality arm, `selfExcludes`, `legalSetsExcluding`, `fillableModes` (Tasks 1–2).
- `Modal.hs` — **new** logic module: mode-scoped readers over any `Modal card` (Task 2).
- `Card.hs` — delegate its five mode readers to `Pawl.Modal` (Task 2).
- `Cast.hs` — route through `Target.fillableModes`/`legalSetsExcluding`; drop local `fillableModes` (Task 2).
- `Type/ActivatedAbility.hs`, `Type/TriggeredAbility.hs` — reshape to `{…, modal}` (Tasks 3a/3b).
- `Mana.hs`, `Activate.hs`, `Resolve.hs`, `Stack.hs`, `Engine.hs`, `Codec.hs` — migrate consumers (Tasks 3a/3b), add prompting/removal (Tasks 4–6).

**Cards (`data/cards/`):**
- Migrate `prodigal-sorcerer`, `llanowar-elves`, `evolving-wilds`, `mindslaver`, `drudge-skeletons` (Task 3a), `rest-in-peace` (Task 3b).
- New: `synthetic-modal-activated.json` (Task 4), `aether-channeler.json` (Task 5), `synthetic-modal-trigger.json` (Task 6).

**Tests (`source/test-suite/Pawl/`):**
- `ModalSpec.hs` — all M4h gameplay + reader + target-spec unit tests.
- `Cards.hs` — a `Printing` field + `loadPrinting` line + `allPrintings` entry per new card.
- `CodecSpec.hs`/`CardsSpec.hs` — round-trip auto-covers new cards via `allPrintings`.
- `ReplaySpec.hs` — a `ChoseModes`-for-a-trigger replay test (Task 5).

**Docs:** `docs/progress.md`, `docs/design.md`, `CLAUDE.md` (Task 7).

---

### Task 1: `NonlandPermanentTarget` target spec + self-exclusion

**Files:**
- Modify: `source/library/Pawl/Type/TargetSpec.hs`
- Modify: `source/library/Pawl/Target.hs`
- Modify: `source/library/Pawl/Codec.hs` (the `targetSpecToJson`/`jsonToTargetSpec` arms)
- Test: `source/test-suite/Pawl/ModalSpec.hs`

**Interfaces:**
- Produces: `TargetSpec.NonlandPermanentTarget`; `Target.selfExcludes :: TargetSpec -> Bool`; `Target.legalSetsExcluding :: ObjectId -> Map SlotName TargetSpec -> GameState -> Map SlotName (Set Recipient)`.

- [ ] **Step 1: Write the failing test**

Add to `source/test-suite/Pawl/ModalSpec.hs` inside its `tests` tree (a new `Tasty.testGroup "M4h NonlandPermanentTarget"`). Use `Pawl.Support` (`as S`) fixtures the way the existing groups do; build a battlefield with two nonland permanents (a creature and an artifact) plus a land, then assert legality and self-exclusion:

```haskell
, HU.testCase "NonlandPermanentTarget excludes lands (CR 305.1)" $ do
    -- srcCreatureId, artifactId, landId placed on the battlefield via S helpers
    let gs = S.boardWithCreatureArtifactLand   -- add this helper in Support (see Step 3b)
        got = Target.legalRecipients TargetSpec.NonlandPermanentTarget gs
    HU.assertEqual "two nonland permanents, no land"
      (Set.fromList [Recipient.ToObject (S.creatureId gs), Recipient.ToObject (S.artifactId gs)])
      got
, HU.testCase "legalSetsExcluding drops the source (CR \"another\")" $ do
    let gs = S.boardWithCreatureArtifactLand
        specs = Map.singleton (SlotName.MkSlotName (Text.pack "x")) TargetSpec.NonlandPermanentTarget
        got = Target.legalSetsExcluding (S.creatureId gs) specs gs
    HU.assertEqual "source excluded from its own set"
      (Map.singleton (SlotName.MkSlotName (Text.pack "x")) (Set.singleton (Recipient.ToObject (S.artifactId gs))))
      got
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "NonlandPermanentTarget"'`
Expected: FAIL to **compile** ("Data constructor not in scope: TargetSpec.NonlandPermanentTarget" / "Variable not in scope: Target.legalSetsExcluding").

- [ ] **Step 3a: Add the constructor and legality arm**

In `source/library/Pawl/Type/TargetSpec.hs`, add before the final `deriving`:

```haskell
  | -- CR 115 / 305.1: "target nonland permanent" -- a permanent on the battlefield
    -- whose PROJECTED card types (M3c) do not include Land. Defined SELF-EXCLUDING
    -- ("another target nonland permanent", Aether Channeler): the exclusion is
    -- applied by Target.legalSetsExcluding, not here (this arm is source-blind). A
    -- non-excluding variant splits the spec when a card needs it (the WallTarget
    -- specific-then-general posture).
    NonlandPermanentTarget
```

In `source/library/Pawl/Target.hs`, add to the `case spec of` in `legalRecipients`:

```haskell
        TargetSpec.NonlandPermanentTarget ->
          let notLand oid = not (Set.member CardType.Land (Projection.cardTypesOf oid gs))
              matches = filter notLand (Set.toList (GameState.battlefield gs))
           in Set.fromList (map Recipient.ToObject matches)
```

Then add, after `legalSets`:

```haskell
-- CR "another": specs that exclude the targeting source from their legal set.
-- Only NonlandPermanentTarget so far; a general per-slot "another" flag is future.
selfExcludes :: TargetSpec -> Bool
selfExcludes spec = case spec of
  TargetSpec.NonlandPermanentTarget -> True
  TargetSpec.AnyTarget -> False
  TargetSpec.CreatureTarget -> False
  TargetSpec.SpellOrPermanentTarget -> False
  TargetSpec.LandTarget -> False
  TargetSpec.PlayerTarget -> False
  TargetSpec.CreatureOrEnchantmentTarget -> False
  TargetSpec.SpellTarget -> False
  TargetSpec.WallTarget -> False

-- legalSets, then drop the source recipient from each self-excluding slot (CR
-- "another"). `source` is the object the targeting is relative to -- the spell
-- object at cast, the source permanent for an ability. A no-op for every non-self-
-- excluding spec (the source recipient is not removed), so Prodigal Sorcerer may
-- still target itself with AnyTarget (CR 115.4). stillLegal (re-validation) stays
-- source-blind: the chosen target is never the source, so it needs no exclusion.
legalSetsExcluding :: ObjectId -> Map SlotName TargetSpec -> GameState -> Map SlotName (Set Recipient)
legalSetsExcluding source specs gs =
  let drop1 spec s = if selfExcludes spec then Set.delete (Recipient.ToObject source) s else s
   in Map.mapWithKey (\slot spec -> drop1 spec (legalRecipients spec gs)) specs
```

`Target.hs` already imports `CardType`, `GameState`, `Recipient`, `Set`, `Map`, `SlotName`, `ObjectId`? It imports `CardType`, `GameState`, `Recipient`, `Set`, `Map`, `Projection`. Add `import Pawl.Type.ObjectId (ObjectId)` if absent (it is used by `legalSetsExcluding`).

- [ ] **Step 3b: Add the Support helper the test needs**

In `source/test-suite/Pawl/Support.hs`, add a fixture that places one creature, one artifact (use `Cards.darksteelMyrPrinting` — a `{3}` artifact creature; it is both, so instead use a genuinely non-creature artifact: `Cards.mindslaverPrinting`), and one land on the battlefield, plus accessors `creatureId`/`artifactId`. Follow the existing Support pattern for placing objects (`S.place`/`S.withBattlefield` — match whatever the file already exposes). Keep it minimal and named clearly. (If Support already has a general "place these printings on the battlefield" helper, use it and add only the accessors.)

- [ ] **Step 3c: Add the Codec arm**

In `source/library/Pawl/Codec.hs`, find `targetSpecToJson` and `jsonToTargetSpec` (they handle `WallTarget`); add the nullary tagged arm alongside it:

```haskell
-- in targetSpecToJson:
  TargetSpec.NonlandPermanentTarget -> Json.tagged (Text.pack "NonlandPermanentTarget") Nothing
-- in jsonToTargetSpec's case:
    ("NonlandPermanentTarget", Nothing) -> Right TargetSpec.NonlandPermanentTarget
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test --test-options='-p "NonlandPermanentTarget"'`
Expected: PASS (both cases).

- [ ] **Step 5: Build warning-clean and commit**

Run: `cabal build all --enable-tests --enable-benchmarks` → zero warnings.
Then `git add -A && hooky fix && git add -A && hooky run`.

```bash
git add -A
git commit -m "M4h: NonlandPermanentTarget + self-exclusion (CR 'another')"
```

---

### Task 2: `Pawl.Modal` reader + `Target.fillableModes` generalization + `Cast` routing

**Files:**
- Create: `source/library/Pawl/Modal.hs`
- Modify: `source/library/Pawl/Card.hs` (delegate its five readers)
- Modify: `source/library/Pawl/Target.hs` (add `fillableModes`)
- Modify: `source/library/Pawl/Cast.hs` (drop local `fillableModes`; route through `Target`)
- Modify: `pawl.cabal` is auto (cabal-gild discover) — run `hooky fix`.
- Test: `source/test-suite/Pawl/ModalSpec.hs`

**Interfaces:**
- Consumes: `TargetSpec.NonlandPermanentTarget`, `Target.legalSetsExcluding` (Task 1).
- Produces: `Modal.allEffects`/`allTargetSpecs`/`modesEffects`/`modesTargetSpecs`/`selectionCount :: … Modal card → …`; `Target.fillableModes :: ObjectId -> Modal.Modal Card -> GameState -> Set ModeIndex`.

- [ ] **Step 1: Write the failing test**

Add to `ModalSpec.hs` a `Tasty.testGroup "M4h Modal reader"`:

```haskell
, HU.testCase "modesEffects reads only chosen modes, ModeIndex order" $ do
    let m = Modal.MkModal
              (Seq.fromList
                [ Mode.MkMode (Seq.singleton (Effect.Draw (Quantity.Literal 1))) Map.empty
                , Mode.MkMode (Seq.singleton Effect.RegenerateSelf) Map.empty ])
              (ModeSelection.ChooseExactly 1)
        chosen = Set.singleton (ModeIndex.MkModeIndex 1)
    HU.assertEqual "only mode 1's effect" [Effect.RegenerateSelf] (Modal.modesEffects chosen m)
    HU.assertEqual "selectionCount is the ChooseExactly count" 1 (Modal.selectionCount m)
```

(Adjust the qualified names to whatever `ModalSpec.hs` already imports; add `import qualified Pawl.Modal as Modal`, `Pawl.Type.Modal as Modal.Type`? — keep the *type* as `Modal.Type.MkModal` only if the logic alias collides. Simplest: `import qualified Pawl.Type.Modal as ModalT` for the constructor and `import qualified Pawl.Modal as Modal` for the functions.)

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "Modal reader"'`
Expected: FAIL to compile ("Not in scope: Modal.modesEffects").

- [ ] **Step 3a: Create `Pawl.Modal`**

`source/library/Pawl/Modal.hs`. Lift the folds M4g put on `Pawl.Card` (they are card-agnostic over a `Modal card`):

```haskell
-- Mode-scoped structural reads over a Modal payload (CR 700.2), shared by the
-- spell (Card.spell) and both ability types. Card-free/parametric in `card` (M4c):
-- imports only Type modules, so no cycle -- Pawl.Card imports THIS.
module Pawl.Modal where

import qualified Data.Foldable as Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Numeric.Natural (Natural)
import Pawl.Type.Effect (Effect)
import qualified Pawl.Type.Modal as Modal
import qualified Pawl.Type.Mode as Mode
import qualified Pawl.Type.ModeIndex as ModeIndex
import qualified Pawl.Type.ModeSelection as ModeSelection
import Pawl.Type.SlotName (SlotName)
import Pawl.Type.TargetSpec (TargetSpec)

-- Every effect across all modes, printed (mode, then written) order (CR 608.2c).
allEffects :: Modal.Modal card -> [Effect card]
allEffects m = concatMap (Foldable.toList . Mode.effects) (Foldable.toList (Modal.modes m))

-- The union of every mode's target specs (slot names unique by authoring
-- discipline; the D4 lint enforces per-mode resolution).
allTargetSpecs :: Modal.Modal card -> Map SlotName TargetSpec
allTargetSpecs m = Map.unions (map Mode.targetSpecs (Foldable.toList (Modal.modes m)))

-- CR 608.2c/700.2: only the CHOSEN modes' effects, in ModeIndex order (the Set is
-- already sorted). Out-of-range indices contribute nothing (total via Seq.lookup).
modesEffects :: Set ModeIndex.ModeIndex -> Modal.Modal card -> [Effect card]
modesEffects chosen m =
  let modeAt (ModeIndex.MkModeIndex n) =
        maybe [] (Foldable.toList . Mode.effects) (Seq.lookup (fromIntegral n) (Modal.modes m))
   in concatMap modeAt (Set.toAscList chosen)

-- CR 601.2c/700.2c: only the CHOSEN modes' target specs (union).
modesTargetSpecs :: Set ModeIndex.ModeIndex -> Modal.Modal card -> Map SlotName TargetSpec
modesTargetSpecs chosen m =
  let specsAt (ModeIndex.MkModeIndex n) =
        maybe Map.empty Mode.targetSpecs (Seq.lookup (fromIntegral n) (Modal.modes m))
   in Map.unions (map specsAt (Set.toAscList chosen))

-- CR 700.2: how many modes the selection demands (the ChooseExactly count).
selectionCount :: Modal.Modal card -> Natural
selectionCount m = case Modal.selection m of
  ModeSelection.ChooseExactly n -> n
```

- [ ] **Step 3b: Delegate `Pawl.Card`'s readers**

In `source/library/Pawl/Card.hs`, replace the bodies of `allEffects`/`allTargetSpecs`/`modesEffects`/`modesTargetSpecs`/`modeTargetSpecs` with delegations to `Pawl.Modal` over `Card.spell` (keep their signatures and comments). Example:

```haskell
allEffects :: Card.Card -> [Effect Card.Card]
allEffects card = Modal.allEffects (Card.spell card)

modesEffects :: Set.Set ModeIndex.ModeIndex -> Card.Card -> [Effect Card.Card]
modesEffects chosen card = Modal.modesEffects chosen (Card.spell card)
```

Add `import qualified Pawl.Modal as Modal`; remove any now-unused imports (`Foldable`, `Seq`) the deleted bodies used.

- [ ] **Step 3c: Add `Target.fillableModes`**

In `source/library/Pawl/Target.hs`, generalize what `Cast.fillableModes` did (now source-aware for self-exclusion):

```haskell
-- CR 700.2a: the mode indices all of whose target slots have a legal recipient
-- (a mode with no slots is trivially fillable). Self-exclusion ("another") is
-- honored via legalSetsExcluding, so a mode whose only nonland-permanent target
-- is the source itself is NOT fillable. Shared by spells (Cast) and abilities
-- (Activate/Engine).
fillableModes :: ObjectId -> Modal.Modal Card -> GameState -> Set ModeIndex
fillableModes source modal gs =
  let ms = Foldable.toList (Modal.modes modal)
      fillable i m =
        let sets = legalSetsExcluding source (Mode.targetSpecs m) gs
         in if any Set.null (Map.elems sets)
              then Nothing
              else Just (ModeIndex.MkModeIndex (fromIntegral i))
   in Set.fromList (Maybe.mapMaybe (uncurry fillable) (zip [0 :: Int ..] ms))
```

Add imports to `Target.hs`: `qualified Data.Foldable as Foldable`, `qualified Data.Maybe as Maybe`, `Pawl.Type.Card (Card)`, `qualified Pawl.Type.Modal as Modal`, `qualified Pawl.Type.Mode as Mode`, `Pawl.Type.ModeIndex (ModeIndex)`, `qualified Pawl.Type.ModeIndex as ModeIndex`.

- [ ] **Step 3d: Route `Cast` through `Target`**

In `source/library/Pawl/Cast.hs`:
- Delete the local `fillableModes` function.
- In `targetable`, replace `Set.size (fillableModes oid gs)` with `Set.size (Target.fillableModes oid (Card.Type.spell card) gs)` and `ModeSelection.ChooseExactly count = …` with `count = Modal.selectionCount (Card.Type.spell card)` (via `import qualified Pawl.Modal as Modal`).
- In `castSpell`, replace `legal = fillableModes oid gs` with `legal = Target.fillableModes oid (Card.Type.spell card) gs`; replace the two `Target.legalSets …` target computations with `Target.legalSetsExcluding oid …` (the spell object id as source — a no-op for every current spell, none self-excluding); replace the `ModeSelection.ChooseExactly count = …` binding with `count = Modal.selectionCount (Card.Type.spell card)`.
- In `castableWhileSearching`, `targetable oid gs` already calls the updated `Target.fillableModes`; no direct `legalSets` call there to change (verify — if `targetable` is the only path, nothing else changes).
- Remove now-unused imports (`Modal.Mode`, `ModeIndex`, `ModeSelection`, `Foldable`, `Maybe`) that only the deleted `fillableModes` used; keep what `castSpell` still needs.

- [ ] **Step 4: Run the reader test and the full suite**

Run: `cabal test --test-options='-p "Modal reader"'` → PASS.
Run: `cabal test` → the **entire existing suite stays green** (this refactor is behavior-preserving: `Cast` routes through the same logic, spells never self-exclude).

- [ ] **Step 5: Build warning-clean and commit**

Run: `cabal build all --enable-tests --enable-benchmarks` → zero warnings (run `cabal-gild` via `hooky fix` so `Pawl.Modal` joins `exposed-modules`).

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4h: Pawl.Modal reader + Target.fillableModes; Cast routes through them"
```

---

### Task 3a: Reshape `ActivatedAbility` to a `Modal` payload (behavior-preserving)

**Files:**
- Modify: `source/library/Pawl/Type/ActivatedAbility.hs`
- Modify: `source/library/Pawl/Mana.hs`, `source/library/Pawl/Activate.hs`, `source/library/Pawl/Resolve.hs`, `source/library/Pawl/Stack.hs`, `source/library/Pawl/Codec.hs`
- Modify: `data/cards/{prodigal-sorcerer,llanowar-elves,evolving-wilds,mindslaver,drudge-skeletons}.json`
- Test: the existing suite (regression net; no new test — this is a behavior-preserving reshape, the M4g `Card`-reshape pattern).

**Interfaces:**
- Consumes: `Pawl.Modal` readers (Task 2), `Target.fillableModes`/`legalSetsExcluding` (Tasks 1–2).
- Produces: `ActivatedAbility.MkActivatedAbility { cost :: AbilityCost, modal :: Modal card }`.

- [ ] **Step 1: Reshape the type**

`source/library/Pawl/Type/ActivatedAbility.hs`:

```haskell
module Pawl.Type.ActivatedAbility where

import Pawl.Type.AbilityCost (AbilityCost)
import Pawl.Type.Modal (Modal)

-- CR 602.1 / 700.2 / 602.2b: "[cost]: [effect]", now modal-capable. VALUE-typed:
-- Action.Activate carries the value and validates by membership
-- (Projection.abilitiesOf), never an index. Parametric in `card` (M4c): a concrete
-- Modal Card would drag Card in and cycle; Card ties the knot at Modal Card. The
-- effects now live in Mode.effects :: Seq (Effect card) -- M4g's interim [Effect]
-- divergence, retired.
data ActivatedAbility card = MkActivatedAbility
  { cost :: AbilityCost,
    modal :: Modal card
  }
  deriving (Eq, Ord, Show)
```

- [ ] **Step 2: Run the build to see every break**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — `ActivatedAbility.effects`/`targetSpecs` gone; errors in `Mana`, `Activate`, `Resolve`, `Stack`, `Codec`.

- [ ] **Step 3a: `Mana.hs`**

Replace `ActivatedAbility.effects ab` with `Modal.allEffects (ActivatedAbility.modal ab)` and `ActivatedAbility.targetSpecs ab` with `Modal.allTargetSpecs (ActivatedAbility.modal ab)` (add `import qualified Pawl.Modal as Modal`):

```haskell
-- manaTypesOf's mapMaybe source:
          (Maybe.mapMaybe Resolve.manaProduced . Modal.allEffects . ActivatedAbility.modal)
-- isManaAbility:
  not (null (Maybe.mapMaybe Resolve.manaProduced (Modal.allEffects (ActivatedAbility.modal ab))))
    && Map.null (Modal.allTargetSpecs (ActivatedAbility.modal ab))
```

- [ ] **Step 3b: `Resolve.hs` — `resolveAbility` reads chosen modes**

`resolveEffects` keeps its signature `ObjectId -> ObjectId -> [Effect Card] -> Map SlotName TargetSpec -> Game ()`. Rewrite `resolveAbility` to compute mode-scoped effects/specs from the object's chosen modes:

```haskell
resolveAbility :: ObjectId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Type.Card -> Game ()
resolveAbility abilId srcId ability = do
  gs <- State.get
  case Game.lookupObject abilId gs of
    Nothing -> pure ()
    Just obj ->
      let chosen = Binding.modesOf (Object.bindings obj)
          modal = ActivatedAbility.modal ability
       in resolveEffects abilId srcId (Modal.modesEffects chosen modal) (Modal.modesTargetSpecs chosen modal)
```

Add `import qualified Pawl.Modal as Modal`.

- [ ] **Step 3c: `Stack.hs` — `OfAbility` arm's search scan**

Replace `any Resolve.searchesLibrary (ActivatedAbility.effects ability)` with the chosen-modes effects (Evolving Wilds is single-mode, so `chosen = {0}` and behavior is unchanged):

```haskell
        Source.OfAbility srcId ability -> do
          let chosen = Binding.modesOf (Object.bindings obj)
          Monad.when (any Resolve.searchesLibrary (Modal.modesEffects chosen (ActivatedAbility.modal ability))) $
            Cast.castWhileSearching (Object.owner obj)
          Resolve.resolveAbility oid srcId ability
```

Add `import qualified Pawl.Binding as Binding` and `import qualified Pawl.Modal as Modal` to `Stack.hs`.

- [ ] **Step 3d: `Activate.hs` — mode-aware `activatable`, forced-mode stamping**

In `activatable`, replace the target line
`&& not (any Set.null (Map.elems (Target.legalSets (ActivatedAbility.targetSpecs ability) gs)))`
with mode-aware fillability (CR 700.2a; single-mode = today):

```haskell
    && Set.size (Target.fillableModes srcId (ActivatedAbility.modal ability) gs)
         >= fromIntegral (Modal.selectionCount (ActivatedAbility.modal ability))
```

In `activateAbility`, after putting the object on the stack, stamp the **forced** modes (no prompt yet — Task 4 adds it) and compute mode-scoped targets via the excluding helper:

```haskell
      decider = Decide.deciderFor pid gs
      chosenModes = Target.fillableModes srcId (ActivatedAbility.modal ability) gs
      sets = Target.legalSetsExcluding srcId (Modal.modesTargetSpecs chosenModes (ActivatedAbility.modal ability)) gs
  State.put onStack
  chosen <-
    if Map.null sets then pure Map.empty
    else Trans.lift (Program.prompt (Prompt.ChooseTargets decider pid abilId sets))
  let keysAgree = Map.keysSet chosen == Map.keysSet sets
      eachLegal = and (Map.intersectionWith Set.member chosen sets)
  if not (keysAgree && eachLegal)
    then State.put gs
    else do
      State.modify' (\g -> g {GameState.objects = Map.adjust (\o -> o {Object.bindings = Binding.fromChoices chosen Map.empty Nothing chosenModes}) abilId (GameState.objects g)})
      … (unchanged cost payment) …
```

`chosenModes` for every existing single-mode ability is `{MkModeIndex 0}`, forced, unprompted — behavior identical. Add `import qualified Pawl.Modal as Modal` to `Activate.hs`; the old `decider`/`sets` bindings are replaced in place.

- [ ] **Step 3e: `Codec.hs` — reshape the activated arm**

```haskell
activatedAbilityToJson aa =
  Object
    [ (Text.pack "cost", abilityCostToJson (ActivatedAbility.cost aa)),
      (Text.pack "modal", modalToJson (ActivatedAbility.modal aa))
    ]

jsonToActivatedAbility value = do
  ps <- Json.asObject value
  c <- Json.field (Text.pack "cost") ps >>= jsonToAbilityCost
  m <- Json.field (Text.pack "modal") ps >>= jsonToModal
  pure (ActivatedAbility.MkActivatedAbility c m)
```

Remove imports of `effectToJson`/`targetSpecsToJson` **only if** nothing else uses them (they are used by `modeToJson` and elsewhere — leave them).

- [ ] **Step 3f: Migrate the five activated JSON files**

For each of `prodigal-sorcerer`, `llanowar-elves`, `evolving-wilds`, `mindslaver`, `drudge-skeletons`, re-nest the single ability's `effects`/`targetSpecs` under a `modal` object with one mode. Example — `prodigal-sorcerer.json`'s `activatedAbilities` becomes:

```json
"activatedAbilities":[{"cost":{"mana":null,"additional":[{"type":"TapSelf"}]},"modal":{"modes":[{"effects":[{"type":"DealDamage","value":["target",{"type":"Literal","value":1}]}],"targetSpecs":[{"slot":"target","spec":{"type":"AnyTarget"}}]}],"selection":{"type":"ChooseExactly","value":1}}}]
```

Apply the identical transform to the other four (each has exactly one activated ability): wrap its `effects`+`targetSpecs` in `{"modal":{"modes":[{"effects":…,"targetSpecs":…}],"selection":{"type":"ChooseExactly","value":1}}}`, dropping the old flat `effects`/`targetSpecs` keys, keeping `cost` unchanged.

- [ ] **Step 4: Build and run the full suite**

Run: `cabal build all --enable-tests --enable-benchmarks` → zero warnings.
Run: `cabal test` → green. The `allPrintings` honesty round-trip (CodecSpec) re-parses/re-renders the five migrated files byte-stable; `loadCards` fails loudly in IO if any file is mis-nested. Prodigal Sorcerer still pings, Llanowar Elves is still a mana ability (off the stack), Evolving Wilds still offers cast-while-searching, Mindslaver/Drudge Skeletons unchanged.

- [ ] **Step 5: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4h: reshape ActivatedAbility to a Modal payload (behavior-preserving)"
```

---

### Task 3b: Reshape `TriggeredAbility` to a `Modal` payload (behavior-preserving)

**Files:**
- Modify: `source/library/Pawl/Type/TriggeredAbility.hs`
- Modify: `source/library/Pawl/Stack.hs` (`OfTrigger` arm), `source/library/Pawl/Engine.hs` (`placeOne`), `source/library/Pawl/Codec.hs`
- Modify: `data/cards/rest-in-peace.json`
- Test: the existing suite (regression net).

**Interfaces:**
- Consumes: `Pawl.Modal` readers, `Target.fillableModes`.
- Produces: `TriggeredAbility.MkTriggeredAbility { condition :: TriggerCondition, modal :: Modal card }`.

- [ ] **Step 1: Reshape the type**

`source/library/Pawl/Type/TriggeredAbility.hs`:

```haskell
module Pawl.Type.TriggeredAbility where

import Pawl.Type.Modal (Modal)
import Pawl.Type.TriggerCondition (TriggerCondition)

-- CR 603.1 / 700.2b / 603.3c: "[condition], [effect]", now modal-capable. Card-free/
-- parametric (M4c). On the stack it shares Resolve's executor with an activated
-- ability. Effects live in Mode.effects :: Seq (M4g's interim [Effect] retired).
data TriggeredAbility card = MkTriggeredAbility
  { condition :: TriggerCondition,
    modal :: Modal card
  }
  deriving (Eq, Ord, Show)
```

`Event.triggersFrom`'s type `[(ObjectId, PlayerId, TriggeredAbility Card)]` is unchanged (the reshape is internal).

- [ ] **Step 2: Run the build to see the breaks**

Run: `cabal build all --enable-tests --enable-benchmarks`
Expected: FAIL — errors in `Stack` (`OfTrigger` arm), `Engine.placeOne`, `Codec`.

- [ ] **Step 3a: `Stack.hs` — `OfTrigger` arm reads chosen modes**

```haskell
        Source.OfTrigger srcId ability ->
          let chosen = Binding.modesOf (Object.bindings obj)
              modal = TriggeredAbility.modal ability
           in Resolve.resolveEffects oid srcId (Modal.modesEffects chosen modal) (Modal.modesTargetSpecs chosen modal)
```

(`Binding`/`Modal` imports were added to `Stack.hs` in Task 3a.)

- [ ] **Step 3b: `Engine.placeOne` — stamp the forced mode**

`placeOne` must stamp the forced chosen modes so `resolveEffects` reads them (an empty `bindings` would make `modesOf` empty → no effects). Rest in Peace is a single empty-target mode, so this is `{MkModeIndex 0}`, forced, no target prompt:

```haskell
placeOne :: (ObjectId, PlayerId, TriggeredAbility.TriggeredAbility Card.Card) -> Game ()
placeOne (srcId, controller, ability) = do
  gs <- State.get
  let (abilId, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      chosenModes = Target.fillableModes srcId (TriggeredAbility.modal ability) gs
      obj =
        Object.MkObject
          { Object.owner = controller,
            Object.source = Source.OfTrigger srcId ability,
            Object.zone = Zone.Stack,
            Object.tapped = TapState.Untapped,
            Object.damage = 0,
            Object.sickness = Sickness.Settled,
            Object.bindings = Binding.fromChoices Map.empty Map.empty Nothing chosenModes,
            Object.counters = Map.empty,
            Object.timestamp = ts
          }
  State.put gs2 {GameState.objects = Map.insert abilId obj (GameState.objects gs2), GameState.stack = abilId : GameState.stack gs2}
```

Add to `Engine.hs`: `import qualified Pawl.Binding as Binding`, `import qualified Pawl.Target as Target`, `import qualified Pawl.Type.TriggeredAbility as TriggeredAbility` (verify which are already present). Note: for Rest in Peace, `fillableModes` on its single empty-slot mode returns `{MkModeIndex 0}` (trivially fillable), so `chosenModes = {0}` and its ETB effects resolve exactly as before.

- [ ] **Step 3c: `Codec.hs` — reshape the triggered arm**

```haskell
triggeredAbilityToJson ta =
  Object
    [ (Text.pack "condition", triggerConditionToJson (TriggeredAbility.condition ta)),
      (Text.pack "modal", modalToJson (TriggeredAbility.modal ta))
    ]

jsonToTriggeredAbility value = do
  ps <- Json.asObject value
  c <- Json.field (Text.pack "condition") ps >>= jsonToTriggerCondition
  m <- Json.field (Text.pack "modal") ps >>= jsonToModal
  pure (TriggeredAbility.MkTriggeredAbility c m)
```

- [ ] **Step 3d: Migrate `rest-in-peace.json`**

Re-nest its one triggered ability's `effects`/`targetSpecs` under a one-mode `modal`:

```json
"triggeredAbilities":[{"condition":{"type":"SelfEnters"},"modal":{"modes":[{"effects":[{"type":"ExileAllGraveyards"}],"targetSpecs":[]}],"selection":{"type":"ChooseExactly","value":1}}}]
```

(Copy the exact `effects` array from the current file — the shape above is Rest in Peace's; confirm against the file before editing.)

- [ ] **Step 4: Build and run the full suite**

Run: `cabal build all --enable-tests --enable-benchmarks` → zero warnings.
Run: `cabal test` → green. Rest in Peace still exiles all graveyards on ETB and still redirects graveyard→exile; the round-trip re-renders `rest-in-peace.json` byte-stable.

- [ ] **Step 5: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4h: reshape TriggeredAbility to a Modal payload (behavior-preserving)"
```

---

### Task 4: Activation-path mode prompt (CR 602.2b) + synthetic modal activated fixture

**Files:**
- Modify: `source/library/Pawl/Activate.hs` (add the `ChooseModes` prompt branch)
- Create: `data/cards/synthetic-modal-activated.json`
- Modify: `source/test-suite/Pawl/Cards.hs` (field + load + `allPrintings`)
- Test: `source/test-suite/Pawl/ModalSpec.hs`

**Interfaces:**
- Consumes: `Target.fillableModes`, `Modal.selectionCount`/`modesTargetSpecs`, `Prompt.ChooseModes`, `Binding.fromChoices`.
- Produces: `Cards.syntheticModalActivatedPrinting :: Cards -> Printing`.

- [ ] **Step 1: Add the fixture card**

`data/cards/synthetic-modal-activated.json` — a `{}` -costed 2/2 creature whose activated ability (`{1}`, no `{T}`, so no sickness gate to fight in the test) is `ChooseExactly 1` over two `CreatureTarget` modes (`DealDamage 1` / put a `+1/+1` counter). Name it clearly as a fixture:

```json
{"name":"Synthetic Modal Activator","manaCost":[{"type":"Generic","value":2}],"typeLine":{"supertypes":[],"types":[{"type":"Creature"}],"subtypes":[]},"power":{"type":"Literal","value":2},"toughness":{"type":"Literal","value":2},"keywords":[],"staticAbilities":[],"spell":{"modes":[{"effects":[],"targetSpecs":[]}],"selection":{"type":"ChooseExactly","value":1}},"activatedAbilities":[{"cost":{"mana":[{"type":"Generic","value":1}],"additional":[]},"modal":{"modes":[{"effects":[{"type":"DealDamage","value":["creature",{"type":"Literal","value":1}]}],"targetSpecs":[{"slot":"creature","spec":{"type":"CreatureTarget"}}]},{"effects":[{"type":"PutCounters","value":[{"type":"PlusOnePlusOne"},{"type":"Literal","value":1},"creature"]}],"targetSpecs":[{"slot":"creature","spec":{"type":"CreatureTarget"}}]}],"selection":{"type":"ChooseExactly","value":1}}}],"replacementEffects":[],"triggeredAbilities":[],"castingPermissions":[]}
```

(Confirm the exact `PutCounters` JSON shape against `data/cards/battlegrowth.json` before writing — copy its tag order.)

- [ ] **Step 2: Register it in the test pool**

In `source/test-suite/Pawl/Cards.hs`: add `syntheticModalActivatedPrinting :: Printing.Printing` to the `Cards` record, a `syntheticModalActivatedPrinting_ <- loadPrinting "synthetic-modal-activated"` line in `loadCards` (and the field in the returned record), and `syntheticModalActivatedPrinting cards` to `allPrintings`.

- [ ] **Step 3: Write the failing test**

In `ModalSpec.hs`, a `Tasty.testGroup "M4h activation modal (CR 602.2b)"` with a gameplay test: put the Synthetic Modal Activator and a second target creature on the battlefield under a player, give the player `{1}`, activate the ability answering `ChooseModes` with mode 0, then a creature target; assert the target took 1 damage and **no** +1/+1 counter (only the chosen mode resolved). Model the prompt-answering on the existing `ModalSpec` cast tests (they already answer `ChooseModes`/`ChooseTargets` through the `Program` interpreter / `S` harness):

```haskell
, HU.testCase "activating a modal ability prompts the mode, only that mode resolves" $ do
    -- build gs: activator + victim creature on battlefield, activator's controller has {1}
    -- run Activate.activateAbility answering ChooseModes {0} then ChooseTargets victim
    -- assert: victim.damage == 1 and PlusOnePlusOne counter absent
    …
, HU.testCase "chosen-mode fizzle (CR 608.2b) when the target leaves" $ do
    -- activate mode 0 at the victim, remove the victim before resolveTop, assert no effect
    …
```

Fill these in with the concrete `S`/`Program` harness the file already uses for the Chaos Charm activation-adjacent tests (match `ActivateSpec.hs`'s prompt-answering helper for activation).

- [ ] **Step 4: Run to verify it fails**

Run: `cabal test --test-options='-p "activation modal"'`
Expected: FAIL — with the current forced-only `activateAbility` (Task 3a), a two-legal-mode ability is not prompted; `Target.fillableModes` returns `{0,1}`, `count = 1`, and the forced path takes `chosenModes = {0,1}` (size 2 ≠ 1), so stamping/targeting is wrong and the assertion fails (or the `ChooseModes` answer is never consumed).

- [ ] **Step 5: Add the prompt branch**

In `Activate.activateAbility`, replace the forced `chosenModes = Target.fillableModes …` with a prompt-or-forced choice mirroring `Cast.castSpell`, and validate:

```haskell
  let decider = Decide.deciderFor pid gs
      legal = Target.fillableModes srcId (ActivatedAbility.modal ability) gs
      count = Modal.selectionCount (ActivatedAbility.modal ability)
  State.put onStack
  chosenModes <-
    if Set.size legal <= fromIntegral count
      then pure legal
      else Trans.lift (Program.prompt (Prompt.ChooseModes decider pid abilId legal count))
  if not (Set.isSubsetOf chosenModes legal && Set.size chosenModes == fromIntegral count)
    then State.put gs -- reject-not-repair: the whole activation is a no-op
    else do
      let sets = Target.legalSetsExcluding srcId (Modal.modesTargetSpecs chosenModes (ActivatedAbility.modal ability)) gs
      chosen <-
        if Map.null sets then pure Map.empty
        else Trans.lift (Program.prompt (Prompt.ChooseTargets decider pid abilId sets))
      let keysAgree = Map.keysSet chosen == Map.keysSet sets
          eachLegal = and (Map.intersectionWith Set.member chosen sets)
      if not (keysAgree && eachLegal)
        then State.put gs
        else do
          State.modify' (\g -> g {GameState.objects = Map.adjust (\o -> o {Object.bindings = Binding.fromChoices chosen Map.empty Nothing chosenModes}) abilId (GameState.objects g)})
          … (unchanged cost payment) …
```

For every existing single-mode ability, `Set.size legal (1) <= count (1)` → forced, unprompted — behavior unchanged (rerun full suite in Step 6).

- [ ] **Step 6: Run the test and full suite**

Run: `cabal test --test-options='-p "activation modal"'` → PASS.
Run: `cabal test` → green (single-mode abilities still unprompted).

- [ ] **Step 7: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4h: mode choice in the activation path (CR 602.2b) + fixture"
```

---

### Task 5: Trigger-placement mode prompt + targeting (CR 700.2b/603.3d) — Aether Channeler gate

**Files:**
- Modify: `source/library/Pawl/Engine.hs` (`placeOne`: prompt + targeting-at-placement)
- Create: `data/cards/aether-channeler.json` (with the embedded Bird token)
- Modify: `source/test-suite/Pawl/Cards.hs` (field + load + `allPrintings`)
- Test: `source/test-suite/Pawl/ModalSpec.hs`, `source/test-suite/Pawl/ReplaySpec.hs`

**Interfaces:**
- Consumes: `Target.fillableModes`/`legalSetsExcluding`, `Modal.selectionCount`/`modesTargetSpecs`, `Prompt.ChooseModes`/`ChooseTargets`, `Binding.fromChoices`.
- Produces: `Cards.aetherChannelerPrinting :: Cards -> Printing`.

- [ ] **Step 1: Add the gate card**

`data/cards/aether-channeler.json` — `{2}{U}` `Creature — Human Wizard` 3/3, no `spell` effects, one ETB `TriggeredAbility` (`SelfEnters`) with a three-mode `ChooseExactly 1` `Modal`: mode 0 `Create (Literal 1) <bird>` (no targets), mode 1 `MoveToZone "permanent" Hand` (`NonlandPermanentTarget`), mode 2 `Draw (Literal 1)` (no targets). The Bird token is a nested card (1/1 `Creature — Bird`, `Flying`, empty spell/abilities) — copy the exact token shape from `data/cards/dragon-fodder.json` and swap name/subtype/keywords:

```json
{"name":"Aether Channeler","manaCost":[{"type":"Generic","value":2},{"type":"OfType","value":{"type":"Colored","value":{"type":"Blue"}}}],"typeLine":{"supertypes":[],"types":[{"type":"Creature"}],"subtypes":[{"type":"Human"},{"type":"Wizard"}]},"power":{"type":"Literal","value":3},"toughness":{"type":"Literal","value":3},"keywords":[],"staticAbilities":[],"spell":{"modes":[{"effects":[],"targetSpecs":[]}],"selection":{"type":"ChooseExactly","value":1}},"activatedAbilities":[],"replacementEffects":[],"triggeredAbilities":[{"condition":{"type":"SelfEnters"},"modal":{"modes":[{"effects":[{"type":"Create","value":[{"type":"Literal","value":1},{"name":"Bird","manaCost":null,"typeLine":{"supertypes":[],"types":[{"type":"Creature"}],"subtypes":[{"type":"Bird"}]},"power":{"type":"Literal","value":1},"toughness":{"type":"Literal","value":1},"keywords":[{"type":"Flying"}],"staticAbilities":[],"spell":{"modes":[{"effects":[],"targetSpecs":[]}],"selection":{"type":"ChooseExactly","value":1}},"activatedAbilities":[],"replacementEffects":[],"triggeredAbilities":[],"castingPermissions":[]}]}],"targetSpecs":[]},{"effects":[{"type":"MoveToZone","value":["permanent",{"type":"Hand"}]}],"targetSpecs":[{"slot":"permanent","spec":{"type":"NonlandPermanentTarget"}}]},{"effects":[{"type":"Draw","value":{"type":"Literal","value":1}}],"targetSpecs":[]}],"selection":{"type":"ChooseExactly","value":1}}}],"castingPermissions":[]}
```

(Verify `Wizard` exists in `Pawl.Type.Subtype`; if not, either add it — a one-line enum addition with a Codec arm — or use only `Human`. Verify the `MoveToZone`/`Keyword.Flying`/`OfType Blue` JSON tags against `unsummon.json`/an existing flyer/`lightning-bolt.json` before writing.)

- [ ] **Step 2: Register it in the test pool**

In `Cards.hs`: add `aetherChannelerPrinting` field, `loadPrinting "aether-channeler"`, and the `allPrintings` entry (as in Task 4 Step 2).

- [ ] **Step 3: Write the failing tests**

In `ModalSpec.hs`, `Tasty.testGroup "M4h trigger modal (CR 700.2b/603.3d)"` — put Aether Channeler onto the battlefield through `Event.createToken`? No: it enters as a resolved permanent, so cast/resolve it or place it via the existing "enters the battlefield" harness the RiP tests use, then run `Engine.placePendingTriggers` answering `ChooseModes` and (for mode 1) `ChooseTargets`:

```haskell
, HU.testCase "create mode makes a 1/1 flying Bird token" $ do
    -- Aether Channeler ETB, answer ChooseModes {0}; assert a new Bird token with Flying
, HU.testCase "bounce mode returns another nonland permanent to hand (CR 601.2c)" $ do
    -- board also has a victim permanent; answer ChooseModes {1}, ChooseTargets victim
    -- assert victim moved to its owner's Hand; only "permanent" slot bound
, HU.testCase "draw mode draws one" $ do
    -- answer ChooseModes {2}; assert controller drew a card
, HU.testCase "bounce mode excludes Aether Channeler itself (CR \"another\")" $ do
    -- with Aether Channeler the only nonland permanent, ChooseModes offers {0,2} (mode 1 unfillable)
```

Add a `ReplaySpec.hs` test: a `DecisionLog` that answers the Aether Channeler `ChooseModes` replays byte-identically (mirror the existing `ChoseModes`-for-a-spell replay test).

- [ ] **Step 4: Run to verify they fail**

Run: `cabal test --test-options='-p "trigger modal"'`
Expected: FAIL — `placeOne` (Task 3b) stamps forced modes but never prompts and never chooses targets, so multi-legal-mode Aether Channeler resolves the wrong/no mode and never bounces.

- [ ] **Step 5: Add the prompt + targeting to `placeOne`**

Make `placeOne` prompt the mode (when a real choice exists) and the chosen modes' targets (CR 603.3d), then stamp both. (The CR 603.3c *removal* guard is Task 6; here `size legal >= count` always holds for Aether Channeler.)

```haskell
placeOne (srcId, controller, ability) = do
  gs <- State.get
  let (abilId, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      decider = Decide.deciderFor controller gs
      legal = Target.fillableModes srcId (TriggeredAbility.modal ability) gs
      count = Modal.selectionCount (TriggeredAbility.modal ability)
      obj = Object.MkObject { Object.owner = controller, Object.source = Source.OfTrigger srcId ability, Object.zone = Zone.Stack, Object.tapped = TapState.Untapped, Object.damage = 0, Object.sickness = Sickness.Settled, Object.bindings = Map.empty, Object.counters = Map.empty, Object.timestamp = ts }
      onStack = gs2 {GameState.objects = Map.insert abilId obj (GameState.objects gs2), GameState.stack = abilId : GameState.stack gs2}
  State.put onStack
  chosenModes <-
    if Set.size legal <= fromIntegral count
      then pure legal
      else Trans.lift (Program.prompt (Prompt.ChooseModes decider controller abilId legal count))
  -- (Task 6 inserts the CR 603.3c removal guard immediately after this line.)
  let sets = Target.legalSetsExcluding srcId (Modal.modesTargetSpecs chosenModes (TriggeredAbility.modal ability)) gs
  chosen <-
    if Map.null sets then pure Map.empty
    else Trans.lift (Program.prompt (Prompt.ChooseTargets decider controller abilId sets))
  State.modify' (\g -> g {GameState.objects = Map.adjust (\o -> o {Object.bindings = Binding.fromChoices chosen Map.empty Nothing chosenModes}) abilId (GameState.objects g)})
```

Add `import qualified Pawl.Decide as Decide`, `import qualified Pawl.Type.Program as Program`, `import qualified Pawl.Type.Prompt as Prompt`, `import qualified Data.Set as Set`, `import qualified Pawl.Modal as Modal` to `Engine.hs` (verify which exist). For Rest in Peace (single empty-target mode) `legal = {0}`, `count = 1` → forced, `sets` empty → no target prompt → unchanged.

Note on target validation: unlike `castSpell`/`activateAbility`, this reject path removes the ability (CR 603.3d). Keep it simple here (fillability guarantees a legal set exists) and let Task 6 own the removal branch; a malformed answer is out of scope of this task's tests. If a reviewer flags the missing reject-not-repair on an illegal `ChooseTargets` answer, add the same `if not (keysAgree && eachLegal) then <remove> ` guard Task 6 introduces.

- [ ] **Step 6: Run the tests and full suite**

Run: `cabal test --test-options='-p "trigger modal"'` → PASS.
Run: `cabal test --test-options='-p "Replay"'` → PASS.
Run: `cabal test` → green (Rest in Peace unchanged; round-trip covers `aether-channeler.json` and its Bird token).

- [ ] **Step 7: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4h: mode choice + targeting at trigger placement (CR 700.2b/603.3d); gate Aether Channeler"
```

---

### Task 6: CR 603.3c removal — no legal mode removes the trigger

**Files:**
- Modify: `source/library/Pawl/Engine.hs` (`placeOne`: the removal guard)
- Create: `data/cards/synthetic-modal-trigger.json`
- Modify: `source/test-suite/Pawl/Cards.hs`
- Test: `source/test-suite/Pawl/ModalSpec.hs`

**Interfaces:**
- Consumes: `Target.fillableModes`, `Modal.selectionCount`.
- Produces: `Cards.syntheticModalTriggerPrinting :: Cards -> Printing`.

- [ ] **Step 1: Add the fixture card**

`data/cards/synthetic-modal-trigger.json` — a 2/2 creature whose ETB `TriggeredAbility` is `ChooseExactly 1` over two **`NonlandPermanentTarget`** modes (self-excluding — `CreatureTarget` would be self-fillable, spec §9), each `MoveToZone` (to `Hand` / to `Exile`):

```json
{"name":"Synthetic Modal Trigger","manaCost":[{"type":"Generic","value":2}],"typeLine":{"supertypes":[],"types":[{"type":"Creature"}],"subtypes":[]},"power":{"type":"Literal","value":2},"toughness":{"type":"Literal","value":2},"keywords":[],"staticAbilities":[],"spell":{"modes":[{"effects":[],"targetSpecs":[]}],"selection":{"type":"ChooseExactly","value":1}},"activatedAbilities":[],"replacementEffects":[],"triggeredAbilities":[{"condition":{"type":"SelfEnters"},"modal":{"modes":[{"effects":[{"type":"MoveToZone","value":["x",{"type":"Hand"}]}],"targetSpecs":[{"slot":"x","spec":{"type":"NonlandPermanentTarget"}}]},{"effects":[{"type":"MoveToZone","value":["x",{"type":"Exile"}]}],"targetSpecs":[{"slot":"x","spec":{"type":"NonlandPermanentTarget"}}]}],"selection":{"type":"ChooseExactly","value":1}}}],"castingPermissions":[]}
```

Register it in `Cards.hs` (field + load + `allPrintings`).

- [ ] **Step 2: Write the failing test**

In `ModalSpec.hs`, in the `"M4h trigger modal"` group:

```haskell
, HU.testCase "no legal mode removes the trigger from the stack (CR 603.3c)" $ do
    -- Synthetic Modal Trigger enters as the ONLY nonland permanent (lands aside).
    -- Both modes' NonlandPermanentTarget sets exclude the source -> empty -> no
    -- legal mode. Run placePendingTriggers; assert the ability is NOT on the stack
    -- (removed), nothing bounced/exiled, and no ChooseModes was ever asked.
    …
```

Build the board so the entering creature is the sole nonland permanent; drive the ETB trigger through `Engine.placePendingTriggers`; assert `GameState.stack` contains no `OfTrigger` object and no other permanent moved. (Follow the RiP ETB harness in `EventSpec.hs`/`ModalSpec.hs`.)

- [ ] **Step 3: Run to verify it fails**

Run: `cabal test --test-options='-p "603.3c"'`
Expected: FAIL — Task 5's `placeOne` has no removal guard, so with `legal = {}` and `count = 1` it takes the forced path `chosenModes = {}` (size 0 ≤ 1), leaves the ability on the stack, and resolves nothing-scoped — the ability lingers instead of being removed.

- [ ] **Step 4: Add the removal guard**

In `placeOne`, immediately after computing `chosenModes` (Task 5's prompt line), insert the CR 603.3c guard:

```haskell
  if Set.size legal < fromIntegral count
    then State.modify' (removeFromStack abilId) -- CR 603.3c/700.2b: no legal mode -> removed
    else do
      … (the target choice + stamp from Task 5) …
```

Add the helper (the `Resolve.cease` shape — inlined so `Engine` needn't import `Resolve`; if `Engine` already imports `Resolve`, call `Resolve.cease` instead):

```haskell
-- CR 603.3c: remove a placed-but-illegal trigger from the stack and objects.
removeFromStack :: ObjectId -> GameState -> GameState
removeFromStack abilId gs =
  gs { GameState.stack = filter (/= abilId) (GameState.stack gs),
       GameState.objects = Map.delete abilId (GameState.objects gs) }
```

Note the guard uses `legal` (fillable modes), so it fires when *fewer than `count`* modes are legal — for `ChooseExactly 1`, exactly "no legal mode." The prompt in Task 5 only runs when `size legal > count`, so a forced selection with `size legal == count` still stamps and resolves. Re-check the branch order: guard (`< count`) first, then the prompt-or-forced choice, then targets — restructure Task 5's body so the guard precedes the `ChooseModes` prompt (a removed trigger must never prompt).

- [ ] **Step 5: Run the test and full suite**

Run: `cabal test --test-options='-p "603.3c"'` → PASS.
Run: `cabal test` → green (Aether Channeler still never removed — it always has ≥1 legal mode; Rest in Peace's single mode is always fillable).

- [ ] **Step 6: Commit**

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "M4h: CR 603.3c -- no legal mode removes the modal trigger + fixture"
```

---

### Task 7: Milestone documentation

**Files:**
- Modify: `docs/progress.md` (append the M4h completion entry)
- Modify: `docs/design.md` (mark the M4g fast-follow landed)
- Modify: `CLAUDE.md` (extend the "Current work" milestone line)

**Interfaces:** none (docs only).

- [ ] **Step 1: Append the `progress.md` entry**

Add one distilled entry after the M4g entry, in the house style: gate card (Aether Channeler), the decision proved (modality is payload-level, adopted by both ability types with no cycle; CR 603.3c removal is the trigger-only novelty), the types/opcodes (zero opcodes; `NonlandPermanentTarget` + self-exclusion; `Pawl.Modal` reader; `ActivatedAbility`/`TriggeredAbility` reshaped, retiring M4g's `[Effect]` divergence), the two synthetic fixtures with their expiries, and the named deferred expiries (spec §13). Cite the spec and this plan.

- [ ] **Step 2: Update `docs/design.md`**

In §3, change the "M4g landed as the last letter, and it names its own fast-follow" paragraph to note the fast-follow **landed as M4h**: both ability types carry `Modal`, the mode choice is wired into activation (602.2b) and trigger placement (700.2b/603.3c), gated by Aether Channeler. Update the "Milestones M0 through M4g are complete" line to "through M4h."

- [ ] **Step 3: Update `CLAUDE.md`**

Extend the "Current work and tracking" bullet: append M4h to the completed list ("… and **M4h** (modality on activated and triggered abilities — the M4g fast-follow, gate Aether Channeler)"), keeping the sentence's structure.

- [ ] **Step 4: Verify and commit**

Run: `grep -c -- '- \[ \] \*\*Step' docs/superpowers/plans/2026-07-20-m4h-modal-abilities.md` → must be `0` (all steps ticked).
Run: `cabal build all --enable-tests --enable-benchmarks && cabal test` → green (docs-only change, sanity check).

```bash
git add -A && hooky fix && git add -A && hooky run
git commit -m "docs: M4h complete -- modality on activated & triggered abilities"
```

---

## Self-Review

**Spec coverage:**
- §1 reshape → Tasks 3a/3b. §2 shared reader → Task 2. §3 mana ability → Task 3a (Step 3a). §4 activation path → Tasks 3a (forced) + 4 (prompt). §5 trigger placement → Tasks 3b (forced) + 5 (prompt/targeting) + 6 (603.3c removal). §6 mode-scoped resolution → Tasks 3a/3b (`resolveEffects` callers pass mode-scoped effects/specs). §7 `NonlandPermanentTarget` + self-exclusion + Bird token → Tasks 1, 5. §8 Codec + JSON migration → Tasks 3a/3b (arms + 6 files). §9 synthetic fixtures → Tasks 4, 6. §10 tests → Tasks 1–6. §11 module notes → honored across tasks. §13 deferred expiries → Task 7 (documented). All covered.
- The `ReplaySpec` `ChoseModes`-for-a-trigger test (spec §10 "Replay") is Task 5 Step 3.
- The behavior-preservation checks (spec exit criterion "Forced choices ask nothing") are the full-suite runs in Tasks 3a/3b/4/5/6.

**Placeholder scan:** The test bodies in Tasks 4/5/6 give the assertions and harness in prose+skeleton rather than fully-typed code, because the exact `S`/`Program` prompt-answering helper is file-local (`ModalSpec.hs`/`ActivateSpec.hs`/`EventSpec.hs` already contain it for the M4g/M3f tests); the implementer copies the neighbouring test's harness. This is deliberate (matching existing patterns beats inventing a divergent harness), not an omitted requirement — the CR-number, the board setup, the answers, and the assertion are all specified.

**Type consistency:** `Target.fillableModes :: ObjectId -> Modal.Modal Card -> GameState -> Set ModeIndex` (Task 2) is used identically in Tasks 3a/3b/4/5/6. `Target.legalSetsExcluding :: ObjectId -> Map SlotName TargetSpec -> GameState -> Map SlotName (Set Recipient)` (Task 1) used consistently. `Modal.selectionCount :: Modal card -> Natural` compared via `fromIntegral count` at every call. `Binding.fromChoices chosen subtypes mAmount chosenModes` used with the same 4-arg shape everywhere (matching the existing signature). `resolveEffects` signature is left unchanged; both callers (`resolveAbility`, `Stack` `OfTrigger`) pass `Modal.modesEffects`/`modesTargetSpecs` — consistent.
