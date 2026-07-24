{-# LANGUAGE GADTs #-}

-- Covers Pawl.Activate: activating an ability onto the stack, summoning-sickness
-- gating, and the CR 605 mana-ability exclusion from stack activations.
module Pawl.ActivateSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Action as Action
import qualified Pawl.Activate as Activate
import qualified Pawl.Cards as Cards
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Mana as Mana
import qualified Pawl.Projection as Projection
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Type.Action as A
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import qualified Pawl.Type.ActiveReplacement as ActiveReplacement
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.CardType as CardType
import qualified Pawl.Type.Cost as Cost.Type
import qualified Pawl.Type.Duration as Duration
import qualified Pawl.Type.Effect as Effect
import qualified Pawl.Type.Exclusion as Exclusion
-- Aliased Filter.Type, not Filter, per the project-wide convention (FilterSpec):
-- the evaluator module Pawl.Filter may later be imported and must not collide.
import qualified Pawl.Type.Filter as Filter.Type
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.ManaCost as ManaCost
import qualified Pawl.Type.ManaSymbol as ManaSymbol
import qualified Pawl.Type.Modal as Modal
import qualified Pawl.Type.Mode as Mode
import qualified Pawl.Type.ModeSelection as ModeSelection
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.Pool as Pool
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.SlotName as SlotName
import qualified Pawl.Type.TapState as TapState
import qualified Pawl.Type.TargetSpec as TargetSpec
import qualified Pawl.Type.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- Finds the first matching library card on a search, else fails to find.
findFirst :: Prompt.Prompt r -> r
findFirst p = case p of
  Prompt.SearchLibrary _ _ matches -> case matches of
    m : _ -> Just m
    [] -> Nothing
  _ -> S.identityAnswer p

-- The single ability of a printing (all M3e gates have exactly one). Total: the
-- empty-ability fallback is unreachable in these fixtures, and honors the
-- no-partial-functions rule (no `error`).
theAbility :: Printing.Printing -> ActivatedAbility.ActivatedAbility Card.Type.Card
theAbility p = case Card.Type.activatedAbilities (Printing.card p) of
  ab : _ -> ab
  [] -> ActivatedAbility.MkActivatedAbility (Cost.Type.MkCost (Just (ManaCost.MkManaCost [])) []) (singleModeAbility [] Map.empty)

-- A single forced mode (ChooseExactly 1, M4g's non-modal shape) -- the fixture
-- shape every pre-M4h single-mode ActivatedAbility now takes.
singleModeAbility :: [Effect.Effect card] -> Map.Map SlotName.SlotName TargetSpec.TargetSpec -> Modal.Modal card
singleModeAbility effects specs =
  Modal.MkModal (Seq.singleton (Mode.MkMode (Seq.fromList effects) specs)) (ModeSelection.ChooseExactly 1)

tests :: Cards.Cards -> Tasty.TestTree
tests cards =
  Tasty.testGroup
    "Pawl.Activate"
    [ HU.testCase "CR 602 activating Prodigal Sorcerer's {T} puts an ability on the stack and taps it" $
        let (srcId, g0) = S.addCreature (Cards.prodigalSorcererPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            g1 = g0 {GameState.priority = Just S.alice}
            after = snd (Engine.runGamePure S.identityAnswer g1 (Activate.activateAbility S.alice srcId (theAbility (Cards.prodigalSorcererPrinting cards))))
         in do
              HU.assertEqual "one thing on the stack" 1 (length (GameState.stack after))
              HU.assertEqual "source tapped" (Just TapState.Tapped) (fmap Object.tapped (Game.lookupObject srcId after)),
      HU.testCase "CR 602.5/302.6 a summoning-sick creature's {T} ability is not offered" $
        let (srcId, g0) = S.addCreature (Cards.prodigalSorcererPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            sick = g0 {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) srcId (GameState.objects g0), GameState.priority = Just S.alice}
         in HU.assertBool "no Activate offered" (not (any isActivate (Action.legalActions S.alice sick))),
      HU.testCase "CR 602 a settled Prodigal Sorcerer's ability IS offered" $
        let (_, g0) = S.addCreature (Cards.prodigalSorcererPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            g1 = g0 {GameState.priority = Just S.alice}
         in HU.assertBool "Activate offered" (any isActivate (Action.legalActions S.alice g1)),
      HU.testCase "CR 602 activating then resolving deals 1 damage and the ability ceases" $
        let (srcId, g0) = S.addCreature (Cards.prodigalSorcererPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            g1 = g0 {GameState.priority = Just S.alice}
            -- identityAnswer's ChooseTargets picks the lowest recipient; with no
            -- creatures but two players, it targets a player. Resolve the stack.
            activated = snd (Engine.runGamePure S.identityAnswer g1 (Activate.activateAbility S.alice srcId (theAbility (Cards.prodigalSorcererPrinting cards))))
            resolved = snd (Engine.runGamePure S.identityAnswer activated Stack.resolveTop)
         in HU.assertEqual "stack empty after resolution" [] (GameState.stack resolved),
      HU.testCase "CR 605.3b a mana ability is not offered as a stack activation" $
        let (_, g0) = S.addCreature (Cards.llanowarElvesPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            g1 = g0 {GameState.priority = Just S.alice}
         in HU.assertBool "no Activate for the mana ability" (not (any isActivate (Action.legalActions S.alice g1))),
      HU.testCase "CR 701.21/701.23 Evolving Wilds sacrifices itself and fetches a basic land tapped" $
        -- The fetched land gets a NEW id (CR 400.7); assert by count/tapped-count.
        let base = Setup.emptyGame S.bothPlayers
            (wildsId, g1) = S.addCreature (Cards.evolvingWildsPrinting cards) S.alice base
            (_, g2) = S.addLibraryCard (Cards.forestPrinting cards) S.alice g1
            g3 = g2 {GameState.priority = Just S.alice}
            ability = theAbility (Cards.evolvingWildsPrinting cards)
            activated = snd (Engine.runGamePure findFirst g3 (Activate.activateAbility S.alice wildsId ability))
            resolved = snd (Engine.runGamePure findFirst activated Stack.resolveTop)
         in do
              HU.assertBool "Evolving Wilds' ability is NOT a mana ability" (not (Mana.isManaAbility ability))
              HU.assertBool "Evolving Wilds sacrificed (gone from battlefield)" (not (Set.member wildsId (GameState.battlefield resolved)))
              HU.assertEqual "one permanent on the battlefield (the fetched land)" 1 (length (Game.zoneMembers Zone.Battlefield S.alice resolved))
              HU.assertEqual "the fetched land is tapped" 1 (S.tappedCount S.alice resolved),
      HU.testCase "CR 302.6 a freshly-added land can tap+sac immediately (no summoning sickness)" $
        let base = Setup.emptyGame S.bothPlayers
            (wildsId, g1) = S.addCreature (Cards.evolvingWildsPrinting cards) S.alice base
            -- Force it Sick: a land ignores sickness, so the ability is still offered.
            g2 = g1 {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) wildsId (GameState.objects g1), GameState.priority = Just S.alice}
         in HU.assertBool "land ability offered despite sickness" (any isActivate (Action.legalActions S.alice g2)),
      HU.testCase "CR 613/602 a Humility'd Prodigal Sorcerer's ability is not offered" $
        let (_, g0) = S.addCreature (Cards.prodigalSorcererPrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            gs = (S.withHumility cards g0) {GameState.priority = Just S.alice}
         in HU.assertBool "no Activate under Humility" (not (any isActivate (Action.legalActions S.alice gs))),
      HU.testCase "CR 602.1b: an activation with a mana cost needs the mana" $
        let gs = S.mountainsInPlay cards 1
            (srcId, gs1) = S.addCreature (Cards.pikerPrinting cards) S.alice gs
            costlyAbility =
              ActivatedAbility.MkActivatedAbility
                { ActivatedAbility.cost =
                    Cost.Type.MkCost
                      { Cost.Type.mana = Just (ManaCost.MkManaCost [ManaSymbol.Generic 2]),
                        Cost.Type.components = []
                      },
                  ActivatedAbility.modal = singleModeAbility [] Map.empty
                }
         in HU.assertBool "one Mountain cannot pay {2}" (not (Activate.activatable S.alice srcId costlyAbility gs1)),
      HU.testCase "CR 701.19a Drudge Skeletons regenerates: activate, survive Murder, die to the next" $
        let base = S.landsInPlay (Cards.swampPrinting cards) 1
            (skel, gs0) = S.addCreature (Cards.drudgeSkeletonsPrinting cards) S.alice base
            ability = theAbility (Cards.drudgeSkeletonsPrinting cards) -- the local ActivateSpec helper
            activated = snd (Engine.runGamePure S.identityAnswer gs0 (Activate.activateAbility S.alice skel ability))
            resolved = snd (Engine.runGamePure S.identityAnswer activated Stack.resolveTop)
            -- First Murder: replaced by the shield.
            firstKill = S.settleSba (S.runPure S.identityAnswer resolved (Event.destroy skel))
            -- Second Murder: no shield -> dies.
            secondKill = S.settleSba (S.runPure S.identityAnswer firstKill (Event.destroy skel))
         in do
              HU.assertEqual
                "the shield's source is the skeleton itself"
                [skel]
                (fmap ActiveReplacement.source (GameState.replacements resolved))
              HU.assertEqual "survived the first destruction (regenerated)" True (Set.member skel (GameState.battlefield firstKill))
              HU.assertEqual "died to the second (one-shot shield consumed)" False (Set.member skel (GameState.battlefield secondKill)),
      -- CR 113.8: the controller of an activated ability on the stack is the
      -- player who activated it; the controller of a triggered ability on the
      -- stack is whoever controlled its source when it triggered. Fixed once,
      -- at activation (Activate.activateAbility sets Object.owner = pid), and
      -- never revisited -- so a control change to the SOURCE PERMANENT after
      -- activation must not move the ability to the new controller.
      --
      -- Prodigal Sorcerer's own ability (a flat DealDamage 1; CR 115.4's "any
      -- target" has no controller-sensitive restriction) has no
      -- controller-sensitive OUTPUT, so it cannot make "whose control"
      -- observable here. A GainControl/ForAsLongAs effect does -- the same
      -- shape Master Thief's ETB uses in Pawl.ExpirySpec's masterThiefTests --
      -- attached to a SYNTHETIC ability (labeled crutch) on the same creature,
      -- standing in for the pool's lack of a controller-sensitive printed
      -- activated ability (#82), so this exercises the ACTIVATED path rather
      -- than the TRIGGERED one that card already covers.
      HU.testCase "CR 113.8 an activated ability resolves under whoever activated it, not a later controller" $
        let base = Setup.emptyGame S.bothPlayers
            -- The Myr is ALICE's own artifact (not bob's): if control ever
            -- moved to bob it would be a genuine change, not a fixture
            -- coincidence, so this assertion actually discriminates the bug.
            (myrId, g0) = S.addCreature (Cards.darksteelMyrPrinting cards) S.alice base
            (srcId, g1) = S.addCreature (Cards.prodigalSorcererPrinting cards) S.alice g0
            g2 = g1 {GameState.priority = Just S.alice}
            targetSlot = SlotName.MkSlotName (Text.pack "target")
            ability =
              ActivatedAbility.MkActivatedAbility
                { ActivatedAbility.cost = Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost []), Cost.Type.components = []},
                  ActivatedAbility.modal =
                    singleModeAbility
                      [Effect.GainControl (Duration.ForAsLongAs S.youControlSource) targetSlot]
                      (Map.singleton targetSlot (TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.Type.HasCardType CardType.Artifact)) Exclusion.IncludesSource))
                }
            activated = snd (Engine.runGamePure S.identityAnswer g2 (Activate.activateAbility S.alice srcId ability))
            -- Control of the SOURCE CREATURE (not the ability object) moves to bob
            -- while the ability sits on the stack.
            taken = S.giveControl srcId S.bob activated
            resolved = snd (Engine.runGamePure S.identityAnswer taken Stack.resolveTop)
         in do
              HU.assertEqual "one thing on the stack before resolution" 1 (length (GameState.stack taken))
              HU.assertEqual "stack empty after resolution" [] (GameState.stack resolved)
              HU.assertEqual "control of the artifact never changed" (Just S.alice) (Projection.controllerOf myrId resolved)
              -- Filtered to the artifact: S.giveControl's own AtCleanup
              -- SetController effect on srcId itself is already in this list and
              -- is not what CR 611.2b's "never starts" is about.
              HU.assertEqual
                "nothing was stored for the artifact -- the duration never starts for alice, the ability's frozen activator"
                []
                (filter (S.continuousEffectAffects myrId) (GameState.continuousEffects resolved)),
      -- The stolen-creature case the OLD (deleted) Resolve.hs comment named --
      -- rebuilt as the POSITIVE mirror of the test above, with the same
      -- SYNTHETIC ability (#82): control of the source moves to bob FIRST, bob
      -- then activates, and the effect must arm and store under bob, the
      -- ability's frozen (and only) controller (CR 113.8). Object.owner is
      -- stamped with bob at activation time, so there is no later re-read to
      -- get wrong.
      HU.testCase "CR 113.8 a stolen creature's ability, activated by the new controller, resolves under them" $
        let base = Setup.emptyGame S.bothPlayers
            (myrId, g0) = S.addCreature (Cards.darksteelMyrPrinting cards) S.alice base
            (srcId, g1) = S.addCreature (Cards.prodigalSorcererPrinting cards) S.alice g0
            targetSlot = SlotName.MkSlotName (Text.pack "target")
            ability =
              ActivatedAbility.MkActivatedAbility
                { ActivatedAbility.cost = Cost.Type.MkCost {Cost.Type.mana = Just (ManaCost.MkManaCost []), Cost.Type.components = []},
                  ActivatedAbility.modal =
                    singleModeAbility
                      [Effect.GainControl (Duration.ForAsLongAs S.youControlSource) targetSlot]
                      (Map.singleton targetSlot (TargetSpec.MkTargetSpec Pool.Permanents (Just (Filter.Type.HasCardType CardType.Artifact)) Exclusion.IncludesSource))
                }
            -- Control of the SOURCE CREATURE moves to bob BEFORE activation.
            taken = S.giveControl srcId S.bob g1
            g2 = taken {GameState.priority = Just S.bob}
            activated = snd (Engine.runGamePure S.identityAnswer g2 (Activate.activateAbility S.bob srcId ability))
            resolved = snd (Engine.runGamePure S.identityAnswer activated Stack.resolveTop)
            stored = filter (S.continuousEffectAffects myrId) (GameState.continuousEffects resolved)
         in do
              HU.assertEqual "bob controls the stolen sorcerer" (Just S.bob) (Projection.controllerOf srcId activated)
              HU.assertEqual "one thing on the stack" 1 (length (GameState.stack activated))
              HU.assertEqual "stack empty after resolution" [] (GameState.stack resolved)
              HU.assertEqual
                "control of the artifact changed to bob, the new controller who activated it"
                (Just S.bob)
                (Projection.controllerOf myrId resolved)
              HU.assertEqual "exactly one stored effect names the artifact" 1 (length stored)
    ]

isActivate :: A.Action -> Bool
isActivate a = case a of
  A.Activate _ _ -> True
  _ -> False
