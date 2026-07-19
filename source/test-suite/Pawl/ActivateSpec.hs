{-# LANGUAGE GADTs #-}

-- Covers Pawl.Activate: activating an ability onto the stack, summoning-sickness
-- gating, and the CR 605 mana-ability exclusion from stack activations.
module Pawl.ActivateSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Action as Action
import qualified Pawl.Activate as Activate
import qualified Pawl.Card as Card
import qualified Pawl.Engine as Engine
import qualified Pawl.Game as Game
import qualified Pawl.Mana as Mana
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Type.AbilityCost as AbilityCost
import qualified Pawl.Type.Action as A
import qualified Pawl.Type.ActivatedAbility as ActivatedAbility
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Sickness as Sickness
import qualified Pawl.Type.TapState as TapState
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
theAbility :: Printing.Printing -> ActivatedAbility.ActivatedAbility
theAbility p = case Card.Type.activatedAbilities (Printing.card p) of
  ab : _ -> ab
  [] -> ActivatedAbility.MkActivatedAbility (AbilityCost.MkAbilityCost []) [] Map.empty

tests :: Tasty.TestTree
tests =
  Tasty.testGroup
    "Pawl.Activate"
    [ HU.testCase "CR 602 activating Prodigal Sorcerer's {T} puts an ability on the stack and taps it" $
        let (srcId, g0) = S.addCreature Card.prodigalSorcererPrinting S.alice (Setup.emptyGame S.bothPlayers)
            g1 = g0 {GameState.priority = Just S.alice}
            after = snd (Engine.runGamePure S.identityAnswer g1 (Activate.activateAbility S.alice srcId (theAbility Card.prodigalSorcererPrinting)))
         in do
              HU.assertEqual "one thing on the stack" 1 (length (GameState.stack after))
              HU.assertEqual "source tapped" (Just TapState.Tapped) (fmap Object.tapped (Game.lookupObject srcId after)),
      HU.testCase "CR 602.5/302.6 a summoning-sick creature's {T} ability is not offered" $
        let (srcId, g0) = S.addCreature Card.prodigalSorcererPrinting S.alice (Setup.emptyGame S.bothPlayers)
            sick = g0 {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) srcId (GameState.objects g0), GameState.priority = Just S.alice}
         in HU.assertBool "no Activate offered" (not (any isActivate (Action.legalActions S.alice sick))),
      HU.testCase "CR 602 a settled Prodigal Sorcerer's ability IS offered" $
        let (_, g0) = S.addCreature Card.prodigalSorcererPrinting S.alice (Setup.emptyGame S.bothPlayers)
            g1 = g0 {GameState.priority = Just S.alice}
         in HU.assertBool "Activate offered" (any isActivate (Action.legalActions S.alice g1)),
      HU.testCase "CR 602 activating then resolving deals 1 damage and the ability ceases" $
        let (srcId, g0) = S.addCreature Card.prodigalSorcererPrinting S.alice (Setup.emptyGame S.bothPlayers)
            g1 = g0 {GameState.priority = Just S.alice}
            -- identityAnswer's ChooseTargets picks the lowest recipient; with no
            -- creatures but two players, it targets a player. Resolve the stack.
            activated = snd (Engine.runGamePure S.identityAnswer g1 (Activate.activateAbility S.alice srcId (theAbility Card.prodigalSorcererPrinting)))
            resolved = snd (Engine.runGamePure S.identityAnswer activated Stack.resolveTop)
         in HU.assertEqual "stack empty after resolution" [] (GameState.stack resolved),
      HU.testCase "CR 605.3b a mana ability is not offered as a stack activation" $
        let (_, g0) = S.addCreature Card.llanowarElvesPrinting S.alice (Setup.emptyGame S.bothPlayers)
            g1 = g0 {GameState.priority = Just S.alice}
         in HU.assertBool "no Activate for the mana ability" (not (any isActivate (Action.legalActions S.alice g1))),
      HU.testCase "CR 701.21/701.23 Evolving Wilds sacrifices itself and fetches a basic land tapped" $
        -- The fetched land gets a NEW id (CR 400.7); assert by count/tapped-count.
        let base = Setup.emptyGame S.bothPlayers
            (wildsId, g1) = S.addCreature Card.evolvingWildsPrinting S.alice base
            (_, g2) = S.addLibraryCard Card.forestPrinting S.alice g1
            g3 = g2 {GameState.priority = Just S.alice}
            ability = theAbility Card.evolvingWildsPrinting
            activated = snd (Engine.runGamePure findFirst g3 (Activate.activateAbility S.alice wildsId ability))
            resolved = snd (Engine.runGamePure findFirst activated Stack.resolveTop)
         in do
              HU.assertBool "Evolving Wilds' ability is NOT a mana ability" (not (Mana.isManaAbility ability))
              HU.assertBool "Evolving Wilds sacrificed (gone from battlefield)" (not (Set.member wildsId (GameState.battlefield resolved)))
              HU.assertEqual "one permanent on the battlefield (the fetched land)" 1 (length (Game.zoneMembers Zone.Battlefield S.alice resolved))
              HU.assertEqual "the fetched land is tapped" 1 (S.tappedCount S.alice resolved),
      HU.testCase "CR 302.6 a freshly-added land can tap+sac immediately (no summoning sickness)" $
        let base = Setup.emptyGame S.bothPlayers
            (wildsId, g1) = S.addCreature Card.evolvingWildsPrinting S.alice base
            -- Force it Sick: a land ignores sickness, so the ability is still offered.
            g2 = g1 {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) wildsId (GameState.objects g1), GameState.priority = Just S.alice}
         in HU.assertBool "land ability offered despite sickness" (any isActivate (Action.legalActions S.alice g2))
    ]

isActivate :: A.Action -> Bool
isActivate a = case a of
  A.Activate _ _ -> True
  _ -> False
