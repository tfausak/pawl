{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: Pawl.Replacement (the CR 616.1 loop, its buckets and its prompt) and
-- the funnels that raise proposed events through it. Gameplay-level throughout:
-- put a board together, cast or resolve, assert on game state.
module Pawl.ReplacementSpec where

import qualified Data.Set as Set
import qualified Pawl.Cards as Cards
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Replay as Replay
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.Game as Game.Type
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Response as Response
import qualified Pawl.Type.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- Every answer the engine asked for, in order -- so a test can assert that a
-- prompt WAS raised (the engine did not decide) or was NOT (the choice was
-- indistinguishable and correctly elided).
answersFor :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> Game.Type.Game a -> [Response.Response]
answersFor answer gs game = snd (Replay.record answer gs game)

wasAskedToReplace :: [Response.Response] -> Bool
wasAskedToReplace = any isReplacement
  where
    isReplacement r = case r of
      Response.ChoseReplacement _ -> True
      _ -> False

tests :: Cards.Cards -> Tasty.TestTree
tests cards =
  Tasty.testGroup
    "Pawl.Replacement"
    [ HU.testCase "CR 614.5 two Rest in Peaces redirect once, not twice" $
        let (_, g0) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            (_, g1) = S.addCreature (Cards.restInPeacePrinting cards) S.alice g0
            (piker, g2) = S.addPiker cards S.bob g1
            after = S.runPure S.identityAnswer g2 (Event.changeZone piker Zone.Graveyard)
         in do
              HU.assertEqual "not in a graveyard" 0 (length (Game.zoneMembers Zone.Graveyard S.bob after))
              HU.assertEqual "exactly one object in exile" 1 (Set.size (GameState.exile after)),
      HU.testCase "CR 616.1 value-equal candidates elide the prompt (nothing to choose)" $
        let (_, g0) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            (_, g1) = S.addCreature (Cards.restInPeacePrinting cards) S.alice g0
            (piker, g2) = S.addPiker cards S.bob g1
            asked = answersFor S.identityAnswer g2 (Event.changeZone piker Zone.Graveyard)
         in HU.assertBool "no ChooseReplacement was raised" (not (wasAskedToReplace asked)),
      HU.testCase "CR 614.1a a move whose destination the pattern misses is untouched" $
        let (_, g0) = S.addCreature (Cards.restInPeacePrinting cards) S.alice (Setup.emptyGame S.bothPlayers)
            (piker, g1) = S.addPiker cards S.bob g0
            -- Rest in Peace watches graveyard-bound moves only; a bounce to hand
            -- is not one, so the loop finds no candidate and the move stands.
            after = S.runPure S.identityAnswer g1 (Event.changeZone piker Zone.Hand)
         in do
              HU.assertEqual "in bob's hand" 1 (length (Game.zoneMembers Zone.Hand S.bob after))
              HU.assertEqual "nothing was exiled" 0 (Set.size (GameState.exile after))
    ]
