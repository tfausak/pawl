{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: Pawl.Event (the placeObject as-enters mark) and Pawl.Engine (the drain),
-- the P2 copy gate (Clone). Gameplay-level: Clone enters via the zone-change funnel
-- and its projected characteristics are asserted.
module Pawl.CopySpec where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Binding as Binding
import qualified Pawl.Cards as Cards
import qualified Pawl.Engine as Engine
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.Object as Object
import Pawl.Type.ObjectId (ObjectId)
import qualified Pawl.Type.Prompt as Prompt
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- The battlefield objects whose printed card is named "Clone" (their source is
-- unchanged by copying -- only their projected characteristics change).
clonesOnBattlefield :: GameState.GameState -> [ObjectId]
clonesOnBattlefield gs = filter isClone (Set.toList (GameState.battlefield gs))
  where
    isClone oid = maybe False (\c -> Card.Type.name c == Text.pack "Clone") (Game.cardOf oid gs)

cloneOnBattlefield :: GameState.GameState -> Maybe ObjectId
cloneOnBattlefield = Maybe.listToMaybe . clonesOnBattlefield

-- The highest-id (most recently entered) object in a list. Total (no partial
-- `maximum`): sort descending by Down, take the head via listToMaybe.
newest :: [ObjectId] -> Maybe ObjectId
newest = Maybe.listToMaybe . List.sortOn Ord.Down

-- Answers the as-enters copy choice with the highest-id legal target (the most
-- recently entered creature), declining only when none is legal. Delegates every
-- other prompt to S.identityAnswer.
copyNewest :: Prompt.Prompt r -> r
copyNewest p = case p of
  Prompt.ChooseCopyTarget _ _ _ legal -> newest legal
  _ -> S.identityAnswer p

-- Resolve the stack top (a permanent enters) AND run the settle boundary (so the
-- as-enters drain fires), under the given answerer.
resolveAndSettle :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
resolveAndSettle answer gs =
  snd (Engine.runGamePure answer gs (Stack.resolveTop >> Engine.settleForPriority))

tests :: Cards.Cards -> Tasty.TestTree
tests cards =
  Tasty.testGroup
    "Copy"
    [ HU.testCase "a copyOnEnter permanent is marked as-enters-pending when it enters" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (_, board) = S.addPiker cards S.alice gs0
            (_cloneStackId, staged) = S.spellOnStack (Cards.clonePrinting cards) S.alice board
            -- Resolve the top of the stack purely (a permanent -> changeZone to the
            -- battlefield), WITHOUT the settle drain, so the mark is observable.
            resolved = snd (Engine.runGamePure S.identityAnswer staged Stack.resolveTop)
         in case cloneOnBattlefield resolved of
              Nothing -> HU.assertFailure "Clone did not reach the battlefield"
              Just cloneId ->
                HU.assertBool
                  "Clone is marked as-enters-pending"
                  (maybe False (Binding.pendingCopy . Object.bindings) (Game.lookupObject cloneId resolved)),
      HU.testCase "Clone copies a creature and projects its P/T (CR 707.2)" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (pikerId, board) = S.addPiker cards S.alice gs0
            (_, staged) = S.spellOnStack (Cards.clonePrinting cards) S.alice board
            resolved = resolveAndSettle copyNewest staged
         in case cloneOnBattlefield resolved of
              Nothing -> HU.assertFailure "Clone left the battlefield unexpectedly"
              Just cloneId -> do
                HU.assertEqual "Clone's power is the Piker's" (Just 2) (Projection.powerOf cloneId resolved)
                HU.assertEqual "Clone's toughness is the Piker's" (Just 1) (Projection.toughnessOf cloneId resolved)
                HU.assertBool "Clone is a creature" (Projection.isCreatureOf cloneId resolved)
                HU.assertBool "the copied Piker is untouched" (Projection.powerOf pikerId resolved == Just 2),
      HU.testCase "Clone with no creature to copy enters as a 0/0 and dies (CR 704.5f)" $
        let gs0 = Setup.emptyGame S.bothPlayers
            (_, staged) = S.spellOnStack (Cards.clonePrinting cards) S.alice gs0
            resolved = resolveAndSettle copyNewest staged
         in HU.assertEqual "the 0/0 Clone is gone (state-based action)" Nothing (cloneOnBattlefield resolved)
    ]
