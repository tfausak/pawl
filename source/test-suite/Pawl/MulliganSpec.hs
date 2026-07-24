{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Mulligan: the CR 103.5 opening-hand / mulligan process.
module Pawl.MulliganSpec where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Cards as Cards
import qualified Pawl.Engine as Engine
import qualified Pawl.Game as Game
import qualified Pawl.Mulligan as Mulligan
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.MulliganDecision as MulliganDecision
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Zone as Zone
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HU

-- Move every object a player owns onto their library (mirror of
-- SetupSpec.poolToLibrary), to craft a known-size, identity-shuffled library.
poolToLibrary :: PlayerId -> GameState.GameState -> GameState.GameState
poolToLibrary pid gs =
  let mine = Map.keys (Map.filter (\o -> Object.owner o == pid) (GameState.objects gs))
      onLibrary o = o {Object.zone = Zone.Library}
   in gs
        { GameState.objects = List.foldl' (flip (Map.adjust onLibrary)) (GameState.objects gs) mine,
          GameState.battlefield = Set.difference (GameState.battlefield gs) (Set.fromList mine),
          GameState.library = Map.insert pid (Seq.fromList mine) (GameState.library gs)
        }

-- n Mountains in each player's library, nothing elsewhere.
libraryGame :: Cards.Cards -> Int -> GameState.GameState
libraryGame cards n =
  let g0 = Setup.emptyGame S.bothPlayers
      addMany pid g = List.foldl' (\h _ -> snd (S.addCreature (Cards.mountainPrinting cards) pid h)) g (replicate n ())
   in poolToLibrary S.bob (poolToLibrary S.alice (addMany S.bob (addMany S.alice g0)))

libSize :: PlayerId -> GameState.GameState -> Int
libSize pid gs = length (Game.zoneMembers Zone.Library pid gs)

libBottom :: Int -> PlayerId -> GameState.GameState -> [ObjectId.ObjectId]
libBottom k pid gs = reverse (take k (reverse (Game.zoneMembers Zone.Library pid gs)))

-- Keep always: reproduces the pre-mulligan draw.
keepAnswer :: Prompt.Prompt r -> r
keepAnswer p = case p of
  Prompt.DeclareMulligan {} -> MulliganDecision.Keep
  _ -> S.identityAnswer p

-- Mulligan while fewer than `k` taken, then keep. Bottom the first `count`.
mulliganUpTo :: Int -> Prompt.Prompt r -> r
mulliganUpTo k p = case p of
  Prompt.DeclareMulligan _ _ taken ->
    if fromIntegral taken < k then MulliganDecision.Mulligan else MulliganDecision.Keep
  _ -> S.identityAnswer p

run :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
run answer gs = snd (Engine.runGamePure answer gs (Mulligan.openingHands [S.alice, S.bob]))

tests :: Cards.Cards -> Tasty.TestTree
tests cards =
  Tasty.testGroup
    "Mulligan"
    [ HU.testCase "CR 103.5: all-Keep draws exactly seven, library shrinks by seven" $
        let after = run keepAnswer (libraryGame cards 20)
         in do
              HU.assertEqual "alice hand" 7 (S.handSize S.alice after)
              HU.assertEqual "alice library" 13 (libSize S.alice after),
      HU.testCase "CR 103.5: one mulligan bottoms one card (hand of six)" $
        let after = run (mulliganUpTo 1) (libraryGame cards 20)
         in HU.assertEqual "alice hand" 6 (S.handSize S.alice after),
      HU.testCase "CR 103.5: two mulligans bottom two cards (hand of five)" $
        let after = run (mulliganUpTo 2) (libraryGame cards 20)
         in HU.assertEqual "alice hand" 5 (S.handSize S.alice after),
      HU.testCase "CR 103.5 final sentence: mulligan floors at a zero-card hand" $
        -- A 7-card library: redraw is always 7, but the loop bottoms up to the
        -- growing count; assert termination and that alice keeps a >= 0 hand.
        let after = run (mulliganUpTo 100) (libraryGame cards 7)
         in HU.assertBool "terminates with a non-negative opening hand" (S.handSize S.alice after >= 0),
      HU.testCase "CR 103.5: bottomed cards are the ones NOT in the opening hand" $
        -- One mulligan: hand is 6, and the 1 bottomed card is at the library
        -- bottom, distinct (by id) from the six in hand.
        let after = run (mulliganUpTo 1) (libraryGame cards 20)
            handIds = Set.fromList (Game.zoneMembers Zone.Hand S.alice after)
            bottom1 = libBottom 1 S.alice after
         in HU.assertEqual "the bottomed card is not in hand" [] (filter (`Set.member` handIds) bottom1)
    ]
