{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Mulligan: the CR 103.5 opening-hand / mulligan process.
module Pawl.MulliganSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Cards as Cards
import qualified Pawl.Engine as Engine
import qualified Pawl.Game as Game
import qualified Pawl.Mulligan as Mulligan
import qualified Pawl.Replay as Replay
import qualified Pawl.Setup as Setup
import qualified Pawl.Support as S
import qualified Pawl.Type.GameState as GameState
import qualified Pawl.Type.MulliganDecision as MulliganDecision
import qualified Pawl.Type.Object as Object
import qualified Pawl.Type.ObjectId as ObjectId
import Pawl.Type.PlayerId (PlayerId)
import qualified Pawl.Type.Printing as Printing
import qualified Pawl.Type.Program as Program
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

-- Alice's library in an explicit, non-uniform order of 20 pairwise-DISTINCT
-- printings, so an individual library card can be told apart by its printed
-- Card identity even after a CR 400.7 zone-change reincarnation:
-- Event.changeZone (Pawl.Event) mints every moved card a FRESH ObjectId --
-- mkObj there only resets zone/tapped/damage/sickness/bindings/counters/
-- timestamp -- so a card's pre-move id is never equal to its post-move id,
-- which is why an order test can't compare ObjectIds across the move. Source
-- (hence the printed Card, read back via Game.cardOf) is the one field mkObj
-- carries forward unchanged, so it survives the reincarnation and is what
-- this test tracks instead. Bob's library is uniform Mountains; only alice
-- mulligans in the accompanying test, so bob's composition never enters the
-- assertion.
distinctPrintings :: Cards.Cards -> [Printing.Printing]
distinctPrintings cards =
  [ Cards.mountainPrinting cards,
    Cards.swampPrinting cards,
    Cards.forestPrinting cards,
    Cards.pikerPrinting cards,
    Cards.birdMaidenPrinting cards,
    Cards.nimbleBirdstickerPrinting cards,
    Cards.ogreSentryPrinting cards,
    Cards.windseekerCentaurPrinting cards,
    Cards.goblinChariotPrinting cards,
    Cards.sabretoothTigerPrinting cards,
    Cards.ridgetopRaptorPrinting cards,
    Cards.typhoidRatsPrinting cards,
    Cards.warMammothPrinting cards,
    Cards.lightningBoltPrinting cards,
    Cards.giantGrowthPrinting cards,
    Cards.humilityPrinting cards,
    Cards.serpentsGiftPrinting cards,
    Cards.bloodMoonPrinting cards,
    Cards.urborgPrinting cards,
    Cards.opalescencePrinting cards
  ]

orderedLibraryGame :: Cards.Cards -> [Printing.Printing] -> GameState.GameState
orderedLibraryGame cards printings =
  let g0 = Setup.emptyGame S.bothPlayers
      addOrdered pid g = List.foldl' (\h p -> snd (S.addCreature p pid h)) g printings
      addUniform pid g = List.foldl' (\h _ -> snd (S.addCreature (Cards.mountainPrinting cards) pid h)) g printings
   in poolToLibrary S.bob (poolToLibrary S.alice (addUniform S.bob (addOrdered S.alice g0)))

-- Alice mulligans exactly twice then keeps; bob keeps at once. On each Bottom
-- prompt, bottoms the FIRST `count` cards of the (post-redraw) hand in
-- REVERSED order -- a deliberately NON-identity order -- so the test can
-- prove the library bottom honors that exact chosen order, not just the
-- chosen set.
bottomReversedAnswer :: Prompt.Prompt r -> r
bottomReversedAnswer p = case p of
  Prompt.DeclareMulligan _ pid taken -> if pid == S.alice && taken < 2 then MulliganDecision.Mulligan else MulliganDecision.Keep
  Prompt.Bottom _ _ hand count -> reverse (take (fromIntegral count) hand)
  _ -> S.identityAnswer p

-- Records each DeclareMulligan's pid, so the test can assert a kept player is
-- not asked again. Alice keeps immediately; bob mulligans twice then keeps.
recordAsks :: Prompt.Prompt r -> State.State [PlayerId] r
recordAsks p = case p of
  Prompt.DeclareMulligan _ pid taken -> do
    State.modify' (pid :)
    pure (if pid == S.bob && taken < 2 then MulliganDecision.Mulligan else MulliganDecision.Keep)
  _ -> pure (S.identityAnswer p)

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
        -- growing count; the process deterministically terminates with alice's
        -- opening hand floored at zero cards (never negative).
        let after = run (mulliganUpTo 100) (libraryGame cards 7)
         in HU.assertEqual "opening hand floored at zero" 0 (S.handSize S.alice after),
      HU.testCase "CR 103.5: bottomed cards are the ones NOT in the opening hand" $
        -- One mulligan: hand is 6, and the 1 bottomed card is at the library
        -- bottom, distinct (by id) from the six in hand.
        let after = run (mulliganUpTo 1) (libraryGame cards 20)
            handIds = Set.fromList (Game.zoneMembers Zone.Hand S.alice after)
            bottom1 = libBottom 1 S.alice after
         in HU.assertEqual "the bottomed card is not in hand" [] (filter (`Set.member` handIds) bottom1),
      HU.testCase "CR 103.5: the chosen bottom ORDER is honored, not just the set" $
        -- alice's deck (top to bottom) is 20 distinct printings, index 0 drawn
        -- first. Opening hand takes indices 0..6; round 1 (taken 0->1) returns
        -- them to the bottom, redraws indices 7..13, and bottoms (reversed)
        -- index 7 alone -- a singleton, no order to prove yet. Round 2 (taken
        -- 1->2) returns the surviving hand (8..13) to the bottom, redraws
        -- indices 14..19,0, and bottoms count=2 reversed: [15,14] -- humility
        -- then giantGrowth, a genuinely non-identity order. Round 3 (taken=2)
        -- keeps, ending the process.
        let printings = distinctPrintings cards
            gs0 = orderedLibraryGame cards printings
            after = run bottomReversedAnswer gs0
            bottomCards = fmap (\oid -> Game.cardOf oid after) (libBottom 2 S.alice after)
            -- The round-2 bottoming returns indices [15, 14] of distinctPrintings
            -- (humility, then giantGrowth); named directly to avoid a partial `!!`.
            expectedCards = fmap (Just . Printing.card) [Cards.humilityPrinting cards, Cards.giantGrowthPrinting cards]
         in HU.assertEqual "library bottom equals the chosen order exactly (humility, then giantGrowth)" expectedCards bottomCards,
      HU.testCase "CR 103.5: keeping is terminal -- a kept player is not asked again" $
        let gs0 = libraryGame cards 20
            ((_, _after), asked) = State.runState (Program.foldProgramM recordAsks (State.runStateT (Mulligan.openingHands [S.alice, S.bob]) gs0)) []
         in do
              HU.assertEqual "alice kept round 1, so was asked exactly once" 1 (length (filter (== S.alice) asked))
              HU.assertBool "bob mulliganed twice then kept, so was asked more than once" (length (filter (== S.bob) asked) > 1),
      HU.testCase "CR 103.5: a game with mulligans replays deterministically" $
        let gs0 = libraryGame cards 20
            ((_, recorded), responses) = Replay.record (mulliganUpTo 2) gs0 (Mulligan.openingHands [S.alice, S.bob])
            (_, replayed) = Replay.replay responses gs0 (Mulligan.openingHands [S.alice, S.bob])
         in do
              HU.assertEqual "alice hand matches" (S.handSize S.alice recorded) (S.handSize S.alice replayed)
              HU.assertEqual "alice library matches" (libSize S.alice recorded) (libSize S.alice replayed)
              HU.assertEqual
                "alice library ORDER matches"
                (Game.zoneMembers Zone.Library S.alice recorded)
                (Game.zoneMembers Zone.Library S.alice replayed),
      HU.testCase "CR 727.3/729.3: a short library still flags drewFromEmpty through the mulligan path" $
        -- bob has a 5-card library; his 7-card opening draw empties it and flags
        -- the failed draw. Driven by an actual mulligan (mulliganUpTo 1) so the
        -- flag is shown to SURVIVE the shuffle-back-and-redraw path, not merely
        -- the initial draw -- "regardless of any mulligans" (CR 727.3 / 729.3).
        let g0 = Setup.emptyGame S.bothPlayers
            addMany pid n g = List.foldl' (\h _ -> snd (S.addCreature (Cards.mountainPrinting cards) pid h)) g (replicate n ())
            gs0 = poolToLibrary S.bob (poolToLibrary S.alice (addMany S.bob 5 (addMany S.alice 20 g0)))
            after = run (mulliganUpTo 1) gs0
         in HU.assertBool "bob drew from an empty library" (Set.member S.bob (GameState.drewFromEmpty after))
    ]
