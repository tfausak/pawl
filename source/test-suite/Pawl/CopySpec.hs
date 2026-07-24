{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: Pawl.Replacement's EntryR AsCopy arm (the CR 614.12a copy choice, run
-- from inside Pawl.Event's changeZone), the P2 copy gate (Clone). Gameplay-level:
-- Clone enters via the zone-change funnel and its projected characteristics are
-- asserted.
module Pawl.CopySpec where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine as Engine
import qualified Pawl.Event as Event
import qualified Pawl.Game as Game
import qualified Pawl.Projection as Projection
import qualified Pawl.Registry as Registry
import qualified Pawl.Setup as Setup
import qualified Pawl.Stack as Stack
import qualified Pawl.Support as S
import qualified Pawl.Type.Card as Card.Type
import qualified Pawl.Type.CounterKind as CounterKind
import qualified Pawl.Type.GameState as GameState
import Pawl.Type.ObjectId (ObjectId)
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Registry as Registry.Type
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
  Prompt.OrderTriggers _ _ sources -> fmap fromIntegral (take (length sources) [0 :: Int ..])
  _ -> S.identityAnswer p

-- Resolve the stack top (a permanent enters -- the copy choice is now made INSIDE
-- that resolution, CR 614.12a) AND run the settle boundary (so a 0/0 Clone with
-- nothing to copy dies to the CR 704.5f state-based action), under the given
-- answerer.
resolveAndSettle :: (forall r. Prompt.Prompt r -> r) -> GameState.GameState -> GameState.GameState
resolveAndSettle answer gs =
  snd (Engine.runGamePure answer gs (Stack.resolveTop >> Engine.settleForPriority))

tests :: Registry.Type.Registry -> Tasty.TestTree
tests registry =
  Tasty.testGroup
    "Copy"
    [ HU.testCase "Clone copies a creature and projects its P/T (CR 707.2)" $ do
        piker <- Registry.printing registry "Goblin Piker"
        clone <- Registry.printing registry "Clone"
        let gs0 = Setup.emptyGame S.bothPlayers
            (pikerId, board) = S.addCreature piker S.alice gs0
            (_, staged) = S.spellOnStack clone S.alice board
            resolved = resolveAndSettle copyNewest staged
        case cloneOnBattlefield resolved of
          Nothing -> HU.assertFailure "Clone left the battlefield unexpectedly"
          Just cloneId -> do
            HU.assertEqual "Clone's power is the Piker's" (Just 2) (Projection.powerOf cloneId resolved)
            HU.assertEqual "Clone's toughness is the Piker's" (Just 1) (Projection.toughnessOf cloneId resolved)
            HU.assertBool "Clone is a creature" (Projection.isCreatureOf cloneId resolved)
            HU.assertBool "the copied Piker is untouched" (Projection.powerOf pikerId resolved == Just 2),
      HU.testCase "Clone with no creature to copy enters as a 0/0 and dies (CR 704.5f)" $ do
        clone <- Registry.printing registry "Clone"
        let gs0 = Setup.emptyGame S.bothPlayers
            (_, staged) = S.spellOnStack clone S.alice gs0
            resolved = resolveAndSettle copyNewest staged
        HU.assertEqual "the 0/0 Clone is gone (state-based action)" Nothing (cloneOnBattlefield resolved),
      HU.testCase "Clone copies base P/T, not a counter-boosted P/T (CR 707.2 falsifier)" $ do
        piker <- Registry.printing registry "Goblin Piker"
        clone <- Registry.printing registry "Clone"
        let gs0 = Setup.emptyGame S.bothPlayers
            (pikerId, board0) = S.addCreature piker S.alice gs0
            -- Put a +1/+1 counter on the Piker: projected 3/2, base 2/1.
            board = S.addCounter CounterKind.PlusOnePlusOne 1 pikerId board0
            (_, staged) = S.spellOnStack clone S.alice board
            resolved = resolveAndSettle copyNewest staged
        case cloneOnBattlefield resolved of
          Nothing -> HU.assertFailure "Clone left the battlefield unexpectedly"
          Just cloneId -> do
            HU.assertEqual "source is boosted to 3/2" (Just 3) (Projection.powerOf pikerId resolved)
            HU.assertEqual "Clone copies the base 2, not 3" (Just 2) (Projection.powerOf cloneId resolved)
            HU.assertEqual "Clone copies the base 1, not 2" (Just 1) (Projection.toughnessOf cloneId resolved),
      HU.testCase "Clone copies a creature's activated abilities (CR 707.2)" $ do
        prodigalSorcerer <- Registry.printing registry "Prodigal Sorcerer"
        clone <- Registry.printing registry "Clone"
        let gs0 = Setup.emptyGame S.bothPlayers
            (_, board) = S.addCreature prodigalSorcerer S.alice gs0
            (_, staged) = S.spellOnStack clone S.alice board
            resolved = resolveAndSettle copyNewest staged
        case cloneOnBattlefield resolved of
          Nothing -> HU.assertFailure "Clone left the battlefield unexpectedly"
          Just cloneId ->
            HU.assertBool
              "Clone has the copied activated ability"
              (not (null (Projection.abilitiesOf cloneId resolved))),
      HU.testCase "a copy of a copy resolves to the underlying creature (self-reference)" $ do
        piker <- Registry.printing registry "Goblin Piker"
        clone <- Registry.printing registry "Clone"
        let gs0 = Setup.emptyGame S.bothPlayers
            (_, board) = S.addCreature piker S.alice gs0
            (_, stagedA) = S.spellOnStack clone S.alice board
            afterA = resolveAndSettle copyNewest stagedA
            (_, stagedB) = S.spellOnStack clone S.alice afterA
            afterB = resolveAndSettle copyNewest stagedB
            -- Both Clones now name "Clone"; the newest (highest id) is B.
            afterBId = newest (clonesOnBattlefield afterB)
        case afterBId of
          Nothing -> HU.assertFailure "no Clones on the battlefield"
          Just bId -> do
            HU.assertEqual "the copy-of-a-copy is a 2/1" (Just 2) (Projection.powerOf bId afterB)
            HU.assertBool "the copy-of-a-copy is a creature" (Projection.isCreatureOf bId afterB),
      HU.testCase "a copy survives its source leaving the battlefield (CR 707.5 lock)" $ do
        piker <- Registry.printing registry "Goblin Piker"
        clone <- Registry.printing registry "Clone"
        let gs0 = Setup.emptyGame S.bothPlayers
            (pikerId, board) = S.addCreature piker S.alice gs0
            (_, staged) = S.spellOnStack clone S.alice board
            resolved = resolveAndSettle copyNewest staged
            afterKill = S.runPure S.identityAnswer resolved (Event.destroy pikerId)
        case cloneOnBattlefield afterKill of
          Nothing -> HU.assertFailure "Clone should survive the source's death"
          Just cloneId -> do
            HU.assertEqual "the source is gone" False (Set.member pikerId (GameState.battlefield afterKill))
            HU.assertEqual "the Clone is still a 2/1" (Just 2) (Projection.powerOf cloneId afterKill)
            HU.assertEqual "the Clone is still 1 toughness" (Just 1) (Projection.toughnessOf cloneId afterKill),
      HU.testCase "Clone of Tarmogoyf copies the ABILITY, so both recompute (CR 707.2a)" $ do
        -- THE FALSIFIER for snapshotting the NUMBER: CR 707.2a says a copy
        -- acquires the abilities of the object it copies, because those values are
        -- derived from its rules text. Seeding the CDA as an evaluated integer
        -- would freeze the Clone at the graveyards' contents at the moment it
        -- entered -- P2's deferred bill, paid here.
        lightningBolt <- Registry.printing registry "Lightning Bolt"
        tarmogoyf <- Registry.printing registry "Tarmogoyf"
        clone <- Registry.printing registry "Clone"
        piker <- Registry.printing registry "Goblin Piker"
        let gs0 = Setup.emptyGame S.bothPlayers
            (_, withBolt) = S.addGraveyardCard lightningBolt S.alice gs0
            (goyfId, board) = S.addCreature tarmogoyf S.alice withBolt
            (_, staged) = S.spellOnStack clone S.alice board
            resolved = resolveAndSettle copyNewest staged
            -- A second card type reaches a graveyard AFTER the Clone entered.
            (_, later) = S.addGraveyardCard piker S.bob resolved
        case cloneOnBattlefield resolved of
          Nothing -> HU.assertFailure "Clone did not reach the battlefield"
          Just cloneId -> do
            HU.assertEqual "at entry the Clone is the Goyf's 1/2" (Just 1) (Projection.powerOf cloneId resolved)
            HU.assertEqual "at entry, toughness 1+1" (Just 2) (Projection.toughnessOf cloneId resolved)
            HU.assertEqual "the source moves to 2" (Just 2) (Projection.powerOf goyfId later)
            HU.assertEqual "and so does the COPY" (Just 2) (Projection.powerOf cloneId later)
            HU.assertEqual "the copy's toughness moves too" (Just 3) (Projection.toughnessOf cloneId later)
    ]
