{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.Resolve's Effect.RemoveCountersAmong arm -- CR 608.2d's
-- removal of counters of one kind from among several permanents, the resolving
-- controller dividing them -- through its three producers, one per
-- Pawl.Types.RemovalCount arm.
module Pawl.RemoveCounterSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterName as CounterName
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Resolve" $ do
  lizrogSpec s registry
  spiderManSpec s registry
  overseerSpec s registry

-- One CR 608.2d division prompt as the answerer below records it: which of the
-- three, its count, and its offer.
data Asked
  = AtLeast Natural (Map.Map ObjectId.ObjectId Natural)
  | Among Natural (Map.Map ObjectId.ObjectId Natural)
  | UpTo Natural (Map.Map ObjectId.ObjectId Natural)
  deriving (Eq, Show)

-- Answers every division prompt with the division given, recording each, and
-- everything else with `rest`. PINNED, Pawl.CostSpec's posture: an answerer
-- searching for a legal division would repair a mutated check.
recording :: Map.Map ObjectId.ObjectId Natural -> (forall a. Prompt.Prompt a -> a) -> Prompt.Prompt r -> State.State [Asked] r
recording answer rest p = case p of
  Prompt.ChooseCounterRemovalAtLeast _ _ _ n offered -> do
    State.modify' (<> [AtLeast n offered])
    pure answer
  Prompt.ChooseCounterRemovalAmong _ _ _ n offered -> do
    State.modify' (<> [Among n offered])
    pure answer
  Prompt.ChooseCounterRemovalUpTo _ _ _ n offered -> do
    State.modify' (<> [UpTo n offered])
    pure answer
  _ -> pure (rest p)

-- Exercises or declines every printed "may", and otherwise attacks with
-- everything, blocks with nothing and aims every target at `target`.
plan :: OptionalDecision.OptionalDecision -> Maybe ObjectId.ObjectId -> Prompt.Prompt r -> r
plan may target p = case p of
  Prompt.ChooseOptional {} -> may
  Prompt.ChooseTargets _ _ _ sets -> case target of
    Just oid -> fmap (const (Set.singleton (Recipient.ToCreature oid))) sets
    Nothing -> S.aggressiveAnswer p
  Prompt.DeclareBlockers {} -> Map.empty
  _ -> S.aggressiveAnswer p

plusOne :: CounterKind.CounterKind Keyword.Keyword
plusOne = CounterKind.PlusOnePlusOne

-- Galloping Lizrog {3}{G}{U} Creature -- Frog Lizard 3/3 (Oracle text checked
-- against Scryfall 2026-09-27): "Trample. When this creature enters, you may
-- remove any number of +1/+1 counters from among creatures you control. If you
-- do, put twice that many +1/+1 counters on this creature."
--
-- RemovalCount.AnyNumber, and the tally the second sentence reads.
--
-- THE BOARD: alice controls a Goblin Piker with two +1/+1 counters and a Hill
-- Giant with one; bob's Hill Giant carries three, and is no creature alice
-- controls. The Lizrog enters with its trigger pending.
lizrogSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
lizrogSpec s registry =
  let run may answer board = State.runState (fmap snd (Engine.runGame (recording answer (plan may Nothing)) board (Engine.placePendingTriggers >> Stack.resolveTop))) []
   in Spec.describe s "Galloping Lizrog" $ do
        -- The gameplay assertion first, the prompt record a proxy after it.
        Spec.it s "CR 608.2d the controller removes any number from among their creatures, and it gets twice that many" $ do
          (lizrogId, pikerId, giantId, bobsId, board) <- lizrogBoard s registry
          let (after, asked) = run OptionalDecision.Exercises (Map.singleton pikerId 2) board
          Spec.assertEqWith s "CR 122.6 the Lizrog has twice the two removed" (S.counterOf plusOne lizrogId after) 4
          Spec.assertEqWith s "CR 122.1 both came off the Piker" (S.counterOf plusOne pikerId after) 0
          Spec.assertEqWith s "and the Giant kept its one" (S.counterOf plusOne giantId after) 1
          Spec.assertEqWith s "and bob's Giant its three" (S.counterOf plusOne bobsId after) 3
          Spec.assertEqWith s "alice was asked once, from zero, over her two creatures" asked [AtLeast 0 (Map.fromList [(pikerId, 2), (giantId, 1)])]
        -- None is an answer to "any number", and the tally is then zero.
        Spec.it s "CR 608.2d removing none leaves every counter and adds none" $ do
          (lizrogId, pikerId, giantId, _, board) <- lizrogBoard s registry
          let (after, _) = run OptionalDecision.Exercises Map.empty board
          Spec.assertEqWith s "the Lizrog has none" (S.counterOf plusOne lizrogId after) 0
          Spec.assertEqWith s "and the Piker and Giant keep theirs" (S.counterOf plusOne pikerId after, S.counterOf plusOne giantId after) (2, 1)
        -- A division taking more than a creature carries is refused, not
        -- repaired into another one.
        Spec.it s "CR 608.2d an answer past what a creature carries removes nothing" $ do
          (lizrogId, pikerId, _, _, board) <- lizrogBoard s registry
          let (after, _) = run OptionalDecision.Exercises (Map.singleton pikerId 3) board
          Spec.assertEqWith s "the Lizrog has none" (S.counterOf plusOne lizrogId after) 0
          Spec.assertEqWith s "and the Piker keeps both" (S.counterOf plusOne pikerId after) 2
        Spec.it s "CR 603.5 declining the may asks nothing and removes nothing" $ do
          (lizrogId, pikerId, _, _, board) <- lizrogBoard s registry
          let (after, asked) = run OptionalDecision.Declines (Map.singleton pikerId 2) board
          Spec.assertEqWith s "the Lizrog has none" (S.counterOf plusOne lizrogId after) 0
          Spec.assertEqWith s "and the Piker keeps both" (S.counterOf plusOne pikerId after) 2
          Spec.assertEqWith s "and nothing was asked" asked []

-- The board lizrogSpec's cases share, described above it.
lizrogBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
lizrogBoard s registry = do
  lizrog <- S.printingOf s registry "Galloping Lizrog"
  piker <- S.printingOf s registry "Goblin Piker"
  giant <- S.printingOf s registry "Hill Giant"
  let (pikerId, withPiker) = S.addPermanent piker S.alice (Setup.emptyGame S.bothPlayers)
      (giantId, withGiant) = S.addPermanent giant S.alice withPiker
      (bobsId, withBobs) = S.addPermanent giant S.bob withGiant
      counted = S.addCounter plusOne 2 pikerId (S.addCounter plusOne 1 giantId (S.addCounter plusOne 3 bobsId withBobs))
      (lizrogId, entered) = S.entersWithTrigger lizrog S.alice counted
  pure (lizrogId, pikerId, giantId, bobsId, entered)

-- Sensational Spider-Man {1}{W}{U} Legendary Creature -- Spider Human Hero 3/3
-- (Oracle text checked against Scryfall 2026-09-27): "Whenever Sensational
-- Spider-Man attacks, tap target creature defending player controls and put a
-- stun counter on it. Then you may remove up to three stun counters from among
-- all permanents. Draw cards equal to the number of stun counters removed this
-- way."
--
-- RemovalCount.UpTo, and a tally a Draw reads.
--
-- THE BOARD: alice attacks with Spider-Man; bob defends with a Goblin Piker,
-- which the trigger targets, and a Hill Giant carrying the stun counters given.
-- alice's library holds five cards.
spiderManSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spiderManSpec s registry =
  let stun = CounterKind.Stun
      run answer (board, pikerId) = State.runState (fmap snd (Engine.runGame (recording answer (plan OptionalDecision.Exercises (Just pikerId))) board S.combatGame)) []
      drawn board after = S.handSize S.alice after - S.handSize S.alice board
   in Spec.describe s "Sensational Spider-Man" $ do
        Spec.it s "CR 608.2d up to three stun counters from among all permanents, and a card for each" $ do
          (board, pikerId, giantId) <- spiderManBoard s registry 2
          let (after, asked) = run (Map.fromList [(pikerId, 1), (giantId, 2)]) (board, pikerId)
          Spec.assertEqWith s "CR 121.2 alice drew three" (drawn board after) 3
          Spec.assertEqWith s "CR 122.1 the Piker's new stun counter came off" (S.counterOf stun pikerId after) 0
          Spec.assertEqWith s "and both of the Giant's" (S.counterOf stun giantId after) 0
          Spec.assertEqWith s "alice was asked once, capped at three, over both" asked [UpTo 3 (Map.fromList [(pikerId, 1), (giantId, 2)])]
        -- Fewer than the cap is an answer.
        Spec.it s "CR 608.2d fewer than three is an answer, and draws that many" $ do
          (board, pikerId, giantId) <- spiderManBoard s registry 2
          let (after, _) = run (Map.singleton giantId 1) (board, pikerId)
          Spec.assertEqWith s "CR 121.2 alice drew one" (drawn board after) 1
          Spec.assertEqWith s "and the Piker and Giant keep one apiece" (S.counterOf stun pikerId after, S.counterOf stun giantId after) (1, 1)
        -- The cap: four counters are there, and an answer taking all of them is
        -- refused rather than cut down.
        Spec.it s "CR 608.2d an answer above three removes nothing" $ do
          (board, pikerId, giantId) <- spiderManBoard s registry 3
          let (after, _) = run (Map.fromList [(pikerId, 1), (giantId, 3)]) (board, pikerId)
          Spec.assertEqWith s "alice drew nothing" (drawn board after) 0
          Spec.assertEqWith s "and the Piker and Giant keep theirs" (S.counterOf stun pikerId after, S.counterOf stun giantId after) (1, 3)

-- The board spiderManSpec's cases share, described above it, with the Giant's
-- stun counters given.
spiderManBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Natural -> m (GameState.GameState, ObjectId.ObjectId, ObjectId.ObjectId)
spiderManBoard s registry stunned = do
  spider <- S.printingOf s registry "Sensational Spider-Man"
  piker <- S.printingOf s registry "Goblin Piker"
  giant <- S.printingOf s registry "Hill Giant"
  forest <- S.printingOf s registry "Forest"
  let (gs0, _, theirs) = S.combatBoardOf [spider] [piker, giant]
      stock g = snd (S.addLibraryCard forest S.alice g)
      stocked = stock (stock (stock (stock (stock gs0))))
  case theirs of
    [pikerId, giantId] -> pure (S.addCounter CounterKind.Stun stunned giantId stocked, pikerId, giantId)
    _ -> Spec.assertFailure s "fixture should give bob a Piker and a Giant"

-- Overseer of Vault 76 {2}{W} Legendary Creature -- Human Advisor 3/3 (Oracle
-- text checked against Scryfall 2026-09-27): "First Contact -- Whenever Overseer
-- of Vault 76 or another creature you control with power 3 or less enters, put a
-- quest counter on Overseer of Vault 76. At the beginning of combat on your turn,
-- you may remove three quest counters from among permanents you control. When
-- you do, put a +1/+1 counter on each creature you control and they gain
-- vigilance until end of turn."
--
-- RemovalCount.Exactly, and CR 608.2d's impossibility: "remove three" is no
-- option where fewer are there (its 2024-03-08 ruling: "exactly three").
--
-- THE BOARD: alice controls the Overseer and a Forest, each carrying the quest
-- counters given, and a Hill Giant; bob's Hill Giant carries four. It is the
-- beginning of combat on alice's turn.
overseerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
overseerSpec s registry =
  let run answer board = State.runState (fmap snd (Engine.runGame (recording answer (plan OptionalDecision.Exercises Nothing)) board Engine.runStep)) []
   in Spec.describe s "Overseer of Vault 76" $ do
        Spec.it s "CR 608.2d three quest counters divided among alice's permanents, and when she does, the boost" $ do
          (overseerId, forestId, giantId, bobsId, board) <- overseerBoard s registry 2 2
          let (after, asked) = run (Map.fromList [(overseerId, 1), (forestId, 2)]) board
          Spec.assertEqWith s "CR 603.12 the Giant got a +1/+1 counter" (S.counterOf plusOne giantId after) 1
          Spec.assertEqWith s "and the Overseer" (S.counterOf plusOne overseerId after) 1
          Spec.assertBool s (Projection.hasKeyword Keyword.Vigilance giantId after) "and the Giant has vigilance"
          Spec.assertEqWith s "CR 122.1 one came off the Overseer" (S.counterOf quest overseerId after) 1
          Spec.assertEqWith s "and both off the Forest" (S.counterOf quest forestId after) 0
          Spec.assertEqWith s "and bob's Giant kept its four" (S.counterOf quest bobsId after) 4
          Spec.assertEqWith s "alice was asked once, for three, over her two" asked [Among 3 (Map.fromList [(overseerId, 2), (forestId, 2)])]
        -- A division that does not add up to three is not taken: the counters
        -- come off in order instead, three and no more.
        Spec.it s "CR 608.2d a division of four is refused for the division of three in order" $ do
          (overseerId, forestId, _, _, board) <- overseerBoard s registry 2 2
          let (after, _) = run (Map.fromList [(overseerId, 2), (forestId, 2)]) board
          Spec.assertEqWith s "both came off the Overseer" (S.counterOf quest overseerId after) 0
          Spec.assertEqWith s "and one off the Forest" (S.counterOf quest forestId after) 1
        -- The pair: the same board with one quest counter fewer on the Forest.
        -- Exactly three is then the only division, so nothing is asked.
        Spec.it s "CR 608.2d exactly three between them is the one division, so nothing is asked" $ do
          (overseerId, forestId, giantId, _, board) <- overseerBoard s registry 2 1
          let (after, asked) = run Map.empty board
          Spec.assertEqWith s "CR 603.12 the Giant got a +1/+1 counter" (S.counterOf plusOne giantId after) 1
          Spec.assertEqWith s "and every quest counter came off" (S.counterOf quest overseerId after, S.counterOf quest forestId after) (0, 0)
          Spec.assertEqWith s "and nothing was asked" asked []
        -- And one fewer again: two is not three, so the "may" is no option at all
        -- and the reflexive ability never triggers.
        Spec.it s "CR 608.2d with two quest counters the removal is impossible, so nothing happens" $ do
          (overseerId, forestId, giantId, _, board) <- overseerBoard s registry 2 0
          let (after, _) = run Map.empty board
          Spec.assertEqWith s "the Giant got no +1/+1 counter" (S.counterOf plusOne giantId after) 0
          Spec.assertEqWith s "and the Overseer kept both" (S.counterOf quest overseerId after) 2
          Spec.assertEqWith s "and the Forest has none" (S.counterOf quest forestId after) 0

quest :: CounterKind.CounterKind Keyword.Keyword
quest = CounterKind.Named (CounterName.UnsafeMkCounterName (Text.pack "quest"))

-- The board overseerSpec's cases share, described above it. The counts are the
-- Overseer's and the Forest's.
overseerBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> Natural -> Natural -> m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
overseerBoard s registry onOverseer onForest = do
  overseer <- S.printingOf s registry "Overseer of Vault 76"
  forest <- S.printingOf s registry "Forest"
  giant <- S.printingOf s registry "Hill Giant"
  let (overseerId, withOverseer) = S.addPermanent overseer S.alice (Setup.emptyGame S.bothPlayers)
      (forestId, withForest) = S.addPermanent forest S.alice withOverseer
      (giantId, withGiant) = S.addPermanent giant S.alice withForest
      (bobsId, withBobs) = S.addPermanent giant S.bob withGiant
      counted = S.addCounter quest onOverseer overseerId (S.addCounter quest onForest forestId (S.addCounter quest 4 bobsId withBobs))
      beginning = Phase.Combat CombatStep.BeginningOfCombat
  pure
    ( overseerId,
      forestId,
      giantId,
      bobsId,
      counted
        { GameState.activePlayer = S.alice,
          GameState.phase = beginning,
          GameState.remaining = S.phasesAfter beginning
        }
    )
