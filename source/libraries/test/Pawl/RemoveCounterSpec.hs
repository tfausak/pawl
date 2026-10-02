{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers Pawl.Engine.Resolve's Effect.RemoveCountersAmong arm -- CR 608.2d's
-- removal of counters from among several permanents, the resolving controller
-- dividing them -- through one producer per Pawl.Types.RemovalCount arm, and
-- Eventide's Shadow for counters of any kind.
module Pawl.RemoveCounterSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CounterName as CounterName
import qualified Pawl.Types.CounterSpread as CounterSpread
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Pawl.Engine.Resolve" $ do
  lizrogSpec s registry
  overseerSpec s registry
  eventidesShadowSpec s registry

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

-- Eventide's Shadow {4}{B} Sorcery (Oracle text checked against Scryfall
-- 2026-09-27): "Remove any number of counters from among permanents on the
-- battlefield. You draw cards and lose life equal to the number of counters
-- removed this way."
--
-- WhichCounters.OfAnyKind as an effect: the division is by kind as well as by
-- permanent (CR 122.1), and the tally both later instructions read.
--
-- THE BOARD: alice holds the Shadow over five Swamps and controls a Goblin Piker
-- carrying a +1/+1 and a vigilance counter; bob's Hill Giant carries two +1/+1
-- counters and a stun counter. alice's library holds five cards.
eventidesShadowSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
eventidesShadowSpec s registry =
  let run answer board shadowId = State.runState (fmap snd (Engine.runGame (recordingMixed answer) board (S.cast S.alice shadowId >> Stack.resolveTop))) []
   in Spec.describe s "Eventide's Shadow" $ do
        -- The gameplay assertions first, the prompt record a proxy after them.
        Spec.it s "CR 122.1 / 608.2d alice removes counters of several kinds from among every permanent, then draws and loses that many" $ do
          (shadowId, pikerId, giantId, board) <- shadowBoard s registry
          let answer = Map.fromList [(pikerId, Map.singleton vigilance 1), (giantId, Map.fromList [(CounterKind.Stun, 1), (plusOne, 1)])]
              (after, asked) = run answer board shadowId
          Spec.assertEqWith s "CR 122.1 the Piker's vigilance counter came off" (S.counterOf vigilance pikerId after) 0
          Spec.assertEqWith s "and its +1/+1 counter stayed" (S.counterOf plusOne pikerId after) 1
          Spec.assertEqWith s "the Giant's stun counter came off" (S.counterOf CounterKind.Stun giantId after) 0
          Spec.assertEqWith s "and one of its two +1/+1 counters" (S.counterOf plusOne giantId after) 1
          Spec.assertEqWith s "CR 121.1 alice drew the three removed" (length (Game.zoneMembers Zone.Hand S.alice after)) 3
          Spec.assertEqWith s "CR 119.3 and lost three life" (S.lifeOf S.alice after) (Just 17)
          Spec.assertEqWith s "alice was asked once, from zero, over every permanent by kind" asked [(CounterSpread.FromAmongAtLeast, 0, Map.fromList [(pikerId, Map.fromList [(plusOne, 1), (vigilance, 1)]), (giantId, Map.fromList [(plusOne, 2), (CounterKind.Stun, 1)])])]

vigilance :: CounterKind.CounterKind Keyword.Keyword
vigilance = CounterKind.Keyword Keyword.Vigilance

-- The board eventidesShadowSpec's cases share, described above it.
shadowBoard :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> m (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
shadowBoard s registry = do
  shadow <- S.printingOf s registry "Eventide's Shadow"
  piker <- S.printingOf s registry "Goblin Piker"
  giant <- S.printingOf s registry "Hill Giant"
  swamp <- S.printingOf s registry "Swamp"
  let lands = S.landsFor swamp S.alice 5 (Setup.emptyGame S.bothPlayers)
      (shadowId, withShadow) = S.addHandCard shadow S.alice lands
      (pikerId, withPiker) = S.addPermanent piker S.alice withShadow
      (giantId, withGiant) = S.addPermanent giant S.bob withPiker
      stocked = foldr (\_ g -> snd (S.addLibraryCard swamp S.alice g)) withGiant [1 .. (5 :: Int)]
      counted =
        S.addCounter plusOne 1 pikerId
          . S.addCounter vigilance 1 pikerId
          . S.addCounter plusOne 2 giantId
          . S.addCounter CounterKind.Stun 1 giantId
          $ stocked
  pure
    ( shadowId,
      pikerId,
      giantId,
      counted
        { GameState.phase = Phase.PrecombatMain,
          GameState.activePlayer = S.alice,
          GameState.priority = Just S.alice
        }
    )

-- Answers Prompt.ChooseMixedCounterRemoval with the division given, recording
-- each prompt's spread, count and offer, and everything else identically.
-- PINNED, `recording`'s posture.
recordingMixed :: Map.Map ObjectId.ObjectId (Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural) -> Prompt.Prompt r -> State.State [(CounterSpread.CounterSpread, Natural, Map.Map ObjectId.ObjectId (Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural))] r
recordingMixed answer p = case p of
  Prompt.ChooseMixedCounterRemoval _ _ _ spread owed offered -> do
    State.modify' (<> [(spread, owed, offered)])
    pure answer
  _ -> pure (S.identityAnswer p)
