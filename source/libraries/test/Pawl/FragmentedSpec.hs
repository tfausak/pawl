{-# LANGUAGE GADTs #-}

-- Covers Pawl.Engine.Fragmented and its call in Pawl.Engine.Engine.priorityLoop:
-- CR 732.3's fragmented loop, out of two synthetic printings -- Synthetic Self
-- Tapper ("{0}: Tap this creature") under alice and Synthetic Rouser ("{0}:
-- Untap target creature") under bob. No printing reaches the rule: Scryfall
-- `o:"{0}: untap"` and `o:"{0}: tap"`, 2026-10-03, answer only untaps limited
-- to one activation a turn or a game (Instill Energy, Nature's Chosen, Touch
-- of Vitae) and Chimeric Idol, which taps lands. CR 732.3's own example
-- undoes a continuous effect, which `Fragmented.digest` does not see recur
-- (gap #4650).
--
-- Two seats, so alice -- active, and involved -- is the named player by
-- construction. The answerer is test-local: it sequences two players through
-- one script and records each menu alice is offered, which the harness cannot.
module Pawl.FragmentedSpec where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.Action as Action
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "a fragmented loop (CR 732.3)" $ do
  Spec.it s "the active player may not repeat the action that returned the game to a state" $ do
    tapper <- S.printingOf s registry "Synthetic Self Tapper"
    rouser <- S.printingOf s registry "Synthetic Rouser"
    -- Alice taps her creature, bob untaps it, and the game is back where it
    -- began: her second offer there lacks the activation, and Pass remains.
    Spec.assertEqWith s "CR 732.3 alice's activation is withheld where the state recurs" (offers (loopBoard tapper rouser Nothing False)) [True, False]

  Spec.it s "a state that does not recur leaves the activation offered" $ do
    tapper <- S.printingOf s registry "Synthetic Self Tapper"
    rouser <- S.printingOf s registry "Synthetic Rouser"
    -- The control: bob untaps alice's OTHER creature, tapped at setup, so the
    -- lap ends in a state not seen before and alice may activate again. Only
    -- the lap after that one, whose untap is a no-op, recurs.
    Spec.assertEqWith s "CR 732.3 a new state is no loop" (offers (loopBoard tapper rouser Nothing True)) [True, True, False]

  Spec.it s "a card reading the turn's history keeps the loop's states distinct" $ do
    tapper <- S.printingOf s registry "Synthetic Self Tapper"
    rouser <- S.printingOf s registry "Synthetic Rouser"
    ghoul <- S.printingOf s registry "Khabál Ghoul"
    -- CR 732.3's example: "nothing in the game cares how many times an ability
    -- has been activated". Khabál Ghoul counts the turn's deaths off the log,
    -- from alice's library, so the log the digest drops is observable and no
    -- state recurs; the answerer's budget is what stops her.
    Spec.assertEqWith s "CR 732.3 the gate keeps every offer" (offers (loopBoard tapper rouser (Just ghoul) False)) (replicate (budget + 1) True)

-- How many times the answerer has alice activate before it passes instead,
-- since without CR 732.3 the loop never ends.
budget :: Int
budget = 3

-- alice, active in her precombat main phase with an empty stack, controls two
-- Synthetic Self Tappers, the second tapped when `otherTapped`; bob controls a
-- Synthetic Rouser and aims it at the first, or at the second when
-- `otherTapped` -- so the two boards differ in exactly bob's target and the
-- second creature's tap state. Nothing is scheduled after this phase.
loopBoard :: Printing.Printing -> Printing.Printing -> Maybe Printing.Printing -> Bool -> (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState)
loopBoard tapper rouser reader otherTapped =
  let base = Setup.emptyGame S.bothPlayers
      (first, gs1) = S.addPermanent tapper S.alice base
      (other, gs2) = S.addPermanent tapper S.alice gs1
      (rouserId, gs3) = S.addPermanent rouser S.bob gs2
      gs4 = maybe gs3 (\printing -> snd (S.addLibraryCard printing S.alice gs3)) reader
      placed =
        gs4
          { GameState.phase = Phase.PrecombatMain,
            GameState.remaining = Seq.empty,
            GameState.activePlayer = S.alice,
            GameState.priority = Just S.alice
          }
   in if otherTapped
        then (first, rouserId, other, S.tapObject other placed)
        else (first, rouserId, first, placed)

-- Run one priority loop on the board and answer whether alice's activation of
-- her first creature was on each menu she got at the head of a lap.
offers :: (ObjectId.ObjectId, ObjectId.ObjectId, ObjectId.ObjectId, GameState.GameState) -> [Bool]
offers (tapperId, rouserId, target, gs) =
  let ((_, _), (_, _, offered)) = State.runState (Engine.runGame (looping tapperId rouserId target) gs Engine.priorityLoop) (0, 0, [])
   in offered

-- One lap of the loop as a script, in stages: 0, alice at the lap's head
-- activates (within the budget) and the offer is recorded; 1, bob passes on
-- her ability; 2, bob activates the Rouser once it has resolved; 3, alice
-- passes on his. Every other prompt passes or declines.
looping :: ObjectId.ObjectId -> ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> State.State (Int, Int, [Bool]) r
looping tapperId rouserId target p = case p of
  Prompt.ChooseAction _ pid actions -> do
    (stage, activations, offered) <- State.get
    let activationsOf oid = filter (activates oid) actions
    case (stage :: Int, activationsOf tapperId, activationsOf rouserId) of
      (0, own, _) | pid == S.alice -> do
        let seen = offered <> [not (null own)]
        case own of
          action : _ | activations < budget -> do
            State.put (1, activations + 1, seen)
            pure action
          _ -> do
            State.put (0, activations, seen)
            pure Action.Pass
      (1, _, _) | pid == S.bob -> do
        State.put (2, activations, offered)
        pure Action.Pass
      (2, _, action : _) | pid == S.bob -> do
        State.put (3, activations, offered)
        pure action
      (3, _, _) | pid == S.alice -> do
        State.put (0, activations, offered)
        pure Action.Pass
      _ -> pure Action.Pass
  -- FILTERED out of the offer, never built (CR 608.2b re-reads it).
  Prompt.ChooseTargets _ _ _ sets -> pure (fmap (Set.filter (== Recipient.ToCreature target) . snd) sets)
  _ -> pure (S.identityAnswer p)

activates :: ObjectId.ObjectId -> Action.Action -> Bool
activates oid action = case action of
  Action.Activate source _ -> source == oid
  _ -> False
