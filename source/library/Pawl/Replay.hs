{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Pawl.Replay where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Type.Action as Action
import Pawl.Type.Game (Game)
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.Program as Program
import Pawl.Type.Prompt (Prompt)
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Recipient as Recipient
import Pawl.Type.Response (Response)
import qualified Pawl.Type.Response as Response

-- Flatten an answer into the log. The GADT refines 'r' per branch, so each
-- constructor pairs with the response that carries its payload.
encode :: Prompt r -> r -> Response
encode p answer = case p of
  Prompt.Shuffle _ -> Response.Shuffled answer
  Prompt.ChooseAction {} -> Response.ChoseAction answer
  Prompt.ChooseDiscard {} -> Response.ChoseDiscard answer
  Prompt.DeclareAttackers {} -> Response.DeclaredAttackers answer
  Prompt.DeclareBlockers {} -> Response.DeclaredBlockers answer
  Prompt.AssignCombatDamage {} -> Response.AssignedCombatDamage answer
  Prompt.ChooseTargets {} -> Response.ChoseTargets answer

-- The inverse of 'encode'. Nothing when the logged response does not match the
-- prompt the engine is actually asking (a stale or foreign transcript).
--
-- The non-matching branches are a wildcard rather than written out. With six
-- prompts and six responses the explicit form is thirty-six branches, and the
-- exhaustiveness that protects us is on Prompt -- the GADT that refines r --
-- which is still total. A new Response constructor correctly decodes to Nothing.
decode :: Prompt r -> Response -> Maybe r
decode p response = case p of
  Prompt.Shuffle _ -> case response of
    Response.Shuffled ids -> Just ids
    _ -> Nothing
  Prompt.ChooseAction {} -> case response of
    Response.ChoseAction action -> Just action
    _ -> Nothing
  Prompt.ChooseDiscard {} -> case response of
    Response.ChoseDiscard ids -> Just ids
    _ -> Nothing
  Prompt.DeclareAttackers {} -> case response of
    Response.DeclaredAttackers ids -> Just ids
    _ -> Nothing
  Prompt.DeclareBlockers {} -> case response of
    Response.DeclaredBlockers assignment -> Just assignment
    _ -> Nothing
  Prompt.AssignCombatDamage {} -> case response of
    Response.AssignedCombatDamage assignment -> Just assignment
    _ -> Nothing
  Prompt.ChooseTargets {} -> case response of
    Response.ChoseTargets chosen -> Just chosen
    _ -> Nothing

-- The answer used when the transcript is exhausted or does not match. Keeping
-- this total is what lets 'replay' avoid a partial escape: an over-short log
-- degrades into a deterministic default rather than crashing.
defaultAnswer :: Prompt r -> r
defaultAnswer p = case p of
  Prompt.Shuffle ids -> ids
  Prompt.ChooseAction _ _ actions -> case actions of
    h : _ -> h
    [] -> Action.Pass
  Prompt.ChooseDiscard _ _ ids n -> take (fromIntegral n) ids
  -- Declining to attack or block is always legal, and is the least eventful
  -- thing a fallback can do.
  Prompt.DeclareAttackers {} -> []
  Prompt.DeclareBlockers {} -> Map.empty
  -- Must be a LEGAL division (Damage.legalAssignment), or the attacker deals
  -- nothing. All power onto the first blocker totals power with the defender at 0.
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    let blockers = filter isCreatureRecipient (Map.keys thresholds)
        isCreatureRecipient r = case r of
          Recipient.ToCreature _ -> True
          Recipient.ToPlayer _ -> False
     in case blockers of
          r : _ -> Map.singleton r n
          [] -> Map.empty
  -- One legal recipient per slot, chosen deterministically (the minimum). A
  -- slot with no legal recipient stays unfilled -- casting rejects that answer.
  Prompt.ChooseTargets _ _ _ sets -> Map.mapMaybe Set.lookupMin sets

-- Run a game under a base interpreter, keeping every answer in order.
record :: (forall r. Prompt r -> r) -> GameState -> Game a -> ((a, GameState), [Response])
record answer gs game =
  let step :: Prompt r -> State.State [Response] r
      step p = do
        let value = answer p
        State.modify' (encode p value :)
        pure value
      (outcome, logged) =
        State.runState (Program.foldProgramM step (State.runStateT game gs)) []
   in (outcome, reverse logged)

-- Re-run a game against a recorded transcript. Because the engine is pure and
-- every decision is a suspension, feeding back the same answers reproduces the
-- same final state — the M0 determinism criterion.
replay :: [Response] -> GameState -> Game a -> (a, GameState)
replay responses gs game =
  let step :: Prompt r -> State.State [Response] r
      step p = do
        remaining <- State.get
        case remaining of
          [] -> pure (defaultAnswer p)
          h : t -> case decode p h of
            Just value -> do
              State.put t
              pure value
            Nothing -> pure (defaultAnswer p)
   in State.evalState (Program.foldProgramM step (State.runStateT game gs)) responses
