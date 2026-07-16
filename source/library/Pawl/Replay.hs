{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Pawl.Replay where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Pawl.Type.Action as Action
import Pawl.Type.Game (Game)
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.Program as Program
import Pawl.Type.Prompt (Prompt)
import qualified Pawl.Type.Prompt as Prompt
import Pawl.Type.Response (Response)
import qualified Pawl.Type.Response as Response

-- Flatten an answer into the log. The GADT refines 'r' per branch, so each
-- constructor pairs with the response that carries its payload.
encode :: Prompt r -> r -> Response
encode p answer = case p of
  Prompt.Shuffle _ -> Response.Shuffled answer
  Prompt.ChooseAction {} -> Response.ChoseAction answer
  Prompt.ChooseDiscard {} -> Response.ChoseDiscard answer

-- The inverse of 'encode'. Nothing when the logged response does not match the
-- prompt the engine is actually asking (a stale or foreign transcript).
decode :: Prompt r -> Response -> Maybe r
decode p response = case p of
  Prompt.Shuffle _ -> case response of
    Response.Shuffled ids -> Just ids
    Response.ChoseAction _ -> Nothing
    Response.ChoseDiscard _ -> Nothing
  Prompt.ChooseAction {} -> case response of
    Response.ChoseAction action -> Just action
    Response.Shuffled _ -> Nothing
    Response.ChoseDiscard _ -> Nothing
  Prompt.ChooseDiscard {} -> case response of
    Response.ChoseDiscard ids -> Just ids
    Response.ChoseAction _ -> Nothing
    Response.Shuffled _ -> Nothing

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
