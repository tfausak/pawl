{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Pawl.Replay where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Cost as Cost
import qualified Pawl.Type.Action as Action
import qualified Pawl.Type.Concession as Concession
import Pawl.Type.Game (Game)
import Pawl.Type.GameState (GameState)
import qualified Pawl.Type.MulliganDecision as MulliganDecision
import qualified Pawl.Type.OptionalDecision as OptionalDecision
import qualified Pawl.Type.Program as Program
import Pawl.Type.Prompt (Prompt)
import qualified Pawl.Type.Prompt as Prompt
import qualified Pawl.Type.Recipient as Recipient
import Pawl.Type.Response (Response)
import qualified Pawl.Type.Response as Response
import qualified Pawl.Type.Subtype as Subtype

-- Flatten an answer into the log. The GADT refines 'r' per branch, so each
-- constructor pairs with the response that carries its payload.
encode :: Prompt r -> r -> Response
encode p answer = case p of
  Prompt.Shuffle _ -> Response.Shuffled answer
  Prompt.RandomFirstPlayer _ -> Response.DeterminedFirstPlayer answer
  Prompt.ChooseAction {} -> Response.ChoseAction answer
  Prompt.Concede _ -> Response.Conceded answer
  Prompt.ChooseDiscard {} -> Response.ChoseDiscard answer
  Prompt.ChooseDefender {} -> Response.ChoseDefender answer
  Prompt.ChooseManaSource {} -> Response.ChoseManaSource answer
  Prompt.ChooseManaType {} -> Response.ChoseManaType answer
  Prompt.ChooseProliferate {} -> Response.ChoseProliferation answer
  Prompt.ChooseLegend {} -> Response.ChoseLegend answer
  Prompt.DeclareAttackers {} -> Response.DeclaredAttackers answer
  Prompt.DeclareBlockers {} -> Response.DeclaredBlockers answer
  Prompt.AssignCombatDamage {} -> Response.AssignedCombatDamage answer
  Prompt.ChooseTargets {} -> Response.ChoseTargets answer
  Prompt.ChooseBasicLandTypes {} -> Response.ChoseBasicLandTypes answer
  Prompt.SearchLibrary {} -> Response.Searched answer
  Prompt.CastWhileSearching {} -> Response.CastWhileSearched answer
  Prompt.ChooseX {} -> Response.ChoseX answer
  Prompt.ChooseModes {} -> Response.ChoseModes answer
  Prompt.ChooseCopyTarget {} -> Response.ChoseCopyTarget answer
  Prompt.ChooseEntryOption {} -> Response.ChoseEntryOption answer
  Prompt.OrderTriggers {} -> Response.OrderedTriggers answer
  Prompt.ChooseReplacement {} -> Response.ChoseReplacement answer
  Prompt.ChooseBoundToken {} -> Response.ChoseBoundToken answer
  Prompt.ChooseSacrifices {} -> Response.ChoseSacrifices answer
  Prompt.ChooseCost {} -> Response.ChoseCost answer
  Prompt.DeclareMulligan {} -> Response.DeclaredMulligan answer
  Prompt.Bottom {} -> Response.PutOnBottom answer
  Prompt.MulliganAction {} -> Response.TookMulliganAction answer
  Prompt.OpeningHandAction {} -> Response.TookOpeningHandAction answer
  Prompt.ChooseOptional {} -> Response.ChoseOptional answer

-- The inverse of 'encode'. Nothing when the logged response does not match the
-- prompt the engine is actually asking (a stale or foreign transcript).
--
-- The non-matching branches are a wildcard rather than written out. Writing
-- every (Prompt, Response) pair explicitly is quadratic in the number of
-- prompt constructors, and the exhaustiveness that protects us is on Prompt --
-- the GADT that refines r -- which is still total. A new Response constructor
-- correctly decodes to Nothing.
decode :: Prompt r -> Response -> Maybe r
decode p response = case p of
  Prompt.Shuffle _ -> case response of
    Response.Shuffled ids -> Just ids
    _ -> Nothing
  Prompt.RandomFirstPlayer _ -> case response of
    Response.DeterminedFirstPlayer pid -> Just pid
    _ -> Nothing
  Prompt.ChooseAction {} -> case response of
    Response.ChoseAction action -> Just action
    _ -> Nothing
  Prompt.Concede _ -> case response of
    Response.Conceded concession -> Just concession
    _ -> Nothing
  Prompt.ChooseDiscard {} -> case response of
    Response.ChoseDiscard ids -> Just ids
    _ -> Nothing
  Prompt.ChooseDefender {} -> case response of
    Response.ChoseDefender pid -> Just pid
    _ -> Nothing
  Prompt.ChooseManaSource {} -> case response of
    Response.ChoseManaSource oid -> Just oid
    _ -> Nothing
  Prompt.ChooseManaType {} -> case response of
    Response.ChoseManaType mt -> Just mt
    _ -> Nothing
  Prompt.ChooseProliferate {} -> case response of
    Response.ChoseProliferation chosen -> Just chosen
    _ -> Nothing
  Prompt.ChooseLegend {} -> case response of
    Response.ChoseLegend oid -> Just oid
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
  Prompt.ChooseBasicLandTypes {} -> case response of
    Response.ChoseBasicLandTypes pair -> Just pair
    _ -> Nothing
  Prompt.SearchLibrary {} -> case response of
    Response.Searched found -> Just found
    _ -> Nothing
  Prompt.CastWhileSearching {} -> case response of
    Response.CastWhileSearched found -> Just found
    _ -> Nothing
  Prompt.ChooseX {} -> case response of
    Response.ChoseX n -> Just n
    _ -> Nothing
  Prompt.ChooseModes {} -> case response of
    Response.ChoseModes modes -> Just modes
    _ -> Nothing
  Prompt.ChooseCopyTarget {} -> case response of
    Response.ChoseCopyTarget m -> Just m
    _ -> Nothing
  Prompt.ChooseEntryOption {} -> case response of
    Response.ChoseEntryOption n -> Just n
    _ -> Nothing
  Prompt.OrderTriggers {} -> case response of
    Response.OrderedTriggers order -> Just order
    _ -> Nothing
  Prompt.ChooseReplacement {} -> case response of
    Response.ChoseReplacement n -> Just n
    _ -> Nothing
  Prompt.ChooseBoundToken {} -> case response of
    Response.ChoseBoundToken oid -> Just oid
    _ -> Nothing
  Prompt.ChooseSacrifices {} -> case response of
    Response.ChoseSacrifices ids -> Just ids
    _ -> Nothing
  Prompt.ChooseCost {} -> case response of
    Response.ChoseCost cost -> Just cost
    _ -> Nothing
  Prompt.DeclareMulligan {} -> case response of
    Response.DeclaredMulligan decision -> Just decision
    _ -> Nothing
  Prompt.Bottom {} -> case response of
    Response.PutOnBottom ids -> Just ids
    _ -> Nothing
  Prompt.MulliganAction {} -> case response of
    Response.TookMulliganAction moid -> Just moid
    _ -> Nothing
  Prompt.OpeningHandAction {} -> case response of
    Response.TookOpeningHandAction moid -> Just moid
    _ -> Nothing
  Prompt.ChooseOptional {} -> case response of
    Response.ChoseOptional decision -> Just decision
    _ -> Nothing

-- The answer used when the transcript is exhausted or does not match. Keeping
-- this total is what lets 'replay' avoid a partial escape: an over-short log
-- degrades into a deterministic default rather than crashing.
defaultAnswer :: Prompt r -> r
defaultAnswer p = case p of
  Prompt.Shuffle ids -> ids
  -- CR 729.2: the head of the turn order is always a legal starting player, and
  -- is the least eventful fallback when a transcript runs short -- it is what a
  -- subgame did before randomness had a channel at all.
  Prompt.RandomFirstPlayer order -> NonEmpty.head order
  Prompt.ChooseAction _ _ actions -> case actions of
    h : _ -> h
    [] -> Action.Pass
  -- CR 104.3a: not conceding is always legal and is the least eventful fallback
  -- when a transcript runs short.
  Prompt.Concede _ -> Concession.Continues
  Prompt.ChooseDiscard _ _ ids n -> List.genericTake n ids
  -- CR 507.1: the first candidate is always a legal answer (the prompt is only
  -- asked with candidates) and is the least eventful fallback when a transcript
  -- runs short. NonEmpty.head is total.
  Prompt.ChooseDefender _ _ candidates -> NonEmpty.head candidates
  -- Any candidate pays; the head is the least eventful fallback. NonEmpty.head is total.
  Prompt.ChooseManaSource _ _ candidates -> NonEmpty.head candidates
  -- Every offered type is producible (tapForMana only offers what the source can
  -- make), so the head is a legal answer and the least eventful fallback.
  Prompt.ChooseManaType _ _ _ candidates -> NonEmpty.head candidates
  -- CR 701.34a: "any number" includes none, and declining is always legal -- the
  -- least eventful thing a fallback can do, the same posture as declining to
  -- attack or block.
  Prompt.ChooseProliferate {} -> (Set.empty, Set.empty)
  -- CR 704.5j: every candidate is a legal thing to keep, so the head is a legal
  -- answer and the least eventful fallback.
  Prompt.ChooseLegend _ _ candidates -> NonEmpty.head candidates
  -- Declining to attack is always legal, and is the least eventful thing a
  -- fallback can do.
  Prompt.DeclareAttackers {} -> []
  -- Declining to BLOCK is not always legal -- a CR 509.1c requirement (Lure) can
  -- make the empty declaration the one illegal answer. Still the least eventful
  -- fallback, and still total: Combat.declareBlockers repairs an illegal
  -- declaration to Combat.forcedBlockDeclaration rather than dropping it.
  Prompt.DeclareBlockers {} -> Map.empty
  -- Must be a LEGAL division (Damage.legalAssignment), or the attacker deals
  -- nothing. All power onto the first blocker totals power with the defender at 0.
  Prompt.AssignCombatDamage _ _ _ thresholds n ->
    let blockers = filter isCreatureRecipient (Map.keys thresholds)
        isCreatureRecipient r = case r of
          Recipient.ToCreature _ -> True
          Recipient.ToPlayer _ -> False
          Recipient.ToObject _ -> False
     in case blockers of
          r : _ -> Map.singleton r n
          [] -> Map.empty
  -- One legal recipient per slot, chosen deterministically (the minimum). A
  -- slot with no legal recipient stays unfilled -- casting rejects that answer.
  Prompt.ChooseTargets _ _ _ sets -> Map.mapMaybe Set.lookupMin sets
  -- A canonical identity hack (Mountain -> Mountain changes nothing): the fallback
  -- when a transcript runs short on a text-changer's binding.
  Prompt.ChooseBasicLandTypes {} -> (Subtype.Mountain, Subtype.Mountain)
  -- CR 701.23b: failing to find is always legal, and is the least eventful
  -- fallback when a transcript runs short on a search.
  Prompt.SearchLibrary {} -> Nothing
  -- Declining the re-entrant cast is always legal -- the least eventful fallback
  -- when a transcript runs short.
  Prompt.CastWhileSearching {} -> Nothing
  -- CR 601.2b: X=0 is always payable and the least eventful fallback when a
  -- transcript runs short on a variable-cost cast.
  Prompt.ChooseX {} -> 0
  -- The first `count` legal modes, deterministically -- the least eventful
  -- fallback when a transcript runs short on a modal cast.
  Prompt.ChooseModes _ _ _ legal count -> Set.fromList (List.genericTake count (Set.toAscList legal))
  -- CR 707.5: declining to copy is always legal, and is the least eventful
  -- fallback -- Clone is a deterministic fixture, never in a random deck, so
  -- this is never exercised in play.
  Prompt.ChooseCopyTarget {} -> Nothing
  -- CR 208.2b: the first offered shape is always a legal answer (this is asked
  -- only when the list has two or more), and is the least eventful fallback.
  Prompt.ChooseEntryOption {} -> 0
  -- CR 603.3b: the canonical order is always a legal answer, and is the least
  -- eventful fallback when a transcript runs short.
  Prompt.OrderTriggers _ _ sources -> zipWith const [0 ..] sources
  -- CR 616.1: index 0 is always a legal answer (the bucket is non-empty when this
  -- is asked), and is the least eventful fallback when a transcript runs short.
  Prompt.ChooseReplacement {} -> 0
  -- CR 603.7c: every minted token is a legal thing for "it" to name, so the head
  -- is a legal answer and the least eventful fallback when a transcript runs
  -- short -- it is exactly what the engine bound before the choice had a channel.
  Prompt.ChooseBoundToken _ _ _ candidates -> NonEmpty.head candidates
  -- The first `count` candidates, which the engine offers in ascending order --
  -- a legal answer whenever the prompt was legal to ask, and the least eventful
  -- fallback when a transcript runs short.
  Prompt.ChooseSacrifices _ _ _ candidates count -> Set.fromList (List.genericTake count candidates)
  -- The first offered candidate is the PRINTED cost for a cast from hand
  -- (Pawl.Cost.costsFor puts it first) -- the least eventful fallback when a
  -- transcript runs short, since it sacrifices nothing. A cast from the graveyard
  -- offers only rule 702.34a's flashback cost, so the head is the sole candidate
  -- and this prompt is not raised at all. Cost.firstOffered keeps this total for
  -- the empty list the engine never produces.
  Prompt.ChooseCost _ _ _ candidates -> Cost.firstOffered candidates
  -- CR 103.5: keeping is always legal and the least-eventful fallback when a
  -- transcript runs short (mirrors Concede -> Continues).
  Prompt.DeclareMulligan {} -> MulliganDecision.Keep
  -- A legal ordered subset of the redrawn hand, deterministically the first
  -- `count` -- the least-eventful fallback when a transcript runs short.
  Prompt.Bottom _ _ hand count -> List.genericTake count hand
  -- CR 103.5b: declining is always legal and the least-eventful fallback when a
  -- transcript runs short (mirrors DeclareMulligan -> Keep).
  Prompt.MulliganAction {} -> Nothing
  -- CR 103.6: declining is always legal and the least-eventful fallback when a
  -- transcript runs short (mirrors MulliganAction -> Nothing).
  Prompt.OpeningHandAction {} -> Nothing
  -- CR 603.5: declining a "may" is always legal and changes nothing, the
  -- least-eventful fallback when a transcript runs short (mirrors Concede ->
  -- Continues and MulliganAction -> Nothing).
  Prompt.ChooseOptional {} -> OptionalDecision.Declines

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
