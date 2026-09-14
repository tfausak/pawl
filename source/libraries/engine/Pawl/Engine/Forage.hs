-- | CR 701.61, forage: "Exile three cards from your graveyard or sacrifice a
-- Food", and the whole of the keyword action.
--
-- Pawl.Engine.Blight's sibling, and standing on the same ground: rule 701 is a
-- keyword-action rule exactly as rule 702 is a keyword rule, so the procedure
-- lives in the engine rather than in card data. The closed\/open invariant forbids
-- the rules core casing on an EFFECT's identity, and nothing here does --
-- Pawl.Engine.Resolve.Effect's Effect.Forage arm calls in without saying which
-- it is.
--
-- ONE module and not two procedures, for Pawl.Engine.Blight's reason: rule
-- 701.61a is one rule however a card demands it: CR 601.2f\/602.1b make it a cost
-- (Thornvault Forager) and an effect asks for it outright (Treetop Sentries), and
-- both come here.
module Pawl.Engine.Forage where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Types.ForageMode as ForageMode
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.Zone as Zone

-- | CR 701.61a's first candidate set: every card in this player's OWN graveyard,
-- unnarrowed -- the rule qualifies by zone and by nothing else. Per-owner by CR
-- 400.3 with CR 108.4, a card in a graveyard having no controller.
--
-- In the graveyard's own order (CR 404.2), which is the order Game.zoneMembers
-- answers, so the forced-choice shortcut below and a transcript are
-- deterministic.
exileCandidates :: PlayerId -> GameState.GameState -> [ObjectId]
exileCandidates = Game.zoneMembers Zone.Graveyard

-- | CR 701.61a's second candidate set: every permanent this player controls with
-- the Food subtype. Read off the PROJECTION (CR 613), so a permanent that is a
-- Food only by a continuous effect is a candidate and one that has stopped being
-- one is not.
--
-- Subtype and not card type: Food is an artifact type (CR 205.3g), and a creature
-- carrying it (Gingerbrute) is as much a Food as a bare artifact is.
--
-- ASCENDING, so the single-Food shortcut below and a transcript are
-- deterministic -- Pawl.Engine.Blight.candidates' posture.
foodCandidates :: PlayerId -> GameState.GameState -> [ObjectId]
foodCandidates pid gs =
  List.sort (filter (\oid -> Set.member Subtype.Food (Projection.subtypesOf oid gs)) (Projection.controls pid gs))

-- | CR 608.2d: can this player forage at all? Neither half of rule 701.61a can
-- be carried out by a player holding fewer than three cards in their graveyard
-- and controlling no Food, so "you may forage" is not offered to them.
--
-- The CLASSIFICATION Pawl.Engine.Resolve.Effect.effectIsImpossible reads. Rule
-- 701.61a states no gate of its own the way CR 701.59b does for collecting
-- evidence; what refuses the offer is CR 608.2d's "the player can't choose an
-- option that's illegal or impossible".
canForage :: PlayerId -> GameState.GameState -> Bool
canForage pid gs = length (exileCandidates pid gs) >= 3 || not (null (foodCandidates pid gs))

-- | CR 701.61a: exile three cards from this player's graveyard, or sacrifice a
-- Food they control. Answers whether the forage happened -- False is the board
-- canForage refuses, which CR 101.3 makes a no-op.
--
-- The ObjectId is the object the prompts name: the spell or ability resolving.
--
-- THREE PROMPTS in all, each raised only where the rules leave something to ask.
-- Which half, where both halves can be carried out; which three cards, where the
-- graveyard holds more than three; which Food, where the player controls more
-- than one. A half that cannot be carried out is not offered (CR 608.2d), which
-- is what makes a lone half no question at all.
--
-- CHOOSE, not target: rule 701.61a says "three cards from your graveyard" and "a
-- Food" without saying "target", so nothing was declared on the stack (CR
-- 601.2c) and there is no CR 608.2b legality to re-check.
--
-- FILTERED, NOT TRUSTED, Pawl.Engine.Blight's posture: an answer naming
-- something never offered falls back to the offered set's own front. An effect
-- has no "unpaid" to answer with, so reject-not-repair (Pawl.Engine.Cost's) is
-- not available here.
forage :: PlayerId -> ObjectId -> Game Bool
forage pid resolving = do
  gs <- State.get
  let cards = exileCandidates pid gs
      foods = foodCandidates pid gs
      canExile = length cards >= 3
      decider = Decide.deciderFor pid gs
  mode <- case (canExile, foods) of
    (False, []) -> pure Nothing
    (True, []) -> pure (Just ForageMode.ExileCards)
    (False, _) -> pure (Just ForageMode.SacrificeFood)
    (True, _) -> fmap Just (Game.choose (Prompt.ChooseForage decider pid resolving))
  did <- case mode of
    Nothing -> pure False
    Just ForageMode.ExileCards -> do
      chosen <-
        if length cards == 3
          then pure (Set.fromList cards)
          else do
            answer <- Game.choose (Prompt.ChooseExilesFromGraveyard decider pid resolving cards 3)
            let kept = Set.intersection answer (Set.fromList cards)
            pure (if Set.size kept == 3 then kept else Set.fromList (take 3 cards))
      -- CR 406.2's move, through the Event.changeZone funnel, so each card gets a
      -- CR 400.7 incarnation and anything watching a graveyard-to-exile move sees
      -- it.
      Monad.mapM_ (\oid -> Event.changeZone oid Zone.Exile) (Set.toAscList chosen)
      pure True
    Just ForageMode.SacrificeFood -> case foods of
      [] -> pure False
      first : rest -> do
        food <- case rest of
          [] -> pure first
          second : more -> do
            let offered = first NonEmpty.:| (second : more)
            answer <- Game.choose (Prompt.ChoosePermanent decider pid resolving offered)
            pure (if List.elem answer (NonEmpty.toList offered) then answer else first)
        -- CR 701.21a, through the one funnel a sacrifice goes through.
        Event.sacrifice pid food
        pure True
  -- CR 701.61a's forage itself, for "whenever you forage" (Corpseberry
  -- Cultivator) to watch. HERE and not at a caller, Pawl.Engine.Blight's reason:
  -- this is the one place every provenance meets, so an effect's forage and a
  -- cost's write the same event.
  --
  -- Only where the action was carried out. Rule 701.61 has no counterpart to CR
  -- 701.54d's "even if some or all of those actions were impossible", so the
  -- board CR 608.2d refuses writes nothing -- and neither half's own Moved event
  -- could stand in for this one, a forage being one action however it was taken.
  Monad.when did (State.modify' (Event.recordEvent (GameEvent.Foraged pid)))
  pure did
