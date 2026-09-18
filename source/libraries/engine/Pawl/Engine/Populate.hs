-- | CR 701.36, populate: "choose a creature token you control and create a
-- token that's a copy of that creature token", and the whole of the keyword
-- action.
--
-- Pawl.Engine.Forage's sibling, and standing on the same ground: rule 701 is a
-- keyword-action rule exactly as rule 702 is a keyword rule, so the procedure
-- lives in the engine rather than in card data. The closed\/open invariant
-- forbids the rules core casing on an EFFECT's identity, and nothing here does
-- -- Pawl.Engine.Resolve.Effect's Effect.Populate arm calls in without saying
-- which effect it is.
--
-- Rule 701.36 has no "whenever a player populates", so there is no GameEvent
-- here and no trigger condition to hang one on -- Pawl.Engine.Forage writes
-- GameEvent.Foraged only because CR 701.61b's counterpart exists. Scryfall
-- @o:"populates"@, 2026-09-18, returns no card; a printing worded that way is
-- what would need one.
module Pawl.Engine.Populate where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.TapState as TapState

-- | CR 701.36a's candidate set: every creature token this player controls.
--
-- The creature half is read off the PROJECTION (CR 613), so a token animated by
-- a continuous effect is a candidate and a creature token that has stopped being
-- a creature is not. The token half is Game.isToken, CR 111.6's fixed answer,
-- which no effect can change.
--
-- ASCENDING, so the single-candidate shortcut below and a transcript are
-- deterministic -- Pawl.Engine.Forage.foodCandidates' posture.
candidates :: PlayerId -> GameState.GameState -> [ObjectId]
candidates pid gs =
  List.sort (filter (\oid -> Game.isToken oid gs && Projection.isCreatureOf oid gs) (Projection.controls pid gs))

-- | CR 701.36a: choose a creature token this player controls and create a token
-- that's a copy of it.
--
-- The ObjectId is the object the prompt names -- the spell or ability resolving.
--
-- ONE PROMPT, raised only where the player controls more than one creature
-- token, since a lone candidate leaves nothing to ask.
--
-- CHOOSE, not target: rule 701.36a says "a creature token you control" without
-- saying "target", so nothing was declared on the stack (CR 601.2c) and there is
-- no CR 608.2b legality to re-check.
--
-- FILTERED, NOT TRUSTED, Pawl.Engine.Forage's posture: an answer naming
-- something never offered falls back to the offered set's own front.
--
-- CR 701.36b: a player controlling no creature token creates nothing, which the
-- rule states outright rather than leaving to CR 608.2d.
--
-- The copy is minted through Event.createTokens with the chosen token's COPIABLE
-- values (CR 707.2), which is the same funnel and the same snapshot
-- Pawl.Engine.Resolve.Effect's Effect.CreateCopy arm uses -- so CR 614.12's
-- entry loop and CR 111.5's rollback point are the ones every token gets. CR
-- 707.2 copies no counters and rule 701.36a states no riders, so the copy enters
-- untapped and bare.
populate :: PlayerId -> ObjectId -> Game ()
populate pid resolving = do
  gs <- State.get
  case candidates pid gs of
    [] -> pure ()
    first : rest -> do
      token <- case rest of
        [] -> pure first
        second : more -> do
          let offered = first NonEmpty.:| (second : more)
          answer <- Game.choose (Prompt.ChoosePermanent (Decide.deciderFor pid gs) pid resolving offered)
          pure (if List.elem answer (NonEmpty.toList offered) then answer else first)
      -- Against the LIVE state and not `gs`, Pawl.Engine.Event.bringInto's care:
      -- Game.choose above wrote the answer into the transcript, and minting off
      -- the state from before the prompt would drop that.
      minting <- State.get
      Monad.forM_ (Game.cardOfWithLastKnown token minting) $ \card ->
        Monad.void (Event.createTokens pid card (Just (Event.copiedSnapshotWithLastKnown token minting)) 1 TapState.Untapped Map.empty Nothing)
