-- Rule 708 in the one voice the rest of the engine cannot supply for itself: CR
-- 116.2b's special action that turns a face-down permanent face up.
--
-- Everything ELSE rule 708 says is arranged so that no module has to know this
-- one exists. CR 708.2's substitution lives at Pawl.Engine.Game.faceOf, so every
-- characteristic read gets it for free; CR 708.3/708.4's turning-over-before-the-
-- move lives at Pawl.Engine.Event.changeZoneFaceDown; CR 708.9's reveal is
-- Object.newIncarnation putting the status back to FaceUp. What is left is an
-- action a player takes, and an action needs a place to be offered from and
-- performed in.
--
-- THE INVARIANT: rule 702.37 is part of the rulebook, so reading
-- Keyword.Morph's cost here is the same closed-half act as reading a Phase. This
-- module never asks which CARD is underneath.
module Pawl.Engine.FaceDown where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.Projection as Projection
import Pawl.Types.Cost (Cost)
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
import Pawl.Types.Game (Game)
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.Keyword (Keyword)
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Payment as Payment
import Pawl.Types.PlayerId (PlayerId)

-- CR 702.37e: "what the permanent's morph cost WOULD BE if it were face up".
-- Nothing when the card underneath has no morph ability, which is the rule's own
-- parenthesis -- "if the permanent wouldn't have a morph cost if it were face
-- up, it can't be turned face up this way".
--
-- Read through Game.faceUpFaceOf, the one door that steps around CR 708.2's
-- substitution, because that substitution is exactly what makes a projected read
-- useless here: a face-down permanent has no keywords at all (CR 708.2a), so
-- Projection.keywordsOf would answer Nothing for every morph creature ever
-- printed. The rule's counterfactual is what licenses it.
--
-- Not a projected read, therefore not affected by Humility: a face-down
-- permanent under a layer-6 ability removal can still be turned face up, since
-- the ability rule 702.37e consults is the one the CARD would have. That is what
-- "would be if it were face up" says, and nothing in the pool observes it.
morphCostOf :: ObjectId -> GameState -> Maybe (Cost Keyword)
morphCostOf oid gs = do
  face <- Game.faceUpFaceOf oid gs
  Keyword.morphCost (Face.keywords face)

-- CR 702.37e / 116.2b: may this player turn this permanent face up right now?
-- Four conjuncts, each a clause of the rule:
--
--   * it is FACE DOWN (CR 708.2b's mirror -- there is nothing to turn up
--     otherwise);
--   * it is a PERMANENT this player CONTROLS ("a face-down permanent you
--     control"), which is why the battlefield membership and the projected
--     controller are both asked;
--   * the card would have a MORPH COST if it were face up;
--   * that cost is payable. An action the player cannot take is not offered,
--     which is Pawl.Engine.Action.legalActions' posture for every other action
--     on the menu.
--
-- The payability check is Cost.canPay and NOT Cost.total's CR 601.2f
-- adjustments: that rule totals the cost of a spell being cast or an ability
-- being activated, and a special action is neither -- the same reading
-- Pawl.Engine.Activate takes of an activation cost (#90).
--
-- The TIMING clause has no conjunct here because the engine is the timing: CR
-- 702.37e's "any time you have priority" is satisfied by legalActions being
-- asked only of the priority holder, exactly as CR 116.2a's land play relies on.
canTurnFaceUp :: PlayerId -> ObjectId -> GameState -> Bool
canTurnFaceUp pid oid gs =
  fmap Object.facing (Game.lookupObject oid gs) == Just Facing.FaceDown
    && Projection.controllerOf oid gs == Just pid
    && case morphCostOf oid gs of
      Nothing -> False
      Just cost -> Cost.canPay pid oid cost gs

-- Every permanent this player may turn face up right now, in battlefield order
-- -- what Action.TurnFaceUp is built from.
turnableFaceUp :: PlayerId -> GameState -> [ObjectId]
turnableFaceUp pid gs =
  filter (\oid -> canTurnFaceUp pid oid gs) (Set.toAscList (GameState.battlefield gs))

-- CR 702.37e, in the rule's own order: show all players what the morph cost
-- would be, pay it, then turn the permanent face up.
--
-- The SHOWING is not modelled. Nothing in pawl hides a face-down permanent's
-- card from a reader in the first place, so there is no concealment for a reveal
-- to lift (#682).
--
-- REJECT-NOT-REPAIR, the posture Cast.castSpell and Activate.activateAbility
-- both take: a payment that fails restores the state from before it was
-- attempted and the permanent stays face down. CR 702.37e's order is what makes
-- that correct rather than merely tidy -- the cost is paid BEFORE the permanent
-- turns over, so a failed payment has turned nothing over to undo.
--
-- CR 708.8 falls out of the shape and is not implemented anywhere: "any effects
-- that have been applied to the face-down permanent still apply to the face-up
-- permanent", and this writes one status field on one object -- no CR 400.7
-- incarnation is minted, so damage, counters, attachments, Auras, the CR 613.7d
-- timestamp and every continuous effect naming the object ride through
-- untouched. Its last sentence is the same non-event: nothing here is a
-- battlefield entry, so no enters-the-battlefield ability is offered one.
--
-- CR 708.11's "as [this permanent] is turned face up" abilities are NOT applied
-- (#917), and no ability triggers on the turning-over either (#918).
turnFaceUp :: PlayerId -> ObjectId -> Game ()
turnFaceUp pid oid = do
  before <- State.get
  if not (canTurnFaceUp pid oid before)
    then pure ()
    else case morphCostOf oid before of
      Nothing -> pure ()
      Just cost -> do
        payment <- Cost.pay pid oid cost
        case payment of
          Payment.Unpaid -> State.put before
          -- CR 708.8: the copiable values revert, which for pawl is the status
          -- flipping -- Game.faceOf reads it, so the substitution simply stops
          -- applying and the card's own face answers again.
          Payment.Paid ->
            State.modify'
              ( \gs ->
                  gs
                    { GameState.objects =
                        Map.adjust (\o -> o {Object.facing = Facing.FaceUp}) oid (GameState.objects gs)
                    }
              )
