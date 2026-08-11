-- CR 603.8 / 603.4 / 611.2b / 604.2: is this Condition currently true? The only
-- module that may evaluate a Pawl.Types.Condition -- the standing
-- Pawl.Engine.Expiry has over Pawl.Types.Expiry and Pawl.Engine.Projection over
-- Modification.
--
-- Total, so it must collapse the Maybe its two inputs carry: an undeterminable
-- quantity on EITHER side makes the condition FALSE. That is the conservative
-- reading of CR 611.2b -- a "for as long as" whose condition cannot be
-- evaluated ends rather than persists. CR 208.2a's substituted 0 is a different
-- rule scoped to a characteristic-defining ability, which a condition is not.
--
-- The VIEW is the caller's, and picking it is a rules decision rather than a
-- detail. Three answers, one per kind of question:
--
--   * CR 603.4's intervening "if" asks about objects that may no longer exist --
--     the source of a leaves-the-battlefield ability, or the entrant rule
--     702.100a's clause is about -- so Event.interveningHolds and both arms of
--     Pawl.Engine.Stack's CR 608.2a re-check (the object-borne one and the
--     inherent one) pass Projection.viewWithLastKnownAnywhere, which owes CR
--     608.2h to every id it is aimed at rather than to one. Nothing here can
--     compensate for the wrong view -- an object read as an empty one simply
--     answers False.
--   * CR 604.7 settles a static ability's "as long as" clause the other way: it
--     "can't use an object's last known information", so
--     Projection.conditionHolds passes a live Projection.viewUpTo -- bounded
--     rather than full, which is a layer question rather than a last-known one.
--   * CR 702.178a's max speed gate is the same static-ability genre and gets a
--     different answer again: Projection.abilitiesGiven asks it OVER the finished
--     projection rather than inside the fold, so there is no layer to bound
--     against and it passes Projection.fullView.
module Pawl.Engine.Condition where

import qualified Pawl.Engine.Count as Count
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Quantity as Quantity
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition.Type
import Pawl.Types.GameState (GameState)
import Pawl.Types.ObjectId (ObjectId)

holds :: Count.ViewOf -> Filter.Context -> GameState -> ObjectId -> Condition.Type.Condition -> Bool
holds viewOf context gs oid condition = case condition of
  -- Both sides are evaluated against `oid` and with the same view, so a
  -- Quantity.Power on either side reads the same object and a Quantity.Count on
  -- either side sweeps the same board. Only the Comparison is oriented.
  Condition.Type.Compares measured comparison threshold ->
    case (evaluate measured, evaluate threshold) of
      (Just n, Just t) -> case comparison of
        Comparison.Exactly -> n == t
        Comparison.AtLeast -> n >= t
        Comparison.AtMost -> n <= t
      _ -> False
  -- Each disjunct collapses its own unanswerable quantities to False, so an
  -- empty list is False and a disjunct reading a slot nothing filled cannot
  -- poison the ones beside it.
  Condition.Type.Any conditions -> any (holds viewOf context gs oid) conditions
  where
    evaluate = Quantity.evaluate viewOf context gs oid
