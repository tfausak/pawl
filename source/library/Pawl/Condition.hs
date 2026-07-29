-- CR 603.8 / 603.4 / 611.2b: is this Condition currently true? The only module
-- that may evaluate a Pawl.Type.Condition -- the standing Pawl.Expiry has over
-- Pawl.Type.Expiry and Pawl.Projection over Modification.
--
-- Total, so it must collapse the Maybe its two inputs carry: an undeterminable
-- quantity on EITHER side makes the condition FALSE. That matches the retired
-- Event.stateHolds and is the conservative reading of CR 611.2b -- a "for as
-- long as" whose condition cannot be evaluated ends rather than persists. CR
-- 208.2a's "use 0 instead of that number" is a different rule with a different
-- scope (#65).
--
-- The VIEW is the caller's, and picking it is a rules decision rather than a
-- detail: CR 603.4's intervening "if" on a leaves-the-battlefield ability asks
-- about a source that no longer exists, so its two callers (Event.interveningHolds
-- and Pawl.Stack's CR 608.2a re-check) pass Projection.viewWithLastKnown and not
-- Projection.fullView. Nothing here can compensate for the wrong one -- a source
-- read as an empty object simply answers False.
module Pawl.Condition where

import qualified Pawl.Count as Count
import qualified Pawl.Filter as Filter
import qualified Pawl.Quantity as Quantity
import qualified Pawl.Type.Comparison as Comparison
import qualified Pawl.Type.Condition as Condition.Type
import Pawl.Type.GameState (GameState)
import Pawl.Type.ObjectId (ObjectId)

holds :: Count.ViewOf -> Filter.Context -> GameState -> ObjectId -> Condition.Type.Condition -> Bool
holds viewOf context gs oid (Condition.Type.MkCondition measured comparison threshold) =
  -- Both sides are evaluated against `oid` and with the same view, so a
  -- Quantity.Power on either side reads the same object and a Quantity.Count on
  -- either side sweeps the same board. That symmetry is the point of the type:
  -- "its power was 3 or greater" and "the number of Zombies is 0" are the same
  -- kind of sentence, differing only in which side carries the constant.
  case (Quantity.evaluate viewOf context gs oid measured, Quantity.evaluate viewOf context gs oid threshold) of
    (Just n, Just t) -> case comparison of
      Comparison.Exactly -> n == t
      Comparison.AtLeast -> n >= t
      Comparison.AtMost -> n <= t
    _ -> False
