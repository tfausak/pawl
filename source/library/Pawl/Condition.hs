-- CR 603.8 / 603.4 / 611.2b: is this Condition currently true? The only module
-- that may evaluate a Pawl.Type.Condition -- the standing Pawl.Expiry has over
-- Pawl.Type.Expiry and Pawl.Projection over Modification.
--
-- Total, so it must collapse the Maybe its two inputs carry: an undeterminable
-- COUNT or an undeterminable THRESHOLD makes the condition FALSE. That matches
-- the retired Event.stateHolds and is the conservative reading of CR 611.2b -- a
-- "for as long as" whose condition cannot be evaluated ends rather than
-- persists. CR 208.2a's "use 0 instead of that number" is a different rule with
-- a different scope (#65).
module Pawl.Condition where

import qualified Pawl.Count as Count
import qualified Pawl.Filter as Filter
import qualified Pawl.Quantity as Quantity
import qualified Pawl.Type.Comparison as Comparison
import qualified Pawl.Type.Condition as Condition.Type
import Pawl.Type.GameState (GameState)
import Pawl.Type.ObjectId (ObjectId)

holds :: Count.ViewOf -> Filter.Context -> GameState -> ObjectId -> Condition.Type.Condition -> Bool
holds viewOf context gs oid (Condition.Type.MkCondition count comparison threshold) =
  case (Count.evaluate viewOf context gs count, Quantity.evaluate viewOf context gs oid threshold) of
    (Just n, Just t) -> case comparison of
      Comparison.Exactly -> n == t
      Comparison.AtLeast -> n >= t
      Comparison.AtMost -> n <= t
    _ -> False
