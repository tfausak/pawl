module Pawl.Type.Condition where

import Pawl.Type.Comparison (Comparison)
import Pawl.Type.Count (Count)
import Pawl.Type.Quantity (Quantity)

-- A predicate over game STATE rather than over an event, with three customers
-- and one vocabulary: a state trigger's condition (CR 603.8, checked at every
-- CR 117.5 boundary), an intervening "if" (CR 603.4 when the trigger event
-- occurs, CR 608.2a again on resolution), and a "for as long as" duration
-- (CR 611.2b, Pawl.Expiry.arm and Pawl.Expiry.sweepConditional).
--
-- Exactly ONE constructor: there is no escape hatch. CR 611.2b's "for as long as
-- you control this creature" is a source-restricted count of one
-- (Filter.IsSource), not a special arm.
--
-- The threshold is a Quantity rather than an Integer because Quantity already
-- exists and already composes; only Pawl.Condition may evaluate this.
data Condition = MkCondition Count Comparison Quantity
  deriving (Eq, Ord, Show)
