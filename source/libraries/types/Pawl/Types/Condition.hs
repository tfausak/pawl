module Pawl.Types.Condition where

import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Quantity as Quantity

-- | A predicate over game STATE rather than over an event, with three customers
-- and one vocabulary: a state trigger's condition (CR 603.8, checked at every
-- CR 117.5 boundary), an intervening "if" (CR 603.4 when the trigger event
-- occurs, CR 608.2a again on resolution), and a "for as long as" duration
-- (CR 611.2b, Pawl.Engine.Expiry.arm and Pawl.Engine.Expiry.sweepConditional).
--
-- Exactly ONE constructor: there is no escape hatch. CR 611.2b's "for as long as
-- you control this creature" is a source-restricted count of one
-- (Filter.IsSource), not a special arm.
--
-- BOTH SIDES are a full Quantity, and both are evaluated symmetrically --
-- Pawl.Engine.Condition.holds passes each through Pawl.Engine.Quantity.evaluate
-- against the same object and view, so a Quantity.Power on either side reads the
-- same object. The field names record only where the pool happens to put the
-- constant; only the Comparison is oriented, AtLeast meaning `measured` is at
-- least `threshold`. That width is what lets a condition read the object it is
-- evaluated against rather than a set swept out of a zone, which CR 603.4's
-- intervening "if" on a leaves-the-battlefield ability needs: the source is gone,
-- so CR 608.2h answers through Projection.viewWithLastKnown.
--
-- A Count's Scope may name a slot (PlayerRef.InSlot), and this Condition may be
-- stored into a Pawl.Types.Expiry.While for a "for as long as" duration. An
-- InSlot count stored that way outlives its slot binding: Pawl.Engine.Count.playersFor
-- then yields Nothing, and Pawl.Engine.Condition.holds collapses that to False
-- silently (#159).
data Condition = MkCondition
  { measured :: Quantity.Quantity,
    comparison :: Comparison.Comparison,
    threshold :: Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
