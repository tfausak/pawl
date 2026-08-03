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
-- BOTH SIDES are a full Quantity. The field names record where the pool happens
-- to put the constant, not a restriction on either side: every card in
-- data/cards so far has a Literal `threshold`, but nothing stops it being a
-- Count, and Pawl.Codec.ConditionSpec round-trips a Literal `measured`.
-- Quantity already embeds `Count` (its Count arm), so this is strictly wider
-- than the Count-on-the-left shape it replaces -- a side that is a count says so
-- through Quantity rather than by being one.
--
-- The two sides are EVALUATED symmetrically even though they are named
-- asymmetrically: Pawl.Engine.Condition.holds passes both through
-- Pawl.Engine.Quantity.evaluate against the same object and the same view, so a
-- Quantity.Power on either side reads the same object. Only the Comparison is
-- oriented -- AtLeast means `measured` is at least `threshold`.
--
-- The widening is what lets a condition read the OBJECT IT IS EVALUATED AGAINST
-- rather than a set swept out of a zone. A Count's Scope can only enumerate a
-- zone's live residents or the event log, so a source that no longer exists is
-- unreachable from either -- and CR 603.4's intervening "if" on a
-- leaves-the-battlefield ability (Deathknell Berserker's "if its power was 3 or
-- greater") is asked exactly about such a source. Quantity.Power against that
-- id, read through Projection.viewWithLastKnown, is CR 608.2h's answer.
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
