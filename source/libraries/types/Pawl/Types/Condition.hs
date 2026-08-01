module Pawl.Types.Condition where

import Pawl.Types.Comparison (Comparison)
import Pawl.Types.Quantity (Quantity)

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
-- BOTH SIDES are a Quantity, and symmetrically so -- neither side is "the
-- measured thing" and the other "the threshold". Quantity already embeds
-- `Count` (its Count arm), so this is strictly wider than the Count-on-the-left
-- shape it replaces and no card file changed: Pawl.Codec.Quantity's toJson emits
-- a Count arm as the count's own tag, so the existing `{"type": "Count", ...}`
-- payloads decode unchanged.
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
data Condition = MkCondition Quantity Comparison Quantity
  deriving (Eq, Ord, Show)
