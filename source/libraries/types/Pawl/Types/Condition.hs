module Pawl.Types.Condition where

import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Quantity as Quantity

-- | A predicate over game STATE rather than over an event, with four customers
-- and one vocabulary: a state trigger's condition (CR 603.8, checked at every
-- CR 117.5 boundary), an intervening "if" (CR 603.4 when the trigger event
-- occurs, CR 608.2a again on resolution), a "for as long as" duration
-- (CR 611.2b, Pawl.Engine.Expiry.arm and Pawl.Engine.Expiry.sweepConditional),
-- and a printed static ability's "as long as" clause (CR 604.2,
-- Pawl.Types.StaticAbility.condition and Pawl.Engine.Projection.gatherStatic).
--
-- The last two are the pair most easily confused, and CR 611.2c's parenthetical
-- keeps them apart: the duration ENDS a stored effect once, while the static
-- ability's clause gates one that is re-derived every projection.
--
-- ONE comparison plus a disjunction of them, and no other escape hatch. CR
-- 611.2b's "for as long as you control this creature" is a source-restricted
-- count of one (Filter.IsSource), not a special arm.
--
-- BOTH SIDES of Compares are a full Quantity, and both are evaluated
-- symmetrically -- Pawl.Engine.Condition.holds passes each through
-- Pawl.Engine.Quantity.evaluate against the same object and view, so a
-- Quantity.Power on either side reads the same object. Only the Comparison is
-- oriented, AtLeast meaning the first side is at least the second. That width is
-- what lets a condition read the object it is evaluated against rather than a set
-- swept out of a zone, which CR 603.4's intervening "if" on a
-- leaves-the-battlefield ability needs: the source is gone, so CR 608.2h answers
-- through Projection.viewWithLastKnownAnywhere -- which owes the same fallback to
-- an object the clause names through a SLOT, rule 702.100a's entrant being one.
--
-- A Count's Scope may name a slot (PlayerRef.InSlot), and this Condition may be
-- stored into a Pawl.Types.Expiry.While for a "for as long as" duration. An
-- InSlot count stored that way outlives its slot binding: Pawl.Engine.Count.playersFor
-- then yields Nothing, and Pawl.Engine.Condition.holds collapses that to False
-- silently (#159).
data Condition
  = -- | One comparison: the first Quantity relates thus to the second.
    Compares Quantity.Quantity Comparison.Comparison Quantity.Quantity
  | -- | True when ANY of these is -- CR 702.100a's "power is greater ... and/or
    -- toughness is greater", which is two comparisons of two different
    -- characteristics and so cannot be folded into one Compares.
    --
    -- Filter's Or, transplanted: a flat sibling arm rather than a wrapper type,
    -- so it nests. There is no And and no Not, those being arms no rule in the
    -- pool asks for. `Any []` is False, which is the fold's unit and not a
    -- trivial-truth arm -- Filter's `And []` spells that.
    Any [Condition]
  deriving (Eq, Ord, Show)
