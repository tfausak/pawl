module Pawl.Types.Condition where

import qualified Pawl.Types.Compares as Compares

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
-- A Count's Scope may name a slot (PlayerRef.InSlot), and this Condition may be
-- stored into a Pawl.Types.Expiry.While for a "for as long as" duration. Such a
-- reference outlives its slot binding, so Pawl.Engine.Condition.bakeBound
-- substitutes the seat for the slot as the duration begins; Pawl.ExpirySpec's
-- Garland, Royal Kidnapper group is what proves the stored condition still
-- answers once the resolution that stored it is over.
data Condition
  = -- | One comparison, whose three parts are Pawl.Types.Compares -- see there
    -- for why both sides are a full Quantity.
    Compares Compares.Compares
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
