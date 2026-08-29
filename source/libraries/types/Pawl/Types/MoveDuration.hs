module Pawl.Types.MoveDuration where

-- | CR 610.3: the event that ends a zone change a card makes "until" something
-- happens, printed on the move itself rather than spelled as a second ability.
--
-- The DURATION on a one-shot zone change, which is a different thing from
-- Pawl.Types.Duration: that one says how long a stored CONTINUOUS effect lasts
-- (CR 611.2), where this one names the event after which rule 610.3's second
-- one-shot effect moves the object back. Two consequences ride on it, and
-- neither is expressible as a pair of triggered abilities -- CR 610.3a and CR
-- 610.3b decline the initial move outright when the event has already happened,
-- and the return is a one-shot effect rather than an ability that uses the stack.
data MoveDuration
  = -- | CR 610.3 (Glorious Protector): "until this creature leaves the
    -- battlefield", said of the ability's own source.
    UntilSourceLeavesTheBattlefield
  deriving (Bounded, Enum, Eq, Ord, Show)
