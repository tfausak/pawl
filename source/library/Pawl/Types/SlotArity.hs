module Pawl.Types.SlotArity where

-- | HOW MANY recipients an effect can read out of one slot -- the arity half of
-- the D4 dataflow lint's read side (Pawl.Engine.Resolve.slotsOf).
--
-- CR 601.2c lets one instance of the word "target" take several, so a slot's
-- binding is a set; but most opcodes name one object or one player and have
-- nowhere to put a second. Reading a two-target slot through one of those would
-- silently affect nothing, which no compiler catches, so the read is classified
-- here and Pawl.CardSpec rejects the card that would do it.
--
-- Ordered so that `min` is the conservative join: a slot read BOTH ways by
-- different effects of one card can hold only one.
data SlotArity
  = -- | The reader takes one recipient, or none -- every bare SlotName field, and
    -- every PlayerRef.InSlot.
    One
  | -- | The reader takes the whole set (CR 608.2f's simultaneous batch): every
    -- ObjectRef.InSlot, which is what "up to two target creatures" is written
    -- with.
    Many
  deriving (Eq, Ord, Show)
