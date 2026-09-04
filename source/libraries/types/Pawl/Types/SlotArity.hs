module Pawl.Types.SlotArity where

-- | HOW MANY recipients an effect can read out of one slot -- the arity half of
-- the D4 dataflow lint's read side (Pawl.Engine.Resolve.Slots.slotsOf).
--
-- CR 601.2c lets one instance of the word "target" take several, so a slot's
-- binding is a set; but most opcodes name one object or one player and have
-- nowhere to put a second. Reading a two-target slot through one of those would
-- silently affect nothing, which no compiler catches, so the read is classified
-- here and Pawl.CardSpec rejects the card that would do it.
--
-- Ordered so that `min` is the conservative join: a slot read BOTH ways by
-- different effects of one card can hold only one.
--
-- Not two answers but three, because Pawl.Types.SlotName is one flat namespace:
-- a read may be of the slot's amount rather than of the objects it names, and
-- that read is not an arity claim about this slot at all. Presence and arity are
-- one map -- Pawl.Engine.Resolve.modeSlots' keys are the D4 dataflow lint's read
-- side and its values are the count lint's -- so the third answer is how the map
-- reports a read while claiming nothing about how many recipients it sees.
data SlotArity
  = -- | The reader takes one recipient, or none -- most bare SlotName fields, and
    -- every PlayerRef.InSlot. Not ALL bare SlotName fields: Pawl.Types.PlayerSacrifices'
    -- and Pawl.Types.CountedDiscard's are read with legalMany, so their arity is
    -- Many below.
    One
  | -- | The reader takes the whole set (CR 608.2f's simultaneous batch): every
    -- ObjectRef.InSlot, which is what "up to two target creatures" is written
    -- with.
    Many
  | -- | The reader takes no recipient at all: it reads the slot's AMOUNT half
    -- (Pawl.Engine.Binding.amountOf) rather than its objects or players, which is
    -- Quantity.InSlot and nothing else. Last so that `min` lets any object read
    -- of the same slot win; see #2774.
    Amount
  deriving (Eq, Ord, Show)
