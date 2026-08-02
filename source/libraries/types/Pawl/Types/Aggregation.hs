module Pawl.Types.Aggregation where

-- | How a Pawl.Types.Count turns its matched set into a number. A real axis,
-- not always "length": CR 208.2a's Tarmogoyf counts the distinct card TYPES
-- among the cards in every graveyard, which is the size of a union.
--
-- The `quantity` parameter is here for the same module-cycle reason
-- Pawl.Types.Effect's `card` is: Greatest reads a Pawl.Types.Quantity off each
-- member, and Quantity already embeds a Count, so a concrete reference here
-- would make Quantity, Count and this module mutually import each other.
-- Parameterizing keeps this module Quantity-free; Pawl.Types.Quantity ties the
-- knot by instantiating `Count Quantity`.
data Aggregation quantity
  = Objects
  | DistinctCardTypes
  | -- | The largest value of a per-member quantity: "the greatest mana value
    -- among artifacts you control" (One with the Machine, Karn, Legacy
    -- Reforged), "the greatest power among creatures you control" (Fungal
    -- Sprouting).
    --
    -- NOT a sibling of the two above, which need only the matched set. This one
    -- also needs to know WHICH per-object quantity to read, and a Quantity is
    -- what Pawl.Engine.Quantity.evaluate already reads against one object -- so the
    -- payload is the existing type rather than a narrower stand-in that would
    -- duplicate its arms.
    --
    -- Least and a summing arm (Ghalta's "total power of creatures you control")
    -- are the obvious neighbours and are NOT here: no card in the pool asks for
    -- them, and an unused arm is the speculative construction the project
    -- forbids.
    Greatest quantity
  deriving (Eq, Ord, Show)
