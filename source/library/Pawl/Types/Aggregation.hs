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
  = -- | How many members the Filter kept. Named for the SCOPE's candidates
    -- rather than for objects: Pawl.Types.Scope.OverPlayers folds over players,
    -- and CR 109.1's list of what an object is has no player in it.
    Members
  | DistinctCardTypes
  | -- | The largest value of a per-member quantity -- "the greatest mana value
    -- among artifacts you control" (Karn, Legacy Reforged). Unlike the two above
    -- it must know WHICH per-object quantity to read, and the payload is the
    -- existing Quantity that Pawl.Engine.Quantity.evaluate already reads against
    -- one object rather than a narrower stand-in duplicating its arms.
    --
    -- Least and a summing arm are the obvious neighbours and are NOT here: no
    -- card in the pool asks for them.
    Greatest quantity
  deriving (Eq, Ord, Show)
