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
    -- Least is not here either, and no card in data/cards asks for it.
    Greatest quantity
  | -- | The SUM of a per-member quantity -- "the total mana value of cards you
    -- own in exile" (Ashiok, Wicked Manipulator's -7). Greatest's neighbour: the
    -- same per-member Quantity, folded with (+) rather than max, and for the same
    -- reason parameterized rather than named concretely.
    --
    -- Unlike Greatest it answers over an EMPTY matched set, because a sum has an
    -- identity and a maximum does not. Pawl.PlaneswalkerSpec's AshiokLoyalty
    -- group is what proves the three aggregations come apart: two exiled cards of
    -- unequal mana value read differently under Members, Greatest and this.
    Total quantity
  deriving (Eq, Ord, Show)
