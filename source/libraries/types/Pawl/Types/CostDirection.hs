module Pawl.Types.CostDirection where

-- | CR 601.2f: whether a spell's own cost sentence takes mana off its total or
-- puts mana on it. Rides Pawl.Types.CostReduction. Not a Bool, for
-- Pawl.Types.CostScale's reason.
data CostDirection
  = -- | "This spell costs {2} less to cast", Bury in Books.
    Less
  | -- | "This spell costs {2} more to cast", Dragon's Prey.
    More
  deriving (Bounded, Enum, Eq, Ord, Show)
