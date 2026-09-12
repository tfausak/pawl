module Pawl.Types.ManaAddedCause where

-- | CR 605.1a: what added the mana Pawl.Types.ManaAdded records. CR 605.5a is
-- what makes the distinction load-bearing -- a triggered ability watching mana
-- arrive is a mana ability only where an activated mana ability put it there --
-- and Pawl.Types.RevealCause is the same shape of fact one rule over.
data ManaAddedCause
  = -- | An activated mana ability, which is the road Pawl.Engine.Cost.tapForManaWith
    -- takes (CR 605.3b).
    ManaAbility
  | -- | A spell or an ability resolving in the ordinary way, which is the road
    -- Pawl.Engine.Resolve.Effect's AddMana arm takes (CR 608.2c).
    Resolution
  deriving (Bounded, Enum, Eq, Ord, Show)
