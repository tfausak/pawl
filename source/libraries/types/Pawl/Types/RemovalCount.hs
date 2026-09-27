module Pawl.Types.RemovalCount where

import qualified Pawl.Types.Quantity as Quantity

-- | How many counters Pawl.Types.RemoveCountersAmong takes off, the player
-- dividing them (CR 608.2d).
data RemovalCount
  = -- | Overseer of Vault 76's "remove three quest counters": that many, and
    -- not an option where fewer are there (CR 608.2d).
    Exactly Quantity.Quantity
  | -- | Sensational Spider-Man's "remove up to three stun counters": any number
    -- from none to that many.
    UpTo Quantity.Quantity
  | -- | Galloping Lizrog's "remove any number of +1\/+1 counters".
    AnyNumber
  deriving (Eq, Ord, Show)

-- | The count the card writes, for a caller reading every Quantity an effect
-- holds.
quantityOf :: RemovalCount -> Maybe Quantity.Quantity
quantityOf x = case x of
  Exactly quantity -> Just quantity
  UpTo quantity -> Just quantity
  AnyNumber -> Nothing
