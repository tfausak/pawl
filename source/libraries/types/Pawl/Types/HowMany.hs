module Pawl.Types.HowMany where

import qualified Pawl.Types.Quantity as Quantity

-- | CR 608.2d: how many of the matching permanents a resolution-time choice
-- names.
data HowMany
  = -- | CR 608.2d: exactly one, asked only at two or more candidates (Hanweir
    -- Battlements' "a creature named Hanweir Garrison").
    One
  | -- | CR 608.2d: any number up to the ceiling, the empty answer legal; no
    -- ceiling is "any number" (Tovolar, Dire Overlord), a ceiling "up to two"
    -- (Teferi, Hero of Dominaria).
    UpTo (Maybe Quantity.Quantity)
  deriving (Eq, Ord, Show)

-- | The Quantity this count reads, where it reads one: the ceiling, SlotCount's
-- `quantity` for the same walks.
quantity :: HowMany -> Maybe Quantity.Quantity
quantity c = case c of
  One -> Nothing
  UpTo q -> q
