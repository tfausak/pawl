module Pawl.Types.CharacteristicPT where

import qualified Pawl.Types.Quantity as Quantity

-- | CR 208.2's characteristic-defining power and toughness -- the pair a printed
-- star box resolves to, evaluated fresh at every projection.

-- BOTH fields are a Quantity and they are NOT interchangeable, so they are named
-- rather than positional: Tarmogoyf's pair is @*@ and @*+1@, and a swap would
-- give it the wrong box in every zone (CR 208.2a makes a CDA function
-- everywhere).
data CharacteristicPT = MkCharacteristicPT
  { power :: Quantity.Quantity,
    toughness :: Quantity.Quantity
  }
  deriving (Eq, Ord, Show)
