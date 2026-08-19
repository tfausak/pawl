module Pawl.Types.Halved where

import qualified Pawl.Types.Rounding as Rounding

-- | The payload of Pawl.Types.Quantity's Halved arm (#1305): CR 107.1a's half,
-- rounded the way the card prints.
--
-- PARAMETRIC in the quantity for Pawl.Types.Plus's reason: the inner value is a
-- whole Quantity and Quantity names this record.
--
-- The rounding is the CARD's word and never an engine rule -- Aspect of Wolf
-- prints both directions in one sentence -- which is why it rides the payload.
-- See Pawl.Types.Rounding.
data Halved quantity = MkHalved
  { rounding :: Rounding.Rounding,
    quantity :: quantity
  }
  deriving (Eq, Ord, Show)
