module Pawl.Types.OfferCast where

import qualified Pawl.Types.CastOffer as CastOffer
import qualified Pawl.Types.SlotName as SlotName

-- | CR 608.2g: offer the resolving controller the cast of the object a slot
-- names, under CR 310.12b's riders.
data OfferCast = MkOfferCast
  { slot :: SlotName.SlotName,
    -- | Elided when the offer carries neither rider, which is an ordinary cast
    -- of the card.
    offer :: CastOffer.CastOffer
  }
  deriving (Eq, Ord, Show)
