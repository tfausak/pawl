{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.OfferCast where

import qualified Pawl.Codec.CastOffer as CastOffer
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.OfferCast as OfferCast

-- | A bare object keyed by the record's field names, with the riders elided when
-- they are the default -- MoveToZone's posture with its EntryRiders.
codec :: Codec.Codec OfferCast.OfferCast
codec = Fields.object $ do
  slot <- Fields.required "slot" SlotName.codec OfferCast.slot
  offer <- Fields.defaulted "offer" CastOffer.defaultValue CastOffer.codec OfferCast.offer
  pure
    OfferCast.MkOfferCast
      { OfferCast.slot = slot,
        OfferCast.offer = offer
      }
