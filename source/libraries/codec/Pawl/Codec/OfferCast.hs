{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.OfferCast where

import qualified Pawl.Codec.CastOffer as CastOffer
import qualified Pawl.Codec.Optionality as Optionality
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.OfferCast as OfferCast
import qualified Pawl.Types.Optionality as Optionality.Type
import qualified Pawl.Types.PlayerRef as PlayerRef.Type
import qualified Pawl.Types.PlayerRelation as PlayerRelation

-- | A bare object keyed by the record's field names, with the riders elided when
-- they are the default -- MoveToZone's posture with its EntryRiders.
--
-- Three of the four keys default, and both defaults added for CR 608.2g's
-- "instructs" half are the "allows" half's reading: the resolving controller
-- casts, and they may decline. A defaulted Mandatory would silently turn every
-- existing offer into an instruction.
codec :: Codec.Codec OfferCast.OfferCast
codec = Fields.object $ do
  slot <- Fields.required "slot" SlotName.codec OfferCast.slot
  caster <- Fields.defaulted "caster" (PlayerRef.Type.Relative PlayerRelation.You) PlayerRef.codec OfferCast.caster
  optionality <- Fields.defaulted "optionality" Optionality.Type.Optional Optionality.codec OfferCast.optionality
  offer <- Fields.defaulted "offer" CastOffer.defaultValue CastOffer.codec OfferCast.offer
  pure
    OfferCast.MkOfferCast
      { OfferCast.slot = slot,
        OfferCast.caster = caster,
        OfferCast.optionality = optionality,
        OfferCast.offer = offer
      }
