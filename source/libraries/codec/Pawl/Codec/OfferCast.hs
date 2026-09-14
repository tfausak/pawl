{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.OfferCast where

import qualified Pawl.Codec.CastObligation as CastObligation
import qualified Pawl.Codec.CastOffer as CastOffer
import qualified Pawl.Codec.CastRepetition as CastRepetition
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.CastObligation as CastObligation.Type
import qualified Pawl.Types.CastRepetition as CastRepetition.Type
import qualified Pawl.Types.OfferCast as OfferCast
import qualified Pawl.Types.PlayerRef as PlayerRef.Type
import qualified Pawl.Types.PlayerRelation as PlayerRelation

-- | A bare object keyed by the record's field names, with the riders elided when
-- they are the default -- MoveToZone's posture with its EntryRiders.
--
-- Four of the five keys default, and every default is the narrow reading: the
-- resolving controller casts, they may decline, and one card is cast. A defaulted
-- Mandatory would silently turn every existing offer into an instruction, and a
-- defaulted AnyNumber would turn every one of them into a repeated offer.
codec :: Codec.Codec OfferCast.OfferCast
codec = Fields.object $ do
  ref <- Fields.required "ref" ObjectRef.codec OfferCast.ref
  caster <- Fields.defaulted "caster" (PlayerRef.Type.Relative PlayerRelation.You) PlayerRef.codec OfferCast.caster
  optionality <- Fields.defaulted "optionality" CastObligation.Type.Optional CastObligation.codec OfferCast.optionality
  offer <- Fields.defaulted "offer" CastOffer.defaultValue CastOffer.codec OfferCast.offer
  repetition <- Fields.defaulted "repetition" CastRepetition.Type.Once CastRepetition.codec OfferCast.repetition
  pure
    OfferCast.MkOfferCast
      { OfferCast.ref = ref,
        OfferCast.caster = caster,
        OfferCast.optionality = optionality,
        OfferCast.offer = offer,
        OfferCast.repetition = repetition
      }
