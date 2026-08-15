{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.GrantPlayFromExile where

import qualified Pawl.Codec.Duration as Duration
import qualified Pawl.Codec.ManaSpending as ManaSpending
import qualified Pawl.Codec.ObjectRef as ObjectRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.GrantPlayFromExile as GrantPlayFromExile
import qualified Pawl.Types.ManaSpending as ManaSpending

-- | A bare object keyed by the record's field names, with the rider elided when
-- it is the default -- Pawl.Codec.OfferCast's posture with its CastOffer, and
-- for the same reason: a permission that says nothing about mana is the ordinary
-- one, so the key belongs only on the card that prints CR 118.14's clause.
codec :: Codec.Codec GrantPlayFromExile.GrantPlayFromExile
codec = Fields.object $ do
  duration <- Fields.required "duration" Duration.codec GrantPlayFromExile.duration
  ref <- Fields.required "ref" ObjectRef.codec GrantPlayFromExile.ref
  spending <- Fields.defaulted "spending" ManaSpending.AsProduced ManaSpending.codec GrantPlayFromExile.spending
  pure
    GrantPlayFromExile.MkGrantPlayFromExile
      { GrantPlayFromExile.duration = duration,
        GrantPlayFromExile.ref = ref,
        GrantPlayFromExile.spending = spending
      }
