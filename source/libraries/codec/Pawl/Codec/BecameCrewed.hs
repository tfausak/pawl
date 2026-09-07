{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.BecameCrewed where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.BecameCrewed as BecameCrewed

-- | A bare object keyed by the record's field names. Runtime-only: GameEvent
-- serialises transcripts, never card data.
codec :: Codec.Codec BecameCrewed.BecameCrewed
codec = Fields.object $ do
  vehicle <- Fields.required "vehicle" ObjectId.codec BecameCrewed.vehicle
  crewedBy <- Fields.required "crewedBy" (Common.set ObjectId.codec) BecameCrewed.crewedBy
  pure
    BecameCrewed.MkBecameCrewed
      { BecameCrewed.vehicle = vehicle,
        BecameCrewed.crewedBy = crewedBy
      }
