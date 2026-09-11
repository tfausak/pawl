{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Crewing where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Crewing as Crewing

-- | A bare object keyed by the record's field names. Runtime-only: GameEvent
-- serialises transcripts, never card data.
codec :: Codec.Codec Crewing.Crewing
codec = Fields.object $ do
  vehicle <- Fields.required "vehicle" ObjectId.codec Crewing.vehicle
  crewedBy <- Fields.required "crewedBy" (Common.set ObjectId.codec) Crewing.crewedBy
  pure
    Crewing.MkCrewing
      { Crewing.vehicle = vehicle,
        Crewing.crewedBy = crewedBy
      }
