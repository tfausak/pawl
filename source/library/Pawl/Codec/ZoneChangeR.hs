{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ZoneChangeR where

import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.Codec.ZoneChangePattern as ZoneChangePattern
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ZoneChangeR as ZoneChangeR

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be (#1464).
codec :: Codec.Codec ZoneChangeR.ZoneChangeR
codec = Fields.object $ do
  matching <- Fields.required "matching" ZoneChangePattern.codec ZoneChangeR.matching
  destination <- Fields.required "destination" Zone.codec ZoneChangeR.destination
  pure
    ZoneChangeR.MkZoneChangeR
      { ZoneChangeR.matching = matching,
        ZoneChangeR.destination = destination
      }
