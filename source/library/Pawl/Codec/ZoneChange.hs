{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ZoneChange where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ZoneChange as ZoneChange

-- | The wire format is unchanged by the conversion to a bundle; what it adds is
-- the schema.
--
-- All four keys are named rather than positional, and two pairs of them share a
-- type: CR 400.7's move has a departing id and an arriving one, and a zone it
-- left and a zone it reached.
codec :: Codec.Codec ZoneChange.ZoneChange
codec = Fields.object $ do
  departed <- Fields.required "departed" ObjectId.codec ZoneChange.departed
  object <- Fields.required "object" ObjectId.codec ZoneChange.object
  from <- Fields.required "from" Zone.codec ZoneChange.from
  to <- Fields.required "to" Zone.codec ZoneChange.to
  pure
    ZoneChange.MkZoneChange
      { ZoneChange.departed = departed,
        ZoneChange.object = object,
        ZoneChange.from = from,
        ZoneChange.to = to
      }
