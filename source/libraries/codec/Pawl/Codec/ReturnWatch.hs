{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ReturnWatch where

import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ReturnWatch as ReturnWatch

codec :: Codec.Codec ReturnWatch.ReturnWatch
codec = Fields.object $ do
  source <- Fields.required "source" ObjectId.codec ReturnWatch.source
  zone <- Fields.required "zone" Zone.codec ReturnWatch.zone
  pure
    ReturnWatch.MkReturnWatch
      { ReturnWatch.source = source,
        ReturnWatch.zone = zone
      }
