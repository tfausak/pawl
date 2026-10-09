{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ReturnWatch where

import qualified Pawl.Codec.ReturnEnding as ReturnEnding
import qualified Pawl.Codec.Zone as Zone
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ReturnWatch as ReturnWatch

codec :: Codec.Codec ReturnWatch.ReturnWatch
codec = Fields.object $ do
  ending <- Fields.required "ending" ReturnEnding.codec ReturnWatch.ending
  zone <- Fields.required "zone" Zone.codec ReturnWatch.zone
  pure
    ReturnWatch.MkReturnWatch
      { ReturnWatch.ending = ending,
        ReturnWatch.zone = zone
      }
