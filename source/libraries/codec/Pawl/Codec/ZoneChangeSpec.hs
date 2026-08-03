{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ZoneChangeSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.ZoneChange as ZoneChange
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChange as ZoneChange

spec :: (Monad m) => Spec.Spec m n -> n ()
spec s =
  Spec.describe s "Pawl.Codec.ZoneChange" . Spec.it s "MkZoneChange" $
    Common.assertJsonCodec
      s
      ZoneChange.toJson
      ZoneChange.fromJson
      (ZoneChange.MkZoneChange (ObjectId.MkObjectId 1) (ObjectId.MkObjectId 1) Zone.Battlefield Zone.Graveyard)
      """ {"departed":1,"object":1,"from":{"type":"Battlefield"},"to":{"type":"Graveyard"}} """
