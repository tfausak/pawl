{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.CounteringSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Countering as Countering
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Countering as Countering
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

spec :: (Monad m) => Spec.Spec m n -> n ()
spec s =
  Spec.describe s "Pawl.Codec.Countering" . Spec.it s "MkCountering" $
    Common.assertJsonCodec
      s
      Countering.toJson
      Countering.fromJson
      Countering.MkCountering
        { Countering.spell = ObjectId.MkObjectId 4,
          Countering.source = ObjectId.MkObjectId 5,
          Countering.controller = PlayerId.MkPlayerId 1
        }
      """ {"spell":4,"source":5,"controller":1} """
