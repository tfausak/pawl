{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.PlayerIdSpec where

import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PlayerId as PlayerId

spec :: (Monad m) => Spec.Spec m n -> n ()
spec s =
  Spec.describe s "Pawl.Codec.PlayerId" . Spec.it s "MkPlayerId" $
    Common.assertJsonCodec
      s
      PlayerId.toJson
      PlayerId.fromJson
      (PlayerId.MkPlayerId 1)
      """ 1 """
