{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.UsesSpec where

import qualified Pawl.Codec.Uses as Uses
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Uses as Uses

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Uses" $ do
  Spec.it s "Unlimited" $
    Common.assertJsonCodec
      s
      Uses.toJson
      Uses.fromJson
      Uses.Unlimited
      """ {"type":"Unlimited"} """
  Spec.it s "Once" $
    Common.assertJsonCodec
      s
      Uses.toJson
      Uses.fromJson
      Uses.Once
      """ {"type":"Once"} """
