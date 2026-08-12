{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.DefenseSpec where

import qualified Pawl.Codec.Defense as Defense
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Defense as Defense

spec :: (Monad m) => Spec.Spec m n -> n ()
spec s =
  Spec.describe s "Pawl.Codec.Defense" . Spec.it s "MkDefense" $
    Common.assertJsonCodec
      s
      Defense.toJson
      Defense.fromJson
      (Defense.MkDefense 5)
      """ 5 """
