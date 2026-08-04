{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.DestructionRewriteSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.DestructionRewrite as DestructionRewrite
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DestructionRewrite as DestructionRewrite

spec :: (Monad m) => Spec.Spec m n -> n ()
spec s =
  Spec.describe s "Pawl.Codec.DestructionRewrite" . Spec.it s "Regenerate" $
    Common.assertJsonCodec
      s
      DestructionRewrite.toJson
      DestructionRewrite.fromJson
      DestructionRewrite.Regenerate
      """ {"type":"Regenerate"} """
