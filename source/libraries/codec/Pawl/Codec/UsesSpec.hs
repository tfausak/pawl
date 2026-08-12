{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.UsesSpec where

import qualified Pawl.Codec.Uses as Uses
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Uses as Uses

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Uses" $ do
  Spec.it s "Unlimited" $
    Common.assertCodec
      s
      Uses.codec
      Uses.Unlimited
      """ {"type":"Unlimited"} """
  Spec.it s "Once" $
    Common.assertCodec
      s
      Uses.codec
      Uses.Once
      """ {"type":"Once"} """
  Spec.it s "has a schema" $
    Common.assertHasSchema s Uses.codec
