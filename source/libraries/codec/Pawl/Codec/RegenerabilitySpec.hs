{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.RegenerabilitySpec where

import qualified Pawl.Codec.Regenerability as Regenerability
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Regenerability as Regenerability

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Regenerability" $ do
  Spec.it s "Regenerable" $
    Common.assertCodec
      s
      Regenerability.codec
      Regenerability.Regenerable
      """ {"type":"Regenerable"} """
  Spec.it s "CantBeRegenerated" $
    Common.assertCodec
      s
      Regenerability.codec
      Regenerability.CantBeRegenerated
      """ {"type":"CantBeRegenerated"} """
  Spec.it s "has a schema" $
    Common.assertHasSchema s Regenerability.codec
