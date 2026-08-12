{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.TapStateSpec where

import qualified Pawl.Codec.TapState as TapState
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.TapState as TapState

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TapState" $ do
  Spec.it s "Untapped" $
    Common.assertCodec
      s
      TapState.codec
      TapState.Untapped
      """ {"type":"Untapped"} """
  Spec.it s "Tapped" $
    Common.assertCodec
      s
      TapState.codec
      TapState.Tapped
      """ {"type":"Tapped"} """
  Spec.it s "has a schema" $
    Common.assertHasSchema s TapState.codec
