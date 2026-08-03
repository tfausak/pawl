{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.OnsetSpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Onset as Onset
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Onset as Onset

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Onset" $ do
  Spec.it s "Immediately" $
    Common.assertJsonCodec
      s
      Onset.toJson
      Onset.fromJson
      Onset.Immediately
      """ {"type":"Immediately"} """
  Spec.it s "FromYourNextTurn" $
    Common.assertJsonCodec
      s
      Onset.toJson
      Onset.fromJson
      Onset.FromYourNextTurn
      """ {"type":"FromYourNextTurn"} """
