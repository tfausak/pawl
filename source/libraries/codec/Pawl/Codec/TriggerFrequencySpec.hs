{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.TriggerFrequencySpec where

import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.TriggerFrequency as TriggerFrequency
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.TriggerFrequency as TriggerFrequency

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TriggerFrequency" $ do
  Spec.it s "EveryTime" $
    Common.assertJsonCodec
      s
      TriggerFrequency.toJson
      TriggerFrequency.fromJson
      TriggerFrequency.EveryTime
      """ {"type":"EveryTime"} """
  Spec.it s "FirstTimeEachTurn" $
    Common.assertJsonCodec
      s
      TriggerFrequency.toJson
      TriggerFrequency.fromJson
      TriggerFrequency.FirstTimeEachTurn
      """ {"type":"FirstTimeEachTurn"} """
