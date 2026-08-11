{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ExchangeSidesSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.ExchangeSides as ExchangeSides
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ExchangeSides as ExchangeSides
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ExchangeSides" $ do
  Spec.it s "WithController" $
    Common.assertJsonCodec
      s
      ExchangeSides.toJson
      ExchangeSides.fromJson
      (ExchangeSides.WithController (SlotName.MkSlotName (Text.pack "target")))
      """ {"type":"WithController","value":"target"} """
  Spec.it s "BetweenTargets" $
    Common.assertJsonCodec
      s
      ExchangeSides.toJson
      ExchangeSides.fromJson
      (ExchangeSides.BetweenTargets (SlotName.MkSlotName (Text.pack "players")))
      """ {"type":"BetweenTargets","value":"players"} """
