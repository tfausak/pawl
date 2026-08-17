module Pawl.Codec.ExchangeSidesSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.ExchangeSides as ExchangeSides
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ExchangeSides as ExchangeSides
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ExchangeSides" $ do
  Spec.it s "WithController" $
    Common.assertCodec
      s
      ExchangeSides.codec
      (ExchangeSides.WithController (SlotName.MkSlotName (Text.pack "target")))
      " {\"type\":\"WithController\",\"value\":\"target\"} "
  Spec.it s "BetweenTargets" $
    Common.assertCodec
      s
      ExchangeSides.codec
      (ExchangeSides.BetweenTargets (SlotName.MkSlotName (Text.pack "players")))
      " {\"type\":\"BetweenTargets\",\"value\":\"players\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ExchangeSides.codec
