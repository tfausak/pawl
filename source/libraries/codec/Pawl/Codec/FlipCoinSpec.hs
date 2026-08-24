module Pawl.Codec.FlipCoinSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.FlipCoin as FlipCoin
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.FlipCoin as FlipCoin
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.FlipCoin" $ do
  -- The slot CR 705.2's win or loss is read back from.
  Spec.it s "MkFlipCoin" $
    Common.assertCodec
      s
      FlipCoin.codec
      FlipCoin.MkFlipCoin {FlipCoin.slot = SlotName.MkSlotName (Text.pack "flip")}
      " {\"slot\":\"flip\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s FlipCoin.codec
