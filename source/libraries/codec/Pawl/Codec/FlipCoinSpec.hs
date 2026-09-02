module Pawl.Codec.FlipCoinSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.FlipCoin as FlipCoin
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CoinReading as CoinReading
import qualified Pawl.Types.FlipCoin as FlipCoin
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.FlipCoin" $ do
  -- One coin flipped for a win, the only shape data/cards/ printed before Flock
  -- of Rabid Sheep: both defaults elide, so this is the wire form every earlier
  -- card already carries.
  Spec.it s "MkFlipCoin" $
    Common.assertCodec
      s
      FlipCoin.codec
      FlipCoin.MkFlipCoin
        { FlipCoin.count = Quantity.Literal 1,
          FlipCoin.reading = CoinReading.Wins,
          FlipCoin.slot = SlotName.MkSlotName (Text.pack "flip")
        }
      " {\"slot\":\"flip\"} "
  Spec.it s "several coins read for their faces" $
    Common.assertCodec
      s
      FlipCoin.codec
      FlipCoin.MkFlipCoin
        { FlipCoin.count = Quantity.Literal 5,
          FlipCoin.reading = CoinReading.Heads,
          FlipCoin.slot = SlotName.MkSlotName (Text.pack "flip")
        }
      " {\"count\":{\"type\":\"Literal\",\"value\":5},\"reading\":{\"type\":\"Heads\"},\"slot\":\"flip\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s FlipCoin.codec
