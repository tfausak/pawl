module Pawl.Codec.SlotPerPlayerSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.SlotPerPlayer as SlotPerPlayer
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.SlotPerPlayer as SlotPerPlayer

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.SlotPerPlayer" $ do
  Spec.it s "MkSlotPerPlayer, each opponent (Riptide Gearhulk)" $
    Common.assertCodec
      s
      SlotPerPlayer.codec
      ( SlotPerPlayer.MkSlotPerPlayer
          { SlotPerPlayer.players = PlayerRelation.Opponent,
            SlotPerPlayer.slot = SlotName.MkSlotName (Text.pack "thatPlayer")
          }
      )
      " {\"players\":{\"type\":\"Opponent\"},\"slot\":\"thatPlayer\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s SlotPerPlayer.codec
