module Pawl.Codec.ChoosePermanentsSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.ChoosePermanents as ChoosePermanents
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AnyNumberMatching as AnyNumberMatching
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.ChoosePermanents as ChoosePermanents
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ChoosePermanents" $ do
  Spec.it s "MkChoosePermanents, the resolving controller choosing elides the chooser" $
    Common.assertCodec
      s
      ChoosePermanents.codec
      ( ChoosePermanents.MkChoosePermanents
          { ChoosePermanents.chooser = PlayerRef.Relative PlayerRelation.You,
            ChoosePermanents.permanents = AnyNumberMatching.MkAnyNumberMatching (Filter.HasCardType CardType.Creature) Nothing,
            ChoosePermanents.slot = SlotName.MkSlotName (Text.pack "chosen")
          }
      )
      " {\"permanents\":{\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}}},\"slot\":\"chosen\"} "
  Spec.it s "MkChoosePermanents, another seat choosing up to two" $
    Common.assertCodec
      s
      ChoosePermanents.codec
      ( ChoosePermanents.MkChoosePermanents
          { ChoosePermanents.chooser = PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "thatPlayer")),
            ChoosePermanents.permanents = AnyNumberMatching.MkAnyNumberMatching (Filter.HasCardType CardType.Creature) (Just (Quantity.Literal 2)),
            ChoosePermanents.slot = SlotName.MkSlotName (Text.pack "chosen")
          }
      )
      " {\"chooser\":{\"type\":\"InSlot\",\"value\":\"thatPlayer\"},\"permanents\":{\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},\"atMost\":{\"type\":\"Literal\",\"value\":2}},\"slot\":\"chosen\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ChoosePermanents.codec
