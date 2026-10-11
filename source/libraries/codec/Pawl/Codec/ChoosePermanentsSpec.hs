module Pawl.Codec.ChoosePermanentsSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.ChoosePermanents as ChoosePermanents
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.ChoosePermanents as ChoosePermanents
import qualified Pawl.Types.ChosenPermanents as ChosenPermanents
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.HowMany as HowMany
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
          { ChoosePermanents.permanents = ChosenPermanents.MkChosenPermanents (Filter.HasCardType CardType.Creature) (PlayerRef.Relative PlayerRelation.You) (HowMany.UpTo Nothing),
            ChoosePermanents.slot = SlotName.MkSlotName (Text.pack "chosen")
          }
      )
      " {\"permanents\":{\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},\"count\":{\"type\":\"UpTo\"}},\"slot\":\"chosen\"} "
  Spec.it s "MkChoosePermanents, another seat choosing up to two" $
    Common.assertCodec
      s
      ChoosePermanents.codec
      ( ChoosePermanents.MkChoosePermanents
          { ChoosePermanents.permanents = ChosenPermanents.MkChosenPermanents (Filter.HasCardType CardType.Creature) (PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "thatPlayer"))) (HowMany.UpTo (Just (Quantity.Literal 2))),
            ChoosePermanents.slot = SlotName.MkSlotName (Text.pack "chosen")
          }
      )
      " {\"permanents\":{\"filter\":{\"type\":\"HasCardType\",\"value\":{\"type\":\"Creature\"}},\"chooser\":{\"type\":\"InSlot\",\"value\":\"thatPlayer\"},\"count\":{\"type\":\"UpTo\",\"value\":{\"type\":\"Literal\",\"value\":2}}},\"slot\":\"chosen\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ChoosePermanents.codec
