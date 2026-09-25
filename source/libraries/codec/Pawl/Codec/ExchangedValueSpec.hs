module Pawl.Codec.ExchangedValueSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.ExchangedValue as ExchangedValue
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ExchangedValue as ExchangedValue
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ExchangedValue" $ do
  Spec.it s "LifeTotal" $
    Common.assertCodec
      s
      ExchangedValue.codec
      (ExchangedValue.LifeTotal (PlayerRef.Relative PlayerRelation.You))
      " {\"type\":\"LifeTotal\",\"value\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}}} "
  Spec.it s "Power" $
    Common.assertCodec
      s
      ExchangedValue.codec
      (ExchangedValue.Power (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "self"))))
      " {\"type\":\"Power\",\"value\":{\"type\":\"InSlot\",\"value\":\"self\"}} "
  Spec.it s "Toughness" $
    Common.assertCodec
      s
      ExchangedValue.codec
      (ExchangedValue.Toughness (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "self"))))
      " {\"type\":\"Toughness\",\"value\":{\"type\":\"InSlot\",\"value\":\"self\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ExchangedValue.codec
