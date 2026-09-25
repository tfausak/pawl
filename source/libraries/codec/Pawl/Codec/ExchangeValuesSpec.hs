module Pawl.Codec.ExchangeValuesSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.ExchangeValues as ExchangeValues
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ExchangeValues as ExchangeValues
import qualified Pawl.Types.ExchangedValue as ExchangedValue
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ExchangeValues" $ do
  Spec.it s "MkExchangeValues" $
    Common.assertCodec
      s
      ExchangeValues.codec
      ( ExchangeValues.MkExchangeValues
          { ExchangeValues.one = ExchangedValue.LifeTotal (PlayerRef.Relative PlayerRelation.You),
            ExchangeValues.other = ExchangedValue.Toughness (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "self")))
          }
      )
      " {\"one\":{\"type\":\"LifeTotal\",\"value\":{\"type\":\"Relative\",\"value\":{\"type\":\"You\"}}},\"other\":{\"type\":\"Toughness\",\"value\":{\"type\":\"InSlot\",\"value\":\"self\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ExchangeValues.codec
