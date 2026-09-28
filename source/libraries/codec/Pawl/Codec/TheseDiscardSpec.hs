module Pawl.Codec.TheseDiscardSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.TheseDiscard as TheseDiscard
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.TheseDiscard as TheseDiscard

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.TheseDiscard" $ do
  Spec.it s "MkTheseDiscard, no bound slot" $
    Common.assertCodec
      s
      TheseDiscard.codec
      ( TheseDiscard.MkTheseDiscard
          { TheseDiscard.cards = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "revealed")),
            TheseDiscard.discarded = Nothing
          }
      )
      " {\"cards\":{\"type\":\"InSlot\",\"value\":\"revealed\"}} "
  Spec.it s "MkTheseDiscard, with the discarded slot" $
    Common.assertCodec
      s
      TheseDiscard.codec
      ( TheseDiscard.MkTheseDiscard
          { TheseDiscard.cards = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "revealed")),
            TheseDiscard.discarded = Just (SlotName.MkSlotName (Text.pack "discarded"))
          }
      )
      " {\"cards\":{\"type\":\"InSlot\",\"value\":\"revealed\"},\"discarded\":\"discarded\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s TheseDiscard.codec
