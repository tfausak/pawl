module Pawl.Codec.CountedDiscardSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.CountedDiscard as CountedDiscard
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CountedDiscard as CountedDiscard
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CountedDiscard" $ do
  -- The bound slot is ELIDED when absent, so this is byte-identical to what the
  -- two-field record wrote before the slot existed.
  Spec.it s "MkCountedDiscard, both keys" $
    Common.assertCodec
      s
      CountedDiscard.codec
      ( CountedDiscard.MkCountedDiscard
          { CountedDiscard.slot = SlotName.MkSlotName (Text.pack "player"),
            CountedDiscard.quantity = Quantity.Literal 1,
            CountedDiscard.discarded = Nothing
          }
      )
      " {\"slot\":\"player\",\"quantity\":{\"type\":\"Literal\",\"value\":1}} "
  Spec.it s "MkCountedDiscard, with the discarded slot" $
    Common.assertCodec
      s
      CountedDiscard.codec
      ( CountedDiscard.MkCountedDiscard
          { CountedDiscard.slot = SlotName.MkSlotName (Text.pack "player"),
            CountedDiscard.quantity = Quantity.Literal 1,
            CountedDiscard.discarded = Just (SlotName.MkSlotName (Text.pack "discarded"))
          }
      )
      " {\"slot\":\"player\",\"quantity\":{\"type\":\"Literal\",\"value\":1},\"discarded\":\"discarded\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CountedDiscard.codec
