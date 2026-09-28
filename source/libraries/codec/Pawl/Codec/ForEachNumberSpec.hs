module Pawl.Codec.ForEachNumberSpec where

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.ForEachNumber as ForEachNumber
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.ForEachNumber as ForEachNumber
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

-- | Instantiated at 'Text.Text' for Pawl.Codec.ForEachSpec's reason.
codec :: Codec.Codec (ForEachNumber.ForEachNumber Text.Text)
codec = ForEachNumber.codec Common.text

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ForEachNumber" $ do
  -- Ornate Imitations' shape: up to the spell's X.
  Spec.it s "MkForEachNumber" $
    Common.assertCodec
      s
      codec
      ( ForEachNumber.MkForEachNumber
          { ForEachNumber.upTo = Quantity.InSlot (SlotName.MkSlotName (Text.pack "X")),
            ForEachNumber.slot = SlotName.MkSlotName (Text.pack "number"),
            ForEachNumber.body = Seq.fromList [Text.pack "first", Text.pack "second"]
          }
      )
      " {\"upTo\":{\"type\":\"InSlot\",\"value\":\"X\"},\"slot\":\"number\",\"body\":[\"first\",\"second\"]} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
