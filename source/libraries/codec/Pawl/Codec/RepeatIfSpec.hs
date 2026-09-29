module Pawl.Codec.RepeatIfSpec where

import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Pawl.Codec.RepeatIf as RepeatIf
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Compares as Compares
import qualified Pawl.Types.Comparison as Comparison
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.RepeatIf as RepeatIf
import qualified Pawl.Types.SlotName as SlotName

-- | Instantiated at 'Text.Text' for Pawl.Codec.ForEachSpec's reason.
codec :: Codec.Codec (RepeatIf.RepeatIf Text.Text)
codec = RepeatIf.codec Common.text

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.RepeatIf" $ do
  -- Grist, the Hunger Tide's shape. The two effect lists differ in length, so a
  -- codec that swapped them is caught.
  Spec.it s "MkRepeatIf" $
    Common.assertCodec
      s
      codec
      ( RepeatIf.MkRepeatIf
          { RepeatIf.process = Seq.fromList [Text.pack "create", Text.pack "mill"],
            RepeatIf.condition = Condition.Compares (Compares.MkCompares (Quantity.InSlot (SlotName.MkSlotName (Text.pack "milled"))) Comparison.AtLeast (Quantity.Literal 1)),
            RepeatIf.ifHolds = Seq.singleton (Text.pack "counter")
          }
      )
      " {\"process\":[\"create\",\"mill\"],\"condition\":{\"type\":\"Compares\",\"value\":{\"measured\":{\"type\":\"InSlot\",\"value\":\"milled\"},\"comparison\":{\"type\":\"AtLeast\"},\"threshold\":{\"type\":\"Literal\",\"value\":1}}},\"ifHolds\":[\"counter\"]} "
  Spec.it s "has a schema" $ Common.assertHasSchema s codec
