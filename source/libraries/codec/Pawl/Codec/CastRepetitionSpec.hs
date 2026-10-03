module Pawl.Codec.CastRepetitionSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.CastRepetition as CastRepetition
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CastRepetition as CastRepetition
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CastRepetition" $ do
  Spec.it s "Once" $
    Common.assertCodec
      s
      CastRepetition.codec
      CastRepetition.Once
      " {\"type\":\"Once\"} "
  Spec.it s "AnyNumber" $
    Common.assertCodec
      s
      CastRepetition.codec
      CastRepetition.AnyNumber
      " {\"type\":\"AnyNumber\"} "
  Spec.it s "WithinTotalManaValue" $
    Common.assertCodec
      s
      CastRepetition.codec
      (CastRepetition.WithinTotalManaValue (Quantity.InSlot (SlotName.MkSlotName (Text.pack "X"))))
      " {\"type\":\"WithinTotalManaValue\",\"value\":{\"type\":\"InSlot\",\"value\":\"X\"}} "
  Spec.it s "has a schema" $
    Common.assertHasSchema s CastRepetition.codec
