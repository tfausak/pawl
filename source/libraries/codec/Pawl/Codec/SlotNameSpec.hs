module Pawl.Codec.SlotNameSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m) => Spec.Spec m n -> n ()
spec s =
  Spec.describe s "Pawl.Codec.SlotName" . Spec.it s "MkSlotName" $
    Common.assertJsonCodec
      s
      SlotName.toJson
      SlotName.fromJson
      (SlotName.MkSlotName (Text.pack "target"))
      "\"target\""
