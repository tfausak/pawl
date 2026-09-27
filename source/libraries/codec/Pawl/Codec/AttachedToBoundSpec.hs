module Pawl.Codec.AttachedToBoundSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.AttachedToBound as AttachedToBound
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AttachedToBound as AttachedToBound
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Subtype as Subtype

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AttachedToBound" $ do
  -- CR 303.4b: the host is a slot and the attachments a Filter, both required.
  Spec.it s "MkAttachedToBound, both keys" $
    Common.assertCodec
      s
      AttachedToBound.codec
      ( AttachedToBound.MkAttachedToBound
          { AttachedToBound.slot = SlotName.MkSlotName (Text.pack "target"),
            AttachedToBound.filter = Filter.HasSubtype Subtype.Equipment
          }
      )
      " {\"slot\":\"target\",\"filter\":{\"type\":\"HasSubtype\",\"value\":{\"type\":\"Equipment\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s AttachedToBound.codec
