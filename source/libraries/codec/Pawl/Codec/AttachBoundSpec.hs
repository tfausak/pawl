module Pawl.Codec.AttachBoundSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.AttachBound as AttachBound
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AttachBound as AttachBound
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AttachBound" $ do
  -- CR 701.3a: the bound object moves to the targeted destination, so the two
  -- slot names are not interchangeable and both keys are required.
  Spec.it s "MkAttachBound, both keys" $
    Common.assertCodec
      s
      AttachBound.codec
      ( AttachBound.MkAttachBound
          { AttachBound.subject = SlotName.MkSlotName (Text.pack "became"),
            AttachBound.destination = SlotName.MkSlotName (Text.pack "target")
          }
      )
      " {\"subject\":\"became\",\"destination\":\"target\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s AttachBound.codec
