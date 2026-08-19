module Pawl.Codec.BecomeCopySpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.BecomeCopy as BecomeCopy
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.BecomeCopy as BecomeCopy
import qualified Pawl.Types.Filter as Filter
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.BecomeCopy" $ do
  -- CR 707.4. BOTH sides are an ObjectRef and they are not interchangeable, so
  -- the fixture gives them DIFFERENT shapes -- Unstable Shapeshifter's own pair
  -- -- since only an asymmetric case catches a codec that swapped them and made
  -- the entrant the copy.
  Spec.it s "MkBecomeCopy, both keys" $
    Common.assertCodec
      s
      BecomeCopy.codec
      ( BecomeCopy.MkBecomeCopy
          { BecomeCopy.original = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "became")),
            BecomeCopy.subject = ObjectRef.EachMatching Filter.IsSource
          }
      )
      " {\"original\":{\"type\":\"InSlot\",\"value\":\"became\"},\"subject\":{\"type\":\"EachMatching\",\"value\":{\"type\":\"IsSource\"}}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s BecomeCopy.codec
