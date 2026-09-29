module Pawl.Codec.DoesNotUntapNextSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.DoesNotUntapNext as DoesNotUntapNext
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.DoesNotUntapNext as DoesNotUntapNext
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DoesNotUntapNext" $ do
  -- CR 611.2a: Telekinesis' "next two untap steps" writes its count.
  Spec.it s "MkDoesNotUntapNext, both keys" $
    Common.assertCodec
      s
      DoesNotUntapNext.codec
      (DoesNotUntapNext.MkDoesNotUntapNext target 2)
      " {\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"},\"steps\":2} "
  -- Elvish Hunter's one step is the default, and is omitted.
  Spec.it s "MkDoesNotUntapNext, steps defaulted" $
    Common.assertCodec
      s
      DoesNotUntapNext.codec
      (DoesNotUntapNext.MkDoesNotUntapNext target 1)
      " {\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s DoesNotUntapNext.codec
  where
    target = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))
