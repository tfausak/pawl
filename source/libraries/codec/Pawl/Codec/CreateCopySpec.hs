module Pawl.Codec.CreateCopySpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.CreateCopy as CreateCopy
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CreateCopy as CreateCopy
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CreateCopy" $ do
  -- A count of one is the default, so every card that mints a single copy
  -- writes the ref alone. Before #1305 that was a second payload SHAPE rather
  -- than an omitted key.
  Spec.it s "MkCreateCopy, a single copy: the count is omitted" $
    Common.assertCodec
      s
      CreateCopy.codec
      (CreateCopy.MkCreateCopy (Quantity.Literal 1) (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      " {\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
  -- Kicked Rite of Replication's five.
  Spec.it s "MkCreateCopy, a count above one: it is written" $
    Common.assertCodec
      s
      CreateCopy.codec
      (CreateCopy.MkCreateCopy (Quantity.Literal 5) (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))))
      " {\"quantity\":{\"type\":\"Literal\",\"value\":5},\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CreateCopy.codec
