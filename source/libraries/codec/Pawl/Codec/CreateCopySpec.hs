module Pawl.Codec.CreateCopySpec where

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Pawl.Codec.CreateCopy as CreateCopy
import qualified Pawl.Codec.EntryRiders as EntryRiders
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.CreateCopy as CreateCopy
import qualified Pawl.Types.EntryRiders as EntryRiders
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
      (CreateCopy.MkCreateCopy (Quantity.Literal 1) (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) EntryRiders.defaultValue)
      " {\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
  -- Kicked Rite of Replication's five.
  Spec.it s "MkCreateCopy, a count above one: it is written" $
    Common.assertCodec
      s
      CreateCopy.codec
      (CreateCopy.MkCreateCopy (Quantity.Literal 5) (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) EntryRiders.defaultValue)
      " {\"quantity\":{\"type\":\"Literal\",\"value\":5},\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
  -- Littjara Mirrorlake's "except it enters with an additional +1/+1 counter on
  -- it": CR 122.6's rider on the copy opcode, elided above when it is CR 110.5b's
  -- default.
  Spec.it s "MkCreateCopy, the counters the copy enters with" $
    Common.assertCodec
      s
      CreateCopy.codec
      (CreateCopy.MkCreateCopy (Quantity.Literal 1) (ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))) EntryRiders.defaultValue {EntryRiders.counters = Map.singleton CounterKind.PlusOnePlusOne (Quantity.Literal 1)})
      " {\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"},\"riders\":{\"counters\":[{\"kind\":{\"type\":\"PlusOnePlusOne\"},\"count\":{\"type\":\"Literal\",\"value\":1}}]}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CreateCopy.codec
