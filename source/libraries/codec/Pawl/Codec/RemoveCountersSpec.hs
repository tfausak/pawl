{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.RemoveCountersSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.RemoveCounters as RemoveCounters
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.RemoveCounters as RemoveCounters
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.RemoveCounters" $ do
  -- A SLOT where PutCounters names an ObjectRef, which is why the two do not
  -- share a record despite coinciding in their other two fields.
  Spec.it s "MkRemoveCounters, all three keys" $
    Common.assertCodec
      s
      RemoveCounters.codec
      ( RemoveCounters.MkRemoveCounters
          { RemoveCounters.kind = CounterKind.PlusOnePlusOne,
            RemoveCounters.quantity = Quantity.Literal 1,
            RemoveCounters.slot = SlotName.MkSlotName (Text.pack "target")
          }
      )
      """ {"kind":{"type":"PlusOnePlusOne"},"quantity":{"type":"Literal","value":1},"slot":"target"} """
  Spec.it s "has a schema" $ Common.assertHasSchema s RemoveCounters.codec
