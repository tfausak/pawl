module Pawl.Codec.DurationRefSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.DurationRef as DurationRef
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.DurationRef as DurationRef
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.DurationRef" $ do
  -- GainControl's payload, and today its only user. Both keys are required:
  -- this payload has no optional part, which is what made it shareable at all,
  -- and an arm that grew one took its own type instead.
  Spec.it s "MkDurationRef, both keys" $
    Common.assertCodec
      s
      DurationRef.codec
      ( DurationRef.MkDurationRef
          { DurationRef.duration = Duration.UntilEndOfTurn,
            DurationRef.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))
          }
      )
      " {\"duration\":{\"type\":\"UntilEndOfTurn\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s DurationRef.codec
