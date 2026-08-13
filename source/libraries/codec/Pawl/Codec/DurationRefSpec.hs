{-# LANGUAGE MultilineStrings #-}

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
  -- Shared by PreventAllDamage, GainControl and GrantPlayFromExile, which differ
  -- only in their tag. Both keys are required: this payload has no optional
  -- part, which is what makes it shareable at all.
  Spec.it s "MkDurationRef, both keys" $
    Common.assertCodec
      s
      DurationRef.codec
      ( DurationRef.MkDurationRef
          { DurationRef.duration = Duration.UntilEndOfTurn,
            DurationRef.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))
          }
      )
      """ {"duration":{"type":"UntilEndOfTurn"},"ref":{"type":"InSlot","value":"target"}} """
  Spec.it s "has a schema" $ Common.assertHasSchema s DurationRef.codec
