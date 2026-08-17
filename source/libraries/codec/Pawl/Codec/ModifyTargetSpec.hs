module Pawl.Codec.ModifyTargetSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.ModifyTarget as ModifyTarget
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Modification as Modification
import qualified Pawl.Types.ModifyTarget as ModifyTarget
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ModifyTarget" $ do
  -- CR 611: a modification over the objects the ref names, for a duration.
  Spec.it s "MkModifyTarget, all three keys" $
    Common.assertCodec
      s
      ModifyTarget.codec
      ( ModifyTarget.MkModifyTarget
          { ModifyTarget.duration = Duration.UntilEndOfTurn,
            ModifyTarget.modification = Modification.GainKeyword Keyword.Flying,
            ModifyTarget.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))
          }
      )
      " {\"duration\":{\"type\":\"UntilEndOfTurn\"},\"modification\":{\"type\":\"GainKeyword\",\"value\":{\"type\":\"Flying\"}},\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ModifyTarget.codec
