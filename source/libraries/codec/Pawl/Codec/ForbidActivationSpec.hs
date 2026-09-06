module Pawl.Codec.ForbidActivationSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.ForbidActivation as ForbidActivation
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.ForbidActivation as ForbidActivation
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ForbidActivation" $ do
  -- CR 602.2. Deadlock Trap's own payload.
  Spec.it s "MkForbidActivation, both keys" $
    Common.assertCodec
      s
      ForbidActivation.codec
      ( ForbidActivation.MkForbidActivation
          { ForbidActivation.duration = Duration.UntilEndOfTurn,
            ForbidActivation.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))
          }
      )
      " {\"duration\":{\"type\":\"UntilEndOfTurn\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ForbidActivation.codec
