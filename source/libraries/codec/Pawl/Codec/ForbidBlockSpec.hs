module Pawl.Codec.ForbidBlockSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.ForbidBlock as ForbidBlock
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.ForbidBlock as ForbidBlock
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ForbidBlock" $ do
  -- CR 509.1b. Zirda, the Dawnwaker's own payload.
  Spec.it s "MkForbidBlock, both keys" $
    Common.assertCodec
      s
      ForbidBlock.codec
      ( ForbidBlock.MkForbidBlock
          { ForbidBlock.duration = Duration.UntilEndOfTurn,
            ForbidBlock.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))
          }
      )
      " {\"duration\":{\"type\":\"UntilEndOfTurn\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ForbidBlock.codec
