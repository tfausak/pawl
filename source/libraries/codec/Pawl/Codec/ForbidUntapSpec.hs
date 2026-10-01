module Pawl.Codec.ForbidUntapSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.ForbidUntap as ForbidUntap
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.ForbidUntap as ForbidUntap
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ForbidUntap" $ do
  -- CR 502.3. Two keys, as Pawl.Codec.ForbidActivation's are.
  Spec.it s "MkForbidUntap, both keys" $
    Common.assertCodec
      s
      ForbidUntap.codec
      ( ForbidUntap.MkForbidUntap
          { ForbidUntap.duration = Duration.UntilEndOfTurn,
            ForbidUntap.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))
          }
      )
      " {\"duration\":{\"type\":\"UntilEndOfTurn\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ForbidUntap.codec
