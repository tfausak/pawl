module Pawl.Codec.ForbidAttackSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.ForbidAttack as ForbidAttack
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.ForbidAttack as ForbidAttack
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.ForbidAttack" $ do
  -- CR 508.1c. Netter en-Dal's own payload.
  Spec.it s "MkForbidAttack, both keys" $
    Common.assertCodec
      s
      ForbidAttack.codec
      ( ForbidAttack.MkForbidAttack
          { ForbidAttack.duration = Duration.UntilEndOfTurn,
            ForbidAttack.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))
          }
      )
      " {\"duration\":{\"type\":\"UntilEndOfTurn\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s ForbidAttack.codec
