module Pawl.Codec.CantBeRegeneratedSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.CantBeRegenerated as CantBeRegenerated
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.CantBeRegenerated as CantBeRegenerated
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.CantBeRegenerated" $ do
  -- CR 701.19c. Hurr Jackal's own payload.
  Spec.it s "MkCantBeRegenerated, both keys" $
    Common.assertCodec
      s
      CantBeRegenerated.codec
      ( CantBeRegenerated.MkCantBeRegenerated
          { CantBeRegenerated.duration = Duration.UntilEndOfTurn,
            CantBeRegenerated.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))
          }
      )
      " {\"duration\":{\"type\":\"UntilEndOfTurn\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s CantBeRegenerated.codec
