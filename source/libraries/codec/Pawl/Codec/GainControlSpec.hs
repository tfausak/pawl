module Pawl.Codec.GainControlSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.GainControl as GainControl
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.GainControl as GainControl
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.GainControl" $ do
  Spec.it s "MkGainControl, to the effect's controller elided" $
    Common.assertCodec
      s
      GainControl.codec
      ( GainControl.MkGainControl
          { GainControl.duration = Duration.UntilEndOfTurn,
            GainControl.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")),
            GainControl.to = PlayerRef.Relative PlayerRelation.You
          }
      )
      " {\"duration\":{\"type\":\"UntilEndOfTurn\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
  Spec.it s "MkGainControl, to a named player" $
    Common.assertCodec
      s
      GainControl.codec
      ( GainControl.MkGainControl
          { GainControl.duration = Duration.Indefinite,
            GainControl.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target")),
            GainControl.to = PlayerRef.InSlot (SlotName.MkSlotName (Text.pack "teammate"))
          }
      )
      " {\"duration\":{\"type\":\"Indefinite\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"},\"to\":{\"type\":\"InSlot\",\"value\":\"teammate\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s GainControl.codec
