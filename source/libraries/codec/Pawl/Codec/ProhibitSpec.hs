module Pawl.Codec.ProhibitSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.Prohibit as Prohibit
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Prohibit as Prohibit
import qualified Pawl.Types.Prohibition as Prohibition
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Prohibit" $ do
  -- CR 509.1b. Every key.
  Spec.it s "MkProhibit, every key" $
    Common.assertCodec
      s
      Prohibit.codec
      ( Prohibit.MkProhibit
          { Prohibit.what = Prohibition.Block,
            Prohibit.duration = Duration.UntilEndOfTurn,
            Prohibit.ref = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "target"))
          }
      )
      " {\"what\":{\"type\":\"Block\"},\"duration\":{\"type\":\"UntilEndOfTurn\"},\"ref\":{\"type\":\"InSlot\",\"value\":\"target\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s Prohibit.codec
