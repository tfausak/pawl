module Pawl.Codec.RequireBlockSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.RequireBlock as RequireBlock
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.RequireBlock as RequireBlock
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.RequireBlock" $ do
  -- CR 509.1c. BOTH sides are an ObjectRef, so the fixture names them
  -- differently on purpose: only an asymmetric case catches a codec that pointed
  -- the requirement the wrong way.
  Spec.it s "MkRequireBlock, all three keys" $
    Common.assertCodec
      s
      RequireBlock.codec
      ( RequireBlock.MkRequireBlock
          { RequireBlock.duration = Duration.UntilEndOfTurn,
            RequireBlock.blocker = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "blocker")),
            RequireBlock.attacker = ObjectRef.InSlot (SlotName.MkSlotName (Text.pack "attacker"))
          }
      )
      " {\"duration\":{\"type\":\"UntilEndOfTurn\"},\"blocker\":{\"type\":\"InSlot\",\"value\":\"blocker\"},\"attacker\":{\"type\":\"InSlot\",\"value\":\"attacker\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s RequireBlock.codec
