module Pawl.Codec.GraveyardScopeSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.GraveyardScope as GraveyardScope
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.GraveyardScope as GraveyardScope
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.GraveyardScope" $ do
  Spec.it s "Scoped" $
    Common.assertCodec
      s
      GraveyardScope.codec
      (GraveyardScope.Scoped PlayerScope.You)
      " {\"type\":\"Scoped\",\"value\":{\"type\":\"You\"}} "
  Spec.it s "InSlot" $
    Common.assertCodec
      s
      GraveyardScope.codec
      (GraveyardScope.InSlot (SlotName.MkSlotName (Text.pack "player")))
      " {\"type\":\"InSlot\",\"value\":\"player\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s GraveyardScope.codec
