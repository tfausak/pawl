module Pawl.Codec.AffectedPlayersSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.AffectedPlayers as AffectedPlayers
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AffectedPlayers as AffectedPlayers
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AffectedPlayers" $ do
  Spec.it s "Scoped" $
    Common.assertCodec
      s
      AffectedPlayers.codec
      (AffectedPlayers.Scoped PlayerScope.Opponents)
      " {\"type\":\"Scoped\",\"value\":{\"type\":\"Opponents\"}} "
  Spec.it s "Named" $
    Common.assertCodec
      s
      AffectedPlayers.codec
      (AffectedPlayers.Named (SlotName.MkSlotName (Text.pack "target")))
      " {\"type\":\"Named\",\"value\":\"target\"} "
  Spec.it s "has a schema" $ Common.assertHasSchema s AffectedPlayers.codec
