module Pawl.Codec.AffectedPlayersSpec where

import qualified Data.Text as Text
import qualified Pawl.Codec.AffectedPlayers as AffectedPlayers
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AffectedPlayers as AffectedPlayers
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.PlayerScope as PlayerScope
import qualified Pawl.Types.SlotName as SlotName

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AffectedPlayers" $ do
  Spec.it s "Scoped" $
    Common.assertCodec
      s
      (AffectedPlayers.codec SlotName.codec)
      (AffectedPlayers.Scoped PlayerScope.Opponents)
      " {\"type\":\"Scoped\",\"value\":{\"type\":\"Opponents\"}} "
  Spec.it s "Named" $
    Common.assertCodec
      s
      (AffectedPlayers.codec SlotName.codec)
      (AffectedPlayers.Named (SlotName.MkSlotName (Text.pack "target")))
      " {\"type\":\"Named\",\"value\":\"target\"} "
  -- The other instantiation, which Pawl.Types.ActivePlayerEffect holds: the same
  -- two tags, with the seat CR 601.2c's slot was answered with in place of the
  -- slot name.
  Spec.it s "Named at a seat rather than a slot" $
    Common.assertCodec
      s
      (AffectedPlayers.codec PlayerId.codec)
      (AffectedPlayers.Named (PlayerId.MkPlayerId 2))
      " {\"type\":\"Named\",\"value\":2} "
  Spec.it s "has a schema" $ Common.assertHasSchema s (AffectedPlayers.codec SlotName.codec)
