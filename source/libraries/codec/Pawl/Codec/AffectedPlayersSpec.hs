{-# LANGUAGE MultilineStrings #-}

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
    Common.assertJsonCodec
      s
      AffectedPlayers.toJson
      AffectedPlayers.fromJson
      (AffectedPlayers.Scoped PlayerScope.Opponents)
      """ {"type":"Scoped","value":{"type":"Opponents"}} """
  Spec.it s "Named" $
    Common.assertJsonCodec
      s
      AffectedPlayers.toJson
      AffectedPlayers.fromJson
      (AffectedPlayers.Named (SlotName.MkSlotName (Text.pack "target")))
      """ {"type":"Named","value":"target"} """
