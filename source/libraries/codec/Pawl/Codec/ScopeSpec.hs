{-# LANGUAGE MultilineStrings #-}

module Pawl.Codec.ScopeSpec where

import qualified Pawl.Codec.Scope as Scope
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.EventShape as EventShape
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.Scope as Scope
import qualified Pawl.Types.Zone as Zone

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.Scope" $ do
  Spec.it s "InZone" $
    Common.assertJsonCodec
      s
      Scope.toJson
      Scope.fromJson
      (Scope.InZone Zone.Battlefield PlayerRef.EachPlayer)
      """ {"type":"InZone","value":[{"type":"Battlefield"},{"type":"EachPlayer"}]} """
  -- CR 608.2i's look-back-in-time domain.
  Spec.it s "InHistory" $
    Common.assertJsonCodec
      s
      Scope.toJson
      Scope.fromJson
      (Scope.InHistory (EventShape.MovedBetween Zone.Battlefield Zone.Graveyard))
      """ {"type":"InHistory","value":{"type":"MovedBetween","value":[{"type":"Battlefield"},{"type":"Graveyard"}]}} """
  -- CR 102.1's domain: the players a reference names, rather than their zones.
  Spec.it s "OverPlayers" $
    Common.assertJsonCodec
      s
      Scope.toJson
      Scope.fromJson
      (Scope.OverPlayers (PlayerRef.Relative PlayerRelation.Opponent))
      """ {"type":"OverPlayers","value":{"type":"Relative","value":{"type":"Opponent"}}} """
