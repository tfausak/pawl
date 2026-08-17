module Pawl.Codec.AffectPlayersSpec where

import qualified Pawl.Codec.AffectPlayers as AffectPlayers
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Spec as Spec
import qualified Pawl.Types.AffectPlayers as AffectPlayers
import qualified Pawl.Types.AffectedPlayers as AffectedPlayers
import qualified Pawl.Types.Duration as Duration
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import qualified Pawl.Types.PlayerScope as PlayerScope

spec :: (Monad m, Monad n) => Spec.Spec m n -> n ()
spec s = Spec.describe s "Pawl.Codec.AffectPlayers" $ do
  -- A player effect installed over the players named, for a duration. The
  -- AffectedPlayers here is the SLOT instantiation, which is the only one with a
  -- codec: the PlayerId one is runtime-only and never reaches a card file.
  Spec.it s "MkAffectPlayers, all three keys" $
    Common.assertCodec
      s
      AffectPlayers.codec
      ( AffectPlayers.MkAffectPlayers
          { AffectPlayers.duration = Duration.UntilEndOfTurn,
            AffectPlayers.players = AffectedPlayers.Scoped PlayerScope.Opponents,
            AffectPlayers.effect = PlayerEffect.CantCastSpells
          }
      )
      " {\"duration\":{\"type\":\"UntilEndOfTurn\"},\"players\":{\"type\":\"Scoped\",\"value\":{\"type\":\"Opponents\"}},\"effect\":{\"type\":\"CantCastSpells\"}} "
  Spec.it s "has a schema" $ Common.assertHasSchema s AffectPlayers.codec
