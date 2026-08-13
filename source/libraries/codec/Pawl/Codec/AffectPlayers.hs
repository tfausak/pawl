{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.AffectPlayers where

import qualified Pawl.Codec.AffectedPlayers as AffectedPlayers
import qualified Pawl.Codec.Duration as Duration
import qualified Pawl.Codec.PlayerEffect as PlayerEffect
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.AffectPlayers as AffectPlayers

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by Pawl.Codec.Effect's AffectPlayers arm.
codec :: Codec.Codec AffectPlayers.AffectPlayers
codec = Fields.object $ do
  duration <- Fields.required "duration" Duration.codec AffectPlayers.duration
  players <- Fields.required "players" AffectedPlayers.codec AffectPlayers.players
  effect <- Fields.required "effect" PlayerEffect.codec AffectPlayers.effect
  pure
    AffectPlayers.MkAffectPlayers
      { AffectPlayers.duration = duration,
        AffectPlayers.players = players,
        AffectPlayers.effect = effect
      }
