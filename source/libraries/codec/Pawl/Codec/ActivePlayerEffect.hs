{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.ActivePlayerEffect where

import qualified Pawl.Codec.AffectedPlayers as AffectedPlayers
import qualified Pawl.Codec.Expiry as Expiry
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerEffect as PlayerEffect
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.Codec.Timestamp as Timestamp
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ActivePlayerEffect as ActivePlayerEffect

-- | `scope` is the PLAYER instantiation of Pawl.Types.AffectedPlayers: a stored
-- row's Named arm holds the seat CR 601.2c's slot was answered with, where a card
-- writes the slot name. Pawl.Codec.AffectedPlayers takes its inner codec as a
-- parameter so both instantiations go through the one bundle.
codec :: Codec.Codec ActivePlayerEffect.ActivePlayerEffect
codec = Fields.object $ do
  source <- Fields.required "source" ObjectId.codec ActivePlayerEffect.source
  controller <- Fields.required "controller" PlayerId.codec ActivePlayerEffect.controller
  timestamp <- Fields.required "timestamp" Timestamp.codec ActivePlayerEffect.timestamp
  expiry <- Fields.required "expiry" Expiry.codec ActivePlayerEffect.expiry
  scope <- Fields.required "scope" (AffectedPlayers.codec PlayerId.codec) ActivePlayerEffect.scope
  effect <- Fields.required "effect" PlayerEffect.codec ActivePlayerEffect.effect
  pure
    ActivePlayerEffect.MkActivePlayerEffect
      { ActivePlayerEffect.source = source,
        ActivePlayerEffect.controller = controller,
        ActivePlayerEffect.timestamp = timestamp,
        ActivePlayerEffect.expiry = expiry,
        ActivePlayerEffect.scope = scope,
        ActivePlayerEffect.effect = effect
      }
