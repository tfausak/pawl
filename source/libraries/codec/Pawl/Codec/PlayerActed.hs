{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PlayerActed where

import qualified Data.Typeable as Typeable
import qualified Pawl.Codec.PlayerAction as PlayerAction
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PlayerActed as PlayerActed

-- | A bare object keyed by the record's field names, the player written with
-- whichever codec its parameter takes.
codec :: (Typeable.Typeable player) => Codec.Codec player -> Codec.Codec (PlayerActed.PlayerActed player)
codec player = Fields.object $ do
  action <- Fields.required "action" PlayerAction.codec PlayerActed.action
  actor <- Fields.required "player" player PlayerActed.player
  pure PlayerActed.MkPlayerActed {PlayerActed.action = action, PlayerActed.player = actor}
