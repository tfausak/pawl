{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.DefendersAre where

import qualified Pawl.Codec.Label as Label
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.DefendersAre as DefendersAre

codec :: Codec.Codec DefendersAre.DefendersAre
codec = Fields.object $ do
  players <- Fields.required "players" (Common.list Label.codec) DefendersAre.players
  pure DefendersAre.MkDefendersAre {DefendersAre.players = players}
