{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PlayerCountersAre where

import qualified Pawl.Codec.Label as Label
import qualified Pawl.Codec.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PlayerCountersAre as PlayerCountersAre

codec :: Codec.Codec PlayerCountersAre.PlayerCountersAre
codec = Fields.object $ do
  player <- Fields.required "player" Label.codec PlayerCountersAre.player
  kind <- Fields.required "kind" PlayerCounterKind.codec PlayerCountersAre.kind
  count <- Fields.required "count" Common.natural PlayerCountersAre.count
  pure PlayerCountersAre.MkPlayerCountersAre {PlayerCountersAre.player = player, PlayerCountersAre.kind = kind, PlayerCountersAre.count = count}
