{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.LifeIs where

import qualified Pawl.Codec.Label as Label
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.LifeIs as LifeIs

codec :: Codec.Codec LifeIs.LifeIs
codec = Fields.object $ do
  player <- Fields.required "player" Label.codec LifeIs.player
  life <- Fields.required "life" Common.integer LifeIs.life
  pure LifeIs.MkLifeIs {LifeIs.player = player, LifeIs.life = life}
