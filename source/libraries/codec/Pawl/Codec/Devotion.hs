{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Devotion where

import qualified Pawl.Codec.Color as Color
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Devotion as Devotion

-- | A bare object keyed by the record's field names; the tag that picks it is
-- Pawl.Codec.Quantity's "Devotion".
codec :: Codec.Codec Devotion.Devotion
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec Devotion.player
  colors <- Fields.required "colors" (Common.set Color.codec) Devotion.colors
  pure Devotion.MkDevotion {Devotion.player = player, Devotion.colors = colors}
