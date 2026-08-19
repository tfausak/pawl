{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.Drew where

import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.Drew as Drew

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be. Runtime-only: GameEvent serialises transcripts,
-- never card data.
codec :: Codec.Codec Drew.Drew
codec = Fields.object $ do
  player <- Fields.required "player" PlayerId.codec Drew.player
  nth <- Fields.required "nth" Common.natural Drew.nth
  pure
    Drew.MkDrew
      { Drew.player = player,
        Drew.nth = nth
      }
