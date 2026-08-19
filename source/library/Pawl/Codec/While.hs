{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.While where

import qualified Pawl.Codec.Condition as Condition
import qualified Pawl.Codec.PlayerId as PlayerId
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.While as While

-- | A bare object keyed by the record's field names, replacing the two-element
-- array this payload used to be.
codec :: Codec.Codec While.While
codec = Fields.object $ do
  player <- Fields.required "player" PlayerId.codec While.player
  condition <- Fields.required "condition" Condition.codec While.condition
  pure While.MkWhile {While.player = player, While.condition = condition}
