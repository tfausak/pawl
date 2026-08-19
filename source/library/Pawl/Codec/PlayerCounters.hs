{-# LANGUAGE ApplicativeDo #-}

module Pawl.Codec.PlayerCounters where

import qualified Pawl.Codec.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Codec.Quantity as Quantity
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.PlayerCounters as PlayerCounters

-- | A bare object keyed by the record's field names. The tag that picks it is
-- written by whichever Pawl.Codec.Effect arm carries it, which is what lets
-- GainPlayerCounters and RemovePlayerCounters share one payload codec without
-- sharing a tag.
codec :: Codec.Codec PlayerCounters.PlayerCounters
codec = Fields.object $ do
  player <- Fields.required "player" PlayerRef.codec PlayerCounters.player
  kind <- Fields.required "kind" PlayerCounterKind.codec PlayerCounters.kind
  quantity <- Fields.required "quantity" Quantity.codec PlayerCounters.quantity
  pure
    PlayerCounters.MkPlayerCounters
      { PlayerCounters.player = player,
        PlayerCounters.kind = kind,
        PlayerCounters.quantity = quantity
      }
