module Pawl.Codec.PlayerId where

import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.JsonSchema.Schema as Schema
import qualified Pawl.Types.PlayerId as PlayerId

-- | SetController's PlayerId is meant to be runtime-only (baked at GainControl
-- resolution, not authored on a card), but the codec must stay total, so this
-- arm ACCEPTS one from card JSON and a corpus lint keeps the pool honest
-- (#199). Encoded as a Natural rather than a bare Integer, so a negative wire
-- value cannot go through a partial fromInteger.
codec :: Codec.Codec PlayerId.PlayerId
codec =
  Common.scalar
    Schema.natural
    (Common.encodeNatural . PlayerId.unwrap)
    (fmap PlayerId.MkPlayerId . Common.decodeNatural)
