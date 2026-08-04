module Pawl.Codec.PlayerId where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.PlayerId as PlayerId

-- | SetController's PlayerId is meant to be runtime-only (baked at GainControl
-- resolution, not authored on a card), but the codec must stay total, so this
-- arm ACCEPTS one from card JSON and a corpus lint keeps the pool honest
-- (#199). Encoded as a Natural rather than a bare Integer, so a negative wire
-- value cannot go through a partial fromInteger.
toJson :: PlayerId.PlayerId -> Value.Value
toJson = Common.encodeNatural . PlayerId.unwrap

fromJson :: Value.Value -> Either Text.Text PlayerId.PlayerId
fromJson = fmap PlayerId.MkPlayerId . Common.decodeNatural
