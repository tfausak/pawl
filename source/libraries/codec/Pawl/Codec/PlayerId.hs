module Pawl.Codec.PlayerId where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.PlayerId as PlayerId

-- | SetController's PlayerId is meant to be runtime-only (a SetController effect
-- is baked at GainControl resolution, not authored on a card), but the codec
-- must stay total, so this arm ACCEPTS one from card JSON and Pawl.CardSpec
-- lints the pool against it instead (#199). Mirrors ObjectId's Natural encoding
-- (Common.encodeNatural/Common.decodeNatural), not a bare Integer: PlayerId
-- wraps a Natural (no partial fromInteger on a negative wire value).
toJson :: PlayerId.PlayerId -> Value.Value
toJson = Common.encodeNatural . PlayerId.unwrap

fromJson :: Value.Value -> Either Text.Text PlayerId.PlayerId
fromJson = fmap PlayerId.MkPlayerId . Common.decodeNatural
