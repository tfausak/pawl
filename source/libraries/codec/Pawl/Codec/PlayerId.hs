-- | The @PlayerId ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.PlayerId where

import Data.Text (Text)
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.PlayerId as PlayerId

-- SetController's PlayerId is meant to be runtime-only (a SetController effect
-- is baked at GainControl resolution, not authored on a card), but the codec
-- must stay total, so this arm ACCEPTS one from card JSON and Pawl.CardSpec
-- lints the pool against it instead (#199). Mirrors ObjectId's Natural encoding
-- (Json.natTo/Json.natFrom), not a bare Integer: PlayerId wraps a Natural (no partial
-- fromInteger on a negative wire value).
playerIdToJson :: PlayerId.PlayerId -> Value
playerIdToJson (PlayerId.MkPlayerId n) = Json.natTo n

jsonToPlayerId :: Value -> Either Text PlayerId.PlayerId
jsonToPlayerId value = PlayerId.MkPlayerId <$> Json.natFrom value
