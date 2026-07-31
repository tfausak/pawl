-- | The @EventShape ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.EventShape where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.Zone (jsonToZone, zoneToJson)
import Pawl.Json.Array (Array (MkArray))
import Pawl.Json.Value (Value (Array))
import qualified Pawl.Types.EventShape as EventShape

eventShapeToJson :: EventShape.EventShape -> Value
eventShapeToJson s = case s of
  EventShape.MovedBetween from to -> Json.tagged (Text.pack "MovedBetween") (Just (Array (MkArray [zoneToJson from, zoneToJson to])))

jsonToEventShape :: Value -> Either Text EventShape.EventShape
jsonToEventShape value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("MovedBetween", Just (Array (MkArray [f, u]))) -> EventShape.MovedBetween <$> jsonToZone f <*> jsonToZone u
    _ -> Left (Text.pack "unknown EventShape: " <> t)
