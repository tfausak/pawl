-- | The @EventShape ⇆ Json@ codec (#481).
module Pawl.Codec.EventShape where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import qualified Pawl.Codec.Zone as Zone
import Pawl.Json.Array (Array (MkArray))
import Pawl.Json.Value (Value (Array))
import qualified Pawl.Types.EventShape as EventShape

eventShapeToJson :: EventShape.EventShape -> Value
eventShapeToJson s = case s of
  EventShape.MovedBetween from to -> Json.tagged (Text.pack "MovedBetween") (Just (Array (MkArray [Zone.toJson from, Zone.toJson to])))

jsonToEventShape :: Value -> Either Text EventShape.EventShape
jsonToEventShape value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("MovedBetween", Just (Array (MkArray [f, u]))) -> EventShape.MovedBetween <$> Zone.fromJson f <*> Zone.fromJson u
    _ -> Left (Text.pack "unknown EventShape: " <> t)
