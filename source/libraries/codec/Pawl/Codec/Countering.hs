-- | The @Countering ⇆ Json@ codec (#481).
module Pawl.Codec.Countering where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.ObjectId (jsonToObjectId, objectIdToJson)
import Pawl.Codec.PlayerId (jsonToPlayerId, playerIdToJson)
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Countering as Countering

counteringToJson :: Countering.Countering -> Value
counteringToJson c =
  Json.jObject
    [ (Text.pack "spell", objectIdToJson (Countering.spell c)),
      (Text.pack "source", objectIdToJson (Countering.source c)),
      (Text.pack "controller", playerIdToJson (Countering.controller c))
    ]

jsonToCountering :: Value -> Either Text Countering.Countering
jsonToCountering value = do
  ps <- Json.asObject value
  s <- Json.field (Text.pack "spell") ps >>= jsonToObjectId
  o <- Json.field (Text.pack "source") ps >>= jsonToObjectId
  c <- Json.field (Text.pack "controller") ps >>= jsonToPlayerId
  pure
    Countering.MkCountering
      { Countering.spell = s,
        Countering.source = o,
        Countering.controller = c
      }
