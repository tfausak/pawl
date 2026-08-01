-- | The @Countering ⇆ Json@ codec (#481).
module Pawl.Codec.Countering where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import qualified Pawl.Codec.ObjectId as ObjectId
import qualified Pawl.Codec.PlayerId as PlayerId
import Pawl.Json.Value (Value)
import qualified Pawl.Types.Countering as Countering

counteringToJson :: Countering.Countering -> Value
counteringToJson c =
  Json.jObject
    [ (Text.pack "spell", ObjectId.toJson (Countering.spell c)),
      (Text.pack "source", ObjectId.toJson (Countering.source c)),
      (Text.pack "controller", PlayerId.toJson (Countering.controller c))
    ]

jsonToCountering :: Value -> Either Text Countering.Countering
jsonToCountering value = do
  ps <- Json.asObject value
  s <- Json.field (Text.pack "spell") ps >>= ObjectId.fromJson
  o <- Json.field (Text.pack "source") ps >>= ObjectId.fromJson
  c <- Json.field (Text.pack "controller") ps >>= PlayerId.fromJson
  pure
    Countering.MkCountering
      { Countering.spell = s,
        Countering.source = o,
        Countering.controller = c
      }
