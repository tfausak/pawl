-- | The @CounterPattern ⇆ Json@ codec (#481).
module Pawl.Codec.CounterPattern where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.ControllerRelation as ControllerRelation
import Pawl.Codec.CounterKind (counterKindToJson, jsonToCounterKind)
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.CounterPattern as CounterPattern

counterPatternToJson :: CounterPattern.CounterPattern -> Value
counterPatternToJson p =
  Json.jObject
    [ (Text.pack "whichKind", Json.maybeTo counterKindToJson (CounterPattern.whichKind p)),
      (Text.pack "whose", ControllerRelation.toJson (CounterPattern.whose p)),
      (Text.pack "onWhat", Filter.toJson (CounterPattern.onWhat p))
    ]

jsonToCounterPattern :: Value -> Either Text CounterPattern.CounterPattern
jsonToCounterPattern value = do
  ps <- Json.asObject value
  k <- Json.field (Text.pack "whichKind") ps >>= Json.maybeFrom jsonToCounterKind
  w <- Json.field (Text.pack "whose") ps >>= ControllerRelation.fromJson
  o <- Json.field (Text.pack "onWhat") ps >>= Filter.fromJson
  pure
    CounterPattern.MkCounterPattern
      { CounterPattern.whichKind = k,
        CounterPattern.whose = w,
        CounterPattern.onWhat = o
      }
