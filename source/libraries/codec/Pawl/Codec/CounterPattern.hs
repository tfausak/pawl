module Pawl.Codec.CounterPattern where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.ControllerRelation as ControllerRelation
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.CounterPattern as CounterPattern

toJson :: CounterPattern.CounterPattern -> Value.Value
toJson p =
  Common.object
    [ Common.pair "whichKind" . Common.encodeMaybe CounterKind.toJson $ CounterPattern.whichKind p,
      Common.pair "whose" . ControllerRelation.toJson $ CounterPattern.whose p,
      Common.pair "onWhat" . Filter.toJson $ CounterPattern.onWhat p
    ]

fromJson :: Value.Value -> Either Text.Text CounterPattern.CounterPattern
fromJson value = do
  ps <- Common.asObject value
  k <- Common.field "whichKind" ps >>= Common.decodeMaybe CounterKind.fromJson
  w <- Common.field "whose" ps >>= ControllerRelation.fromJson
  o <- Common.field "onWhat" ps >>= Filter.fromJson
  pure
    CounterPattern.MkCounterPattern
      { CounterPattern.whichKind = k,
        CounterPattern.whose = w,
        CounterPattern.onWhat = o
      }
