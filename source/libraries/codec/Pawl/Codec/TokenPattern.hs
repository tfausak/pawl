module Pawl.Codec.TokenPattern where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.ControllerRelation as ControllerRelation
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.TokenPattern as TokenPattern

toJson :: TokenPattern.TokenPattern -> Value.Value
toJson p =
  Common.object [Common.pair "whose" . ControllerRelation.toJson $ TokenPattern.whose p]

fromJson :: Value.Value -> Either Text.Text TokenPattern.TokenPattern
fromJson value = do
  ps <- Common.asObject value
  w <- Common.field "whose" ps >>= ControllerRelation.fromJson
  pure TokenPattern.MkTokenPattern {TokenPattern.whose = w}
