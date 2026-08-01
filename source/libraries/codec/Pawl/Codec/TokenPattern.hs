-- | The @TokenPattern ⇆ Json@ codec (#481).
module Pawl.Codec.TokenPattern where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.ControllerRelation as ControllerRelation
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.TokenPattern as TokenPattern

tokenPatternToJson :: TokenPattern.TokenPattern -> Value
tokenPatternToJson p =
  Json.jObject [(Text.pack "whose", ControllerRelation.toJson (TokenPattern.whose p))]

jsonToTokenPattern :: Value -> Either Text TokenPattern.TokenPattern
jsonToTokenPattern value = do
  ps <- Json.asObject value
  w <- Json.field (Text.pack "whose") ps >>= ControllerRelation.fromJson
  pure TokenPattern.MkTokenPattern {TokenPattern.whose = w}
