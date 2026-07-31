-- | The @TokenPattern ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.TokenPattern where

import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.ControllerRelation (controllerRelationToJson, jsonToControllerRelation)
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.TokenPattern as TokenPattern

tokenPatternToJson :: TokenPattern.TokenPattern -> Value
tokenPatternToJson p =
  Json.jObject [(Text.pack "whose", controllerRelationToJson (TokenPattern.whose p))]

jsonToTokenPattern :: Value -> Either Text TokenPattern.TokenPattern
jsonToTokenPattern value = do
  ps <- Json.asObject value
  w <- Json.field (Text.pack "whose") ps >>= jsonToControllerRelation
  pure TokenPattern.MkTokenPattern {TokenPattern.whose = w}
