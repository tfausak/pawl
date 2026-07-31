-- | The @CounterKind ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.CounterKind where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.Keyword (jsonToKeyword, keywordToJson)
import Pawl.Json.Value (Value)
import qualified Pawl.Types.CounterKind as CounterKind

-- No longer uniformly nullary: CR 122.1b's keyword counter carries the keyword it
-- grants, so this tags like every other payload-bearing sum here rather than
-- delegating the whole type to the Json.nullary helper.
counterKindToJson :: CounterKind.CounterKind -> Value
counterKindToJson k = case k of
  CounterKind.PlusOnePlusOne -> Json.nullary (Text.pack "PlusOnePlusOne")
  CounterKind.MinusOneMinusOne -> Json.nullary (Text.pack "MinusOneMinusOne")
  CounterKind.Keyword kw -> Json.tagged (Text.pack "Keyword") (Just (keywordToJson kw))

jsonToCounterKind :: Value -> Either Text CounterKind.CounterKind
jsonToCounterKind value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("PlusOnePlusOne", _) -> Right CounterKind.PlusOnePlusOne
    ("MinusOneMinusOne", _) -> Right CounterKind.MinusOneMinusOne
    ("Keyword", Just v) -> CounterKind.Keyword <$> jsonToKeyword v
    _ -> Left (Text.pack "unknown CounterKind: " <> t)
