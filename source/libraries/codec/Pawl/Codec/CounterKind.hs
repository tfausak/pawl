module Pawl.Codec.CounterKind where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.CounterKind as CounterKind

-- | Not Common.decodeNullary's table shape any more: CR 122.1b's keyword counter
-- carries the keyword it grants, so this tags like every other payload-bearing
-- sum here rather than delegating the whole type to the nullary-table helper.
toJson :: CounterKind.CounterKind -> Value.Value
toJson k = case k of
  CounterKind.PlusOnePlusOne -> Common.nullary "PlusOnePlusOne"
  CounterKind.MinusOneMinusOne -> Common.nullary "MinusOneMinusOne"
  CounterKind.Keyword kw -> Common.tagged "Keyword" . Just $ Keyword.toJson kw
  CounterKind.Loyalty -> Common.nullary "Loyalty"
  CounterKind.Lore -> Common.nullary "Lore"

fromJson :: Value.Value -> Either Text.Text CounterKind.CounterKind
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("PlusOnePlusOne", _) -> Right CounterKind.PlusOnePlusOne
    ("MinusOneMinusOne", _) -> Right CounterKind.MinusOneMinusOne
    ("Keyword", Just v) -> CounterKind.Keyword <$> Keyword.fromJson v
    ("Loyalty", _) -> Right CounterKind.Loyalty
    ("Lore", _) -> Right CounterKind.Lore
    _ -> Left . Text.pack $ "unknown CounterKind: " <> t
