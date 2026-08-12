module Pawl.Codec.CounterKind where

import qualified Data.Text as Text
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.CounterKind as CounterKind

-- | Not Common.decodeNullary's table shape any more: CR 122.1b's keyword counter
-- carries the keyword it grants, so this tags like every other payload-bearing
-- sum here rather than delegating the whole type to the nullary-table helper.
-- Threads the keyword's own encoder, exactly as Pawl.Codec.Filter does and for
-- the same reason: the type is parametric in it.
toJson :: (keyword -> Value.Value) -> CounterKind.CounterKind keyword -> Value.Value
toJson encode k = case k of
  CounterKind.PlusOnePlusOne -> Common.nullary "PlusOnePlusOne"
  CounterKind.MinusOneMinusOne -> Common.nullary "MinusOneMinusOne"
  CounterKind.Keyword kw -> Common.tagged "Keyword" . Just $ encode kw
  CounterKind.Loyalty -> Common.nullary "Loyalty"
  CounterKind.Lore -> Common.nullary "Lore"
  CounterKind.Defense -> Common.nullary "Defense"
  CounterKind.Time -> Common.nullary "Time"
  CounterKind.Shield -> Common.nullary "Shield"

fromJson :: (Value.Value -> Either Text.Text keyword) -> Value.Value -> Either Text.Text (CounterKind.CounterKind keyword)
fromJson decode value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("PlusOnePlusOne", _) -> Right CounterKind.PlusOnePlusOne
    ("MinusOneMinusOne", _) -> Right CounterKind.MinusOneMinusOne
    ("Keyword", Just v) -> CounterKind.Keyword <$> decode v
    ("Loyalty", _) -> Right CounterKind.Loyalty
    ("Lore", _) -> Right CounterKind.Lore
    ("Defense", _) -> Right CounterKind.Defense
    ("Time", _) -> Right CounterKind.Time
    ("Shield", _) -> Right CounterKind.Shield
    _ -> Left . Text.pack $ "unknown CounterKind: " <> t
