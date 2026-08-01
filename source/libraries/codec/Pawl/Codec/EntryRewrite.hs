module Pawl.Codec.EntryRewrite where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.EntryOption as EntryOption
import qualified Pawl.Json.Array as Array
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.EntryRewrite as EntryRewrite

toJson :: EntryRewrite.EntryRewrite -> Value.Value
toJson r = case r of
  EntryRewrite.AsCopy -> Common.nullary "AsCopy"
  EntryRewrite.ChoiceOf options -> Common.tagged "ChoiceOf" . Just $ Common.encodeList EntryOption.toJson options
  EntryRewrite.WithCounters kind n -> Common.tagged "WithCounters" . Just . Common.array $ [CounterKind.toJson kind, Common.encodeNatural n]

fromJson :: Value.Value -> Either Text.Text EntryRewrite.EntryRewrite
fromJson value = do
  (t, mv) <- Common.asTagged value
  case (t, mv) of
    ("AsCopy", _) -> Right EntryRewrite.AsCopy
    ("ChoiceOf", Just v) -> EntryRewrite.ChoiceOf <$> Common.decodeList EntryOption.fromJson v
    ("WithCounters", Just (Value.Array (Array.MkArray [k, n]))) -> do
      kind <- CounterKind.fromJson k
      count <- Common.decodeNatural n
      pure (EntryRewrite.WithCounters kind count)
    _ -> Left . Text.pack $ "unknown EntryRewrite: " <> t
