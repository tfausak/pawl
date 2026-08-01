-- | The @EntryRewrite ⇆ Json@ codec (#481).
module Pawl.Codec.EntryRewrite where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.CounterKind as CounterKind
import qualified Pawl.Codec.EntryOption as EntryOption
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Array (Array (MkArray))
import Pawl.Json.Value (Value (Array))
import qualified Pawl.Types.EntryRewrite as EntryRewrite

entryRewriteToJson :: EntryRewrite.EntryRewrite -> Value
entryRewriteToJson r = case r of
  EntryRewrite.AsCopy -> Json.nullary (Text.pack "AsCopy")
  EntryRewrite.ChoiceOf options -> Json.tagged (Text.pack "ChoiceOf") (Just (Json.listTo EntryOption.toJson options))
  EntryRewrite.WithCounters kind n -> Json.tagged (Text.pack "WithCounters") (Just (Array (MkArray [CounterKind.toJson kind, Json.natTo n])))

jsonToEntryRewrite :: Value -> Either Text EntryRewrite.EntryRewrite
jsonToEntryRewrite value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("AsCopy", _) -> Right EntryRewrite.AsCopy
    ("ChoiceOf", Just v) -> fmap EntryRewrite.ChoiceOf (Json.listFrom EntryOption.fromJson v)
    ("WithCounters", Just (Array (MkArray [k, n]))) -> do
      kind <- CounterKind.fromJson k
      count <- Json.natFrom n
      pure (EntryRewrite.WithCounters kind count)
    _ -> Left (Text.pack "unknown EntryRewrite: " <> t)
