-- | The @EntryRewrite ⇆ Json@ codec, split out of the former
-- Pawl.Codec.All (#481).
module Pawl.Codec.EntryRewrite where

import Data.Text (Text)
import qualified Data.Text as Text
import Pawl.Codec.EntryOption (entryOptionToJson, jsonToEntryOption)
import qualified Pawl.Codec.Json as Json
import Pawl.Json.Value (Value)
import qualified Pawl.Types.EntryRewrite as EntryRewrite

entryRewriteToJson :: EntryRewrite.EntryRewrite -> Value
entryRewriteToJson r = case r of
  EntryRewrite.AsCopy -> Json.nullary (Text.pack "AsCopy")
  EntryRewrite.ChoiceOf options -> Json.tagged (Text.pack "ChoiceOf") (Just (Json.listTo entryOptionToJson options))

jsonToEntryRewrite :: Value -> Either Text EntryRewrite.EntryRewrite
jsonToEntryRewrite value = do
  (t, mv) <- Json.tag value
  case (Text.unpack t, mv) of
    ("AsCopy", _) -> Right EntryRewrite.AsCopy
    ("ChoiceOf", Just v) -> fmap EntryRewrite.ChoiceOf (Json.listFrom jsonToEntryOption v)
    _ -> Left (Text.pack "unknown EntryRewrite: " <> t)
