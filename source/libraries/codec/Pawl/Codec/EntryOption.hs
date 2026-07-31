-- | The @EntryOption ⇆ Json@ codec (#481).
module Pawl.Codec.EntryOption where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Pawl.Codec.Json as Json
import Pawl.Codec.Keyword (jsonToKeyword, keywordToJson)
import Pawl.Json.Value (Value)
import qualified Pawl.Types.EntryOption as EntryOption

entryOptionToJson :: EntryOption.EntryOption -> Value
entryOptionToJson o =
  Json.jObject
    [ (Text.pack "power", Json.jInt (EntryOption.power o)),
      (Text.pack "toughness", Json.jInt (EntryOption.toughness o)),
      (Text.pack "keywords", Json.setTo keywordToJson (EntryOption.keywords o))
    ]

jsonToEntryOption :: Value -> Either Text EntryOption.EntryOption
jsonToEntryOption value = do
  ps <- Json.asObject value
  p <- Json.field (Text.pack "power") ps >>= Json.asInteger
  t <- Json.field (Text.pack "toughness") ps >>= Json.asInteger
  ks <- Json.field (Text.pack "keywords") ps >>= Json.setFrom jsonToKeyword
  pure
    EntryOption.MkEntryOption
      { EntryOption.power = p,
        EntryOption.toughness = t,
        EntryOption.keywords = ks
      }
