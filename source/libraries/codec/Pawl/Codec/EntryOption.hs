module Pawl.Codec.EntryOption where

import qualified Data.Text as Text
import qualified Pawl.Codec.Common as Common
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Json.Value as Value
import qualified Pawl.Types.EntryOption as EntryOption

toJson :: EntryOption.EntryOption -> Value.Value
toJson o =
  Common.object
    [ Common.pair "power" . Common.integer $ EntryOption.power o,
      Common.pair "toughness" . Common.integer $ EntryOption.toughness o,
      Common.pair "keywords" . Common.encodeSet Keyword.toJson $ EntryOption.keywords o
    ]

fromJson :: Value.Value -> Either Text.Text EntryOption.EntryOption
fromJson value = do
  ps <- Common.asObject value
  p <- Common.field "power" ps >>= Common.asInteger
  t <- Common.field "toughness" ps >>= Common.asInteger
  ks <- Common.field "keywords" ps >>= Common.decodeSet Keyword.fromJson
  pure
    EntryOption.MkEntryOption
      { EntryOption.power = p,
        EntryOption.toughness = t,
        EntryOption.keywords = ks
      }
