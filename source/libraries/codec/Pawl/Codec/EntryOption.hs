module Pawl.Codec.EntryOption where

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.EntryOption as EntryOption

-- | 'power' and 'toughness' stay REQUIRED (R5 of the omit-defaults design): a
-- token whose printed characteristics are 0/0 is a legal EntryOption, so an
-- absent power must not read as 0 when it means the file forgot to state one.
toJson :: EntryOption.EntryOption -> Value.Value
toJson o =
  Value.object . concat $
    [ Common.requiredPair "power" Value.integer (EntryOption.power o),
      Common.requiredPair "toughness" Value.integer (EntryOption.toughness o),
      Common.optionalPair "keywords" Set.empty (Common.encodeSet Keyword.toJson) (EntryOption.keywords o)
    ]

fromJson :: Value.Value -> Either Text.Text EntryOption.EntryOption
fromJson value = do
  ps <- Common.asObject value
  p <- Common.field "power" ps >>= Common.asInteger
  t <- Common.field "toughness" ps >>= Common.asInteger
  ks <- Common.defaultedField "keywords" Set.empty (Common.decodeSet Keyword.fromJson) ps
  pure
    EntryOption.MkEntryOption
      { EntryOption.power = p,
        EntryOption.toughness = t,
        EntryOption.keywords = ks
      }
