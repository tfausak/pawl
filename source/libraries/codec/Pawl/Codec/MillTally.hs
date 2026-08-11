module Pawl.Codec.MillTally where

import qualified Data.Text as Text
import qualified Pawl.Codec.Filter as Filter
import qualified Pawl.Codec.Keyword as Keyword
import qualified Pawl.Codec.SlotName as SlotName
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.MillTally as MillTally

-- | A BARE OBJECT keyed by the record's field names, the shape every
-- single-constructor record in this codec writes. Pawl.Codec.Effect's Mill arm
-- is the only caller, and it is what says the value is a mill's tally.
toJson :: MillTally.MillTally -> Value.Value
toJson tally =
  Common.object
    ( Common.requiredPair "slot" SlotName.toJson (MillTally.slot tally)
        <> Common.requiredPair "filter" (Filter.toJson Keyword.toJson) (MillTally.filter tally)
    )

fromJson :: Value.Value -> Either Text.Text MillTally.MillTally
fromJson value = do
  ps <- Common.asObject value
  s <- Common.field "slot" ps >>= SlotName.fromJson
  f <- Common.field "filter" ps >>= Filter.fromJson Keyword.fromJson
  pure
    MillTally.MkMillTally
      { MillTally.slot = s,
        MillTally.filter = f
      }
