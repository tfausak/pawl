module Pawl.Codec.ManaCount where

import qualified Data.Text as Text
import qualified Pawl.Codec.ManaFilter as ManaFilter
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.Json.Value as Value
import qualified Pawl.JsonCodec.Common as Common
import qualified Pawl.Types.ManaCount as ManaCount

-- | A BARE OBJECT keyed by the record's field names, the shape every
-- single-constructor record in this codec writes. The tag that picks it is
-- written by Pawl.Codec.Quantity's ManaCount arm, this codec's only caller.
toJson :: ManaCount.ManaCount -> Value.Value
toJson count =
  Value.object
    ( Common.requiredPair "player" PlayerRef.toJson (ManaCount.player count)
        <> Common.requiredPair "filter" ManaFilter.toJson (ManaCount.filter count)
    )

fromJson :: Value.Value -> Either Text.Text ManaCount.ManaCount
fromJson value = do
  ps <- Common.asObject value
  p <- Common.field "player" ps >>= PlayerRef.fromJson
  f <- Common.field "filter" ps >>= ManaFilter.fromJson
  pure
    ManaCount.MkManaCount
      { ManaCount.player = p,
        ManaCount.filter = f
      }
