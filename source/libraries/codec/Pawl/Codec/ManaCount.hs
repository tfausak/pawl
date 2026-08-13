module Pawl.Codec.ManaCount where

import qualified Pawl.Codec.ManaFilter as ManaFilter
import qualified Pawl.Codec.PlayerRef as PlayerRef
import qualified Pawl.JsonCodec.Codec as Codec
import qualified Pawl.JsonCodec.Fields as Fields
import qualified Pawl.Types.ManaCount as ManaCount

-- | A BARE OBJECT keyed by the record's field names, the shape every
-- single-constructor record in this codec writes. The tag that picks it is
-- written by Pawl.Codec.Quantity's ManaCount arm, this codec's only caller.
codec :: Codec.Codec ManaCount.ManaCount
codec =
  Fields.object $
    ManaCount.MkManaCount
      <$> Fields.required "player" PlayerRef.codec ManaCount.player
      <*> Fields.required "filter" ManaFilter.codec ManaCount.filter
