module Pawl.Types.PlacesSticker where

import qualified Data.Set as Set
import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.StickerKind as StickerKind

-- | CR 123.3: "whenever [a player] places a sticker", narrowed to these
-- kinds, PermanentGetsCounters' posture for a counter kind.
data PlacesSticker = MkPlacesSticker
  { placer :: PlayerRelation.PlayerRelation,
    kinds :: Set.Set StickerKind.StickerKind
  }
  deriving (Eq, Ord, Show)
