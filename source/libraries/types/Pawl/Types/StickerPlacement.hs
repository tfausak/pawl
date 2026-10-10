module Pawl.Types.StickerPlacement where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.StickerRef as StickerRef
import qualified Pawl.Types.Timestamp as Timestamp

-- | CR 123.1: a sticker on an object.
data StickerPlacement = MkStickerPlacement
  { sticker :: StickerRef.StickerRef,
    -- | CR 613.7k.
    timestamp :: Timestamp.Timestamp,
    -- | CR 123.6b-c: a name sticker's position, the number of words before it
    -- as it was placed; Nothing for the other kinds.
    position :: Maybe Natural.Natural
  }
  deriving (Eq, Ord, Show)
