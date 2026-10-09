module Pawl.Types.StickerPlacement where

import qualified Pawl.Types.StickerRef as StickerRef
import qualified Pawl.Types.Timestamp as Timestamp

-- | CR 123.1: a sticker on an object.
data StickerPlacement = MkStickerPlacement
  { sticker :: StickerRef.StickerRef,
    -- | CR 613.7k.
    timestamp :: Timestamp.Timestamp
  }
  deriving (Eq, Ord, Show)
