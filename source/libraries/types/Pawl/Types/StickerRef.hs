module Pawl.Types.StickerRef where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.StickerKind as StickerKind

-- | CR 123.3a: one sticker, by where it is printed, never by its text.
data StickerRef = MkStickerRef
  { -- | CR 123.3: the player whose sheets it is on.
    owner :: PlayerId.PlayerId,
    -- | CR 123.3a: the sheet's position in that player's Player.stickerSheets.
    sheet :: Natural.Natural,
    kind :: StickerKind.StickerKind,
    -- | Which sticker of that kind on the sheet, from 0.
    index :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
