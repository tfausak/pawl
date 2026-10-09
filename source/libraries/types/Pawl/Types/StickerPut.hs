module Pawl.Types.StickerPut where

import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.StickerKind as StickerKind

-- | CR 123.3: a player put a sticker of this kind on this object.
data StickerPut = MkStickerPut
  { placer :: PlayerId.PlayerId,
    object :: ObjectId.ObjectId,
    kind :: StickerKind.StickerKind
  }
  deriving (Eq, Ord, Show)
