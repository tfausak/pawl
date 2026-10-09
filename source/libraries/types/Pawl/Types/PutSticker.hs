module Pawl.Types.PutSticker where

import qualified Data.Set as Set
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.StickerKind as StickerKind

-- | CR 123.3: the players `player` names each put an available sticker of
-- one of `kinds` on each named object they own.
--
-- Not implemented: CR 123.3d's move of a sticker already on an object (#4889).
data PutSticker = MkPutSticker
  { player :: PlayerRef.PlayerRef,
    ref :: ObjectRef.ObjectRef,
    kinds :: Set.Set StickerKind.StickerKind
  }
  deriving (Eq, Ord, Show)
