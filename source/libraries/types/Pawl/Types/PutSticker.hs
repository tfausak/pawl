module Pawl.Types.PutSticker where

import qualified Data.Set as Set
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayerRef as PlayerRef
import qualified Pawl.Types.Quantity as Quantity
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.StickerKind as StickerKind

-- | CR 123.3: the players `player` names each put an available sticker of
-- one of `kinds` on each named object they own.
--
-- Not implemented: CR 123.3d's move of a sticker already on an object (#4889).
data PutSticker = MkPutSticker
  { player :: PlayerRef.PlayerRef,
    ref :: ObjectRef.ObjectRef,
    kinds :: Set.Set StickerKind.StickerKind,
    -- | CR 123.3c: "with ticket cost X or less" (Pin Collection).
    ticketCap :: Maybe Quantity.Quantity,
    -- | CR 123.3c: "without paying that sticker's ticket cost".
    free :: Bool,
    -- | CR 123.6e: the slot the placed sticker is bound under, for "that
    -- sticker".
    bound :: Maybe SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
