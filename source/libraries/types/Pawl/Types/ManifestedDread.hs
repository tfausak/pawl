module Pawl.Types.ManifestedDread where

import qualified Data.Sequence as Seq
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 701.62b: a player manifested dread, and the cards CR 701.62a put into
-- their graveyard as the incarnations CR 400.7 minted there -- none where the
-- library was too small or a replacement sent the card elsewhere.
data ManifestedDread = MkManifestedDread
  { player :: PlayerId.PlayerId,
    cards :: Seq.Seq ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)
