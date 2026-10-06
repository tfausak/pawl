module Pawl.Types.ManifestedDread where

import qualified Data.Sequence as Seq
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 701.62b: a player completed CR 701.62a's manifest dread -- who, and the
-- cards that process put into their graveyard, as the incarnations CR 400.7
-- minted there. Empty where nothing reached the graveyard (an empty or one-card
-- library, or a CR 614 replacement sending the card elsewhere), which is still
-- an event: rule 701.62b fires the trigger even when the actions were
-- impossible.
data ManifestedDread = MkManifestedDread
  { player :: PlayerId.PlayerId,
    cards :: Seq.Seq ObjectId.ObjectId
  }
  deriving (Eq, Ord, Show)
