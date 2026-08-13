module Pawl.Types.Discarded where

import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 701.9a: a player discarded a card, and what the discard WAS.

-- The cause is a FIELD rather than a sibling constructor because CR 702.29a makes
-- cycling a discard: one act has to be visible to both a "when you cycle this
-- card" trigger and a "whenever a player discards a card" one, and CR 702.29d
-- caps a "cycles or discards" ability at one trigger per card. A separate Cycled
-- event beside this one would be a second record of a single discard, and any
-- reader matching both would answer twice.
data Discarded = MkDiscarded
  { player :: PlayerId.PlayerId,
    card :: ObjectId.ObjectId,
    cause :: DiscardCause.DiscardCause
  }
  deriving (Eq, Ord, Show)
