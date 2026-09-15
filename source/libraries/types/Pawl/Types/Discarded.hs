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
    cause :: DiscardCause.DiscardCause,
    -- | CR 702.35a's "exiled this way": rule 702.35a's own replacement is what
    -- redirected this discard into exile, rather than the card having been put
    -- into a graveyard or exiled by somebody else's redirect. False for every
    -- discard that is not one, which is every discard of a card without madness
    -- and every discard whose CR 616.1 choice fell on another row -- Rest in
    -- Peace's, which exiles the card without madness applying.
    --
    -- A FIELD beside the cause rather than a second event: the act is one
    -- discard, and rule 702.35a's trigger asks about the replacement applied to
    -- it, not about a further thing that happened.
    exiledForMadness :: Bool
  }
  deriving (Eq, Ord, Show)
