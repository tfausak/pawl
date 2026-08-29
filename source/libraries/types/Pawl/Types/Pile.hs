module Pawl.Types.Pile where

import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Timestamp as Timestamp

-- | CR 406.4: one pile of face-down exiled cards, kept apart from the others by
-- when and how its cards were exiled. Named rather than enumerated, because the
-- whole point of the rule is that a chooser picks the pile without picking a
-- card in it; Pawl.Engine.Exile.pileOf is where a card is sorted into one.
data Pile
  = -- | CR 702.143e: a foretold card is kept differentiable from every other
    -- face-down card its owner owns, so it is a pile of its own, named by the
    -- timestamp its exile stamped on it (CR 613.7d).
    OfForetold Timestamp.Timestamp
  | -- | Every other face-down exiled card this player owns.
    --
    -- Not implemented: CR 406.4's separation of those into piles by when and how
    -- they were exiled, so two effects that each exile a hand face down make one
    -- pile here rather than two (#2566).
    OfFaceDown PlayerId.PlayerId
  deriving (Eq, Ord, Show)
