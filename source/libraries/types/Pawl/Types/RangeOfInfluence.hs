module Pawl.Types.RangeOfInfluence where

import qualified Data.Map.Strict as Map
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 801.2a: each player's range of influence, in seats. A player with no
-- entry has an unlimited range, which is every game not using CR 801's option.
--
-- A MAP and not one number, because CR 801.2a lets different players have
-- different ranges.
newtype RangeOfInfluence = MkRangeOfInfluence
  { unwrap :: Map.Map PlayerId.PlayerId Natural.Natural
  }
  deriving (Eq, Ord, Show)

-- | CR 801.1: a game not using the limited range of influence option.
unlimited :: RangeOfInfluence
unlimited = MkRangeOfInfluence Map.empty

-- | A player's range in seats, or Nothing for an unlimited one.
rangeOf :: RangeOfInfluence -> PlayerId.PlayerId -> Maybe Natural.Natural
rangeOf ranges pid = Map.lookup pid (unwrap ranges)
