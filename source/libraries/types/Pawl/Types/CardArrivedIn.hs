module Pawl.Types.CardArrivedIn where

import qualified Data.Set as Set
import qualified Pawl.Types.Zone as Zone

-- | CR 712.21e's second half: which zone the cards arrived in, and which origins
-- do not count. The payload of Pawl.Types.EventShape's arm of the same name,
-- which documents why the destination is the load-bearing half.
data CardArrivedIn = MkCardArrivedIn
  { to :: Zone.Zone,
    -- | CR 400.7: the origins an arrival is NOT counted from -- Dimir
    -- Strandcatcher's "from anywhere other than the battlefield". Empty is the
    -- rule's own default, "from anywhere", which is what CR 712.21e's "changed
    -- zones" puts no condition on and what every other producer in the pool
    -- asks.
    --
    -- An EXCLUSION rather than the inclusive origin set Pawl.Types.MovedBetween
    -- names, because that is the shape the printed template takes: an inclusive
    -- origin beside a destination is MovedBetween's own question asked of cards
    -- instead of objects, and no card in data/cards/ asks it.
    excluding :: Set.Set Zone.Zone
  }
  deriving (Eq, Ord, Show)
