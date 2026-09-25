module Pawl.Types.Printing where

import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Card as Card

data Printing = MkPrinting
  { card :: Card.Card,
    -- | CR 717.1: the numbers lit up on an Attraction, empty for every other
    -- card. Printing data rather than a characteristic (CR 109.3): two
    -- printings of one Attraction light different numbers.
    lights :: Set.Set Natural.Natural
  }
  deriving (Eq, Ord, Show)

-- | A printing with no lights, which is every card but an Attraction.
ofCard :: Card.Card -> Printing
ofCard c = MkPrinting {card = c, lights = Set.empty}
