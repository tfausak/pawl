module Pawl.Types.Seat where

import qualified Data.Sequence as Seq
import qualified Pawl.Types.Label as Label
import qualified Pawl.Types.Placement as Placement

-- | One player on a scenario's board. Each zone lists its cards in creation
-- order, which is what numbers a card-name reference.
data Seat = MkSeat
  { name :: Label.Label,
    life :: Integer,
    battlefield :: Seq.Seq Placement.Placement,
    hand :: Seq.Seq Placement.Placement,
    graveyard :: Seq.Seq Placement.Placement,
    -- | Top card first (CR 401.1).
    library :: Seq.Seq Placement.Placement
  }
  deriving (Eq, Ord, Show)
