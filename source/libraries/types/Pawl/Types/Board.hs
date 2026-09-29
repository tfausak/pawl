module Pawl.Types.Board where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Pawl.Types.Label as Label
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Seat as Seat

-- | The observable state a scenario starts from, placed directly rather than
-- played in, on turn 1. Seat order is turn order.
data Board = MkBoard
  { seats :: NonEmpty.NonEmpty Seat.Seat,
    active :: Label.Label,
    phase :: Phase.Phase
  }
  deriving (Eq, Ord, Show)
