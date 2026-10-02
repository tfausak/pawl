module Pawl.Types.Board where

import qualified Data.List.NonEmpty as NonEmpty
import Numeric.Natural (Natural)
import qualified Pawl.Types.AttackOption as AttackOption
import qualified Pawl.Types.Label as Label
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Seat as Seat

-- | The observable state a scenario starts from, placed directly rather than
-- played in. Seat order is turn order.
data Board = MkBoard
  { seats :: NonEmpty.NonEmpty Seat.Seat,
    active :: Label.Label,
    -- | CR 500.1: which turn of the game it is.
    turn :: Natural,
    phase :: Phase.Phase,
    -- | CR 725.1: the seat holding the monarch designation, if any.
    monarch :: Maybe Label.Label,
    -- | CR 806.2b: the attack option the game uses; Nothing is CR 507.1's
    -- choice among every opponent.
    attackOption :: Maybe AttackOption.AttackOption,
    -- | CR 903.12a: whether the game is Brawl.
    brawl :: Bool,
    -- | CR 805.1: whether each team takes its turns together.
    sharedTeamTurns :: Bool,
    -- | CR 804.2: whether each creature can be deployed to a teammate.
    deployCreatures :: Bool
  }
  deriving (Eq, Ord, Show)
