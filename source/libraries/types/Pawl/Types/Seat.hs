module Pawl.Types.Seat where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Numeric.Natural (Natural)
import qualified Pawl.Types.Label as Label
import qualified Pawl.Types.ManaType as ManaType
import qualified Pawl.Types.Placement as Placement
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.TeamId as TeamId

-- | One player on a scenario's board. Each zone lists its cards in creation
-- order, which is what numbers a card-name reference.
data Seat = MkSeat
  { name :: Label.Label,
    life :: Integer,
    -- | CR 122.1: the player's counters, by kind.
    counters :: Map.Map PlayerCounterKind.PlayerCounterKind Natural,
    -- | CR 808.1: the player's team, if the game is played between teams.
    team :: Maybe TeamId.TeamId,
    -- | CR 801.2a: the player's range of influence; Nothing is unlimited.
    range :: Maybe Natural,
    -- | CR 809.2: whether the player is their team's emperor.
    emperor :: Bool,
    -- | CR 106.4: the unrestricted mana already in the player's pool, one type
    -- per unit.
    manaPool :: [ManaType.ManaType],
    battlefield :: Seq.Seq Placement.Placement,
    hand :: Seq.Seq Placement.Placement,
    graveyard :: Seq.Seq Placement.Placement,
    -- | Top card first (CR 401.1).
    library :: Seq.Seq Placement.Placement,
    -- | Face up (CR 406.3), owned by this seat.
    exile :: Seq.Seq Placement.Placement,
    -- | CR 408.1: the command zone, owned by this seat.
    command :: Seq.Seq Placement.Placement
  }
  deriving (Eq, Ord, Show)
