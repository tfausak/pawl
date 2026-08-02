module Pawl.Types.Player where

import qualified Data.Map.Strict as Map
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.PlayerCounterKind as PlayerCounterKind
import qualified Pawl.Types.Status as Status

data Player = MkPlayer
  { life :: Integer,
    status :: Status.Status,
    -- | CR 122.1: player counters, counted per kind. Unlike object counters (CR
    -- 122.2, which cease to exist on a zone change), these persist for the whole
    -- game -- a player never changes zones. Absent kind means zero
    -- (Map.findWithDefault 0), the convention Object.counters uses.
    counters :: Map.Map PlayerCounterKind.PlayerCounterKind Natural.Natural
  }
  deriving (Eq, Show)
