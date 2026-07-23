module Pawl.Type.Player where

import Data.Map.Strict (Map)
import Numeric.Natural (Natural)
import Pawl.Type.PlayerCounterKind (PlayerCounterKind)
import Pawl.Type.Status (Status)

data Player = MkPlayer
  { life :: Integer,
    status :: Status,
    -- CR 122.1: player counters, counted per kind. Unlike object counters (CR
    -- 122.2, which cease to exist on a zone change), these persist for the whole
    -- game -- a player never changes zones. Absent kind means zero
    -- (Map.findWithDefault 0), the convention Object.counters uses.
    counters :: Map PlayerCounterKind Natural
  }
  deriving (Eq, Show)
