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
    counters :: Map.Map PlayerCounterKind.PlayerCounterKind Natural.Natural,
    -- | CR 701.54c: how many times the Ring has tempted this player. Zero until
    -- it does, and only ever climbs -- nothing in rule 701.54 takes a temptation
    -- back.
    --
    -- Counted rather than derived, because CR 701.54d makes the count larger than
    -- anything the board remembers: "the Ring tempts a player whenever they
    -- complete the actions in 701.54a, EVEN IF SOME OR ALL OF THOSE ACTIONS WERE
    -- IMPOSSIBLE". A player with no creatures is tempted and designates nothing,
    -- so neither the Ring-bearer nor the emblem's existence can stand in for this.
    --
    -- NOT a PlayerCounterKind, though the field beside it would hold a Natural
    -- per player for free: CR 122.1 makes a counter a MARKER an effect can add or
    -- remove and a card can count, and rule 701.54 never calls this one. Proliferate
    -- (CR 701.34a) would find it if it were.
    --
    -- Read by nothing yet. CR 701.54c makes the emblem's ability set a function of
    -- this number, and none of its four tiers is built: the base one (#707), and
    -- the two-, three- and four-temptation ones (#706).
    ringTemptations :: Natural.Natural
  }
  deriving (Eq, Show)
