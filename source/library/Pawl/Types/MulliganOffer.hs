module Pawl.Types.MulliganOffer where

import qualified Numeric.Natural as Natural

-- | CR 103.5: what a player is told when they declare whether they will take a
-- mulligan -- how many they have already taken, and what taking another would
-- cost them.
--
-- The two are NOT the same number, which is why this is a record rather than a
-- bare Natural. CR 103.5 bottoms one card per mulligan taken, but CR 103.5c
-- exempts the first in a multiplayer game, so at three or more seats a player who
-- has taken one mulligan bottoms zero. An answerer sees only this payload, never
-- the GameState, so a raw count left it deciding with less information than the
-- rules give a player at a table.
--
-- Both fields are built by Pawl.Engine.Mulligan.offerFor, which is also what
-- takeMulligan bottoms by, so the prompt and the cost cannot drift apart.
data MulliganOffer = MkMulliganOffer
  { -- | CR 103.5c subtracts FROM this: the raw number of mulligans taken so far,
    -- 0 on the first declaration.
    taken :: Natural.Natural,
    -- | CR 103.5 / 103.5c: how many cards this player would put on the bottom of
    -- their library if they mulligan now. Uncapped by hand size -- the redraw
    -- has not happened yet, and Pawl.Engine.Mulligan.takeMulligan's own min against
    -- the redrawn hand is a totality guard, not a rule.
    bottomCount :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
