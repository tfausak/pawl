module Pawl.Types.MulliganOffer where

import qualified Numeric.Natural as Natural

-- | CR 103.5: what a player is told when they declare whether they will take a
-- mulligan -- how many they have already taken, and what taking another would
-- cost them.
--
-- The two are NOT the same number, which is the whole reason this is a record
-- and not the bare Natural it replaced. CR 103.5 bottoms "a number of those
-- cards equal to the number of times that player has taken a mulligan", but CR
-- 103.5c exempts the first mulligan in a multiplayer game, so at three or more
-- seats a player who has taken one mulligan bottoms zero cards, not one. An
-- answerer sees only this payload, never the GameState, so a raw count alone
-- left it deciding with strictly less information than the rules give a player
-- at a table.
--
-- Both fields are built by Pawl.Engine.Mulligan.offerFor, which is also what
-- takeMulligan bottoms by -- one function, so what the prompt promises and what
-- the mulligan actually costs cannot drift apart.
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
