module Pawl.Types.CoinFlipped where

import qualified Pawl.Types.PlayerId as PlayerId

-- | CR 705.1's flip as a game event: who flipped, and whether CR 705.2 made it a
-- win.
--
-- Recorded on EVERY flip rather than only on a won one, which is the whole
-- reason the outcome is a field instead of two constructors. CR 705.1 makes the
-- FLIP the thing that happened; CR 705.2's win or loss is a property of it,
-- decided after the fact by comparing the call to the face. A log that entered
-- nothing for a lost flip would have no entry for "whenever you flip a coin" to
-- match, and no entry at all for CR 705.2's first sentence -- the flips for which
-- "no player wins or loses" (#2251).
--
-- The flipper is CR 109.5's "you" on the resolving object, which CR 705.2's last
-- sentence ("no other players are involved") makes the only seat this event
-- concerns.
--
-- No FACE. CR 705.2 draws the line itself: an effect either cares about the
-- win\/loss or cares about heads\/tails, and Pawl.Types.FlipCoin's haddock argues
-- the two readings apart at length. A face field today would be the speculative
-- half of #2251, which is also where the winnerless flip goes -- and that flip
-- wants this record's outcome to become optional rather than a second event
-- beside it.
--
-- Construct with BRACE syntax everywhere. That face is a new field, and
-- positional construction absorbs a new field in argument order with nothing red
-- (#2009, #2021).
data CoinFlipped = MkCoinFlipped
  { flipper :: PlayerId.PlayerId,
    won :: Bool
  }
  deriving (Eq, Ord, Show)
