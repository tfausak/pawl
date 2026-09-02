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
-- match.
--
-- The outcome is a MAYBE because CR 705.2's first sentence describes flips for
-- which there is no outcome to record: "no player wins or loses a coin flip for
-- this kind of effect" (Molten Sentry, through
-- Pawl.Types.EntryRewrite's ChoiceByCoinFlip). Nothing is that flip, and it is
-- not the same claim as Just False -- a lost flip is one the flipper called and
-- missed.
--
-- Such a flip still reaches Just True when CR 705.3 states a win, which is
-- Edgar, King of Figaro and the sentence "this can cause a player to win a flip
-- that couldn't otherwise be won" -- proved by Pawl.CoinSpec's "CR 705.3 reaches
-- the flip CR 705.2 leaves winnerless".
--
-- NO BOARD TELLS Nothing FROM Just False TODAY.
-- TriggerCondition.PlayerWinsCoinFlip is the only reader of this field and
-- answers False to both. What would tell them apart is a "whenever you lose a
-- coin flip" condition, which does not exist (gap #2306, Karplusan Minotaur).
--
-- The flipper is CR 109.5's "you" on the resolving object, which CR 705.2's last
-- sentence ("no other players are involved") makes the only seat this event
-- concerns.
--
-- No FACE. CR 705.2 draws the line itself: an effect either cares about the
-- win\/loss or cares about heads\/tails, and Pawl.Types.FlipCoin's haddock argues
-- the two readings apart at length. Nothing in rule 603's vocabulary watches a
-- face -- TriggerCondition.PlayerWinsCoinFlip is the only condition over this
-- event -- and an effect that reads the face reads it off the tally
-- Pawl.Types.FlipCoin binds, never off the log.
--
-- Construct with BRACE syntax everywhere: positional construction absorbs a new
-- field in argument order with nothing red (#2009, #2021).
data CoinFlipped = MkCoinFlipped
  { flipper :: PlayerId.PlayerId,
    won :: Maybe Bool
  }
  deriving (Eq, Ord, Show)
