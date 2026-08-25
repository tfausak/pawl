module Pawl.Types.EntryFlip where

import qualified Pawl.Types.EntryOption as EntryOption

-- | CR 705.2's first sentence inside CR 614.1c: the two shapes a coin picks
-- between as a permanent enters. Molten Sentry's are (5,2,{Haste}) for heads
-- and (2,5,{Defender}) for tails.
--
-- Pawl.Types.EntryRewrite's ChoiceOf payload with the CHOOSER removed. That arm
-- offers its options to a player (CR 208.2b); this one offers them to nobody --
-- "some effects that instruct a player to flip a coin care only about whether
-- the coin comes up heads or tails. No player wins or loses a coin flip for this
-- kind of effect."
--
-- TWO NAMED FIELDS rather than a list: CR 705.1's coin has exactly two sides and
-- Pawl.Types.CoinFace has exactly two constructors, so a list would admit
-- malformed data (none, one, three) that ChoiceOf's arm has to defend against
-- and this one cannot be handed. Named rather than positional because both
-- fields have the same type, which is exactly the shape a positional
-- construction absorbs silently (#2009, #2021) -- construct with BRACE syntax
-- everywhere.
data EntryFlip = MkEntryFlip
  { heads :: EntryOption.EntryOption,
    tails :: EntryOption.EntryOption
  }
  deriving (Eq, Ord, Show)
