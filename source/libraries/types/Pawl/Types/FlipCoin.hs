module Pawl.Types.FlipCoin where

import qualified Pawl.Types.SlotName as SlotName

-- | The payload of Pawl.Types.Effect's FlipCoin arm (CR 705.1): the slot CR
-- 705.2's win or loss is bound at, for a later effect of the same resolution to
-- read as Pawl.Types.Quantity's InSlot.
--
-- The bound value is 1 for a flip the player WON and 0 for one they lost, which
-- is CR 705.2's win/lose reading and NOT its heads/tails reading. Quantity's
-- vocabulary is numeric -- IsMonarch, IsStartingPlayer and IsActivePlayer each
-- say so at their own sites -- so a yes-or-no arrives there as a comparison
-- against a 0 or a 1, and no new Quantity arm is owed.
--
-- Not implemented: CR 705.2's OTHER kind of effect, the one that "cares only
-- about whether the coin comes up heads or tails" and for which "no player wins
-- or loses a coin flip" (#2251). That reading wants a SECOND slot on this
-- record, holding the face rather than the outcome, and a flip with no winner
-- asks no Prompt.CallCoin at all -- never a reinterpretation of this slot, whose
-- 0 and 1 name a loss and a win.
--
-- Construct with BRACE syntax everywhere. That face slot is a new field, and
-- positional construction absorbs a new field in argument order with nothing red
-- (#2009, #2021).
newtype FlipCoin = MkFlipCoin
  { slot :: SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
