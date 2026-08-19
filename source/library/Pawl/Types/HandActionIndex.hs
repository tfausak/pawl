module Pawl.Types.HandActionIndex where

import qualified Numeric.Natural as Natural

-- | CR 103.5b / CR 103.6: WHICH of a hand card's actions is meant, as an ordinal
-- into the actions that face prints (Pawl.Types.Face's mulliganActions and
-- openingHandActions). A card is free to grant more than one, and nothing in CR
-- 103 caps the number, so the granting card alone does not name an action.
--
-- An ORDINAL rather than the action's effects, the Pawl.Types.ModeIndex
-- precedent and against Pawl.Types.TriggerEntry's: unlike a triggered ability,
-- which reaches its batch from five different places, an action of this kind is
-- always read out of ONE printed list on ONE face, so the position into that
-- list exists and identifies the action exactly. Two actions printed identically
-- would show up as two entries where TriggerEntry would collapse them to one --
-- harmless here, since taking either performs the same effects on the same card.
--
-- A newtype, not a bare Natural, so a prompt that pairs it with an ObjectId
-- cannot silently take the wrong one of the two.
newtype HandActionIndex = MkHandActionIndex
  { unwrap :: Natural.Natural
  }
  deriving (Eq, Ord, Show)
