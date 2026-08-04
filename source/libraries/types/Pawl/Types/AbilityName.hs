module Pawl.Types.AbilityName where

import qualified Data.Text as Text

-- | The name of a delayed triggered ability declared on a card (CR 603.7),
-- joining Effect.ArmDelayedTrigger to Face.delayedAbilities. SlotName's exact
-- shape, and for the same reason: named, never positional.
--
-- The join is policed the way SlotName's is: the dataflow lint checks that every
-- armed name is declared and every declared name is armed, so a dangling name is
-- a failing test rather than a trigger that silently never fires.
newtype AbilityName = MkAbilityName
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)
