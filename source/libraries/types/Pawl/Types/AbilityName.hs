module Pawl.Types.AbilityName where

import qualified Data.Text as Text

-- | The name a card gives one of its own abilities, so another clause on the
-- same card can refer to it. SlotName's exact shape, and for the same reason:
-- named, never positional.
--
-- Two joins use it. A delayed triggered ability declared on a card (CR 603.7)
-- joins Effect.ArmDelayedTrigger to Face.delayedAbilities; and
-- Modification.LoseNamedAbility joins a CR 613.1f removal to the
-- ActivatedAbility.name it removes -- "this creature loses this ability", the
-- Licid clause, where "this ability" is the very one being resolved.
--
-- The delayed join is policed the way SlotName's is: the dataflow lint checks
-- that every armed name is declared and every declared name is armed, so a
-- dangling name is a failing test rather than a trigger that silently never
-- fires. Pawl.CardSpec's removal lint is the same check for the second join.
newtype AbilityName = MkAbilityName
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)
