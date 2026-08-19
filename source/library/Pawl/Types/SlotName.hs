module Pawl.Types.SlotName where

import qualified Data.Text as Text

-- | The name of a binding slot: an effect references a slot by name, and casting
-- fills it. Targets, modes and X share this one namespace; a payment binding
-- would join it. The dataflow lint checks every reference resolves, so a
-- dangling name is a failing test rather than a silent no-op. Resolving is not
-- yet the whole guarantee: a slot a Create bound to a whole GROUP of tokens is
-- read by Effect.Sacrifice and by every ObjectRef-taking opcode, but the
-- remaining bare-SlotName opcodes still project the slot's single target, so one
-- of those naming a group resolves and does nothing (#760).
newtype SlotName = MkSlotName
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)
