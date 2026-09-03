module Pawl.Types.SlotName where

import qualified Data.Text as Text

-- | The name of a binding slot: an effect references a slot by name, and casting
-- fills it. Targets, modes, X and a cost payment's own bindings
-- (Pawl.Engine.Binding's sacrificedPermanent and tappedPermanent) all share this
-- one namespace. The dataflow lint checks every reference resolves, so a
-- dangling name is a failing test rather than a silent no-op. A slot a Create
-- bound to a whole GROUP of tokens is read by every ObjectRef-taking opcode; the
-- opcodes that still take a bare SlotName project its single target, which is
-- what each of them means -- Attach moves one object, Evolve and Mentor name one
-- creature, ChooseOpponent names a player -- see #1397 and #1398, which moved the
-- four that wanted a group.
newtype SlotName = MkSlotName
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)
