module Pawl.Type.SlotName where

import Data.Text (Text)

-- The name of a binding slot (prior-art D4): an effect references a slot by
-- name; casting fills it. Targets are the first binding slots, not the last --
-- payments, modes, and X join this namespace in later milestones. The dataflow
-- lint (test suite) checks every reference resolves, so a dangling name is a
-- failing test, never a silent no-op.
newtype SlotName = MkSlotName Text
  deriving (Eq, Ord, Show)
