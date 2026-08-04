module Pawl.Types.SlotName where

import qualified Data.Text as Text

-- | The name of a binding slot: an effect references a slot by name, and casting
-- fills it. Targets, payments, modes and X share this one namespace. The dataflow
-- lint checks every reference resolves, so a dangling name is a failing test
-- rather than a silent no-op.
newtype SlotName = MkSlotName
  { unwrap :: Text.Text
  }
  deriving (Eq, Ord, Show)
