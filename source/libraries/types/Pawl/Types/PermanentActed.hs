module Pawl.Types.PermanentActed where

import qualified Pawl.Types.PermanentAction as PermanentAction

-- | CR 603.2: a permanent performed a Pawl.Types.PermanentAction, beside the
-- permanent the `permanent` parameter names -- the actor's id on
-- Pawl.Types.GameEvent's PermanentActed, and a Pawl.Types.ActingPermanent on
-- Pawl.Types.TriggerCondition's PermanentActs.
data PermanentActed permanent = MkPermanentActed
  { action :: PermanentAction.PermanentAction,
    permanent :: permanent
  }
  deriving (Eq, Ord, Show)
