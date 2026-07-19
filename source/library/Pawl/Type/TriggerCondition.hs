{-# LANGUAGE DeriveLift #-}

module Pawl.Type.TriggerCondition where

import Language.Haskell.TH.Syntax (Lift)

-- CR 603.6a: the event pattern that fires a triggered ability. SelfEnters =
-- "when this ... enters [the battlefield]" -- fires when the object bearing the
-- ability enters. A general "whenever a [type] enters" is a future condition.
-- Only Pawl.Event may case on it.
data TriggerCondition = SelfEnters
  deriving (Eq, Lift, Ord, Show)
