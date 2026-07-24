module Pawl.Type.MulliganDecision where

-- CR 103.5: a player's per-round mulligan declaration. A sum type, not a Bool
-- (no boolean blindness): "declare whether they will take a mulligan."
data MulliganDecision
  = Mulligan
  | Keep
  deriving (Eq, Show)
