module Pawl.Type.DamagePattern where

import Pawl.Type.DamageKind (DamageKind)

-- CR 615.1: which damage events a prevention intercepts. Fog is (Just Combat).
-- Nothing means any kind. CR 615's shields that name a SOURCE, an AMOUNT, or a
-- RECIPIENT are P9's to add; this carries the minimum Fog needs.
newtype DamagePattern = MkDamagePattern
  { whichKind :: Maybe DamageKind
  }
  deriving (Eq, Ord, Show)
