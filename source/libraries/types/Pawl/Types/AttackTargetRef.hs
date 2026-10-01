module Pawl.Types.AttackTargetRef where

import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.PlayerRef as PlayerRef

-- | CR 508.1b: WHAT a resolution-created attacking requirement says the creature
-- has to attack, named by the refs a resolution reads. The printed counterpart
-- of Pawl.Types.AttackTarget, whose arms it reaches at resolution.
data AttackTargetRef
  = -- | CR 508.1b: the players a ref names (Alluring Siren's "attacks you").
    Players PlayerRef.PlayerRef
  | -- | CR 306.6 / 310.5: the planeswalkers or battles a ref names (Gideon
    -- Jura's "attack Gideon Jura").
    Permanents ObjectRef.ObjectRef
  deriving (Eq, Ord, Show)
