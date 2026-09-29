module Pawl.Types.AttackersAre where

import qualified Data.Map.Strict as Map
import qualified Pawl.Types.Reference as Reference

-- | CR 508.1b: every attacking creature, each with the player, planeswalker or
-- battle it attacks.
newtype AttackersAre = MkAttackersAre
  { attackers :: Map.Map Reference.Reference Reference.Reference
  }
  deriving (Eq, Ord, Show)
