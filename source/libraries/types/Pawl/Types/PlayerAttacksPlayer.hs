module Pawl.Types.PlayerAttacksPlayer where

import qualified Pawl.Types.PlayerRelation as PlayerRelation

-- | CR 508.3e: which player's declaration fires the ability, and which player
-- they have to have sent creatures at -- Seifer, Balamb Rival's "whenever you
-- attack a player" against Mirkwood Trapper's "whenever a player attacks you".
--
-- A record rather than two positional relations for Pawl.Types.PlayerAttacksWith's
-- reason, and one of its own: the two fields have the same TYPE, so a positional
-- pair would let a construction site swap them with nothing to catch it.
data PlayerAttacksPlayer = MkPlayerAttacksPlayer
  { attacker :: PlayerRelation.PlayerRelation,
    attacked :: PlayerRelation.PlayerRelation
  }
  deriving (Eq, Ord, Show)
