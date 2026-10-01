module Pawl.Types.AttackedPlayer where

import qualified Pawl.Types.PlayerRelation as PlayerRelation

-- | CR 508.3e's second subject: which attacked player fires the ability.
data AttackedPlayer
  = -- | A player standing in this relation to CR 109.5's "you" -- Lulu, Stern
    -- Guardian's "attacks you".
    Related PlayerRelation.PlayerRelation
  | -- | CR 303.4b: the player this ability's source enchants -- Archnemesis'
    -- "attack enchanted player".
    Enchanted
  deriving (Eq, Ord, Show)
