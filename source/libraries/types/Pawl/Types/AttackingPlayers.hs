module Pawl.Types.AttackingPlayers where

import qualified Pawl.Types.PlayerRelation as PlayerRelation
import qualified Pawl.Types.SlotName as SlotName

-- | CR 508.6: the players "attacking [a player]" -- the ones controlling a
-- creature that is attacking the player the slot names -- narrowed to those
-- standing in the given relation to CR 109.5's "you". Curse of Vitality's "each
-- opponent attacking that player" is the whole of it: `Opponent` and the slot the
-- Curse's own trigger bound.
--
-- Two fields rather than one, because the card text has two halves and neither
-- implies the other: the relation is about the ABILITY'S controller and the slot
-- is about a third player, and on a Curse enchanting its own controller the two
-- pick out different seats.
--
-- PRESENT tense, unlike Pawl.Engine.Turn.attackedThisStep's historical reading of
-- the same rule: the sentence says "attacking", so it is read off
-- Pawl.Types.Combat's live record as the effect applies, and a creature removed
-- from combat (CR 506.4) stops its controller being in the set.
data AttackingPlayers = MkAttackingPlayers
  { relation :: PlayerRelation.PlayerRelation,
    attacked :: SlotName.SlotName
  }
  deriving (Eq, Ord, Show)
