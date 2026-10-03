module Pawl.Types.AimedAt where

import Data.Set (Set)
import qualified Pawl.Types.AimedPlayers as AimedPlayers
import qualified Pawl.Types.AttackTargetKind as AttackTargetKind

-- | CR 508.1c through CR 802.3a: what a resolution-generated attack restriction
-- says the attack may not be AIMED at -- whose side of the board, and which of
-- CR 506.3's attackable things on that side. Chronomantic Escape's "creatures
-- can't attack you".
--
-- Pawl.Types.CantAttackPlayer's defenders-and-kinds pair, split out so the stored
-- carrier (Pawl.Types.ActiveAttackProhibition) and the effect
-- (Pawl.Types.ForbidAttack) hold the same two fields. Pawl.Types.AimedPlayers
-- rather than that type's PlayerScope, since a resolution can also name the
-- players a slot holds (Chaos Dragon).
data AimedAt = MkAimedAt
  { defenders :: AimedPlayers.AimedPlayers,
    -- | CR 506.3: which announcements aimed at a player in 'defenders' are
    -- barred. Chronomantic Escape writes OfPlayer alone.
    kinds :: Set AttackTargetKind.AttackTargetKind
  }
  deriving (Eq, Ord, Show)
