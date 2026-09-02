module Pawl.Types.AimedAt where

import Data.Set (Set)
import qualified Pawl.Types.AttackTargetKind as AttackTargetKind
import qualified Pawl.Types.PlayerScope as PlayerScope

-- | CR 508.1c through CR 802.3a: what a resolution-generated attack restriction
-- says the attack may not be AIMED at -- whose side of the board, and which of
-- CR 506.3's attackable things on that side. Chronomantic Escape's "creatures
-- can't attack you".
--
-- Pawl.Types.CantAttackPlayer's defenders-and-kinds pair, split out so the stored
-- carrier (Pawl.Types.ActiveAttackProhibition) and the effect
-- (Pawl.Types.ForbidAttack) hold the same two fields; a PlayerScope and not a
-- seat for that type's reason, CR 109.5's "you" being read against the stored
-- controller on every declaration.
data AimedAt = MkAimedAt
  { defenders :: PlayerScope.PlayerScope,
    -- | CR 506.3: which announcements aimed at a player in 'defenders' are
    -- barred. Chronomantic Escape writes OfPlayer alone.
    kinds :: Set AttackTargetKind.AttackTargetKind
  }
  deriving (Eq, Ord, Show)
