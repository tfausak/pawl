module Pawl.Types.AttackLimitUnless where

import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.PlayerScope as PlayerScope

-- | A SIZE BOUND on how many creatures may be declared as attackers, the players
-- CR 802.3a scopes it to, and CR 508.1c's "unless" gate. The bound names no
-- creature, which is why the key is @limit@ rather than @affected@.

-- Pawl.Types.LimitUnless's attacking half, split off it the moment `defenders`
-- gave this one a field the blocking half has no sentence for: CR 509.1b's bound
-- is already asked of one defending player's own declaration (CR 802.4b), so
-- there is nothing there for a scope to narrow.
data AttackLimitUnless = MkAttackLimitUnless
  { limit :: Natural.Natural,
    -- | CR 802.3a: Nothing is the bound on the WHOLE declaration (Silent
    -- Arbiter), Just the bound on the creatures attacking those players
    -- (Crawlspace's "can attack you"). Elided rather than written null.
    --
    -- Which announcements a scoped bound counts is not a second field: CR
    -- 802.3a counts "creatures attacking that player", and Crawlspace's own
    -- ruling spells that out as excluding the planeswalkers that player
    -- controls. So there is no Pawl.Types.AttackTargetKind set here as
    -- Pawl.Types.CantAttackPlayer has one.
    defenders :: Maybe PlayerScope.PlayerScope,
    -- | Nothing is the unconditional restriction. Elided rather than written
    -- null.
    unless :: Maybe Condition.Condition
  }
  deriving (Eq, Ord, Show)
