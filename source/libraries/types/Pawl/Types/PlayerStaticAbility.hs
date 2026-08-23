module Pawl.Types.PlayerStaticAbility where

import qualified Pawl.Types.Condition as Condition
import qualified Pawl.Types.PlayerEffect as PlayerEffect
import qualified Pawl.Types.PlayerScope as PlayerScope

-- | A card's printed player/rules-modifying static ability (CR 604.1/604.2: a
-- static ability creates a continuous effect active while its permanent is on
-- the battlefield). The player-axis sibling of Pawl.Types.StaticAbility, whose
-- Affected/Modification pair this mirrors with a PlayerScope/PlayerEffect pair.
--
-- Gathered LIVE from every battlefield permanent by Pawl.Engine.PlayerEffect.applying on
-- every read, never captured -- so Rule of Law leaving the battlefield lifts its
-- restriction with nothing to unwind. Most declarers state one -- Rule of Law,
-- Thalia, Sapphire Medallion, Reliquary Tower, Paladin Class -- while Null
-- Chamber states two, because CR 305.1 makes playing a land a special action and
-- its one printed sentence therefore prohibits on two different axes.
data PlayerStaticAbility = MkPlayerStaticAbility
  { scope :: PlayerScope.PlayerScope,
    -- | The ability's "as long as" clause -- Paladin Class' "during your turn" --
    -- or Nothing for an ability that functions unconditionally, which is most of
    -- them. Pawl.Types.StaticAbility.condition on the object-facing carrier, with
    -- the same meaning and the same CR 604.1/604.2 reading: a second gate on top
    -- of "on the battlefield and has the ability", re-asked on every read rather
    -- than latched.
    --
    -- Evaluated against the SOURCE permanent and from its controller's
    -- perspective, which is what makes "your turn" the Class controller's turn
    -- rather than the taxed player's -- CR 109.5's "you". Pawl.Engine.PlayerEffect
    -- gathers these rows AFTER the seven layers (CR 613.10/613.11), so the clause
    -- reads the finished projection, the answer CR 702.178a's max speed gate takes
    -- for the same reason.
    condition :: Maybe Condition.Condition,
    effect :: PlayerEffect.PlayerEffect
  }
  deriving (Eq, Ord, Show)
