module Pawl.Types.PlayerControl where

import qualified Pawl.Types.ControlDuration as ControlDuration
import qualified Pawl.Types.Decider as Decider

-- | CR 723: one player being controlled by another -- who decides for them, for
-- how long, and what rule 723.7's restriction says they may do.
--
-- Keyed per controlled player in GameState.control. A record and not a bare
-- Decider because rule 723.2's control is scoped to one resolution while rule
-- 723.1's runs for a turn, and rule 723.3 lets neither assume the controlled
-- player is the active one; see #881.
data PlayerControl = MkPlayerControl
  { decider :: Decider.Decider,
    duration :: ControlDuration.ControlDuration,
    -- | CR 723.7's restriction, in the one form a card prints it: Word of
    -- Command's "the player can activate mana abilities only if they're from
    -- lands that player controls". Read by Pawl.Engine.Mana.manaSourcesGiven.
    --
    -- A Bool for Pawl.Types.CastOffer's reason -- it is the presence or absence
    -- of one printed clause, and rule 723.7 is open-ended enough that a second
    -- restriction would be a second field rather than another value of this one.
    -- False is rule 723.1's unrestricted control.
    manaFromLandsOnly :: Bool
  }
  deriving (Eq, Ord, Show)
