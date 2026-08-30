module Pawl.Types.ZoneChangeR where

import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern

-- | The payload of Pawl.Types.ReplacementEffect's ZoneChangeR arm (#1305): the
-- zone change intercepted, the zone the object goes to instead, and the two
-- actions the redirect may perform alongside the move.
--
-- Both riders default to False and are elided rather than written, DamageR's
-- `riders` posture: a redirect that only names a destination is the common
-- shape.
data ZoneChangeR = MkZoneChangeR
  { matching :: ZoneChangePattern.ZoneChangePattern,
    destination :: Zone.Zone,
    -- | CR 701.20a: show the moving card to all players, as Nexus of Fate's
    -- "reveal Nexus of Fate and shuffle it into its owner's library instead"
    -- does.
    revealing :: Bool,
    -- | CR 701.24a: randomize the moving card's owner's library, which is what
    -- separates "shuffle it into its owner's library" from putting it on the
    -- bottom.
    shuffling :: Bool
  }
  deriving (Eq, Ord, Show)
