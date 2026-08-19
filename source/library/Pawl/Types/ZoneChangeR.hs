module Pawl.Types.ZoneChangeR where

import qualified Pawl.Types.Zone as Zone
import qualified Pawl.Types.ZoneChangePattern as ZoneChangePattern

-- | The payload of Pawl.Types.ReplacementEffect's ZoneChangeR arm (#1305): the
-- zone change intercepted, and the zone the object goes to instead.
data ZoneChangeR = MkZoneChangeR
  { matching :: ZoneChangePattern.ZoneChangePattern,
    destination :: Zone.Zone
  }
  deriving (Eq, Ord, Show)
