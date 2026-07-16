module Pawl.Type.Object where

import Numeric.Natural (Natural)
import Pawl.Type.PlayerId (PlayerId)
import Pawl.Type.Sickness (Sickness)
import Pawl.Type.Source (Source)
import Pawl.Type.TapState (TapState)
import Pawl.Type.Zone (Zone)

data Object = MkObject
  { owner :: PlayerId,
    source :: Source,
    zone :: Zone,
    tapped :: TapState,
    -- CR 120.3e: damage dealt to a creature is MARKED on it. A count, not a list
    -- of tagged units -- unlike mana, every damage rider (wither, infect,
    -- lifelink, toxic) is consumed at deal time and never re-read, and CR 704.5g
    -- itself reads only "the total damage marked on it". See the M1b spec, §2.
    --
    -- Removed at cleanup (CR 514.2). Per-incarnation state: reset by changeZone.
    damage :: Natural,
    -- CR 302.6. Per-incarnation state: reset by changeZone.
    sickness :: Sickness
  }
  deriving (Eq, Ord, Show)
