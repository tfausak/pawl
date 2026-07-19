module Pawl.Type.Object where

import Data.Map.Strict (Map)
import Numeric.Natural (Natural)
import Pawl.Type.PlayerId (PlayerId)
import Pawl.Type.Recipient (Recipient)
import Pawl.Type.Sickness (Sickness)
import Pawl.Type.SlotName (SlotName)
import Pawl.Type.Source (Source)
import Pawl.Type.Subtype (Subtype)
import Pawl.Type.TapState (TapState)
import Pawl.Type.Timestamp (Timestamp)
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
    sickness :: Sickness,
    -- CR 601.2c: the targets chosen while casting, by slot name. Empty for
    -- everything but a spell on the stack. Per-incarnation state: reset by
    -- changeZone, so CR 400.7 forgets them when the spell moves.
    targets :: Map SlotName Recipient,
    -- CR 612 / the D4 binding: the basic-land-type pairs chosen while casting a
    -- text-changing spell, by slot name. Empty for everything but a text-changer
    -- on the stack. Per-incarnation state: reset by changeZone, so CR 400.7
    -- forgets them when the spell moves -- the negative Magical-Hack-on-a-spell
    -- test (Task 8) rides on exactly this reset.
    chosenSubtypes :: Map SlotName (Subtype, Subtype),
    -- CR 613.7d: when this object entered its current zone. A static ability's
    -- continuous effect shares this timestamp (CR 613.7a); stamped fresh on every
    -- zone change (CR 400.7 makes each a new object). Read by the projection when
    -- ordering layer 6/7.
    timestamp :: Timestamp
  }
  deriving (Eq, Ord, Show)
