module Pawl.Types.Object where

import Data.Map.Strict (Map)
import Numeric.Natural (Natural)
import Pawl.Types.Binding (Binding)
import Pawl.Types.CounterKind (CounterKind)
import Pawl.Types.PlayerId (PlayerId)
import Pawl.Types.Recipient (Recipient)
import Pawl.Types.Sickness (Sickness)
import Pawl.Types.SlotName (SlotName)
import Pawl.Types.Source (Source)
import Pawl.Types.TapState (TapState)
import Pawl.Types.Timestamp (Timestamp)
import Pawl.Types.Zone (Zone)

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
    -- CR 302.6, carrying WHICH player the permanent settled under -- the rule's
    -- subject is a player, not the object. Per-incarnation state: reset by
    -- changeZone. Not purely stored: Engine.checkControlContinuity drops the
    -- claim when the derived controller stops matching it.
    sickness :: Sickness,
    -- CR 601.2: the choices bound while casting, by slot name. Empty for
    -- everything but a spell or ability on the stack. Per-incarnation state:
    -- reset by changeZone, so CR 400.7 forgets them when the object moves.
    -- Replaces the M3a `targets` and M3d `chosenSubtypes` fields, unified as the
    -- risk-register's D4 named binding slots when X arrived (the second customer).
    bindings :: Map SlotName Binding,
    -- CR 122.1: counters placed on this permanent, counted per kind. Persistent
    -- permanent state -- unlike `damage`, cleanup does NOT clear it (a counter is
    -- not an "until end of turn" effect). Per-incarnation: reset by changeZone,
    -- because CR 122.2 says counters "simply cease to exist" when an object changes
    -- zones (the CR 400.7 mechanism that also resets damage/sickness/bindings). A
    -- +1/+1 or -1/-1 count feeds P/T via the projection (CR 122.1a / 613.4c); both
    -- kinds present trigger the CR 704.5q annihilation SBA.
    counters :: Map CounterKind Natural,
    -- The object OR PLAYER this permanent is attached to -- what CR 303.4b calls
    -- "enchanted" for an Aura and CR 301.5a calls "equipped" for an Equipment.
    -- One field for both, because attachment is one relation: CR 701.3's Attach
    -- keyword action moves either, and Affected.Attached reads either. Nothing for
    -- every permanent that is not attached to something.
    --
    -- A Recipient rather than an ObjectId, because CR 303.4 says an Aura "enters
    -- the battlefield attached to an object OR PLAYER" and CR 702.5d's
    -- enchant-player Auras (Curse of Death's Hold) are attached to nothing else.
    -- Recipient is the existing player-or-object reference, and reusing it is
    -- what lets CR 303.4c's legality re-check hand the stored value straight back
    -- to Target.stillLegal -- the recipient a Pool's own candidates are tagged
    -- with is the recipient stored here, so the tag needs no re-deriving (see
    -- Pawl.Sba.stillLegalEnchant).
    --
    -- BASE state, not projected: attachment is a fact about the object, and no CR
    -- 613 layer reads or writes it. Per-incarnation, like damage and counters:
    -- changeZone resets it, because CR 400.7 makes the moved object a new one with
    -- no memory of what it was attached to.
    --
    -- One direction only. "What is attached to me" is derived by scanning the
    -- battlefield, the posture Projection.controls already takes toward control,
    -- so there is no reverse index to keep consistent across zone changes.
    attachedTo :: Maybe Recipient,
    -- CR 613.7d: when this object entered its current zone. A static ability's
    -- continuous effect shares this timestamp (CR 613.7a); stamped fresh on every
    -- zone change (CR 400.7 makes each a new object). Read by the projection when
    -- ordering layer 6/7.
    timestamp :: Timestamp
  }
  deriving (Eq, Ord, Show)
