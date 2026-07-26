module Pawl.Type.Object where

import Data.Map.Strict (Map)
import Numeric.Natural (Natural)
import Pawl.Type.Binding (Binding)
import Pawl.Type.CounterKind (CounterKind)
import Pawl.Type.ObjectId (ObjectId)
import Pawl.Type.PlayerId (PlayerId)
import Pawl.Type.Sickness (Sickness)
import Pawl.Type.SlotName (SlotName)
import Pawl.Type.Source (Source)
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
    -- CR 303.4b: the object this permanent is attached to -- what CR 303.4b calls
    -- "enchanted". Nothing for every permanent that is not an attached Aura.
    --
    -- BASE state, not projected: attachment is a fact about the object, and no CR
    -- 613 layer reads or writes it. Per-incarnation, like damage and counters:
    -- changeZone resets it, because CR 400.7 makes the moved object a new one with
    -- no memory of what it enchanted.
    --
    -- One direction only. "What is attached to me" is derived by scanning the
    -- battlefield, the posture Projection.controls already takes toward control,
    -- so there is no reverse index to keep consistent across zone changes.
    --
    -- Maybe ObjectId, not a Recipient: CR 702.5d's enchant-player Auras cannot be
    -- expressed and need this widened (#190).
    attachedTo :: Maybe ObjectId,
    -- CR 613.7d: when this object entered its current zone. A static ability's
    -- continuous effect shares this timestamp (CR 613.7a); stamped fresh on every
    -- zone change (CR 400.7 makes each a new object). Read by the projection when
    -- ordering layer 6/7.
    timestamp :: Timestamp
  }
  deriving (Eq, Ord, Show)
