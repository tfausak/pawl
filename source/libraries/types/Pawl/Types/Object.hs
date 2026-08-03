module Pawl.Types.Object where

import qualified Data.Map.Strict as Map
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Binding as Binding
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.Zone as Zone

data Object = MkObject
  { owner :: PlayerId.PlayerId,
    -- | CR 110.2: "A permanent's controller is, by DEFAULT, the player under
    -- whose control it entered the battlefield." That default is a fact about
    -- the permanent rather than a continuous effect, and this field is where it
    -- is recorded -- read by Pawl.Engine.Projection.controllerOfGiven as the base
    -- a CR 613.1b layer-2 effect then overrides.
    --
    -- Nothing means "no recorded entry controller, so use the owner". That
    -- covers two things at once: CR 108.4a's fallback for a card that is not a
    -- permanent at all ("if anything asks for the controller of a card that
    -- doesn't have one ... use its owner instead"), and, for a permanent, the
    -- fact that every effect in this pool that puts one onto the battlefield is
    -- controlled by that permanent's own owner, so CR 110.2a's "under that
    -- player's control" and the owner are the same player. The one write is a CR
    -- 616.1b replacement redirecting the entry, which is CR 110.2a's "unless the
    -- effect states otherwise".
    --
    -- CR 110.2a's OTHER shape -- an effect naming a controller who is not the
    -- owner -- does not come through this field: Resolve.applyEntryControl still
    -- expresses it as a stored layer-2 effect, and no card reaches that store
    -- (#582).
    --
    -- NOT a control-changing EFFECT, and the difference is CR 800.4c's in as many
    -- words: it distinguishes "an effect that gives a player still in the game
    -- control of an object" from "the player who controlled that object by
    -- default". Writing
    -- a CR 616.1b rewrite as a layer-2 effect would put it on the wrong side of
    -- that line.
    --
    -- Per-incarnation state, like damage and counters: reset by changeZone,
    -- because CR 400.7 makes the moved object a new one and CR 110.2's "entered
    -- the battlefield" is about the entry this incarnation made.
    enteredUnder :: Maybe PlayerId.PlayerId,
    source :: Source.Source,
    zone :: Zone.Zone,
    tapped :: TapState.TapState,
    -- | CR 120.3e: damage dealt to a creature is MARKED on it. A count, not a list
    -- of tagged units -- unlike mana, every damage rider (wither, infect,
    -- lifelink, toxic) is consumed at deal time and never re-read, and CR 704.5g
    -- itself reads only "the total damage marked on it". See the M1b spec, §2.
    --
    -- Removed at cleanup (CR 514.2). Per-incarnation state: reset by changeZone.
    damage :: Natural.Natural,
    -- | CR 302.6, carrying WHICH player the permanent settled under -- the rule's
    -- subject is a player, not the object. Per-incarnation state: reset by
    -- changeZone. Not purely stored: Engine.checkControlContinuity drops the
    -- claim when the derived controller stops matching it.
    sickness :: Sickness.Sickness,
    -- | CR 601.2: the choices bound while casting, by slot name. Empty for
    -- everything but a spell or ability on the stack. Per-incarnation state:
    -- reset by changeZone, so CR 400.7 forgets them when the object moves.
    -- Replaces the M3a `targets` and M3d `chosenSubtypes` fields, unified as the
    -- risk-register's D4 named binding slots when X arrived (the second customer).
    bindings :: Map.Map SlotName.SlotName Binding.Binding,
    -- | CR 122.1: counters placed on this permanent, counted per kind. Persistent
    -- permanent state -- unlike `damage`, cleanup does NOT clear it (a counter is
    -- not an "until end of turn" effect). Per-incarnation: reset by changeZone,
    -- because CR 122.2 says counters "simply cease to exist" when an object changes
    -- zones (the CR 400.7 mechanism that also resets damage/sickness/bindings). A
    -- +1/+1 or -1/-1 count feeds P/T via the projection (CR 122.1a / 613.4c); both
    -- kinds present trigger the CR 704.5q annihilation SBA.
    counters :: Map.Map CounterKind.CounterKind Natural.Natural,
    -- | The object OR PLAYER this permanent is attached to -- what CR 303.4b calls
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
    -- Pawl.Engine.Sba.stillLegalEnchant).
    --
    -- BASE state, not projected: attachment is a fact about the object, and no CR
    -- 613 layer reads or writes it. Per-incarnation, like damage and counters:
    -- changeZone resets it, because CR 400.7 makes the moved object a new one with
    -- no memory of what it was attached to.
    --
    -- One direction only. "What is attached to me" is derived by scanning the
    -- battlefield, the posture Projection.controls already takes toward control,
    -- so there is no reverse index to keep consistent across zone changes.
    attachedTo :: Maybe Recipient.Recipient,
    -- | CR 614.1c: a colour this object's controller chose as it entered
    -- ("As this creature enters, choose a color" -- Painter's Servant). Read by
    -- Modification.AddChosenColor off the effect's SOURCE, never off the affected
    -- object.
    --
    -- NOT a copiable value, unlike the P/T an EntryOption writes into the
    -- copiable snapshot. CR 707.5 is why the ability runs again at all: "If the
    -- text that's being copied includes any abilities that replace the
    -- enters-the-battlefield event (such as ... 'as [this] enters' abilities),
    -- those abilities will take effect." CR 707.6 is the direct authority for
    -- why the OLD choice doesn't carry over: "When copying a permanent, any
    -- choices that have been made for that permanent aren't copied. Instead,
    -- ... the object's controller will get to make any 'as [this] enters the
    -- battlefield' choices for it" -- so a copy of Painter's Servant runs the
    -- copied ability and makes its own NEW choice.
    --
    -- Per-incarnation state, like damage and counters: reset by changeZone,
    -- because CR 400.7 makes the moved object a new one.
    --
    -- One of TWO as-enters choice fields; chosenSubtype below is the other.
    chosenColor :: Maybe Color.Color,
    -- | CR 614.1c: a basic land type this object's controller chose as it
    -- entered ("As this Aura enters, choose a basic land type" -- Convincing
    -- Mirage). Read by Modification.SetLandSubtypeToChosen off the effect's
    -- SOURCE, never off the affected object -- the same direction
    -- Modification.AddChosenColor reads chosenColor above.
    --
    -- A sibling of chosenColor rather than one generalized choice map: the two
    -- carry different types and are read by different modifications, and a
    -- sum-typed value would make every reader re-narrow what the field already
    -- knows. A THIRD as-enters choice of a third type would be the thing that
    -- changes that call.
    --
    -- NOT a copiable value, for chosenColor's reason (CR 707.5) and for CR
    -- 707.6, which says it outright: "When copying a permanent, any choices that
    -- have been made for that permanent aren't copied. Instead, if an object
    -- enters the battlefield as a copy of another permanent, the object's
    -- controller will get to make any 'as [this] enters the battlefield' choices
    -- for it." Its worked example is an "As this creature enters, choose a
    -- creature type" card, which is this field's shape exactly.
    --
    -- Per-incarnation state: reset by changeZone, because CR 400.7 makes the
    -- moved object a new one.
    chosenSubtype :: Maybe Subtype.Subtype,
    -- | CR 613.7d: when this object entered its current zone. A static ability's
    -- continuous effect shares this timestamp (CR 613.7a); stamped fresh on every
    -- zone change (CR 400.7 makes each a new object). Read by the projection when
    -- ordering layer 6/7.
    timestamp :: Timestamp.Timestamp
  }
  deriving (Eq, Ord, Show)
