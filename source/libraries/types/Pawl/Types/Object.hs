module Pawl.Types.Object where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Binding as Binding
import qualified Pawl.Types.CardName as CardName
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
    -- | CR 110.2: a permanent's controller is, by DEFAULT, the player under whose
    -- control it entered the battlefield. That default is a fact about the
    -- permanent rather than a continuous effect, and this field is where it is
    -- recorded -- the base a CR 613.1b layer-2 effect then overrides.
    --
    -- Nothing means "no recorded entry controller, so use the owner". That
    -- covers two things at once: CR 108.4a's fallback for a card that is not a
    -- permanent at all, and, for a permanent, the fact that every effect in this
    -- pool that puts one onto the battlefield is controlled by that permanent's
    -- own owner. The one write is a CR 616.1b replacement redirecting the entry,
    -- which is CR 110.2a's "unless the effect states otherwise".
    --
    -- CR 110.2a's OTHER shape -- an effect naming a controller who is not the
    -- owner -- does not come through this field: Resolve.applyEntryControl still
    -- expresses it as a stored layer-2 effect, and no card reaches that store
    -- (#582).
    --
    -- NOT a control-changing EFFECT: CR 800.4c distinguishes an effect that gives
    -- a player control of an object from the player who controlled it by
    -- default, and writing a CR 616.1b rewrite as a layer-2 effect would put it
    -- on the wrong side of that line.
    --
    -- Per-incarnation state, like damage and counters: reset by changeZone,
    -- because CR 400.7 makes the moved object a new one and CR 110.2's entry is
    -- the one this incarnation made.
    enteredUnder :: Maybe PlayerId.PlayerId,
    source :: Source.Source,
    zone :: Zone.Zone,
    tapped :: TapState.TapState,
    -- | CR 120.3e: damage dealt to a creature is MARKED on it. A count, not a list
    -- of tagged units -- unlike mana, every damage rider (wither, infect,
    -- lifelink, toxic) is consumed at deal time and never re-read, and CR 704.5g
    -- reads only the total marked on it.
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
    bindings :: Map.Map SlotName.SlotName Binding.Binding,
    -- | CR 122.1: counters placed on this permanent, counted per kind. Persistent
    -- permanent state -- unlike `damage`, cleanup does NOT clear it (a counter is
    -- not an "until end of turn" effect). Per-incarnation: reset by changeZone,
    -- because CR 122.2 makes counters cease to exist when an object changes
    -- zones. A +1/+1 or -1/-1 count feeds P/T via the projection (CR 122.1a /
    -- 613.4c); both kinds present trigger the CR 704.5q annihilation SBA.
    counters :: Map.Map CounterKind.CounterKind Natural.Natural,
    -- | The object OR PLAYER this permanent is attached to -- what CR 303.4b calls
    -- "enchanted" for an Aura and CR 301.5a calls "equipped" for an Equipment.
    -- One field for both, because attachment is one relation: CR 701.3's Attach
    -- keyword action moves either, and Affected.Attached reads either. Nothing for
    -- every permanent that is not attached to something.
    --
    -- A Recipient rather than an ObjectId, because CR 303.4 attaches an Aura to
    -- an object OR PLAYER and CR 702.5d's enchant-player Auras (Curse of Death's
    -- Hold) are attached to nothing else. Recipient is the existing
    -- player-or-object reference, and reusing it is what lets CR 303.4c's
    -- legality re-check hand the stored value straight back to Target.stillLegal
    -- without re-deriving the tag.
    --
    -- BASE state, not projected: attachment is a fact about the object, and no CR
    -- 613 layer reads or writes it. Per-incarnation, like damage and counters:
    -- changeZone resets it, because CR 400.7 makes the moved object a new one.
    --
    -- One direction only. "What is attached to me" is derived by scanning the
    -- battlefield, so there is no reverse index to keep consistent across zone
    -- changes.
    attachedTo :: Maybe Recipient.Recipient,
    -- | CR 614.1c: a colour this object's controller chose as it entered
    -- (Painter's Servant). Read by Modification.AddChosenColor off the effect's
    -- SOURCE, never off the affected object.
    --
    -- NOT a copiable value, unlike the P/T an EntryOption writes into the
    -- copiable snapshot. CR 707.5 is why the ability runs again at all, and
    -- CR 707.6 is why the OLD choice does not carry over -- a copy of Painter's
    -- Servant runs the copied ability and makes its own NEW choice.
    --
    -- Per-incarnation state, like damage and counters: reset by changeZone,
    -- because CR 400.7 makes the moved object a new one.
    --
    -- One of THREE as-enters choice fields; chosenSubtype and chosenNames below
    -- are the others.
    chosenColor :: Maybe Color.Color,
    -- | CR 614.1c: a basic land type this object's controller chose as it
    -- entered (Convincing Mirage). Read by Modification.SetLandSubtypeToChosen
    -- off the effect's SOURCE, never off the affected object -- the same
    -- direction Modification.AddChosenColor reads chosenColor above.
    --
    -- A sibling of chosenColor rather than one generalized choice map: the two
    -- carry different types and are read by different modifications, and a
    -- sum-typed value would make every reader re-narrow what the field already
    -- knows.
    --
    -- NOT a copiable value, for chosenColor's reason (CR 707.5, CR 707.6).
    --
    -- Per-incarnation state: reset by changeZone, because CR 400.7 makes the
    -- moved object a new one.
    chosenSubtype :: Maybe Subtype.Subtype,
    -- | CR 614.1c / CR 201.4: the card names chosen as this object entered
    -- ("As this enchantment enters, you and an opponent each choose a card name"
    -- -- Null Chamber). Read by Pawl.Engine.PlayerEffect off the effect's SOURCE,
    -- the same direction Modification.AddChosenColor reads chosenColor.
    --
    -- The third as-enters choice field, and still a sibling of the two above
    -- rather than the generalized choice map a third arrival was expected to
    -- force. A `Map ChoiceKind ChoiceValue` would need a sum over colour,
    -- subtype and name, which every reader would then have to re-narrow at a
    -- site where the wrong arm is unrepresentable today; three typed fields keep
    -- each read total. A fourth would not change that either.
    --
    -- A SET rather than one name or a name per chooser. Null Chamber has two
    -- players each name a card, and its prohibition asks only whether a name is
    -- among them -- so who named which is not a distinction any reader can make,
    -- and two players naming the same card is one prohibition either way. Empty
    -- for everything that never chose (CR 201.2a: an object with no name).
    --
    -- NOT a copiable value, for chosenColor's reason (CR 707.5, CR 707.6).
    --
    -- Per-incarnation state: reset by changeZone, because CR 400.7 makes the
    -- moved object a new one.
    chosenNames :: Set.Set CardName.CardName,
    -- | CR 613.7d: when this object entered its current zone. A static ability's
    -- continuous effect shares this timestamp (CR 613.7a); stamped fresh on every
    -- zone change (CR 400.7 makes each a new object). Read by the projection when
    -- ordering layer 6/7.
    timestamp :: Timestamp.Timestamp,
    -- | CR 709.3b / 712.8f: which face this object is showing, where the rules
    -- single one out. Nothing everywhere else, and the layout decides -- see
    -- Pawl.Engine.Game.faceOf.
    --
    -- A CardName rather than a positional index: CR 709.3 has a player choose
    -- which half they are casting, and CR 709.4a is what gives a card's faces
    -- names to choose from. Resolved against the object's STORED card, never a
    -- projected one, so CR 612.2a's rename (which reaches only a
    -- token-definition card a Create names) cannot dangle it. A name that no
    -- longer names a face of that card falls back to the combined view
    -- (Game.resolveFace) rather than failing -- unreachable in practice, since
    -- Pawl.CardSpec's "a card's face names are pairwise distinct" corpus lint
    -- holds that of every loadable card, and the only writer of this field draws
    -- the name from that same card's faces.
    --
    -- Per-incarnation state, like damage and counters: cleared by changeZone,
    -- because CR 400.7 makes the moved object a new one.
    face :: Maybe CardName.CardName
  }
  deriving (Eq, Ord, Show)
