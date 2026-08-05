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
    -- Per-incarnation state, like damage and counters: reset by newIncarnation,
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
    -- Removed at cleanup (CR 514.2). Per-incarnation state: reset by newIncarnation.
    damage :: Natural.Natural,
    -- | CR 302.6, carrying WHICH player the permanent settled under -- the rule's
    -- subject is a player, not the object. Per-incarnation state: reset by
    -- newIncarnation. Not purely stored: Engine.checkControlContinuity drops the
    -- claim when the derived controller stops matching it.
    sickness :: Sickness.Sickness,
    -- | CR 601.2: the choices bound while casting, by slot name. Empty for
    -- everything but a spell or ability on the stack. Per-incarnation state:
    -- reset by newIncarnation, so CR 400.7 forgets them when the object moves.
    bindings :: Map.Map SlotName.SlotName Binding.Binding,
    -- | CR 122.1: counters placed on this permanent, counted per kind. Persistent
    -- permanent state -- unlike `damage`, cleanup does NOT clear it (a counter is
    -- not an "until end of turn" effect). Per-incarnation: reset by newIncarnation,
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
    -- newIncarnation resets it, because CR 400.7 makes the moved object a new one.
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
    -- Per-incarnation state, like damage and counters: reset by newIncarnation,
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
    -- Per-incarnation state: reset by newIncarnation, because CR 400.7 makes the
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
    -- for everything that never chose, which matches no card at all rather than
    -- every card: the shape CR 201.2a gives an object with no name.
    --
    -- NOT a copiable value, for chosenColor's reason (CR 707.5, CR 707.6).
    --
    -- Per-incarnation state: reset by newIncarnation, because CR 400.7 makes the
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
    -- Per-incarnation state, like damage and counters: cleared by newIncarnation,
    -- because CR 400.7 makes the moved object a new one.
    face :: Maybe CardName.CardName,
    -- | CR 715.3d: the player who may play this card while it remains exiled --
    -- an Adventure spell's controller, written as the resolution that exiled it
    -- finishes. Nothing for every object that did not get there that way, which
    -- is what the rule's own "if an adventurer card ends up in exile for any
    -- other reason" means.
    --
    -- STATE, where every other casting permission pawl has is a fact about a
    -- CARD: Face.castingPermissions and Pawl.Engine.Keyword.castingPermissionsOf
    -- are true of every copy of a card in every game, and
    -- Pawl.Engine.Cast.permissionsOf reads them off a face. This one is true of
    -- one incarnation and names one player, so a CastingPermission arm could not
    -- carry it.
    --
    -- Per-incarnation, like damage and counters: cleared by newIncarnation, because
    -- CR 400.7 makes the moved object a new one. That IS CR 715.3d's "for as
    -- long as that card remains exiled" -- the permission ends when the card
    -- leaves, with no sweep to run and nothing to unwind.
    --
    -- The player is the Adventure's CONTROLLER (CR 715.3d's "its controller
    -- exiles it"), while Pawl.Engine.Game.zoneMembers filters exile by OWNER --
    -- so a player who cast an opponent's adventurer card is named here and still
    -- cannot find the card (#668).
    --
    -- PLAYABLE and not castable, after the rule's own word. What reads it is
    -- narrower than that: only Pawl.Engine.Cast does, so a land under this
    -- permission would be permitted nothing (#670).
    playableFromExileBy :: Maybe PlayerId.PlayerId,
    -- | CR 701.54b: the Ring-bearer designation, as the player it was made for.
    -- Nothing for every permanent that is not anyone's Ring-bearer, which is
    -- almost all of them.
    --
    -- ON THE OBJECT, where GameState.monarch is on the game, because CR 701.54b
    -- says so in as many words: "Ring-bearer is a designation a permanent can
    -- have." The monarch is one designation naming a player; this is many
    -- designations, each naming a permanent, one per player at most.
    --
    -- A Maybe PlayerId rather than the Bool that CR 701.54e's wording alone
    -- would suggest ("that creature is on the battlefield under your control and
    -- has the Ring-bearer designation" reads as an unqualified mark, with "yours"
    -- coming from control). A bare Bool cannot answer CR 701.54a's SECOND
    -- ending: "until ... another player gains control of it" ends the
    -- designation, and a mark that remembers nobody would instead hand the
    -- Ring-bearer to whoever took the creature. Storing the player the choice was
    -- made for is what lets Pawl.Engine.Ring.endOnControlChange tell a gained
    -- creature from a kept one.
    --
    -- NOT a copiable value (CR 701.54b's second sentence), and that falls out
    -- with nothing to enforce: CR 707.2's copy path snapshots
    -- ProjectedCharacteristics into a Binding, and an Object field is not among
    -- them. Pawl.RingSpec's "CR 701.54b a Clone of the Ring-bearer is not a
    -- Ring-bearer" is the test that keeps it so.
    --
    -- Per-incarnation state, like damage and counters: cleared by newIncarnation,
    -- because CR 400.7 makes the moved object a new one -- a Ring-bearer that
    -- dies and returns is a different creature, and no longer designated.
    ringBearerFor :: Maybe PlayerId.PlayerId
  }
  deriving (Eq, Ord, Show)

-- | CR 400.7: "an object that moves from one zone to another becomes a new
-- object with no memory of, or relation to, its previous existence" -- the
-- forgetting, as one function. Every field above documented as per-incarnation
-- goes back to its no-memory value here, and nothing else is touched.
--
-- The single place that knows the whole set, because there is more than one
-- path into a zone: Event.changeZoneAttaching is the funnel every in-game move
-- uses, while Setup.startGameFromCards (CR 727.2), Setup.funnelBack (CR 729.5)
-- and Departure.remainingControlledExiled (CR 800.4a) move objects by hand.
-- Spelling the reset out at each of them is what let the four drift apart, so a
-- field added to Object is reset everywhere exactly when it is added HERE.
--
-- Leaves `zone` and `timestamp` alone, plus `owner` and `source`. The last two
-- are not per-incarnation at all (CR 108.3: ownership follows the card, not the
-- object). The first two ARE, but they are what the caller is DECIDING rather
-- than forgetting: the move names its destination, and CR 613.7d wants the
-- moment of entry, which only a caller in the Game monad can mint. A caller
-- overrides the rest the same way -- CR 110.5b's "enters tapped" and CR 701.3's
-- attach-on-entry are choices the move makes about the new object, not memories
-- of the old one, so they are reset here and set again by the funnel.
newIncarnation :: Object -> Object
newIncarnation object =
  object
    { tapped = TapState.Untapped,
      damage = 0,
      sickness = Sickness.Sick,
      bindings = Map.empty,
      counters = Map.empty,
      attachedTo = Nothing,
      enteredUnder = Nothing,
      chosenColor = Nothing,
      chosenSubtype = Nothing,
      chosenNames = Set.empty,
      face = Nothing,
      playableFromExileBy = Nothing,
      ringBearerFor = Nothing
    }
