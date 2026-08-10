module Pawl.Types.Object where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Binding as Binding
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.ExilePlayPermission as ExilePlayPermission
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Keyword as Keyword
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
    -- Nothing means "no recorded entry controller, so use the owner". That is CR
    -- 108.4a's fallback, and it covers every entry no effect spoke about: a
    -- permanent spell resolving (CR 110.2b), a token, a land played from hand.
    --
    -- Two writers, both of them CR 110.2a. Event.changeZoneAttaching records the
    -- player an effect instructed to put the object onto the battlefield, which
    -- is the rule's main clause; Pawl.Engine.Replacement's entry loop overwrites
    -- that with a CR 616.1b rewrite's taker (Gather Specimens), which is its
    -- "unless the effect states otherwise". Both write only for a BATTLEFIELD
    -- entry, the rule's own scope (CR 110.2, CR 110.5d).
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
    -- | CR 110.5: the second of rule 110.5's four status categories pawl models
    -- -- face up or face down. `tapped` above is the first, and the two are
    -- separate fields because the rule makes the categories independent.
    --
    -- WHAT IT DOES, and it is not a characteristic (CR 110.5a): FaceDown
    -- SUBSTITUTES the object's printed characteristics wholesale, at
    -- Pawl.Engine.Game.faceOf, for CR 708.2a's 2/2 creature with no name, no
    -- text, no subtypes and no mana cost (Pawl.Engine.Card.faceDownFace). CR
    -- 708.2 is why that is a substitution and not a CR 613 layer: the listed
    -- characteristics ARE the object's copiable values, so they belong where the
    -- fold starts rather than anywhere inside it.
    --
    -- Reaches a SPELL as well as a permanent, though CR 110.5d gives only
    -- permanents status: CR 708.4 turns an object face down before it is put
    -- onto the stack, and the face-down spell has to answer for its own
    -- characteristics while it waits there. Nothing writes FaceDown for an
    -- object in any other zone.
    --
    -- Per-incarnation state, like damage and counters: reset to FaceUp by
    -- newIncarnation, which is CR 110.5b's default for a battlefield entry and
    -- CR 708.9's reveal for a departure from one -- a face-down permanent that
    -- leaves the battlefield is a face-up card again wherever it lands. The one
    -- move that must NOT forget it is CR 708.4's last sentence ("the permanent
    -- the spell becomes will be a face-down permanent"), and
    -- Event.changeZoneFaceDown is the door that carries it.
    --
    -- NOT hidden from anything that inspects the game state: Object.source still
    -- holds the printing, so CR 708.5's "you can't look at face-down permanents
    -- controlled by another player" is unimplemented (#682).
    facing :: Facing.Facing,
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
    counters :: Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural.Natural,
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
    -- One of THREE `chosen` fields; chosenSubtype and chosenNames below are the
    -- others. What makes those three a family is not that the choice is made as
    -- the object enters -- `protector` below is made then too -- but that each is
    -- read back by a MODIFICATION off the effect's source. A protector is a
    -- designation rule 310 reads directly, so it is a sibling of ringBearerFor
    -- instead.
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
    -- The third `chosen` field, and still a sibling of the two above rather than
    -- the generalized choice map a third arrival was expected to force. A
    -- `Map ChoiceKind ChoiceValue` would need a sum over colour, subtype and
    -- name, which every reader would then have to re-narrow at a site where the
    -- wrong arm is unrepresentable today; three typed fields keep each read
    -- total. A fourth would not change that either -- and CR 310.8a's protector
    -- is evidence for the prediction rather than against it: it arrived as its
    -- own field too, for the reason chosenColor's note gives.
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
    -- | CR 709.3b / 712.8e / 712.8f: which face this object is showing, where the
    -- rules single one out. Nothing everywhere else, and the layout decides --
    -- see Pawl.Engine.Game.faceOf.
    --
    -- Nothing for a ROOM permanent in particular, though its spell was cast as
    -- one half like any other split card's: CR 709.5 gives a permanent with a
    -- shared type line both halves at once, subtracting the locked ones, so no
    -- single face is up. What the cast half became is unlockedHalves below (CR
    -- 709.5d), and Pawl.Engine.Game.resolveFaceFor is where the two part.
    --
    -- A CardName rather than a positional index: CR 709.3 has a player choose
    -- which half they are casting, and CR 709.4a is what gives a card's faces
    -- names to choose from. Resolved against the object's STORED card, never a
    -- projected one, so CR 612.2a's rename (which reaches only a
    -- token-definition card a Create names) cannot dangle it. A name that no
    -- longer names a face of that card falls back to the combined view
    -- (Game.resolveFace) rather than failing -- unreachable in practice, since
    -- Pawl.CardSpec's "a card's face names are pairwise distinct" corpus lint
    -- holds that of every loadable card, and every writer of this field draws
    -- the name from that same card's faces: Pawl.Engine.Event.changeZoneShowing
    -- names the half being put onto the stack (CR 709.3a) and the face a
    -- resolving permanent spell enters with (CR 712.13), the gate-side
    -- Pawl.Engine.Cast.asProposed names that same half (CR 709.3b), and
    -- Pawl.Engine.Resolve's Transform arm names the face CR 701.27a turned the
    -- permanent over to.
    --
    -- Per-incarnation state, like damage and counters: cleared by newIncarnation,
    -- because CR 400.7 makes the moved object a new one, and set again by a move
    -- only where the move itself says which face the object arrives showing
    -- (CR 709.3a, CR 712.13). That clear is also CR
    -- 712.8a -- a transformed permanent that leaves the battlefield is a card
    -- with only its front face's characteristics again -- rather than only a
    -- forgetting.
    face :: Maybe CardName.CardName,
    -- | CR 701.27f: WHEN this permanent last turned over, so that "it hasn't
    -- transformed or converted since the ability was put onto the stack" is a
    -- comparison rather than a guess. Nothing for a permanent that never has.
    --
    -- A Timestamp and not a count, because the other side of the comparison is
    -- already one: an ability object's own `timestamp` above IS the moment it
    -- was put onto the stack (Activate.activateAbility and Engine.placeBorne
    -- each mint it as they create the object), so the rule's reference point
    -- needs nothing stamped onto the ability. Two objects, one monotone
    -- sequence, one `>`.
    --
    -- NOT CR 613.7d's timestamp, and deliberately a second field rather than a
    -- refresh of that one: turning over is not a zone change, so the permanent's
    -- entry order -- which is what orders its static ability in layers 6 and 7 --
    -- must not move underneath it.
    --
    -- Per-incarnation state, like `face` above: cleared by newIncarnation,
    -- because CR 400.7 makes the moved object a new one that has never
    -- transformed.
    turnedOverAt :: Maybe Timestamp.Timestamp,
    -- | CR 704.5k: WHEN this permanent last became world, which is the clock the
    -- world rule measures ("the one that has had the world supertype for the
    -- shortest amount of time"). Nothing for everything that is not world, which
    -- is almost every object.
    --
    -- NOT CR 613.7d's `timestamp` above, and that is the whole point: world-ness
    -- is a layer-4 projection (CR 613.1d, Modification.AddSupertype), so a
    -- permanent can become world long after it entered, and its entry stamp is
    -- then the wrong clock.
    --
    -- STORED where world-ness is DERIVED, so it is SAMPLED rather than computed:
    -- Engine.sampleWorldSince writes it once per settle pass, before the
    -- state-based-action check reads it. One writer, so a permanent that enters
    -- already world and one granted the supertype later carry comparable clocks.
    --
    -- Per-incarnation state, like damage and counters: cleared by newIncarnation,
    -- because CR 400.7 makes the moved object a new one -- a world permanent that
    -- is bounced and replayed has been world only since it returned.
    worldSince :: Maybe Timestamp.Timestamp,
    -- | CR 601.3: the standing permission to play this card, as the player who
    -- holds it and the duration it lasts. Nothing for every object nothing has
    -- permitted, which is almost all of them -- and for an adventurer card that
    -- reached exile some other way, which is what CR 715.3d's own ruling ("if an
    -- adventurer card ends up in exile for any other reason") means.
    --
    -- TWO producers, one field. Pawl.Engine.Resolve.finishSpell writes CR
    -- 715.3d's ("for as long as that card remains exiled, that player may play
    -- it"), and Effect.GrantPlayFromExile writes the one a card states, with the
    -- duration it states. The first takes Expiry.Never, because CR 715.3d states
    -- no duration and CR 611.2a's default is the end of the game -- CR 400.7
    -- ends that one, not a sweep.
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
    -- leaves, with no sweep to run and nothing to unwind. A STATED duration is
    -- the part that does need a sweep, and Pawl.Engine.Expiry runs it over this
    -- field as its fifth carrier.
    --
    -- The player is the granting effect's controller (CR 109.5, and CR 715.3d's
    -- "its controller exiles it"), while Pawl.Engine.Game.zoneMembers filters
    -- exile by OWNER -- so a player permitted to play someone else's exiled card
    -- is named here and still cannot find it (#668).
    --
    -- PLAYABLE and not castable, after the rules' own word (CR 601.1a: "playing
    -- a card means playing that card as a land or casting that card as a spell").
    -- What reads it is narrower than that: only Pawl.Engine.Cast does, so a land
    -- under this permission would be permitted nothing (#670).
    playableFromExile :: Maybe ExilePlayPermission.ExilePlayPermission,
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
    ringBearerFor :: Maybe PlayerId.PlayerId,
    -- | CR 310.8: the player designated as this battle's protector. Chosen as the
    -- battle enters (CR 310.8a) by the CR 614.12a as-enters route every other
    -- choice-on-entry takes, which is why it is an Object field and not a
    -- projection: CR 310.8g keeps the designation across the permanent ceasing to
    -- be a battle or becoming a copy of another one, so nothing a layer computes
    -- may be allowed to move it.
    --
    -- A Maybe rather than a bare PlayerId for two reasons the rules give, not for
    -- convenience. CR 704.5w names the state "no player in the game designated as
    -- its protector" outright and makes recovering from it a state-based action,
    -- so it is a state the rules expect to observe. And no non-battle object has a
    -- protector at all -- CR 310.8's designation is battle-only, so the field is
    -- Nothing for the rest of the board.
    --
    -- Nothing is NOT "the controller by default". CR 310.8a's fallback to the
    -- controller applies only to a battle with no battle types, and every printed
    -- battle is a Siege (CR 310.11), whose protector CR 310.11a requires to be an
    -- opponent. Reading Nothing as the controller would therefore invent the one
    -- designation CR 704.5x exists to undo.
    --
    -- NOT a copiable value: CR 707.2 lists characteristics, and CR 310.8g says a
    -- battle that becomes a copy of another battle keeps its own protector. Falls
    -- out with nothing to enforce, as ringBearerFor's note above explains.
    --
    -- Per-incarnation state, like damage and counters: cleared by newIncarnation,
    -- because CR 400.7 makes the moved object a new one -- a battle that returns
    -- to the battlefield is a new battle and chooses a protector afresh (CR
    -- 310.8a).
    protector :: Maybe PlayerId.PlayerId,
    -- | CR 709.5c: the UNLOCKED DESIGNATIONS this permanent has. "'Left half
    -- unlocked' and 'right half unlocked' are designations that a permanent on
    -- the battlefield can have. Together, they are called the unlocked
    -- designations. A particular half of a permanent is said to be 'unlocked' if
    -- it has the appropriate unlocked designation. Otherwise, that half is said
    -- to be locked."
    --
    -- Named by HALF rather than positionally, and a Set rather than a pair of
    -- Bools: docs/design.md section 2.11's standing rule against baking arity
    -- into the card model, and CR 709.4a's own convention that a split card's
    -- halves are referred to by name (the same reading `face` above takes). The
    -- names are the halves' own, so Pawl.Engine.Card.faceNamed and the
    -- pairwise-distinct corpus lint that backs it are what make a member of this
    -- set pick out one half.
    --
    -- Empty for every object that is not a Room permanent, and for a Room that
    -- entered with neither door open (CR 709.5d's last sentence: "If it's
    -- entering the battlefield and neither half was cast as a spell, it enters
    -- with neither unlocked designation"). Those two are the same value because
    -- the rules make them the same fact -- a permanent with no unlocked
    -- designations -- and only a card with a shared type line has halves for one
    -- to name.
    --
    -- STORED rather than projected, for protector's reason: CR 709.5e's special
    -- action and CR 709.5f/709.5g's unlock and lock all WRITE it, so nothing a
    -- layer computes may be allowed to move it. What it feeds is the substitution
    -- at Pawl.Engine.Game.resolveFaceFor (Pawl.Engine.Card.roomFace), which sits
    -- before layer 1 rather than in it.
    --
    -- NOT a copiable value, and CR 709.5 draws that line itself: the two static
    -- abilities and "which half of that permanent a characteristic is in" are
    -- copiable, the DESIGNATIONS are not. So a permanent that becomes a copy of a
    -- Room copies the doors' text and keeps its own doors open or shut.
    --
    -- Per-incarnation state, like damage and counters: cleared by newIncarnation,
    -- because CR 400.7 makes the moved object a new one -- and CR 709.5d is what
    -- re-decides it, from the half that was cast, every time the permanent
    -- enters.
    unlockedHalves :: Set.Set CardName.CardName,
    -- | CR 702.112b: the RENOWNED designation. "Renowned is a designation that has
    -- no rules meaning other than to act as a marker that the renown ability and
    -- other spells and abilities can identify."
    --
    -- A Bool where ringBearerFor above is a Maybe PlayerId, because rule 702.112b
    -- names no player: it is a mark on the permanent alone, and nothing ends it on
    -- a change of control the way CR 701.54a ends the Ring-bearer's.
    --
    -- STORED rather than projected, and rule 702.112b says why outright --
    -- "neither an ability nor part of the permanent's copiable values". So no CR
    -- 613 layer writes it, and a Clone of a renowned creature is not renowned, for
    -- the reason ringBearerFor's note gives.
    --
    -- Per-incarnation state, like damage and counters: cleared by newIncarnation.
    -- That IS rule 702.112b's "once a permanent becomes renowned, it stays renowned
    -- until it leaves the battlefield" -- the designation ends with the
    -- incarnation, so there is no sweep to run.
    renowned :: Bool,
    -- | CR 701.37b: the MONSTROUS designation, "a marker that the monstrosity
    -- action and other spells and abilities can identify".
    --
    -- Everything renowned's note above says holds word for word here, because
    -- rule 701.37b is worded the same: a Bool with no player, stored rather than
    -- projected ("neither an ability nor part of the permanent's copiable
    -- values"), and per-incarnation, which IS rule 701.37b's "it stays monstrous
    -- until it leaves the battlefield". Two fields rather than one designation
    -- set (#1193).
    monstrous :: Bool
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
-- overrides the rest the same way -- CR 110.5b's "enters tapped", CR 708.4's
-- face-down status and CR 701.3's attach-on-entry are choices the move makes
-- about the new object, not memories of the old one, so they are reset here and
-- set again by the funnel.
newIncarnation :: Object -> Object
newIncarnation object =
  object
    { tapped = TapState.Untapped,
      -- CR 110.5b for a battlefield entry, CR 708.9 for a departure from one:
      -- a permanent enters face up unless a spell or ability says otherwise,
      -- and a face-down permanent leaving the battlefield is revealed to all
      -- players as it goes. Event.changeZoneFaceDown is the "otherwise".
      facing = Facing.FaceUp,
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
      turnedOverAt = Nothing,
      worldSince = Nothing,
      playableFromExile = Nothing,
      ringBearerFor = Nothing,
      protector = Nothing,
      unlockedHalves = Set.empty,
      renowned = False,
      monstrous = False
    }
