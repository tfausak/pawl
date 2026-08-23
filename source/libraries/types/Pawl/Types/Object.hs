module Pawl.Types.Object where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Binding as Binding
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.ClassLevel as ClassLevel
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.ExilePlayPermission as ExilePlayPermission
import qualified Pawl.Types.Facing as Facing
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Mana as Mana
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.RoomIndex as RoomIndex
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.SlotName as SlotName
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.Zone as Zone

data Object = MkObject
  { owner :: PlayerId.PlayerId,
    -- | The player this incarnation arrived under the control of, by DEFAULT --
    -- a fact about the object rather than a continuous effect, and the base a CR
    -- 613.1b layer-2 effect then overrides. CR 109.4 gives a controller only to
    -- objects on the stack and the battlefield, and this field is written for
    -- exactly those two arrivals, under one rule each:
    --
    --   * CR 110.2: a permanent's controller is, by default, the player under
    --     whose control it entered the battlefield.
    --   * CR 405.4 / 601.2a: "the controller of a spell is the player who cast
    --     it", fixed as the card is put onto the stack and never re-derived from
    --     the board afterwards (#83).
    --
    -- Nothing means "no recorded controller, so use the owner". That is CR
    -- 108.4a's fallback, and it covers every arrival no rule above spoke about: a
    -- token, a land played from hand, an ability put onto the stack (whose
    -- controller CR 113.8 makes the owner this engine stamps on it).
    --
    -- Three writers. Event.changeZoneAttaching records the player an effect
    -- instructed to put the object onto the battlefield (CR 110.2a's main clause)
    -- and the caster Pawl.Engine.Cast hands it for a move onto the stack;
    -- Pawl.Engine.Replacement's entry loop overwrites the first with a CR 616.1b
    -- rewrite's taker (Gather Specimens), which is CR 110.2a's "unless the effect
    -- states otherwise". Every other destination clears it, the rules' own scope
    -- (CR 109.4, CR 110.5d).
    --
    -- NOT a control-changing EFFECT: CR 800.4c distinguishes an effect that gives
    -- a player control of an object from the player who controlled it by
    -- default, and writing a CR 616.1b rewrite as a layer-2 effect would put it
    -- on the wrong side of that line.
    --
    -- Per-incarnation state, like damage and counters: reset by newIncarnation,
    -- because CR 400.7 makes the moved object a new one and the arrival recorded
    -- here is the one this incarnation made.
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
    -- Pawl.Engine.Game.faceOf, for the ones the FaceDown arm carries
    -- (Pawl.Engine.Card.faceDownFace) -- CR 708.2's "those listed by the ability
    -- or rules that allowed the spell or permanent to be face down", which is CR
    -- 708.2a's 2/2 with no name, no text, no subtypes and no mana cost for
    -- everything that lists none. CR 708.2 is why that is a substitution and not
    -- a CR 613 layer: the listed characteristics ARE the object's copiable
    -- values, so they belong where the fold starts rather than anywhere inside
    -- it.
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
    -- | CR 406.3: this card was "exiled face down". NOT `facing` above, and
    -- deliberately a second field rather than a second writer of that one: CR
    -- 110.5d says in as many words that "although an exiled card may be face
    -- down, this has no correlation to the face-down status of a permanent",
    -- and the two differ in every consequence -- CR 708.2a substitutes a 2/2
    -- creature where CR 406.3a leaves no characteristics at all, and only this
    -- one is about who may LOOK.
    --
    -- WHAT IT DOES today, which is CR 406.4's first half: a player may choose a
    -- specific face-down exiled card "only if the player is allowed to look at
    -- that card", and pawl grants that permission to nobody, so
    -- Pawl.Engine.Target.exileRecipients offers no such card as a target.
    --
    -- Not implemented: CR 406.3a's "no characteristics", so a filter that read a
    -- face-down exiled card's card types would see the printed ones (#1479). Nor
    -- CR 406.4's second half -- the pile a player who may not look chooses
    -- instead, and the random card out of it -- nor CR 406.3's continuing
    -- permission to look (#1480).
    --
    -- Per-incarnation state, like `facing`: reset by newIncarnation, so a card
    -- that leaves exile is face up again wherever it lands (CR 400.7). The one
    -- door that writes it is Event.changeZoneEntering, off Effect.MoveToZone's
    -- rider, which is CR 406.3's "by default" being overridden by the effect
    -- that does the exiling.
    exiledFaceDown :: Bool,
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
    -- | CR 613.7c: when each kind's counters were put on. One timestamp per
    -- kind, because rule 613.7c restamps a kind's counters when another of that
    -- kind arrives. Written by Pawl.Engine.Event.putCounters; a kind missing
    -- here falls back to this object's own timestamp, which today is only
    -- Pawl.Engine.Cost's loyalty, a kind no CR 613 layer reads. A
    -- stamp left behind by a removed kind is inert -- `counters` decides what
    -- the projection emits.
    counterTimestamps :: Map.Map (CounterKind.CounterKind Keyword.Keyword) Timestamp.Timestamp,
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
    -- battlefield -- Pawl.Engine.Filter.View's `attachedViews` is that scan, and
    -- CR 303.4b's "enchanted" reads it -- so there is no reverse index to keep
    -- consistent across zone changes.
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
    -- One of FOUR `chosen` fields; chosenSubtype, chosenNames and chosenPlayer
    -- below are the others. What makes those four a family is not that the choice
    -- is made as the object enters -- `protector` below is made then too -- but
    -- that each is read back OFF THE EFFECT'S SOURCE. A protector is a
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
    -- total. The fourth did not change that either -- chosenPlayer below arrived
    -- as its own field on the same argument -- and CR 310.9a's protector is
    -- evidence for the prediction rather than against it: it arrived as its own
    -- field too, for the reason chosenColor's note gives.
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
    -- | CR 614.1c: a player this object's controller chose as it entered ("As
    -- this creature enters, choose a player" -- Stuffy Doll). Read by
    -- Pawl.Engine.Resolve's ObjectRef.ChosenPlayer arm off the effect's SOURCE,
    -- the same direction Modification.AddChosenColor reads chosenColor.
    --
    -- The FOURTH `chosen` field, and still a sibling rather than the generalized
    -- choice map three arrivals did not force either -- chosenNames' note above
    -- gives the argument, and predicted this. What it did not predict is that the
    -- reader would not be a Modification: the Doll's payload is a triggered
    -- ability's DealDamage rather than a static ability's projection, so the read
    -- happens in Pawl.Engine.Resolve instead. That widens the family's shared
    -- property from "read by a Modification" to "read off the effect's source",
    -- which is what still separates all four from `protector` below (a
    -- designation rule 310 reads directly).
    --
    -- NOT a copiable value, for chosenColor's reason (CR 707.2, CR 707.6): CR
    -- 707.2's copiable values are characteristics, and a player is not one.
    --
    -- Per-incarnation state: reset by newIncarnation, because CR 400.7 makes the
    -- moved object a new one.
    chosenPlayer :: Maybe PlayerId.PlayerId,
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
    -- holds it, the duration it lasts and how they may spend mana for it (CR
    -- 118.14). Nothing for every object nothing has
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
    -- Pawl.Engine.Cast.permissionsWith reads them off a face. This one is true of
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
    -- a card means playing that card as a land or casting that card as a spell"),
    -- and both halves read it: Pawl.Engine.Cast.permitsCastFromExile for the
    -- spell, Pawl.Engine.Action.playableLands for CR 305.1's special action.
    playableFromExile :: Maybe ExilePlayPermission.ExilePlayPermission,
    -- | CR 702.170a: this exiled card is a PLOTTED card, stamped with the turn on
    -- which it became one. Nothing for everything that is not plotted.
    --
    -- The TURN NUMBER and not a Bool, because CR 702.170d's permission is scoped
    -- by it: "during any turn AFTER THE TURN IN WHICH IT BECAME PLOTTED".
    -- GameState.turnNumber counts every turn the game takes, extra turns included
    -- (CR 500.7), so a strict comparison against the stamp is that clause exactly
    -- -- where a "plotted this turn" flag would need clearing at a boundary and
    -- would answer wrongly for a card plotted during an extra turn.
    --
    -- Not Pawl.Types.TurnWindow, whose OnTurn names a turn an effect is WAITING
    -- for: this stamp is a turn already past when it is read, so the two rule
    -- shapes are opposites.
    --
    -- Beside playableFromExile above rather than inside it, though both are
    -- permissions to cast an exiled card. CR 715.3d's names a PLAYER and states no
    -- cost; CR 702.170d's names the card's OWNER, makes the cast free, and fixes
    -- the timing -- so one field cannot answer both without a tag, and the tag
    -- would be this field.
    --
    -- Per-incarnation, like damage and counters: cleared by newIncarnation,
    -- because CR 400.7 makes the moved object a new one. A plotted card that is
    -- cast and dies is not plotted in its graveyard, which is CR 702.170d read
    -- forward -- the permission is about a card in exile.
    plotted :: Maybe Natural.Natural,
    -- | CR 702.143a: this exiled card is a FORETOLD card, stamped with the turn
    -- on which it was foretold. Nothing for everything that is not foretold.
    --
    -- plotted's field one rule over, and every argument above applies unchanged:
    -- the turn number rather than a Bool because CR 702.143a scopes the cast by
    -- it ("after the current turn has ended"), per-incarnation because CR 400.7
    -- mints a new object, and beside playableFromExile rather than inside it
    -- because the cost and the player each rule names differ.
    --
    -- SEPARATE from plotted rather than one "exiled with a delayed permission"
    -- field carrying a tag: the two costs are opposites -- rule 702.170d's cast
    -- is free and rule 702.143a's is a foretell cost read off the card -- and
    -- Pawl.Engine.Cost prices them in two different arms. A card cannot be both.
    --
    -- Object.exiledFaceDown is a separate field still: CR 702.143a exiles the
    -- card face down, but CR 702.143d makes a card foretold that was already in
    -- exile face up, so neither field implies the other.
    foretold :: Maybe Natural.Natural,
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
    -- | CR 310.9: the player designated as this battle's protector. Chosen as the
    -- battle enters (CR 310.9a), though NOT by a replacement effect the way the
    -- four `chosen` fields above are -- Pawl.Engine.Event.designateProtector says
    -- why rule 310.9a is a bare instruction instead. It is an Object field and not
    -- a projection because CR 310.9g keeps the designation across the permanent ceasing to
    -- be a battle or becoming a copy of another one, so nothing a layer computes
    -- may be allowed to move it.
    --
    -- A Maybe rather than a bare PlayerId for two reasons the rules give, not for
    -- convenience. CR 704.5x names the state "no player in the game designated as
    -- its protector" outright and makes recovering from it a state-based action,
    -- so it is a state the rules expect to observe. And no non-battle object has a
    -- protector at all -- CR 310.9's designation is battle-only, so the field is
    -- Nothing for the rest of the board.
    --
    -- Nothing is NOT "the controller by default". CR 310.9a's fallback to the
    -- controller applies only to a battle with no battle types, and every battle
    -- printed so far has the Siege subtype CR 310.12 describes, whose protector CR
    -- 310.12a requires to be an opponent. Reading Nothing as the controller would
    -- therefore invent the one designation CR 704.5y exists to undo.
    --
    -- NOT a copiable value: CR 707.2 lists characteristics, and CR 310.9g says a
    -- battle that becomes a copy of another battle keeps its own protector. Falls
    -- out with nothing to enforce, as ringBearerFor's note above explains.
    --
    -- Per-incarnation state, like damage and counters: cleared by newIncarnation,
    -- because CR 400.7 makes the moved object a new one -- a battle that returns
    -- to the battlefield is a new battle and chooses a protector afresh (CR
    -- 310.9a).
    protector :: Maybe PlayerId.PlayerId,
    -- | CR 309.4: the room this object's venture marker is on, for a dungeon card
    -- in the command zone. Nothing for everything else, which is every object but a
    -- dungeon.
    --
    -- On the OBJECT rather than on the player, because that is where CR 309.4 puts
    -- it -- "a venture marker placed on the dungeon card they own". The marker is
    -- the player's (CR 701.49b says "their venture marker") but it sits on the card,
    -- and CR 309.3's one-dungeon-per-player makes the two readings agree; the
    -- object's `owner` is the player whose marker this is. Storing it on the player
    -- instead would leave CR 309.6's state-based action -- whose subject is the
    -- dungeon card -- reading a field on something else.
    --
    -- Per-incarnation state, cleared by newIncarnation like every neighbour here.
    -- Never actually exercised: CR 309.2c forbids a dungeon card leaving the command
    -- zone except as it leaves the game, so no dungeon ever gets a second
    -- incarnation. Cleared anyway, because the reset set is the rule and an
    -- exception would have to be argued rather than assumed.
    ventureRoom :: Maybe RoomIndex.RoomIndex,
    -- | CR 716.2b: the LEVEL designation this permanent has, or Nothing for the
    -- overwhelming majority of permanents, which have never been given one. "A
    -- level is a designation that any permanent can have."
    --
    -- On every Object rather than only on a Class, because rule 716.2b says "any
    -- permanent" and then "a Class retains its level even if it stops being a
    -- Class" -- so the mark cannot be keyed off the subtype it usually arrives
    -- with. Nothing here reads the type line, and that is the rule rather than an
    -- oversight.
    --
    -- A Maybe rather than a Natural initialised to 1: CR 716.2d treats a
    -- permanent with NO level as level 1 when something asks, which
    -- Pawl.Types.ClassLevel.defaulted does at the read, leaving "has no level" its
    -- own representation. The two are indistinguishable to every reader today and
    -- the rule still writes them as different states.
    --
    -- NOT a counter, which is the whole of CR 716.4's separation from rule 711's
    -- levelers -- see Pawl.Types.CounterKind's Level arm, which says the same
    -- thing from the other side. The two must not share a field.
    --
    -- NOT a copiable value (CR 716.2b's last sentence), and that falls out with
    -- nothing to enforce, exactly as ringBearerFor's note above explains: a copy
    -- effect's payload is a ProjectedCharacteristics, never an Object.
    -- Pawl.ClassSpec's "levels are not a copiable characteristic" is what proves
    -- it at gameplay level, a Copy Enchantment entering as a copy of a level-2
    -- Paladin Class and being offered that Class's level-1 bar.
    --
    -- Per-incarnation state, like damage and counters: cleared by newIncarnation,
    -- because CR 400.7 makes the moved object a new one. CR 716.2b's retention
    -- clause is about a permanent that stops being a CLASS, not about one that
    -- changes zones.
    classLevel :: Maybe ClassLevel.ClassLevel,
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
    -- | Every designation this permanent has: CR 702.112b's renowned, CR 701.37b's
    -- monstrous, CR 701.60b's suspected and CR 719.3b's solved, which
    -- Pawl.Types.Designation holds as one type because those rules word the mark
    -- identically.
    --
    -- A Set where ringBearerFor above is a Maybe PlayerId, because none of those
    -- rules names a player: each is a mark on the permanent alone, and
    -- nothing ends one on a change of control the way CR 701.54a ends the
    -- Ring-bearer's.
    --
    -- STORED rather than projected, and every one of those rules says why outright --
    -- "neither an ability nor part of the permanent's copiable values". So no CR
    -- 613 layer writes this, and a Clone of a renowned creature is not renowned,
    -- for the reason ringBearerFor's note gives.
    --
    -- Per-incarnation state, like damage and counters: cleared by newIncarnation.
    -- That IS rule 702.112b's "once a permanent becomes renowned, it stays renowned
    -- until it leaves the battlefield", rule 701.37b's and rule 719.3b's same
    -- sentence for monstrous and for solved, and rule 701.60a's "until it leaves
    -- the battlefield" for suspected -- the designation ends with the
    -- incarnation, so there is no sweep to run.
    --
    -- Suspected is the one member with rules meaning of its own: CR 701.60c gives
    -- the permanent menace and "this creature can't block" for as long as it is
    -- suspected. Both are read off this set live rather than stamped when it is
    -- written -- Pawl.Engine.Projection.designationGathered for the keyword and
    -- Pawl.Engine.CombatRestriction.inForce for the restriction -- so nothing has
    -- to be unwound if it ends. Rule 701.60a's other ending, "until a spell or
    -- ability causes it to no longer be suspected", is Effect.Unsuspect, and it
    -- deletes that one member.
    designations :: Set.Set Designation.Designation,
    -- | CR 702.33d: has this SPELL been kicked? "If a spell's controller declares
    -- the intention to pay any of that spell's kicker costs, that spell has been
    -- 'kicked'", and this is that declaration, stamped by Pawl.Engine.Cast at CR
    -- 601.2b onto the stack incarnation CR 601.2a made.
    --
    -- Stored for the designations' reason above: nothing a CR 613 layer computes
    -- may move it, since it records a choice rather than a characteristic. A Bool
    -- and not a member of Pawl.Types.Designation because it is a fact about a
    -- SPELL, where every member of that type is a mark "only permanents can have".
    --
    -- Per-incarnation, which CR 400.7 makes the whole of rule 702.33d's duration:
    -- the designation belongs to the spell, and the card in a graveyard or in
    -- exile that the spell becomes is a new object with no memory of it.
    --
    -- THE ONE EXCEPTION is CR 400.7d, and the rule states it as one: "an ability
    -- of a permanent can reference information about the spell that became that
    -- permanent as it resolved, including what costs were paid". Monstrous
    -- War-Leech's rule 702.33e payoff is a CR 614.1c entry replacement asked of
    -- the PERMANENT, so the flag is carried across that one move by
    -- Pawl.Engine.Event.changeZoneAttaching, `announcedX` below's route exactly.
    -- It stays False for every permanent no kicked spell became.
    --
    -- A Bool and not a count: CR 702.33c's multikicker is payable "any number of
    -- times", and no card in the pool has two kicker costs either (#1234, #1235).
    kicked :: Bool,
    -- | CR 601.2b with CR 107.4f: how many of the Phyrexian mana symbols in the
    -- cost of the SPELL that became this permanent its controller announced they
    -- would pay 2 life for. Rule 702.150a's compleated is the one reader, through
    -- Pawl.Engine.Projection.intrinsicReplacementsOf.
    --
    -- `kicked` above's exception, verbatim: CR 400.7d lets an ability of a
    -- permanent reference "what costs were paid" to cast the spell it was, so
    -- Pawl.Engine.Event.changeZoneAttaching carries the number across that one
    -- move and newIncarnation forgets it everywhere else.
    --
    -- A COUNT and not a Bool, where `kicked` is one: rule 702.150a subtracts "two
    -- for EACH of those mana symbols", and a cost may print more than one
    -- (CR 107.4f's own {W/P}{W/P} example).
    --
    -- Zero for every object that was not cast for life this way: a token, a
    -- permanent an effect put onto the battlefield, and every spell whose cost
    -- printed no Phyrexian symbol or whose controller announced mana for all of
    -- them.
    phyrexianLifePaid :: Natural.Natural,
    -- | CR 107.4h with CR 601.2h: the mana that was SPENT to pay the cost of
    -- casting the SPELL that became this object -- the record CR 107.4h's third
    -- sentence needs, "the {S} symbol can also be used to refer to mana of any
    -- type produced by a snow source spent to pay a cost". Berg Strider's "if
    -- {S} was spent to cast this spell" is the one reader, through
    -- Pawl.Engine.Filter.View's `manaSpentTags` and Quantity.SnowWasSpent.
    --
    -- THE UNITS and not the answer to one question. Pawl.Types.ManaUnit already
    -- carries everything a card can ask about a mana it was paid with -- the
    -- production tags, the type -- so recording the units leaves "was it snow?"
    -- and "what colour was it?" (Boreal Outrider, #2008) as reads rather than as
    -- fields.
    --
    -- Written by Pawl.Engine.Cost.payMana, which knows WHICH spell it is paying
    -- for (its `casting` argument) and restores the whole state when the cost
    -- goes unpaid, so a rejected cast records nothing.
    --
    -- `kicked` above's exception, and rule 400.7d names this field's contents
    -- outright: an ability of a permanent may reference "what mana was spent to
    -- pay those costs" to cast the spell it was. So
    -- Pawl.Engine.Event.changeZoneAttaching carries it across that one move and
    -- newIncarnation forgets it everywhere else -- which is what Berg Strider
    -- needs, its clause being an ability of the PERMANENT.
    --
    -- Not implemented: an ACTIVATION cost's mana is not recorded, since
    -- Pawl.Engine.Activate pays with no object to record against (#2007).
    --
    -- Empty for every object nothing was spent on: a token, a permanent an
    -- effect put onto the battlefield, and every spell whose cost was free.
    manaSpent :: Mana.Mana,
    -- | CR 107.3m: the value of X chosen for the SPELL that became this
    -- permanent, which is the value of X for the permanent's
    -- enters-the-battlefield replacement effects -- Nissa, Steward of Elements'
    -- CR 306.5b intrinsic loyalty ability being the one reader today, through
    -- Pawl.Engine.Projection.intrinsicReplacementsOf.
    --
    -- The one exception CR 400.7 admits on this path, and the rule states it as
    -- one: `bindings` above carries the announcement while the spell is on the
    -- stack, and newIncarnation forgets it like every other per-incarnation
    -- field, so the value is copied across the move by
    -- Pawl.Engine.Event.changeZoneAttaching off the departing spell's own
    -- bindings. A SNAPSHOT taken at that instant, never a live read: rule 601.2b
    -- fixed the number as the spell was cast and nothing can change it after.
    --
    -- NOT the permanent's own X, which rule 107.3m puts at 0 in the same
    -- sentence. Nothing reads this as a Quantity, and Quantity.InSlot cannot
    -- reach it -- it is not a binding.
    --
    -- Nothing for every object that did not enter the battlefield as a cast
    -- spell with an announced X: a token, a permanent an effect put onto the
    -- battlefield, and every spell whose cost declared no variable.
    announcedX :: Maybe Natural.Natural,
    -- | CR 701.35a: this permanent is DETAINED -- it "can't attack or block and
    -- its activated abilities can't be activated" -- until the next turn of each
    -- player named here. Empty for every permanent nothing has detained, which is
    -- almost all of them.
    --
    -- On the VICTIM rather than in a GameState list, which is
    -- doesNotUntapNext below's reason: a zone change reuses the ObjectId
    -- (Pawl.Engine.Event.changeZoneAttaching re-inserts under the same key and
    -- replaces the value with `newIncarnation`), so a list keyed by id would
    -- follow the card back onto the battlefield as a permanent CR 400.7 makes a
    -- new object the detaining effect never named. As a field it is
    -- per-incarnation like damage and counters, cleared by newIncarnation, and
    -- the forgetting IS that rule.
    --
    -- A SET OF PLAYERS and not a Pawl.Types.Expiry, because rule 701.35a fixes
    -- the duration itself -- "until the next turn of the controller of that spell
    -- or ability" is the whole of it, and no card states another one. So the only
    -- thing there is to remember is WHOSE next turn, which is CR 109.5's "you"
    -- resolved at the resolution. An Expiry would admit five durations no detain
    -- can arm, and would owe the three sweeps that end them a carrier they could
    -- never match. Pawl.Engine.Expiry.dropAtTurnOf is the one sweep that does
    -- reach this, and it drops a seat's entry at the handoff the same way it ends
    -- an Expiry.AtTurnOf -- CR 800.4m's departed player included.
    --
    -- A SET and not one player, because two detains of the same permanent by
    -- different players run to two different turns and the later one has to
    -- outlast the earlier. Two detains by the SAME player collapse for free,
    -- which is right: rule 701.35a states a duration and not a count.
    detainedUntil :: Set.Set PlayerId.PlayerId,
    -- | CR 701.15b: the players who have goaded this permanent, each entry
    -- running until that player's next turn (CR 701.15a). Read by
    -- Pawl.Engine.Goad, and turned into CR 508.1d requirements by
    -- Pawl.Engine.AttackRequirement.
    --
    -- detainedUntil's shape exactly, and for its reasons: rule 701.15a fixes the
    -- duration ("until the next turn of the controller of that spell or
    -- ability"), so the only thing to remember is WHOSE next turn, and
    -- Pawl.Engine.Expiry.dropAtTurnOf is the one sweep that reaches it. A SET
    -- because CR 701.15c lets several players goad one creature and the later
    -- turn has to outlast the earlier, and because CR 701.15d says a second goad
    -- by the SAME player creates no additional requirement -- which a set
    -- collapses for free, where a list would count it twice and CR 508.1d counts
    -- requirements.
    --
    -- CR 701.15b calls goaded a designation, but it is not a
    -- Pawl.Types.Designation: that type's marks are per-permanent and permanent,
    -- where this one is per-PLAYER and expires. Object.ringBearerFor is the
    -- sibling that argument already excluded.
    --
    -- Per-incarnation like everything around it (CR 400.7): the goaded permanent
    -- that leaves the battlefield and comes back is a new object, and nobody
    -- goaded that one.
    goadedBy :: Set.Set PlayerId.PlayerId,
    -- | CR 502.3 / CR 611.2: a ONE-SHOT untap prohibition standing over this
    -- permanent, said of ITS CONTROLLER's next untap step -- "that creature
    -- doesn't untap during its controller's next untap step" (Elvish Hunter).
    -- Written by Effect.DoesNotUntapNext as a spell or ability resolves, and by
    -- nothing else: CR 701.43a's exert names a PLAYER rather than "its
    -- controller", so it rides exertedBy below instead.
    --
    -- Pawl.Types.UntapRestriction's stored counterpart, and it is a field on the
    -- AFFECTED permanent where that one is a field on the PRINTING that forbids.
    -- The printed carrier is a static ability and so re-derived live from the
    -- battlefield every untap step; this one outlives the object that made it
    -- (Elvish Hunter can die, Frost Breath is already in a graveyard), so it has
    -- to be stored somewhere the source's departure cannot reach.
    --
    -- ON THE VICTIM rather than in a GameState list beside
    -- GameState.blockRequirements, and the reason is CR 400.7: an object keeps
    -- its ObjectId across a zone change (Event.changeZoneAttaching re-inserts
    -- under the same key), so a list keyed by id would follow the card back onto
    -- the battlefield as a new permanent the effect never named. As a field it is
    -- forgotten by newIncarnation with everything else.
    --
    -- CLEARED WHERE IT APPLIES, by Engine.untapAll, which is why it needs no
    -- Pawl.Types.Expiry and takes no part in any Pawl.Engine.Expiry sweep. CR
    -- 611.2a gives the effect the duration its own sentence states, and that
    -- sentence names ONE untap step; CR 502.3 runs that step for whoever controls
    -- the permanent THEN, so application and expiry are one event and a control
    -- change between the resolution and the step needs nothing baked and nothing
    -- rewritten.
    --
    -- A Bool and not a count, for the same reading: two such effects over one
    -- permanent both expire at that one step, so they cannot stack. Telekinesis' "next TWO untap steps" is the shape a Bool cannot hold,
    -- and no card in the pool prints it (gap #1653).
    doesNotUntapNext :: Bool,
    -- | CR 701.43a: the players who have EXERTED this permanent -- "you choose to
    -- have it not untap during your next untap step". Written by
    -- Pawl.Engine.Combat.declareAttackers paying CR 508.1g's optional cost to
    -- attack, which is a keyword action and never goes on the stack, and read and
    -- emptied of a seat by Pawl.Engine.Engine.untapAll at that seat's untap step.
    -- Empty for every permanent nobody has exerted, which is almost all of them.
    --
    -- SEPARATE from doesNotUntapNext above rather than folded into it, because
    -- the two sentences name different untap steps and only coincide while the
    -- exerter still controls the permanent. Rule 701.43a says "YOUR next untap
    -- step", so the prohibition is keyed to a player and survives a control
    -- change; Elvish Hunter's says "its controller's", which is a live read of
    -- whoever controls the permanent at the step. One field could hold only one
    -- of those readings.
    --
    -- A SET OF PLAYERS, detainedUntil's argument in the same shape: CR 701.43b
    -- lets a permanent be exerted more than once, two exerts by the SAME player
    -- collapse for free (the rule states a duration, not a count) and two by
    -- DIFFERENT players -- alice exerts it, bob takes control and exerts it in
    -- turn -- run to two different untap steps, so the later one has to outlast
    -- the earlier.
    --
    -- Per-incarnation like everything above it: CR 400.7 forgets it, and CR
    -- 701.43c ("an object that isn't on the battlefield can't be exerted") is why
    -- nothing writes it back. It needs no Pawl.Engine.Expiry sweep and no
    -- departure sweep either -- untapAll is the only reader, and it asks only
    -- about the seat whose untap step is running, so a seat that has left the
    -- game (CR 800.4m) leaves an entry no step can ever read.
    exertedBy :: Set.Set PlayerId.PlayerId
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
      -- CR 406.3: exiled cards are kept face up by default, and every other zone
      -- is face up outright, so the forgetting puts every arrival back to the
      -- default. Event.changeZoneEntering is the "otherwise", off the effect's
      -- own rider.
      exiledFaceDown = False,
      damage = 0,
      sickness = Sickness.Sick,
      bindings = Map.empty,
      counters = Map.empty,
      counterTimestamps = Map.empty,
      attachedTo = Nothing,
      enteredUnder = Nothing,
      chosenColor = Nothing,
      chosenSubtype = Nothing,
      chosenNames = Set.empty,
      chosenPlayer = Nothing,
      face = Nothing,
      turnedOverAt = Nothing,
      worldSince = Nothing,
      playableFromExile = Nothing,
      plotted = Nothing,
      foretold = Nothing,
      ringBearerFor = Nothing,
      protector = Nothing,
      ventureRoom = Nothing,
      classLevel = Nothing,
      unlockedHalves = Set.empty,
      designations = Set.empty,
      kicked = False,
      -- CR 400.7 forgets the announcement, `kicked` above's route; CR 601.2b's
      -- record is written back by Pawl.Engine.Event.changeZoneAttaching's mkObj.
      phyrexianLifePaid = 0,
      -- CR 400.7 forgets the payment with the announcement, `kicked` above's
      -- route; CR 400.7d's exception is written back by
      -- Pawl.Engine.Event.changeZoneAttaching's mkObj.
      manaSpent = Mana.MkMana [],
      -- CR 400.7 forgets the announcement like everything else; CR 107.3m's
      -- exception is written back by the move that carries it, in
      -- Pawl.Engine.Event.changeZoneAttaching's mkObj.
      announcedX = Nothing,
      -- CR 400.7 again, and rule 701.35a needs it: the detained permanent that
      -- leaves the battlefield and comes back is a new object, and nothing
      -- detained that one.
      detainedUntil = Set.empty,
      -- CR 400.7 with CR 701.15b: goaded is a designation A PERMANENT has, and
      -- the object that returns to the battlefield is a different permanent.
      goadedBy = Set.empty,
      -- CR 400.7 forgets the prohibition with everything else, and no rule
      -- writes it back: the effect named a permanent, and the object that
      -- returns to the battlefield is not that permanent.
      doesNotUntapNext = False,
      -- CR 400.7 with CR 701.43c: the permanent that leaves the battlefield and
      -- comes back is a new object, and nobody exerted that one.
      exertedBy = Set.empty
    }
