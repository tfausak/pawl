module Pawl.Types.Object where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Numeric.Natural as Natural
import qualified Pawl.Types.Binding as Binding
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.ClassLevel as ClassLevel
import qualified Pawl.Types.Color as Color
import qualified Pawl.Types.Cost as Cost
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
    -- the base a CR 613.1b layer-2 effect then overrides, written for the two
    -- arrivals CR 109.4 gives a controller: CR 110.2's battlefield entry and CR
    -- 405.4 / 601.2a's cast, which fixes a spell's controller as the card is put
    -- onto the stack (#83). Nothing is CR 108.4a's fallback to the owner, which
    -- covers a token, a land played from hand and an ability (CR 113.8).
    --
    -- NOT a control-changing EFFECT: CR 800.4c distinguishes an effect that gives
    -- a player control of an object from the player who controlled it by default,
    -- so CR 616.1b's rewrite of an entry's taker (Gather Specimens) is written
    -- here rather than as a layer-2 effect.
    --
    -- Per-incarnation state: reset by newIncarnation (CR 400.7).
    enteredUnder :: Maybe PlayerId.PlayerId,
    source :: Source.Source,
    zone :: Zone.Zone,
    tapped :: TapState.TapState,
    -- | CR 110.5: face up or face down, the second of that rule's status
    -- categories pawl models, `tapped` above being the first. FaceDown
    -- SUBSTITUTES the object's printed characteristics wholesale at
    -- Pawl.Engine.Game.faceOf for the ones CR 708.2 lists -- CR 708.2a's 2/2 with
    -- no name for everything that lists none -- rather than being a CR 613 layer,
    -- those characteristics being the object's copiable values.
    --
    -- Reaches a SPELL as well as a permanent, though CR 110.5d gives only
    -- permanents status: CR 708.4 turns an object face down before it is put onto
    -- the stack. Per-incarnation, reset to FaceUp by newIncarnation (CR 110.5b,
    -- CR 708.9); the one move that must not forget it is CR 708.4's last
    -- sentence, which Event.changeZoneFaceDown carries.
    --
    -- Not implemented: CR 708.5's "you can't look at face-down permanents
    -- controlled by another player" -- Object.source still holds the printing
    -- (#1412).
    facing :: Facing.Facing,
    -- | CR 406.3: this card was "exiled face down", which CR 110.5d says in as
    -- many words has no correlation to `facing` above. What it does is CR 406.4's
    -- first half: Pawl.Engine.Target offers the card itself only to a player
    -- Pawl.Engine.Exile.mayLookAt names, and everybody else the pile
    -- Pawl.Engine.Exile.pileOf sorts it into.
    --
    -- Not implemented: CR 406.3a's "no characteristics", so a filter that read a
    -- face-down exiled card's card types would see the printed ones (#1479).
    --
    -- Per-incarnation: reset by newIncarnation (CR 400.7), the effect's own rider
    -- through Event.changeZoneEntering being CR 406.3's "otherwise".
    exiledFaceDown :: Bool,
    -- | CR 120.3e: damage dealt to a creature is MARKED on it. A count and not a
    -- list of tagged units, every damage rider being consumed at deal time and CR
    -- 704.5g reading only the total. Removed at cleanup (CR 514.2), and
    -- per-incarnation: reset by newIncarnation.
    damage :: Natural.Natural,
    -- | CR 302.6, carrying WHICH player the permanent settled under -- the rule's
    -- subject is a player, not the object. Per-incarnation state: reset by
    -- newIncarnation. Not purely stored: Engine.checkControlContinuity drops the
    -- claim when the derived controller stops matching it.
    sickness :: Sickness.Sickness,
    -- | CR 601.2: the choices bound while casting, by slot name. Empty for
    -- everything but a spell or ability on the stack. Per-incarnation: reset by
    -- newIncarnation, so CR 400.7 forgets them when the object moves. A
    -- resolution can outlive the object holding them (CR 729.5's subgame case),
    -- which is what GameState.detachedBindings is for.
    bindings :: Map.Map SlotName.SlotName Binding.Binding,
    -- | CR 122.1: counters placed on this permanent, counted per kind.
    -- Persistent where `damage` is not -- cleanup does not clear a counter -- but
    -- per-incarnation all the same, CR 122.2 making counters cease to exist when
    -- an object changes zones. A +1/+1 or -1/-1 count feeds P/T through the
    -- projection (CR 122.1a / 613.4c); both kinds present trigger the CR 704.5q
    -- annihilation SBA.
    counters :: Map.Map (CounterKind.CounterKind Keyword.Keyword) Natural.Natural,
    -- | CR 613.7c: when each kind's counters were put on, one timestamp per kind,
    -- because that rule restamps a kind's counters when another of that kind
    -- arrives. A kind missing here falls back to this object's own timestamp, and
    -- a stamp left behind by a removed kind is inert -- `counters` decides what
    -- the projection emits.
    counterTimestamps :: Map.Map (CounterKind.CounterKind Keyword.Keyword) Timestamp.Timestamp,
    -- | The object OR PLAYER this permanent is attached to -- CR 303.4b's
    -- "enchanted" and CR 301.5a's "equipped". One field for both, attachment
    -- being one relation (CR 701.3), and a Recipient because CR 303.4 attaches an
    -- Aura to an object or a player (CR 702.5d).
    --
    -- BASE state that no CR 613 layer reads or writes, and per-incarnation: reset
    -- by newIncarnation (CR 400.7). One direction only -- "what is attached to
    -- me" is derived by scanning the battlefield (Pawl.Engine.Filter.View's
    -- `attachedViews`), so there is no reverse index to keep consistent.
    attachedTo :: Maybe Recipient.Recipient,
    -- | CR 614.1c: a colour this object's controller chose as it entered
    -- (Painter's Servant). Read by Modification.AddChosenColor off the effect's
    -- SOURCE, never off the affected object. NOT a copiable value, so a copy runs
    -- the copied ability and makes its own new choice (CR 707.5, CR 707.6).
    -- Per-incarnation: reset by newIncarnation (CR 400.7).
    --
    -- One of FOUR `chosen` fields -- chosenSubtype, chosenNames and chosenPlayer
    -- below are the others -- whose shared property is that each is read back OFF
    -- THE EFFECT'S SOURCE. `protector` below is chosen as the object enters too
    -- and is not one of them, being a designation rule 310 reads directly.
    chosenColor :: Maybe Color.Color,
    -- | CR 614.1c: a basic land type this object's controller chose as it entered
    -- (Convincing Mirage). Read by Modification.SetLandSubtypeToChosen off the
    -- effect's SOURCE. A sibling of chosenColor rather than one generalized
    -- choice map, whose sum-typed value every reader would have to re-narrow. Not
    -- a copiable value and per-incarnation, for that field's reasons.
    chosenSubtype :: Maybe Subtype.Subtype,
    -- | CR 201.4: the card names this object's controller has chosen. TWO moments
    -- write it -- CR 614.1c's as-enters choice (Null Chamber) and CR 608.2c's
    -- on-resolution one (Ancient Vendetta) -- and both readers ask the effect's
    -- SOURCE, chosenColor's direction.
    --
    -- A SET rather than one name or a name per chooser: Null Chamber has two
    -- players each name a card and its prohibition asks only whether a name is
    -- among them. Empty for everything that never chose, which matches no card at
    -- all (CR 201.2a). Not a copiable value and per-incarnation, for chosenColor's
    -- reasons.
    chosenNames :: Set.Set CardName.CardName,
    -- | CR 614.1c: a player this object's controller chose as it entered (Stuffy
    -- Doll). Read by Pawl.Engine.Resolve's ObjectRef.ChosenPlayer arm off the
    -- effect's SOURCE -- a triggered ability's payload rather than a
    -- Modification, which is what makes the family's shared property "read off
    -- the effect's source" rather than "read by a Modification".
    --
    -- Not a copiable value (CR 707.2's copiable values are characteristics, and a
    -- player is not one; CR 707.6), and per-incarnation, for chosenColor's
    -- reasons.
    chosenPlayer :: Maybe PlayerId.PlayerId,
    -- | CR 613.7d: when this object entered its current zone. A static ability's
    -- continuous effect shares this timestamp (CR 613.7a); stamped fresh on every
    -- zone change (CR 400.7 makes each a new object). Read by the projection when
    -- ordering layer 6/7.
    timestamp :: Timestamp.Timestamp,
    -- | CR 709.3b / 712.8e / 712.8f: which face this object is showing, where the
    -- rules single one out. Nothing everywhere else, and the layout decides --
    -- see Pawl.Engine.Game.faceOf. Nothing for a ROOM permanent in particular,
    -- CR 709.5 giving a permanent with a shared type line both halves at once;
    -- what the cast half became is unlockedHalves below (CR 709.5d).
    --
    -- A CardName rather than a positional index, CR 709.3 having a player choose
    -- which half they are casting and CR 709.4a giving a card's faces names.
    -- Resolved against the object's STORED card, so CR 612.2a's rename cannot
    -- dangle it, and a name that names no face falls back to the combined view.
    --
    -- Per-incarnation: cleared by newIncarnation and set again only by a move
    -- that says which face the object arrives showing (CR 709.3a, CR 712.13).
    -- That clear is also CR 712.8a rather than only a forgetting.
    face :: Maybe CardName.CardName,
    -- | CR 701.27f: WHEN this permanent last turned over, so that "it hasn't
    -- transformed or converted since the ability was put onto the stack" is a
    -- comparison rather than a guess. Nothing for a permanent that never has.
    --
    -- A second field and not CR 613.7d's `timestamp` above, which a transform
    -- refreshes too (CR 613.7g): every permanent has that one and only a
    -- transformed permanent has this, and this one is shared across a batch so
    -- that CR 701.27f cannot tell two simultaneous victims apart.
    --
    -- Per-incarnation: cleared by newIncarnation (CR 400.7).
    turnedOverAt :: Maybe Timestamp.Timestamp,
    -- | CR 704.5k: WHEN this permanent last became world, the clock that rule
    -- measures. Not CR 613.7d's `timestamp` above, world-ness being a layer-4
    -- projection (CR 613.1d) a permanent can acquire long after it entered.
    --
    -- SAMPLED rather than computed, by Engine.sampleWorldSince once per settle
    -- pass before the state-based-action check reads it, so a permanent that
    -- enters world and one granted the supertype later carry comparable clocks.
    -- Per-incarnation: cleared by newIncarnation (CR 400.7).
    worldSince :: Maybe Timestamp.Timestamp,
    -- | CR 601.3: the standing permission to play this card, as the player who
    -- holds it, the duration it lasts and how they may spend mana for it (CR
    -- 118.14). TWO producers: Pawl.Engine.Resolve.finishSpell writes CR 715.3d's,
    -- taking Expiry.Never since that rule states no duration (CR 611.2a), and
    -- Effect.GrantPlayFromExile writes the one a card states.
    --
    -- STATE, where every other casting permission pawl has is a fact about a
    -- CARD: this one is true of one incarnation and names one player, so a
    -- CastingPermission arm could not carry it. Per-incarnation: cleared by
    -- newIncarnation, which IS CR 715.3d's "for as long as that card remains
    -- exiled"; a STATED duration is what Pawl.Engine.Expiry sweeps.
    --
    -- PLAYABLE and not castable, after CR 601.1a, and both halves read it:
    -- Pawl.Engine.Cast.permitsCastFromExile and Pawl.Engine.Action.playableLands
    -- for CR 305.1's special action.
    playableFromExile :: Maybe ExilePlayPermission.ExilePlayPermission,
    -- | CR 702.170a: this exiled card is a PLOTTED card, stamped with the turn on
    -- which it became one. The TURN NUMBER and not a Bool because CR 702.170d
    -- scopes the permission by it, and GameState.turnNumber counts extra turns
    -- too (CR 500.7), so a strict comparison is that clause exactly.
    --
    -- Beside playableFromExile above rather than inside it: CR 715.3d's
    -- permission names a PLAYER and states no cost, where CR 702.170d's names the
    -- owner, makes the cast free and fixes the timing. Per-incarnation: cleared
    -- by newIncarnation (CR 400.7), the permission being about a card in exile.
    plotted :: Maybe Natural.Natural,
    -- | CR 702.143a: this exiled card is a FORETOLD card, stamped with the turn on
    -- which it was foretold. `plotted` above one rule over, with every argument
    -- unchanged, and SEPARATE from it because the two costs are opposites -- rule
    -- 702.170d's cast is free and this one's is a foretell cost -- and a card
    -- cannot be both.
    --
    -- Object.exiledFaceDown stays a separate field: CR 702.143a exiles the card
    -- face down, but CR 702.143d makes a card foretold that was already in exile
    -- face up, so neither field implies the other.
    foretold :: Maybe Natural.Natural,
    -- | CR 701.54b: the Ring-bearer designation, as the player it was made for.
    -- On the object, that rule making it "a designation a permanent can have",
    -- where GameState.monarch is one designation naming a player.
    --
    -- A Maybe PlayerId rather than a Bool because CR 701.54a's second ending --
    -- "until ... another player gains control of it" -- needs to know whom the
    -- choice was made for; Pawl.Engine.Ring.endOnControlChange is the reader.
    --
    -- NOT a copiable value (CR 701.54b), which falls out with nothing to enforce,
    -- CR 707.2's copy path snapshotting ProjectedCharacteristics rather than an
    -- Object. Pawl.RingSpec's "CR 701.54b a Clone of the Ring-bearer is not a
    -- Ring-bearer" is the test that keeps it so. Per-incarnation: cleared by
    -- newIncarnation (CR 400.7).
    ringBearerFor :: Maybe PlayerId.PlayerId,
    -- | CR 310.9: the player designated as this battle's protector, chosen as the
    -- battle enters (CR 310.9a). An Object field and not a projection because CR
    -- 310.9g keeps the designation across the permanent ceasing to be a battle or
    -- becoming a copy of another one.
    --
    -- A Maybe for two reasons the rules give: CR 704.5x names "no player in the
    -- game designated as its protector" as a state to recover from, and no
    -- non-battle object has a protector at all. Nothing is NOT "the controller by
    -- default" -- CR 310.9a's fallback applies only to a battle with no battle
    -- types, and CR 310.12a requires a Siege's protector to be an opponent, so
    -- that reading would invent the designation CR 704.5y exists to undo.
    --
    -- NOT a copiable value (CR 707.2, CR 310.9g), for ringBearerFor's reason.
    -- Per-incarnation: cleared by newIncarnation, a returning battle choosing a
    -- protector afresh (CR 310.9a).
    protector :: Maybe PlayerId.PlayerId,
    -- | CR 309.4: the room this object's venture marker is on, for a dungeon card
    -- in the command zone. On the OBJECT because that is where that rule puts it
    -- -- "a venture marker placed on the dungeon card they own" -- and CR 309.3's
    -- one dungeon per player makes the object's `owner` the player whose marker
    -- it is, which is what CR 309.6's state-based action reads.
    --
    -- Per-incarnation, cleared by newIncarnation like every neighbour here,
    -- though CR 309.2c means no dungeon ever gets a second incarnation.
    ventureRoom :: Maybe RoomIndex.RoomIndex,
    -- | CR 716.2b: the LEVEL designation this permanent has -- "a designation
    -- that any permanent can have", which is why it is on every Object rather
    -- than keyed off the Class subtype it usually arrives with.
    --
    -- A Maybe rather than a Natural initialised to 1: CR 716.2d treats a
    -- permanent with no level as level 1 when something asks, which
    -- Pawl.Types.ClassLevel.defaulted does at the read, leaving "has no level" its
    -- own representation. NOT a counter, which is the whole of CR 716.4's
    -- separation from rule 711's levelers.
    --
    -- NOT a copiable value (CR 716.2b), for ringBearerFor's reason.
    -- Pawl.ClassSpec's "levels are not a copiable characteristic" is what proves
    -- it at gameplay level. Per-incarnation: cleared by newIncarnation, rule
    -- 716.2b's retention clause being about a permanent that stops being a CLASS
    -- rather than one that changes zones.
    classLevel :: Maybe ClassLevel.ClassLevel,
    -- | CR 709.5c: the UNLOCKED DESIGNATIONS this permanent has. Named by HALF
    -- rather than positionally, and a Set rather than a pair of Bools:
    -- docs/design.md section 2.11's rule against baking arity into the card
    -- model, and CR 709.4a's convention that a split card's halves are referred
    -- to by name. Empty for every object that is not a Room permanent and for a
    -- Room that entered with neither door open (CR 709.5d).
    --
    -- STORED rather than projected, for protector's reason: CR 709.5e's special
    -- action and CR 709.5f/709.5g's unlock and lock all write it, and what it
    -- feeds is the substitution at Pawl.Engine.Game.resolveFaceFor, which sits
    -- before layer 1. NOT a copiable value -- CR 709.5 makes the two static
    -- abilities copiable and the designations not.
    --
    -- Per-incarnation: cleared by newIncarnation, and CR 709.5d re-decides it
    -- from the half that was cast every time the permanent enters.
    unlockedHalves :: Set.Set CardName.CardName,
    -- | Every designation this permanent has: CR 702.112b's renowned, CR 701.37b's
    -- monstrous, CR 701.60b's suspected and CR 719.3b's solved, which
    -- Pawl.Types.Designation holds as one type because those rules word the mark
    -- identically. A Set where ringBearerFor above is a Maybe PlayerId, none of
    -- those rules naming a player.
    --
    -- STORED rather than projected, each of those rules calling the mark "neither
    -- an ability nor part of the permanent's copiable values", so a Clone of a
    -- renowned creature is not renowned. Per-incarnation: cleared by
    -- newIncarnation, which IS each rule's "until it leaves the battlefield".
    --
    -- Suspected is the one member with rules meaning of its own: CR 701.60c gives
    -- the permanent menace and "this creature can't block" for as long as it is
    -- suspected, both read off this set live rather than stamped, so nothing has
    -- to be unwound when Effect.Unsuspect deletes the member (CR 701.60a).
    designations :: Set.Set Designation.Designation,
    -- | CR 702.33d: how many times did this SPELL's controller declare each of
    -- its kicker costs? Stamped by Pawl.Engine.Cast at CR 601.2b onto the stack
    -- incarnation, and empty for a spell that was not kicked.
    --
    -- KEYED BY THE COST, and a COUNT per key: CR 702.33b's "kicker [cost 1]
    -- and/or [cost 2]" has payoffs naming one of them (CR 702.33f), and the count
    -- is CR 702.33c's multikicker. Quantity.TimesKickedWith reads a key;
    -- Quantity.WasKicked asks rule 702.33d's yes-or-no across the map. Stored
    -- rather than projected, since it records a choice rather than a
    -- characteristic, and not a Pawl.Types.Designation, whose marks only
    -- permanents can have.
    --
    -- Per-incarnation, save for CR 400.7d's exception -- "an ability of a
    -- permanent can reference information about the spell that became that
    -- permanent as it resolved, including what costs were paid" -- which CR
    -- 702.33e's payoff needs, so Pawl.Engine.Event.changeZoneAttaching carries it
    -- across that one move.
    kicked :: Map.Map (Cost.Cost Keyword.Keyword) Natural.Natural,
    -- | CR 702.103b: is this object BESTOWED? Stamped by Pawl.Engine.Cast at CR
    -- 601.2b, and read by Pawl.Engine.Projection.bestowGathered, which mints the
    -- three modifications that rule names on every projection.
    --
    -- A Bool and not the effect's timestamp: CR 702.103a makes bestow a static
    -- ability, so CR 613.7a gives its effect the object's own timestamp, which
    -- bestowGathered reads instead. Carried across the stack-to-battlefield move
    -- by Pawl.Engine.Event.changeZoneAttaching, `kicked`'s route, for a stronger
    -- reason than rule 400.7d's: rule 702.103b's effects last until the permanent
    -- the spell becomes ceases to be bestowed.
    --
    -- One-way, and cleared where "ceases to be bestowed" happens: CR 702.103f's
    -- unattachment (Pawl.Engine.Sba) and CR 702.103e's illegal target
    -- (Pawl.Engine.Stack.resolveTopWith). CR 702.103c's copy is bestowed by
    -- construction, which Pawl.CopySpec's "CR 702.103c a copy of a bestowed
    -- Rollicker resolves as a token Aura attached to the same host" proves; CR
    -- 702.103g's phasing in unattached reaches the CR 702.103f pass one pass
    -- later (CR 702.26i), which Pawl.PhasingSpec's "CR 702.103g a bestowed Aura
    -- that phases in unattached is a creature again" proves.
    bestowed :: Bool,
    -- | CR 601.2b with CR 107.4f: how many of the Phyrexian mana symbols in the
    -- cost of the SPELL that became this permanent its controller announced they
    -- would pay 2 life for. CR 702.150a's compleated is the one reader, through
    -- Pawl.Engine.Projection.intrinsicReplacementsOf.
    --
    -- ONE count for the whole cost, where `kicked` above keys by which cost was
    -- announced: rule 702.150a subtracts two for EACH of those symbols of the one
    -- cost being paid. Zero for every object that was not cast for life this way,
    -- and carried across the one move CR 400.7d admits, `kicked`'s route.
    phyrexianLifePaid :: Natural.Natural,
    -- | CR 107.4h with CR 601.2h and CR 602.2b: the mana that was SPENT to pay
    -- the cost of casting the SPELL that became this object, or of activating the
    -- ability this object is. Berg Strider's "if {S} was spent to cast this
    -- spell" and Forsworn Paladin's "if mana from a Treasure was spent to
    -- activate this ability" are the readers.
    --
    -- THE UNITS and not the answer to one question, Pawl.Types.ManaUnit already
    -- carrying everything a card can ask, which leaves "was it snow?" and "what
    -- colour was it?" (Boreal Outrider, #2008) as reads rather than fields.
    --
    -- Written by Pawl.Engine.Cost.payMana, which restores the whole state when
    -- the cost goes unpaid, so a rejected cast records nothing. An ACTIVATION's
    -- units land on the ability object and never on its source -- Pawl.ManaSpec's
    -- "CR 400.7d an activation's record goes on the ability object, not on its
    -- source" is the proof. Carried across the one move CR 400.7d admits,
    -- `kicked`'s route, which is what Berg Strider's clause needs.
    manaSpent :: Mana.Mana,
    -- | CR 107.3m: the value of X chosen for the SPELL that became this
    -- permanent, which is the value of X for the permanent's
    -- enters-the-battlefield replacement effects. Two readers: CR 306.5b's
    -- intrinsic loyalty ability (Nissa, Steward of Elements) and a CR 614.1c row
    -- a CARD writes (Protean Hydra), through
    -- Pawl.Engine.Quantity.substituteAnnouncedX.
    --
    -- A SNAPSHOT copied across the move by Pawl.Engine.Event.changeZoneAttaching
    -- off the departing spell's own `bindings`, never a live read, CR 601.2b
    -- having fixed the number as the spell was cast. NOT the permanent's own X,
    -- which rule 107.3m puts at 0, which is why substituteAnnouncedX puts it into
    -- an entry row's quantity and nowhere else.
    announcedX :: Maybe Natural.Natural,
    -- | CR 601.2a: the zone this spell was moved to the stack FROM, which is what
    -- Aven Interrupter's "spells your opponents cast from graveyards or from
    -- exile" names. Read by Pawl.Engine.Filter's WasCastFrom atom.
    --
    -- STORED rather than derived, CR 400.7 leaving the stack object no memory of
    -- the zone it left, where Object.zone answers Stack for every spell CR 601.2f
    -- prices. Written TWICE for one cast, at the two moments CR 601.2 gives the
    -- question an answer, both in Pawl.Engine.Cast: onto the pre-move card by
    -- `asProposed`, the state the castability gate measures, and onto the stack
    -- incarnation by `stampCastFrom`. The two must agree or a card's gate and its
    -- payment price the same spell differently.
    --
    -- Nothing for every object that was not cast, and forgotten by
    -- newIncarnation: CR 400.7d's exception is about costs paid, and the zone a
    -- spell came from is not one.
    castFrom :: Maybe Zone.Zone,
    -- | CR 701.35a: this permanent is DETAINED -- it "can't attack or block and
    -- its activated abilities can't be activated" -- until the next turn of each
    -- player named here.
    --
    -- On the VICTIM rather than in a GameState list, doesNotUntapNext below's
    -- reason: an object keeps its ObjectId across a zone change, so a list keyed
    -- by id would follow the card back onto the battlefield as a permanent CR
    -- 400.7 makes a new object. As a field it is per-incarnation, and the
    -- forgetting IS that rule.
    --
    -- A SET OF PLAYERS and not a Pawl.Types.Expiry, rule 701.35a fixing the
    -- duration itself, so the only thing to remember is WHOSE next turn (CR
    -- 109.5). A set because two detains by different players run to two different
    -- turns and the later must outlast the earlier, while two by the same player
    -- collapse for free. Pawl.Engine.Expiry.dropAtTurnOf is the one sweep that
    -- reaches it.
    detainedUntil :: Set.Set PlayerId.PlayerId,
    -- | CR 701.15b: the players who have goaded this permanent, each entry
    -- running until that player's next turn (CR 701.15a). Read by
    -- Pawl.Engine.Goad, and turned into CR 508.1d requirements by
    -- Pawl.Engine.AttackRequirement.
    --
    -- detainedUntil's shape and its reasons, with CR 701.15c's several goaders
    -- needing the later turn to outlast the earlier and CR 701.15d's second goad
    -- by the same player creating no additional requirement, which a set
    -- collapses for free. CR 701.15b calls goaded a designation, but it is not a
    -- Pawl.Types.Designation: that type's marks are per-permanent and permanent,
    -- where this one is per-PLAYER and expires. Per-incarnation (CR 400.7).
    goadedBy :: Set.Set PlayerId.PlayerId,
    -- | CR 502.3 / CR 611.2: a ONE-SHOT untap prohibition standing over this
    -- permanent, said of ITS CONTROLLER's next untap step (Elvish Hunter).
    -- Written by Effect.DoesNotUntapNext and by nothing else, CR 701.43a's exert
    -- naming a PLAYER instead and riding exertedBy below.
    --
    -- Pawl.Types.UntapRestriction's stored counterpart: that one is a field on
    -- the PRINTING that forbids and is re-derived live, where this outlives the
    -- object that made it. On the VICTIM for detainedUntil's reason, and cleared
    -- where it applies by Engine.untapAll, so it needs no Pawl.Types.Expiry --
    -- CR 611.2a gives the effect the duration its own sentence states, and CR
    -- 502.3 runs that step for whoever controls the permanent then.
    --
    -- Not implemented: Telekinesis' "next TWO untap steps", which a Bool cannot
    -- hold and no card in the pool prints (gap #1653).
    doesNotUntapNext :: Bool,
    -- | CR 701.43a: the players who have EXERTED this permanent -- "you choose to
    -- have it not untap during your next untap step". Written by
    -- Pawl.Engine.Combat.declareAttackers paying CR 508.1g's optional cost, and
    -- read and emptied of a seat by Pawl.Engine.Engine.untapAll at that seat's
    -- untap step.
    --
    -- SEPARATE from doesNotUntapNext above because the two sentences name
    -- different untap steps: rule 701.43a's is keyed to a player and survives a
    -- control change, where Elvish Hunter's is a live read of whoever controls
    -- the permanent at the step. A SET for detainedUntil's reason, CR 701.43b
    -- letting a permanent be exerted more than once.
    --
    -- Per-incarnation (CR 400.7), and CR 701.43c is why nothing writes it back.
    exertedBy :: Set.Set PlayerId.PlayerId
  }
  deriving (Eq, Ord, Show)

-- | CR 400.7: "an object that moves from one zone to another becomes a new
-- object with no memory of, or relation to, its previous existence" -- the
-- forgetting, as one function. Every field above documented as per-incarnation
-- goes back to its no-memory value here, and nothing else is touched, so a field
-- added to Object is reset everywhere exactly when it is added HERE.
--
-- Leaves `owner` and `source` alone, which are not per-incarnation at all (CR
-- 108.3), and `zone` and `timestamp`, which the caller is DECIDING rather than
-- forgetting. A caller overrides the rest the same way -- CR 110.5b's "enters
-- tapped", CR 708.4's face-down status and CR 701.3's attach-on-entry are
-- choices the move makes about the new object.
newIncarnation :: Object -> Object
newIncarnation object =
  object
    { tapped = TapState.Untapped,
      -- CR 110.5b for a battlefield entry, CR 708.9 for a departure from one;
      -- Event.changeZoneFaceDown is the "otherwise".
      facing = Facing.FaceUp,
      -- CR 406.3: exiled cards are kept face up by default, and every other zone
      -- is face up outright. Event.changeZoneEntering is the "otherwise".
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
      kicked = Map.empty,
      -- CR 702.103b's record is written back by
      -- Pawl.Engine.Event.changeZoneAttaching's mkObj for the one move that
      -- keeps it, `kicked` above's route.
      bestowed = False,
      -- CR 601.2b's record is written back by
      -- Pawl.Engine.Event.changeZoneAttaching's mkObj.
      phyrexianLifePaid = 0,
      -- CR 400.7d's exception is written back by
      -- Pawl.Engine.Event.changeZoneAttaching's mkObj.
      manaSpent = Mana.MkMana [],
      -- CR 107.3m's exception is written back by the move that carries it, in
      -- Pawl.Engine.Event.changeZoneAttaching's mkObj.
      announcedX = Nothing,
      -- Nothing writes it back: no permanent reads it.
      castFrom = Nothing,
      -- CR 701.35a: the detained permanent that comes back is a new object, and
      -- nothing detained that one.
      detainedUntil = Set.empty,
      -- CR 701.15b: goaded is a designation a permanent has, and the object that
      -- returns is a different permanent.
      goadedBy = Set.empty,
      -- Nothing writes it back: the effect named a permanent, and the object
      -- that returns is not that permanent.
      doesNotUntapNext = False,
      -- CR 701.43c: nobody exerted the object that comes back.
      exertedBy = Set.empty
    }
