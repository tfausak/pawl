-- The trigger scan (CR 603.2, CR 603.10): which abilities trigger on a batch of
-- events, the look-back and batch scoping that decide where a source is read
-- from, delayed triggers coming due, and the state triggers. Asks
-- Pawl.Engine.Event.Match per condition and Pawl.Engine.Event.Binding per
-- match. Split out of Pawl.Engine.Event for size; nothing here raises an
-- event, so it sits beside the loop rather than above it.
module Pawl.Engine.Event.Trigger where

import qualified Control.Monad as Monad
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Battle as Battle
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.EffectZone as EffectZone
import Pawl.Engine.Event.Binding (eventBindings)
import Pawl.Engine.Event.Match (matchesTriggerGiven)
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection
import qualified Pawl.Engine.Replacement as Replacement
import qualified Pawl.Engine.Vanguard as Vanguard
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.BattlefieldCandidate as BattlefieldCandidate
import Pawl.Types.Card (Card)
import Pawl.Types.DelayedTrigger (DelayedTrigger)
import qualified Pawl.Types.DelayedTrigger as DelayedTrigger
import qualified Pawl.Types.DiscardCause as DiscardCause
import qualified Pawl.Types.Discarded as Discarded
import qualified Pawl.Types.EventGroup as EventGroup
import qualified Pawl.Types.Face as Face
import Pawl.Types.Game (Game)
import Pawl.Types.GameEvent (GameEvent)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.LastKnown as LastKnown
import qualified Pawl.Types.LoggedEvent as LoggedEvent
import qualified Pawl.Types.Moved as Moved
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PendingTrigger (PendingTrigger)
import qualified Pawl.Types.PendingTrigger as PendingTrigger
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.RevealCause as RevealCause
import qualified Pawl.Types.Revealed as Revealed
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.SpellWasCast as SpellWasCast
import qualified Pawl.Types.Subtype as Subtype
import Pawl.Types.TriggerCondition (TriggerCondition)
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggerEntry as TriggerEntry
import qualified Pawl.Types.TriggerSource as TriggerSource
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.TriggeredAbilitySource as TriggeredAbilitySource
import qualified Pawl.Types.TurnWindow as TurnWindow
import qualified Pawl.Types.TypeLine as TypeLine
import Pawl.Types.Zone (Zone)
import qualified Pawl.Types.Zone as Zone
import Pawl.Types.ZoneChange (ZoneChange)
import qualified Pawl.Types.ZoneChange as ZoneChange

-- CR 603.10's "objects that exist immediately after an event", taken of the
-- battlefield: every permanent standing on it, the player controlling it, and the
-- projection its abilities are read out of.
--
-- The whole battlefield rather than a diff or an overrides-only map, because all
-- three of the answers the scan wants can move and only a full reading tells
-- "unchanged" from "not there". The controller half is the one that used to be
-- kept alone, as a layer-2 overrides map; folding it in here costs nothing extra,
-- since the projection this walks is the same one the controller walk needs.
--
-- Projected ONCE for the whole board rather than per permanent, Projection.project
-- rerunning the whole-board gather fold on every call.
battlefieldCandidates :: GameState -> Map.Map ObjectId (BattlefieldCandidate.BattlefieldCandidate PC.ProjectedCharacteristics)
battlefieldCandidates gs =
  let projected = Projection.projectAll gs
      grants = Projection.controlGrants gs
   in Map.fromList
        ( Maybe.mapMaybe
            ( \oid -> case Map.lookup oid projected of
                -- Unreachable: projectAll is keyed on the same battlefield set this
                -- list walks, so every oid drawn from it has an entry.
                Nothing -> Nothing
                Just pc -> fmap (\ctrl -> (oid, BattlefieldCandidate.MkBattlefieldCandidate {BattlefieldCandidate.controller = ctrl, BattlefieldCandidate.characteristics = pc})) (Projection.controllerOfGiven grants Set.empty oid gs)
            )
            (Set.toAscList (GameState.battlefield gs))
        )

-- CR 603.10, FIRST sentence: the battlefield as it stood immediately after the
-- event `group` names, which is the board that rule checks a trigger condition
-- against. `recordEvent` files one sample per group; the live board answers for a
-- group no sample names, `eventTriggers`' fallback and its reason.
battlefieldAt :: EventGroup.EventGroup -> GameState -> Map.Map ObjectId (BattlefieldCandidate.BattlefieldCandidate PC.ProjectedCharacteristics)
battlefieldAt group gs = Map.findWithDefault (battlefieldCandidates gs) group (GameState.battlefieldWhenTriggered gs)

-- The zone change an event describes, if it is one.
movedOf :: GameEvent -> Maybe ZoneChange
movedOf event = case event of
  GameEvent.Moved (Moved.MkMoved zc _ _) -> Just zc
  -- Not implemented: CR 712.21's second card DID change zones, so this could
  -- answer its ZoneChange too. Its two engine callers are the graveyard
  -- candidate sources below, whose business is a card bearing an ability that
  -- functions from a graveyard; no meld pair in data/cards/ prints one (#3106).
  GameEvent.CardArrived _ -> Nothing
  GameEvent.DamageDealt _ -> Nothing
  GameEvent.DamagePrevented {} -> Nothing
  GameEvent.StepBegan {} -> Nothing
  GameEvent.SpellCast {} -> Nothing
  GameEvent.BecameMonarch _ -> Nothing
  GameEvent.TookInitiative _ -> Nothing
  -- The Moved event emitted by the same discard is the zone change; this one
  -- says the move WAS a discard (CR 701.9a).
  GameEvent.Discarded {} -> Nothing
  GameEvent.Drew {} -> Nothing
  -- CR 701.20b: a reveal is never a zone change, even when the card is about to
  -- make one.
  GameEvent.Revealed {} -> Nothing
  GameEvent.AttackerDeclared {} -> Nothing
  GameEvent.BecameBlocking {} -> Nothing
  GameEvent.BlocksDeclared {} -> Nothing
  GameEvent.AttackerBlocked {} -> Nothing
  GameEvent.AttackerUnblocked _ -> Nothing
  -- The Moved event `counter` records alongside this one is rule 701.6a's zone
  -- change; this one only says the move WAS a countering. The Discarded case.
  GameEvent.SpellCountered _ -> Nothing
  -- And the sibling is not a zone change at all: CR 608.2n's ceasing ability
  -- leaves no zone for a Moved event to name.
  GameEvent.AbilityCountered _ -> Nothing
  GameEvent.HalfUnlocked {} -> Nothing
  GameEvent.TurnedFaceUp _ -> Nothing
  GameEvent.Transformed {} -> Nothing
  GameEvent.BecameDesignated {} -> Nothing
  GameEvent.Evolved _ -> Nothing
  GameEvent.Mentored {} -> Nothing
  GameEvent.Trained _ -> Nothing
  GameEvent.PermanentSacrificed {} -> Nothing
  GameEvent.AbilityTriggered {} -> Nothing
  GameEvent.LoyaltyAbilityActivated _ -> Nothing
  GameEvent.LifeLost {} -> Nothing
  GameEvent.LifeGained {} -> Nothing
  GameEvent.CountersPut {} -> Nothing
  GameEvent.CountersRemoved {} -> Nothing
  GameEvent.ControlChanged {} -> Nothing
  GameEvent.VentureMarkerEntered {} -> Nothing
  GameEvent.BecameTarget {} -> Nothing
  GameEvent.BecameAttached {} -> Nothing
  GameEvent.BecameUnattached {} -> Nothing
  GameEvent.LeftTheGame _ -> Nothing
  GameEvent.Milled {} -> Nothing
  GameEvent.Scried _ -> Nothing
  GameEvent.DungeonCompleted _ -> Nothing
  GameEvent.Surveiled _ -> Nothing
  GameEvent.DiceRolled _ -> Nothing
  GameEvent.ClassLevelSet _ -> Nothing
  GameEvent.Plotted _ -> Nothing
  GameEvent.Explored _ -> Nothing
  GameEvent.Exerted _ -> Nothing
  GameEvent.BecameAttacked _ -> Nothing
  GameEvent.AttackersDeclared _ -> Nothing
  GameEvent.BecameTapped _ -> Nothing
  GameEvent.BecameUntapped _ -> Nothing
  GameEvent.TappedForMana _ -> Nothing
  GameEvent.CoinFlipped {} -> Nothing
  GameEvent.RingTempted _ -> Nothing
  GameEvent.Blighted _ -> Nothing

-- CR 603.10a: is this one of the conditions the game "looks back in time" for?
--
-- The rule states a CLOSED list of four families -- "leaves-the-battlefield
-- abilities, abilities that trigger when a player sacrifices a permanent,
-- abilities that trigger when a card leaves a graveyard, and abilities that
-- trigger when an object that all players can see is put into a hand or library"
-- -- and CR 603.10b through CR 603.10g add six more, none of which pawl has a
-- condition for. Everything else takes CR 603.10's first sentence.
--
-- A TOTAL case, with no wildcard, which is the whole reason this is a function
-- rather than a guard at the use site: a new condition must be READ against that
-- list, and a wildcard would silently give it whichever answer was convenient.
-- -Werror is what makes that a compile error rather than a rules bug.
--
-- Not a case on an EFFECT's identity. CR 603.10a enumerates trigger CONDITIONS,
-- the way rule 702 enumerates keywords, so asking which family a condition falls
-- in is reading the rulebook rather than reading a card.
--
-- What the answer decides is narrow: whether a bearer that left the battlefield
-- in the event's OWN group is offered for this ability. A bearer that left at a
-- later group is offered either way (CR 603.10's first sentence), and a
-- condition's own matcher still has the last word.
looksBack :: TriggerCondition -> Bool
looksBack condition = case condition of
  -- CR 603.10a is about a bearer that left the battlefield, and CR 309.2c keeps a
  -- dungeon card in the command zone until it leaves the game.
  TriggerCondition.RoomEntered _ -> False
  -- Not on CR 603.10a's list either, and CR 309.2c takes the dungeon card out of
  -- the GAME rather than to a zone, so CR 603.10's first sentence governs.
  TriggerCondition.PlayerCompletesDungeon _ -> False
  -- None of the four is on CR 603.10a's list, and none is a zone change at
  -- all: CR 701.22a moves cards within one library, CR 701.25a and CR 701.44a
  -- do move cards but their events say the keyword action COMPLETED rather
  -- than that anything left a zone, and CR 702.170b's exile is a card leaving
  -- a HAND. So CR 603.10's first sentence governs all four.
  TriggerCondition.PlayerScries _ -> False
  TriggerCondition.RingTemptsPlayer _ -> False
  TriggerCondition.PlayerSurveils _ -> False
  TriggerCondition.SelfBecomesPlotted -> False
  TriggerCondition.PermanentExplores _ -> False
  -- Not on CR 603.10a's list either, and no zone change at all: CR 701.68a
  -- puts counters on a permanent that stays where it is.
  TriggerCondition.PlayerBlights _ -> False
  -- Not on CR 603.10a's list, and CR 706.1's roll is no zone change: it moves
  -- no object at all, so CR 603.10's first sentence governs.
  TriggerCondition.PlayerRollsDice _ -> False
  TriggerCondition.PlayerWinsCoinFlip _ -> False
  -- The same answer once more, and the most plainly: CR 701.43c can only exert a
  -- permanent that is ON the battlefield, so nothing has changed zones.
  TriggerCondition.SelfExerted -> False
  -- CR 603.10a's list does not reach an attachment either. CR 701.3a moves a
  -- permanent ONTO another one without changing its zone, and the one route that
  -- is a zone change -- CR 608.3c's Aura spell arriving attached -- leaves both
  -- objects on the battlefield for a live read.
  TriggerCondition.SelfBecomesAttachedBy _ -> False
  -- The attachment's own scope on the same event, for the same reason: CR 701.3a
  -- moves it onto the host without changing its zone.
  TriggerCondition.SelfBecomesAttachedTo _ -> False
  -- CR 603.10c NAMES this one: "abilities that trigger specifically when an
  -- object becomes unattached look back in time". The rule earns it -- CR 701.3d
  -- counts the attachment leaving the battlefield as becoming unattached, so the
  -- bearer is routinely gone by the CR 117.5 boundary.
  TriggerCondition.SelfBecomesUnattachedFrom _ -> True
  -- CR 603.6c's two written forms, which CR 603.10a names first:
  -- leaves-the-battlefield abilities. CR 700.4 narrows the second to a
  -- graveyard, and narrowing the destination does not leave the family.
  TriggerCondition.SelfDies -> True
  TriggerCondition.PermanentDies _ -> True
  -- The batch reading of that same form is in the same family: CR 603.10a names
  -- leaves-the-battlefield abilities without counting them. Load-bearing rather
  -- than tidy -- it is what offers a bearer swept up in its own batch the
  -- group-mates that died beside it (CR 603.10a's own Example).
  TriggerCondition.PermanentsDie _ -> True
  TriggerCondition.SelfLeavesTheBattlefield -> True
  TriggerCondition.PermanentLeavesTheBattlefield _ -> True
  -- CR 603.10a twice over: this is a leaves-the-battlefield ability, and it is
  -- also an ability that triggers when an object all players can see is put
  -- into a hand.
  TriggerCondition.PermanentReturnedToHand _ -> True
  -- The batch reading is in the same family, PermanentsDie's reason: a bearer
  -- swept up in its own batch still sees the group-mates returned beside it.
  TriggerCondition.PermanentsReturnedToHand _ -> True
  -- CR 603.10a's third family, named in that rule's own list: "abilities that
  -- trigger when a card leaves a graveyard".
  TriggerCondition.CardLeavesGraveyard {} -> True
  -- CR 603.10a's first family read off the HOST rather than the bearer: this
  -- triggers when a permanent leaves the battlefield, so the rule reaches the
  -- ability however the bearer is found.
  TriggerCondition.AttachedCreatureDies -> True
  -- Not on CR 603.10a's list at all: that rule names leaves-the-battlefield
  -- abilities, sacrifices, cards leaving a graveyard and objects put into a hand
  -- or library, and a permanent becoming tapped is none of them. It stays on the
  -- battlefield, so the ordinary CR 603.10 reading -- the board as it is now --
  -- is the right one.
  TriggerCondition.AttachedCreatureBecomesTapped -> False
  -- Nor is becoming untapped: CR 701.26b leaves the permanent standing on
  -- the battlefield too, so the same live read is the right one.
  TriggerCondition.SelfBecomesUntapped -> False
  -- Not on CR 603.10a's list either, the arm above's reason: the permanent
  -- tapped for mana is standing on the battlefield when the ability is
  -- gathered.
  TriggerCondition.AttachedPermanentTappedForMana -> False
  -- The same event and the same reason, whoever watches it.
  TriggerCondition.PermanentTappedForMana {} -> False
  -- CR 603.10a's first family again, read off the event rather than off the
  -- bearer: this triggers when a permanent leaves the battlefield. Inert today --
  -- the bearer is a card in exile, which no look-back source can offer -- but a
  -- classification the rule decides, not the scan.
  TriggerCondition.HauntedCreatureDies -> True
  -- CR 603.10a's second family in as many words: "abilities that trigger when a
  -- player sacrifices a permanent".
  TriggerCondition.PermanentSacrificed {} -> True
  -- CR 603.1b: one ability, several conditions. It looks back if ANY of them
  -- does -- the ability is on the rule's list if the rule reaches it at all, and
  -- a condition that does not look back is unaffected, since its own matcher
  -- still has to admit the candidate.
  TriggerCondition.AnyOf conditions -> any looksBack conditions
  -- CR 603.6c's own last sentence puts this OUTSIDE the family: a card put into
  -- a graveyard from anywhere is not a leaves-the-battlefield ability, and CR
  -- 603.10's normal reading applies. The constructor's own Haddock argues it.
  TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> False
  -- The bystander reading of the arm above, and outside the family for the same
  -- sentence of CR 603.6c.
  TriggerCondition.CardPutIntoGraveyard _ -> False
  -- Library to graveyard, which is on none of CR 603.10a's four families: a card
  -- leaving a LIBRARY is not a card leaving a graveyard, and the bearer this
  -- reads is the CR 400.7 incarnation that arrived.
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> False
  -- CR 708.8 leaves the permanent on the battlefield, so there is no departure
  -- for a look-back to recover and the live read is what CR 603.10's first
  -- sentence asks for. Both written forms.
  TriggerCondition.SelfTurnedFaceUp -> False
  TriggerCondition.PermanentTurnedFaceUp _ -> False
  -- CR 712.18 is the same claim about transforming, and states it outright: the
  -- permanent "doesn't become a new object", so there is no departure for CR
  -- 603.10a to look back at.
  TriggerCondition.SelfTransformedInto _ -> False
  -- The bystander form of that same claim: CR 712.18's permanent is still
  -- there, whoever is watching it.
  TriggerCondition.PermanentTransforms _ -> False
  -- CR 702.112b's designation is given to a permanent that stays where it is, so
  -- there is no departure here either.
  TriggerCondition.PermanentBecomesDesignated {} -> False
  -- Nor here: rule 702.100b's counters are put on a permanent on the battlefield.
  TriggerCondition.SelfEvolves -> False
  -- Nor here, for the same reason one rule over: rule 702.134a's counter goes on a
  -- creature that CR 508.1k has made an attacking creature, and a permanent leaving
  -- the battlefield is removed from combat (CR 506.4) rather than mentored.
  TriggerCondition.AttachedCreatureMentors -> False
  -- Nor here, and by the same sentence: rule 702.149a's counter goes on an
  -- attacking creature, which CR 506.4 has removed from combat if it left.
  TriggerCondition.SelfTrains -> False
  -- Entries, not departures (CR 603.6a). The rule's own CR 603.6a checks "all
  -- permanents on the battlefield (including the newcomers)" AFTER the event.
  TriggerCondition.SelfEnters -> False
  TriggerCondition.PermanentEnters _ -> False
  -- Turn structure, the stack, damage, life, counters and the rest: none names a
  -- zone change at all, so CR 603.10a's list cannot reach them.
  TriggerCondition.StepBegins {} -> False
  TriggerCondition.StateIs _ -> False
  TriggerCondition.SelfDealsCombatDamageToPlayer -> False
  TriggerCondition.SelfIsDealtDamage -> False
  TriggerCondition.PermanentDealsCombatDamageToPlayer _ -> False
  TriggerCondition.PermanentsDealCombatDamageToPlayer _ -> False
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> False
  TriggerCondition.CreaturesDealtCombatDamageToInitiative -> False
  TriggerCondition.PlayerTookInitiative -> False
  TriggerCondition.OpponentLostLifeDuringYourTurn -> False
  TriggerCondition.SelfCycled -> False
  TriggerCondition.SelfRevealedForMiracle -> False
  TriggerCondition.SelfDiscarded -> False
  TriggerCondition.PlayerDiscards _ -> False
  TriggerCondition.PlayerCycles _ -> False
  TriggerCondition.PlayerDrawsNthCard {} -> False
  TriggerCondition.SelfAttacks _ -> False
  TriggerCondition.SelfAttacksWithAnother _ -> False
  TriggerCondition.CreatureAttacksAlone _ -> False
  TriggerCondition.CreatureAttacksYou -> False
  TriggerCondition.AttachedPlayerIsAttacked -> False
  TriggerCondition.PlayerAttacks _ -> False
  TriggerCondition.PlayerAttacksWith {} -> False
  TriggerCondition.PlayerAttacksPlayer {} -> False
  TriggerCondition.SelfAttacksPlayerWithMostLife -> False
  TriggerCondition.SelfBlocks -> False
  TriggerCondition.SelfBlocksCreature _ -> False
  TriggerCondition.SelfBlocksAtLeast _ -> False
  TriggerCondition.SelfBlocksOneOrMore _ -> False
  TriggerCondition.SelfBecomesBlocked -> False
  TriggerCondition.SelfBecomesBlockedBy _ -> False
  TriggerCondition.PermanentBecomesBlockedBy _ -> False
  TriggerCondition.SelfBecomesBlockedByOneOrMore _ -> False
  TriggerCondition.CreatureBecomesBlockedByAtLeast {} -> False
  TriggerCondition.SelfAttacksUnblocked -> False
  TriggerCondition.SpellOrAbilityCounters _ -> False
  TriggerCondition.AbilityIsCountered -> False
  TriggerCondition.DamageToPlayerPrevented _ -> False
  TriggerCondition.SelfPreventsDamage _ -> False
  TriggerCondition.PlayerGainsLife _ -> False
  -- CR 603.10a's list names no life change under either scope, so the batch
  -- reading looks back no further than the per-seat one above.
  TriggerCondition.PlayersGainLife _ -> False
  TriggerCondition.PlayerLosesLife _ -> False
  TriggerCondition.SelfCountersReached {} -> False
  TriggerCondition.SelfBecomesClassLevel _ -> False
  TriggerCondition.SelfLastCounterRemoved _ -> False
  TriggerCondition.SelfCountersRemoved _ -> False
  -- CR 603.10a's list does not name counter placements, so the normal reading
  -- applies: the abilities checked are the ones existing immediately after the
  -- event. Every counter mirror above answers alike, both placement scopes
  -- included.
  TriggerCondition.PermanentsGetCounters {} -> False
  TriggerCondition.PermanentGetsCounters {} -> False
  TriggerCondition.SpellCast {} -> False
  TriggerCondition.SelfCast -> False
  TriggerCondition.SelfBecomesTargeted _ -> False
  TriggerCondition.ControllerBecomesTarget {} -> False
  TriggerCondition.SelfHalfUnlocked _ -> False
  TriggerCondition.RoomFullyUnlocked _ -> False
  -- CR 603.3b's second class names no zone change at all -- its event is another
  -- ability triggering -- so CR 603.10a's four families cannot reach it. Its
  -- bearer is a permanent standing on the battlefield watching a Saga, and CR
  -- 603.10's first sentence is what reads it.
  TriggerCondition.SagaFinalChapterTriggers _ -> False
  -- CR 725.1's crowning names no zone change either -- it moves a DESIGNATION,
  -- not an object -- so none of CR 603.10a's four families reaches it.
  TriggerCondition.PlayerBecomesMonarch _ -> False
  -- CR 603.10a's look-back is for a bearer that left; this condition's subject is a
  -- permanent still on the battlefield, Engine.sampleControl sampling nowhere else,
  -- and its bearer is a CR 603.7 delayed entry the store keeps rather than the log.
  TriggerCondition.LoseControlOfBound _ -> False
  -- CR 603.10a's look-back IS this condition's subject -- the land is gone by the
  -- time the ability triggers -- but the match reads the bound id rather than any
  -- characteristic of it, so nothing here needs a snapshot to answer from.
  TriggerCondition.BoundDiesOrIsExiled _ -> False
  -- CR 603.10a's look-back is a question about which event fired the ability, and
  -- a reflexive is fired by none. Its bearer is a CR 603.7 delayed entry too.
  TriggerCondition.Reflexive -> False

-- CR 603.2c: does this condition's trigger event CONTAIN the occurrences, or IS
-- each occurrence its trigger event? "Whenever one or more
-- other creatures you control die" names the whole CR 704.3 / CR 608.2f batch, so
-- a sweep that buries three contains one occurrence of it; "whenever another
-- creature you control dies" names each death, so the same sweep contains three,
-- which is the rule's own Example.
--
-- Read by the two scans that gather triggers -- eventTriggers for an object's own
-- ability and delayedPending for a CR 603.7 entry -- and only to decide how many
-- pending triggers one Pawl.Types.EventGroup may yield per (bearer, ability).
-- matchesTriggerGiven sees one event at a time and so answers the same for both
-- readings -- that is its contract, and this predicate is what keeps it intact.
-- A gatherer that forgot to ask it fired a batch condition once per member; see
-- #2384.
--
-- A total case over TriggerCondition and never a wildcard, for looksBack's reason:
-- the fork is one CR 603.2c forces on every zone-change and event-watching
-- condition, so a new one must be classified rather than defaulted. Everything but
-- the batch reading is per-occurrence, which is the rule's SECOND sentence -- one
-- event containing several occurrences triggers repeatedly -- where the batch
-- reading is its FIRST, one trigger event and so one trigger. But a future "whenever one or more" printing on any other
-- event would answer True here, so the arms are written out rather than folded.
--
-- CR 603.1b lets one ability carry several conditions, and `any` makes such an
-- ability batch-scoped as a whole: a mixed AnyOf would fire at most once per group
-- rather than once per group plus once per member. Nothing in data/cards mixes
-- the two readings, and CardSpec's anyOfOffends already narrows what an AnyOf may
-- hold; a card that did would need this arm re-derived rather than reused.
batchScoped :: TriggerCondition -> Bool
batchScoped condition = case condition of
  TriggerCondition.RoomEntered _ -> False
  TriggerCondition.PlayerScries _ -> False
  TriggerCondition.RingTemptsPlayer _ -> False
  TriggerCondition.PlayerBlights _ -> False
  TriggerCondition.PlayerCompletesDungeon _ -> False
  TriggerCondition.PlayerSurveils _ -> False
  TriggerCondition.SelfBecomesPlotted -> False
  TriggerCondition.PermanentExplores _ -> False
  -- Per-occurrence, and indistinguishable from the batch reading of Feywild
  -- Trickster's "one or more dice": one Effect.RollDie records exactly one
  -- GameEvent.DiceRolled however many dice CR 706.1's count threw, so the
  -- ability fires at most once either way. What would make the two readings
  -- differ is an event per DIE, which the printed words do not ask for.
  TriggerCondition.PlayerRollsDice _ -> False
  TriggerCondition.PlayerWinsCoinFlip _ -> False
  TriggerCondition.SelfExerted -> False
  TriggerCondition.SelfBecomesAttachedBy _ -> False
  TriggerCondition.SelfBecomesAttachedTo _ -> False
  -- Per-occurrence too: CR 701.3d's routes each unattach one permanent from one
  -- host, and no printing reads a batch of them.
  TriggerCondition.SelfBecomesUnattachedFrom _ -> False
  TriggerCondition.SelfDies -> False
  TriggerCondition.PermanentDies _ -> False
  TriggerCondition.PermanentsDie _ -> True
  TriggerCondition.SelfLeavesTheBattlefield -> False
  TriggerCondition.PermanentLeavesTheBattlefield _ -> False
  -- Per-permanent too: Justice, Vance Astrovik's "whenever another nonland
  -- permanent you control is returned" is CR 603.2c's second sentence.
  TriggerCondition.PermanentReturnedToHand _ -> False
  -- A True beside PermanentsDie: CR 608.2f's sweep returns every permanent as
  -- one action, Pawl.Engine.Resolve brackets it as one Pawl.Types.EventGroup,
  -- and "one or more noncreature permanents are returned to hand" (Tameshi,
  -- Reality Architect) names that whole group as its trigger event.
  TriggerCondition.PermanentsReturnedToHand _ -> True
  -- Per-card, PermanentReturnedToHand's answer: Kishla Skimmer's "whenever a card
  -- leaves your graveyard" is CR 603.2c's second sentence, so a resolution that
  -- moved two cards out of one graveyard fires it twice.
  --
  -- Not implemented: the batch reading of the same family, Fang, Fearless l'Cie's
  -- "whenever one or more cards leave your graveyard", which would stand beside
  -- this arm as PermanentsReturnedToHand stands beside PermanentReturnedToHand
  -- (gap #3222).
  TriggerCondition.CardLeavesGraveyard {} -> False
  TriggerCondition.AttachedCreatureDies -> False
  -- CR 603.2e names the MOMENT a permanent becomes tapped, and a moment holds one
  -- occurrence; no printing of that event says "one or more".
  TriggerCondition.AttachedCreatureBecomesTapped -> False
  -- The untap direction the same way, CR 502.3's simultaneous batch
  -- included: the condition is about the BEARER, so a batch that untaps a
  -- whole board still holds one occurrence of it.
  TriggerCondition.SelfBecomesUntapped -> False
  -- CR 106.12a names one resolution of one mana ability, so one occurrence;
  -- no printing of it says "one or more".
  TriggerCondition.AttachedPermanentTappedForMana -> False
  -- One occurrence for the arm above's reason, bystander or not.
  TriggerCondition.PermanentTappedForMana {} -> False
  TriggerCondition.HauntedCreatureDies -> False
  TriggerCondition.PermanentSacrificed {} -> False
  TriggerCondition.AnyOf conditions -> any batchScoped conditions
  -- CR 603.2c's second sentence: Planar Void's printing is singular, so each
  -- card put into a graveyard is its own trigger event.
  TriggerCondition.CardPutIntoGraveyard _ -> False
  TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> False
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> False
  TriggerCondition.SelfTurnedFaceUp -> False
  TriggerCondition.PermanentTurnedFaceUp _ -> False
  TriggerCondition.SelfTransformedInto _ -> False
  TriggerCondition.PermanentTransforms _ -> False
  TriggerCondition.PermanentBecomesDesignated {} -> False
  TriggerCondition.SelfEvolves -> False
  TriggerCondition.AttachedCreatureMentors -> False
  TriggerCondition.SelfTrains -> False
  TriggerCondition.SelfEnters -> False
  TriggerCondition.PermanentEnters _ -> False
  TriggerCondition.StepBegins {} -> False
  TriggerCondition.StateIs _ -> False
  TriggerCondition.SelfDealsCombatDamageToPlayer -> False
  TriggerCondition.SelfIsDealtDamage -> False
  TriggerCondition.PermanentDealsCombatDamageToPlayer _ -> False
  -- A True beside PermanentsDie: CR 510.2 deals every combat damage of a step
  -- simultaneously, Pawl.Engine.Damage.dealWave brackets the step as one
  -- Pawl.Types.EventGroup, and "one or more artifact creatures ... deal combat
  -- damage" (Pia Nalaar, Chief Mechanic) names that whole group as its trigger
  -- event, which occurs once -- where the arm above is CR 603.2c's second
  -- sentence and fires once per damager.
  TriggerCondition.PermanentsDealCombatDamageToPlayer _ -> True
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> False
  -- CR 726.2's own "one or more creatures a player controls deal combat
  -- damage", the True beside PermanentsDealCombatDamageToPlayer's. Unreached
  -- through this classifier, which serves the gatherer that walks bearers:
  -- Pawl.Engine.Initiative.inherentPending does the grouping itself for a
  -- condition no card can bear. A take is one occurrence per taker, so the arm
  -- below is False.
  TriggerCondition.CreaturesDealtCombatDamageToInitiative -> True
  TriggerCondition.PlayerTookInitiative -> False
  TriggerCondition.OpponentLostLifeDuringYourTurn -> False
  TriggerCondition.SelfCycled -> False
  TriggerCondition.SelfRevealedForMiracle -> False
  TriggerCondition.SelfDiscarded -> False
  TriggerCondition.PlayerDiscards _ -> False
  TriggerCondition.PlayerCycles _ -> False
  TriggerCondition.PlayerDrawsNthCard {} -> False
  TriggerCondition.SelfAttacks _ -> False
  TriggerCondition.SelfAttacksWithAnother _ -> False
  TriggerCondition.CreatureAttacksAlone _ -> False
  TriggerCondition.CreatureAttacksYou -> False
  TriggerCondition.AttachedPlayerIsAttacked -> False
  TriggerCondition.PlayerAttacks _ -> False
  TriggerCondition.PlayerAttacksWith {} -> False
  TriggerCondition.PlayerAttacksPlayer {} -> False
  TriggerCondition.SelfAttacksPlayerWithMostLife -> False
  TriggerCondition.SelfBlocks -> False
  TriggerCondition.SelfBlocksCreature _ -> False
  -- FALSE despite naming batches in the RULES, which is the one group of answers
  -- here that is not what it looks like. Rule 509.3e's "one or more" and rule
  -- 508.3b's are already once-per-declaration, structurally: their events
  -- (GameEvent.BlocksDeclared, GameEvent.AttackerBlocked, GameEvent.BecameAttacked)
  -- are minted at that arity by Pawl.Engine.Combat, the one emitter that sees a
  -- whole declaration, so there is nothing left for this predicate to dedup and
  -- True would be a second dedup over an already-unique trigger. Deaths get the
  -- other treatment because they reach a graveyard from four places and no event
  -- carries the arity; GameEvent.BecameAttacked's own haddock draws that line.
  TriggerCondition.SelfBlocksAtLeast _ -> False
  TriggerCondition.SelfBlocksOneOrMore _ -> False
  TriggerCondition.SelfBecomesBlocked -> False
  TriggerCondition.SelfBecomesBlockedBy _ -> False
  TriggerCondition.PermanentBecomesBlockedBy _ -> False
  TriggerCondition.SelfBecomesBlockedByOneOrMore _ -> False
  TriggerCondition.CreatureBecomesBlockedByAtLeast {} -> False
  TriggerCondition.SelfAttacksUnblocked -> False
  TriggerCondition.SpellOrAbilityCounters _ -> False
  TriggerCondition.AbilityIsCountered -> False
  TriggerCondition.DamageToPlayerPrevented _ -> False
  -- Per PREVENTION, which is what the record already is: groupPreventions
  -- collapsed the batch to one entry per applying instance -- over recipients as
  -- well as events -- so rule 615.13's "one or more simultaneous damage events"
  -- is spent before this is asked; the DamageToPlayerPrevented arm above's
  -- reasoning, one identity over.
  TriggerCondition.SelfPreventsDamage _ -> False
  TriggerCondition.PlayerGainsLife _ -> False
  -- The one True that is not about objects: "whenever one or more players gain
  -- life" names the whole CR 608.2f batch as its trigger event, so "each player
  -- gains 4 life" is one occurrence of it and not one per seat -- where the
  -- PlayerGainsLife arm above is CR 603.2c's second sentence and fires once for
  -- each.
  TriggerCondition.PlayersGainLife _ -> True
  TriggerCondition.PlayerLosesLife _ -> False
  TriggerCondition.SelfCountersReached {} -> False
  TriggerCondition.SelfBecomesClassLevel _ -> False
  TriggerCondition.SelfLastCounterRemoved _ -> False
  TriggerCondition.SelfCountersRemoved _ -> False
  -- A True beside PermanentsDie and PlayersGainLife: CR 603.2c's FIRST sentence
  -- is what this constructor exists for -- "one or more counters ... on one or
  -- more permanents" names the whole group as the trigger event, which occurs
  -- once.
  TriggerCondition.PermanentsGetCounters {} -> True
  -- And the SECOND sentence, which is the whole of the difference between this
  -- constructor and the one above: "on A creature" names each permanent the
  -- batch touched, so a sweep that counters three of them contains three
  -- occurrences.
  TriggerCondition.PermanentGetsCounters {} -> False
  TriggerCondition.SpellCast {} -> False
  TriggerCondition.SelfCast -> False
  TriggerCondition.SelfBecomesTargeted _ -> False
  TriggerCondition.ControllerBecomesTarget {} -> False
  TriggerCondition.SelfHalfUnlocked _ -> False
  TriggerCondition.RoomFullyUnlocked _ -> False
  TriggerCondition.SagaFinalChapterTriggers _ -> False
  TriggerCondition.PlayerBecomesMonarch _ -> False
  TriggerCondition.LoseControlOfBound _ -> False
  TriggerCondition.BoundDiesOrIsExiled _ -> False
  TriggerCondition.Reflexive -> False

-- The log cut into its CR 704.3 / CR 608.2f events: one block per
-- Pawl.Types.EventGroup, in log order, blocks and members alike.
--
-- Groups are non-decreasing along the log -- Event.recordEvent only mints a fresh
-- one or repeats the frozen one -- so the members of one event are contiguous and
-- an adjacent grouping is exact rather than approximate. That holds of any
-- order-preserving SUBLIST of the log too, filtering dropping members and never
-- reordering them, which is what lets delayedPending cut its already-matched
-- events with this same knife.
--
-- One function because two scans ask the question. eventTriggers asks it of the
-- whole batch and delayedPending of a CR 603.7 entry's matches, and a second
-- reading of "which events happened at once" could drift from this one without
-- anything noticing; see #2385. NonEmpty because a group with no members cannot
-- occur, which spares both callers an impossible arm.
eventGroups :: [LoggedEvent.LoggedEvent] -> [NonEmpty.NonEmpty LoggedEvent.LoggedEvent]
eventGroups = NonEmpty.groupBy (\a b -> LoggedEvent.group a == LoggedEvent.group b)

-- CR 603.6a: every event is checked against every permanent currently on the
-- battlefield, not only the object the event names -- a step trigger belongs to a
-- permanent with nothing to do with the event.
--
-- "Currently" is the word CR 603.10's first sentence corrects, and the battlefield
-- reading here is per EVENT GROUP for that reason: each group's permanents, their
-- abilities and their controllers are the ones GameState.battlefieldWhenTriggered
-- sampled as the group was recorded, never the board the scan finds. The sample is
-- one projectAll per group rather than per (event, permanent) pair --
-- Projection.project reruns the whole-board `gather` fold on every call, which made
-- this scan quadratic in board size.
--
-- The battlefield is not the only scanned zone -- every GRAVEYARD and the whole
-- EXILE zone are scanned for the abilities CR 113.6k puts there, a spell that
-- just became cast is offered from the STACK for the same rule, the card a player
-- revealed as they drew it is offered from their HAND for it too, and an EMBLEM is
-- offered from the command zone under CR 114.4. The rest of the command zone is
-- unscanned: the only other thing it holds is a dungeon card, whose room
-- abilities CR 309.4c mints rather than prints, leaving this scan nothing on a
-- face to read.
-- Pawl.Engine.Dungeon.roomPending gathers those.
--
-- Two holes are left in the BATTLEFIELD half of that reading, and last known
-- information fills both. A permanent that left WITHIN its own group is missing
-- from that group's sample, the sample being taken as the last of the group's
-- members is recorded and CR 704.3's whole destruction batch being one group; and
-- a group nothing sampled has no reading but the live board, which the departed
-- are not on either.
--
-- So each event ALSO contributes the permanents that left the battlefield at a
-- LATER EVENT GROUP in the same batch, read from CR 608.2h last known information
-- -- `laterGroups` below, which the sample outranks wherever both hold the same id.
-- Four things make that exact rather than approximate:
--
--   * The same reading, one event later. A permanent removed by a later event
--     existed immediately after this one, which is what the rule asks. It reaches
--     the event's own newcomer for free: a creature entering as a 0/0 and buried
--     by CR 704.5f leaves at a later group than its entry.
--   * No double fire, structurally: `lastKnown` is written by the zone change that
--     DELETES an id, and CR 400.7 mints a fresh id per move, so no id is in both.
--   * The right snapshot: `lastKnown` holds the permanent as it was on the
--     battlefield, continuous effects applied, which CR 603.10 demands.
--   * A canonical place in the order: candidates are a Map keyed by ObjectId and
--     traversed ascending, so extras sort in rather than being appended.
--
-- CR 603.10a is the other half of that rule, the exception rather than the normal
-- case, and it contributes twice. A DEPARTURE event contributes the permanent it
-- took off the battlefield (`leftBattlefield`), and every event contributes the
-- permanents that left at its OWN group (`sameGroup`) -- for a look-back
-- condition alone, since only those read "the appearance of objects immediately
-- prior to the event". For both, the last-known reading is what the rule asks for
-- rather than a repair for a late boundary.
--
-- Which conditions those are is `looksBack`, a total case over TriggerCondition
-- and never a wildcard: CR 603.10a states a closed list, so a condition the rule
-- has not been read against must be classified rather than defaulted.
--
-- The GRAVEYARD half has a hole of the same shape, and last known information
-- fills it the same way: a card the batch put into a graveyard and took back out
-- again is not in the graveyard the boundary scan walks, so `arrivedInGraveyard`
-- offers it to its own arrival event from CR 608.2h. The other direction of that
-- half is narrowed rather than widened: the graveyard the boundary scan walks
-- holds cards the batch put there, and `arrivedLater` withholds each of them from
-- every event of a strictly earlier group, which is the same per-event reading
-- `laterGroups` gives the battlefield.
--
-- Not reconstructed: a permanent that ENTERED later in the batch and left before
-- the boundary is still offered to the batch's earlier events (#441). Nor is a
-- departed graveyard card offered to any event but its own arrival: the three
-- conditions zonesTriggeredFrom sends to a graveyard are self-referential arrival
-- conditions whose only matching event IS that arrival, so the only ability that
-- could observe the difference is a CR 113.6m one -- Squee, Goblin Nabob's
-- upkeep, read from a graveyard (#1732).
--
-- Events outer, permanents inner (ascending by id): the deterministic canonical
-- order the CR 603.3b ordering prompt indexes into. Groups do not disturb it --
-- they decide which permanents an event is offered, never in what order.
--
-- CR 603.2c's batch conditions do not disturb it either, which is why they are a
-- DEDUP (`oncePerBatch` below) rather than a second pass: dropping every match
-- after a group's first leaves each surviving trigger at the position its earliest
-- matching event gave it, where appending the batch's triggers to the block would
-- have moved them.
eventTriggers :: [LoggedEvent.LoggedEvent] -> GameState -> [PendingTrigger]
eventTriggers events gs =
  let -- CR 702.70a: a keyword can BE a triggered ability, so a permanent's
      -- abilities are its printed-and-granted ones plus the ones rule 702 mints
      -- from its keywords. Derived from POST-LAYER counts, so Humility takes them
      -- away and a layer-6 grant adds them without special-casing. Shared by both
      -- candidate sources, so a live and a last-known permanent read alike.
      -- CR 310.12b's Siege ability is minted the same way and for the same reason,
      -- off the same finished projection: rule 310 gives it, no card prints it, and
      -- the scan below never learns which rule produced any of these.
      --
      -- Through Projection.mintedTriggeredAbilitiesOf rather than
      -- Keyword.triggeredAbilitiesOf directly, so CR 612.2a's text change reaches
      -- the words rule 702 writes -- the Spirit an afterlife trigger creates. Rule
      -- 310.12b's Siege ability names no subtype word, so it needs no such wrapper.
      abilitiesOf pc = PC.triggeredAbilities pc <> Projection.mintedTriggeredAbilitiesOf pc <> Battle.triggeredAbilitiesOf pc
      -- CR 113.6m's "functions ONLY in that zone", asked of a permanent read AS
      -- BEING ON THE BATTLEFIELD: a Squee, Goblin Nabob standing there does not
      -- see its own upkeep, because the ability that watches for it functions in
      -- the graveyard. The mirror of the filter
      -- Pawl.Engine.Activate.abilitiesForGiven puts on its battlefield arm.
      --
      -- Applied to every reading that says "this permanent was on the
      -- battlefield": the live `onBattlefield` set, the two group-scoped unions
      -- below (`laterGroups` and `sameGroup`), which recover a permanent that WAS
      -- on the battlefield at this event and has left by the CR 117.5 boundary,
      -- and `leftBattlefield` -- CR 603.10a's look-back at the permanent THIS
      -- event removed. Last known information (CR 608.2h) is how a departed
      -- permanent is read, not a statement about which zone it is being read IN,
      -- so the zone CR 113.6m compares against is the battlefield either way.
      --
      -- Safe for `leftBattlefield` because `zoneFunctionedFrom`'s
      -- `conditionPutsSelfInto` reads the rule's own exception: a dies trigger
      -- whose effect names the graveyard it just died into is exempted back to
      -- the battlefield default, the same way `enchantedObjectLeaves` exempts an
      -- Aura's own death trigger. Endless Cockroaches ("when this creature dies,
      -- return it to its owner's hand") is the shape the rule means, though its
      -- own payload never reaches the exception today: its effect names
      -- Binding.became rather than the reserved source slot, so
      -- Pawl.Engine.EffectZone.zoneFunctionedFrom already answers Nothing for it
      -- (Pawl.ZoneTriggerSpec's `becameSlotSpec` proves the trigger fires either
      -- way). Ivory Gargoyle's delayed-trigger pair (CR 113.6m's final
      -- sentence, #2500) is what reaches the exception instead, its own payload
      -- naming the reserved slot: `Pawl.LeavesTriggerSpec.ivoryGargoyleSpec` proves
      -- a mutation dropping the exception empties this event's own candidate
      -- list.
      --
      -- The delayed map is the HOST's own face while `abilitiesOf` is the
      -- PROJECTED list, so an ability granted by another object that arms a
      -- delayed trigger the GRANTOR declared resolves to no name here and takes
      -- CR 113.6's battlefield default. Nothing in data/cards/ grants an ability
      -- that arms one; Pawl.Engine.Activate.abilitiesForGiven carries the same
      -- pairing and the same note.
      battlefieldAbilitiesOf oid pc = filter (functionsIn (PC.subtypes pc) (Game.delayedAbilitiesOf oid gs) Zone.Battlefield) (abilitiesOf pc)
      -- CR 603.10's first sentence, per EVENT GROUP: the permanents that existed
      -- immediately after the event, with the abilities and the CR 603.3a
      -- controller each of them had THEN. The three readings the live board gets
      -- wrong are the three this recovers, and each has a board that tells them
      -- apart: a permanent that entered later in the batch was not there to be
      -- checked against an earlier event, one whose abilities were stripped after
      -- the event still triggered on it (Pawl.TriggerSpec's "abilities as of the
      -- event"), and one whose layer-2 controller changed after it (CR 514.2's
      -- "until end of turn" ending between CR 514.1's discard and CR 514.3a's
      -- placement) triggered under the old one.
      --
      -- Out of the record and into the pair the rest of the scan's unions use.
      -- Only the STORED shape is named (#126); `candidates` below unions this
      -- with six sibling maps that are computed here and never serialized.
      --
      -- `battlefieldAt` is the sample, and its fallback to the LIVE board is the
      -- honest reading for an event this module never recorded: a fixture that
      -- appends to the log directly has no sampled board, and the game as it
      -- stands is the only one there is. Lazy, so a scan whose every group is
      -- sampled never projects it.
      onBattlefieldAt group =
        Map.mapWithKey
          (\oid candidate -> (BattlefieldCandidate.controller candidate, battlefieldAbilitiesOf oid (BattlefieldCandidate.characteristics candidate)))
          (battlefieldAt group gs)
      -- The permanent this event took OFF the battlefield, read from
      -- CR 608.2h last known information -- both the abilities and the objects'
      -- appearance immediately prior to the event, which is what CR 603.10 says
      -- looking back means. Both live in the single `lastKnown` record, written
      -- from the pre-move state by the zone change that deleted the id, so the
      -- ability is read as it existed on the battlefield and CR 603.3a's
      -- controller is who controlled the permanent as it left.
      --
      -- Possible only because Moved names BOTH ids: `object` is the CR 400.7
      -- incarnation in the destination zone, which `lastKnown` knows nothing
      -- about, while `departed` is the key it files under.
      --
      -- Keyed by that departing id, which by construction no longer exists, so
      -- this source collides with no other -- one entry per id means one pass of
      -- `forOne` without leaning on Map.unions' bias.
      --
      -- EVERY battlefield departure contributes, not only the deaths: which
      -- destinations a condition accepts is the CONDITION's business, and keeping
      -- that out of the candidate source is what let CR 603.6c's wider "leaves the
      -- battlefield" arrive as a matcher arm alone.
      --
      -- The `to /= Battlefield` guard is CR 603.6c's own word "another": the
      -- pseudo-move recordMintedEntry emits for a permanent minted straight onto
      -- the battlefield is not a departure.
      --
      -- The departing id is what the placed trigger carries as its SOURCE (CR
      -- 113.7a). CR 603.6c's arriving incarnation is a SECOND slot rather than a
      -- different value in this one -- eventBindings binds it under `became`.
      --
      -- Empty for a permanent that ceased without a zone change running over it,
      -- which files no last known information. That hole is the two group-scoped
      -- unions' too.
      --
      -- Every caller below wants the SAME set out of one recovery now: the
      -- abilities CR 113.6m leaves functioning on the battlefield, whether the
      -- permanent recovered stood there through the event (`laterGroups`,
      -- `sameGroup`) or was taken off BY it (`leftBattlefield`, CR 603.10a's
      -- look-back). It takes the departing id as well as the characteristics: CR
      -- 113.6m's final sentence reads the card's CR 603.7 declarations, which are
      -- not characteristics and so are reached through Game.delayedAbilitiesOf
      -- off the id.
      leftBattlefield event = case event of
        GameEvent.Moved (Moved.MkMoved zc _ _)
          | ZoneChange.from zc == Zone.Battlefield && ZoneChange.to zc /= Zone.Battlefield ->
              case Map.lookup (ZoneChange.departed zc) (GameState.lastKnown gs) of
                Nothing -> Map.empty
                Just lk ->
                  Map.singleton
                    (ZoneChange.departed zc)
                    (LastKnown.controller lk, battlefieldAbilitiesOf (ZoneChange.departed zc) (LastKnown.characteristics lk))
        -- CR 800.4a's removal, recovered the same way and from the same record:
        -- the permanent is not on the battlefield at the CR 117.5 boundary
        -- because it is not in the game at all, so last known information is the
        -- only reading of it there is. The id is its own -- nothing was minted
        -- for it to become -- and it too no longer exists, so this collides with
        -- no other source.
        GameEvent.LeftTheGame oid -> case Map.lookup oid (GameState.lastKnown gs) of
          Nothing -> Map.empty
          Just lk -> Map.singleton oid (LastKnown.controller lk, battlefieldAbilitiesOf oid (LastKnown.characteristics lk))
        GameEvent.Milled {} -> Map.empty
        GameEvent.Scried _ -> Map.empty
        GameEvent.DungeonCompleted _ -> Map.empty
        GameEvent.Surveiled _ -> Map.empty
        GameEvent.DiceRolled _ -> Map.empty
        GameEvent.ClassLevelSet _ -> Map.empty
        GameEvent.Plotted _ -> Map.empty
        GameEvent.Explored _ -> Map.empty
        GameEvent.Exerted _ -> Map.empty
        GameEvent.BecameAttacked _ -> Map.empty
        GameEvent.AttackersDeclared _ -> Map.empty
        GameEvent.BecameTapped _ -> Map.empty
        GameEvent.BecameUntapped _ -> Map.empty
        GameEvent.TappedForMana _ -> Map.empty
        GameEvent.CoinFlipped {} -> Map.empty
        GameEvent.RingTempted _ -> Map.empty
        GameEvent.Blighted _ -> Map.empty
        GameEvent.CardArrived _ -> Map.empty
        GameEvent.Moved {} -> Map.empty
        GameEvent.DamageDealt _ -> Map.empty
        GameEvent.DamagePrevented {} -> Map.empty
        GameEvent.StepBegan {} -> Map.empty
        GameEvent.SpellCast {} -> Map.empty
        GameEvent.BecameMonarch _ -> Map.empty
        GameEvent.TookInitiative _ -> Map.empty
        GameEvent.Discarded {} -> Map.empty
        GameEvent.Drew {} -> Map.empty
        GameEvent.Revealed {} -> Map.empty
        GameEvent.AttackerDeclared {} -> Map.empty
        GameEvent.BecameBlocking {} -> Map.empty
        GameEvent.BlocksDeclared {} -> Map.empty
        GameEvent.AttackerBlocked {} -> Map.empty
        GameEvent.AttackerUnblocked _ -> Map.empty
        GameEvent.SpellCountered _ -> Map.empty
        GameEvent.AbilityCountered _ -> Map.empty
        GameEvent.HalfUnlocked {} -> Map.empty
        GameEvent.TurnedFaceUp _ -> Map.empty
        GameEvent.Transformed {} -> Map.empty
        GameEvent.BecameDesignated {} -> Map.empty
        GameEvent.Evolved _ -> Map.empty
        GameEvent.Mentored {} -> Map.empty
        GameEvent.Trained _ -> Map.empty
        GameEvent.PermanentSacrificed {} -> Map.empty
        GameEvent.AbilityTriggered {} -> Map.empty
        GameEvent.LoyaltyAbilityActivated _ -> Map.empty
        GameEvent.LifeLost {} -> Map.empty
        GameEvent.LifeGained {} -> Map.empty
        GameEvent.CountersPut {} -> Map.empty
        GameEvent.CountersRemoved {} -> Map.empty
        GameEvent.ControlChanged {} -> Map.empty
        GameEvent.VentureMarkerEntered {} -> Map.empty
        GameEvent.BecameTarget {} -> Map.empty
        GameEvent.BecameAttached {} -> Map.empty
        GameEvent.BecameUnattached {} -> Map.empty
      -- CR 603.10a's look-back at the permanent this event removed: the
      -- abilities it had that functioned on the battlefield, `battlefieldAbilitiesOf`
      -- above's filter and `conditionPutsSelfInto`'s exception both applying.
      -- CR 400.7f's own datum, and part of the one thing `eventBindings` is told
      -- that it could not read off the event it was handed: for each permanent
      -- this batch put from the battlefield into a graveyard, the id it BECAME
      -- there. `departed` is the key because that is the id a borne trigger
      -- carries as its source (CR 113.7a), so `pend` below can ask it about the
      -- bearer -- and because CR 603.10a's look-back events name the pre-move id
      -- too, which is why the WHOLE table goes to eventBindings beside the
      -- bearer's own row: a sacrifice is recorded before its move, so nothing on
      -- GameEvent.PermanentSacrificed names CR 400.7e's new object.
      --
      -- Battlefield to GRAVEYARD alone, that being the only destination the rule
      -- names ("in its owner's graveyard"); every other departure contributes
      -- nothing, and a bearer with no entry gets Nothing.
      --
      -- The whole scanned batch rather than one group, because the rule's second
      -- sentence is the CR 704.5m burial, which happens at a strictly later SBA
      -- pass than the host's death. No wider than the rule even so: a permanent
      -- whose graveyard arrival was EARLIER than the event was not on the
      -- battlefield when it happened, so none of the candidate sources below
      -- offers its abilities and no trigger of its is ever gathered to ask about.
      becameInGraveyard =
        Map.fromList
          ( Maybe.mapMaybe
              ( ( \event -> case event of
                    GameEvent.Moved (Moved.MkMoved zc _ _)
                      | ZoneChange.from zc == Zone.Battlefield && ZoneChange.to zc == Zone.Graveyard ->
                          Just (ZoneChange.departed zc, ZoneChange.object zc)
                    _ -> Nothing
                )
                  . LoggedEvent.event
              )
              events
          )
      -- The batch cut into its CR 704.3 / CR 608.2f events, by the one reading of
      -- simultaneity delayedPending shares (`eventGroups` above).
      groups = eventGroups events
      -- `leftBattlefield` over one event group, computed ONCE per group and
      -- shared by `laterGroups` and `sameGroup` below.
      perGroup = fmap (Map.unions . fmap (leftBattlefield . LoggedEvent.event)) groups
      -- CR 603.10's first sentence, per EVENT GROUP: the permanents still on the
      -- battlefield when each event happened that have left by the CR 117.5
      -- boundary. Entry i is the union of `leftBattlefield` over the events at a
      -- strictly LATER group -- so neither an event's own departure nor a
      -- SIMULTANEOUS one is in its entry. That is not an optimisation: a
      -- permanent removed by this same event does not exist immediately after it,
      -- and reaches it only through the look-back below.
      --
      -- Strictly later GROUP, never strictly later index: two events of one group
      -- happened at the same time, so neither is "after" the other, and ordering
      -- them by the order the implementation happened to record them in is the
      -- order-dependence this whole binding exists to remove (#615).
      --
      -- A right scan over the groups rather than a lookup table: scanr shares each
      -- suffix's union with the one before, so the batch costs one pass, and a
      -- whole-board combat death batch that collapses into ONE group now costs one
      -- union rather than one per death. `drop 1` is the alignment, shifting
      -- scanr's "from i onward" to "from i+1 onward".
      --
      -- The controller and abilities here are the ones the permanent had as it
      -- LEFT, one moment after the event that triggered them rather than at it --
      -- so this is the SECOND reading of such a permanent, and loses to the first.
      -- `onBattlefieldAt` above already holds it, sampled at the event itself,
      -- because a permanent that departs at a later group was still standing when
      -- this group's sample was taken; Map.unions is left-biased and that entry
      -- comes first. What is left for this binding is the group the sample does not
      -- name, where last known information is the only reading there is.
      --
      -- CR 113.6m applies here the same way it applies to `leftBattlefield` now:
      -- this permanent WAS on the battlefield when the event happened, so one of
      -- its abilities that functions only in a graveyard was no more watching
      -- then than it is now. The behaviour's proving case is Squee, Goblin Nabob
      -- leaving the battlefield after an upkeep began in the same batch, in
      -- Pawl.TriggerSpec's `bystanderZoneSpec` -- which reaches the same answer
      -- through the sample above, that being the reading that wins. The filter is
      -- kept identical here so the two cannot disagree on the fallback path.
      laterGroups = drop 1 (List.scanr Map.union Map.empty perGroup)
      -- The ids a graveyard arrival in this block minted, keyed by the ARRIVING
      -- incarnation -- ZoneChange.object, the key `inGraveyards` would hold them
      -- under, `arrivedInGraveyard` below arguing that the two ids coincide.
      arrivalsIn block = Set.fromList (Maybe.mapMaybe (arrivedInGraveyardAt . LoggedEvent.event) (Foldable.toList block))
      arrivedInGraveyardAt event = case movedOf event of
        Just zc | ZoneChange.to zc == Zone.Graveyard -> Just (ZoneChange.object zc)
        _ -> Nothing
      -- CR 603.10's first sentence on the ARRIVAL side, and `laterGroups`' mirror:
      -- a card that reached a graveyard at a STRICTLY LATER group did not exist
      -- immediately after this group's events, so it is no witness to them and is
      -- subtracted from `inGraveyards` below. Strictly later for `laterGroups`'
      -- reason -- two events of one group happened at the same time, so a card
      -- buried by an event of the SAME group did exist immediately after it, and
      -- keeping the two boundaries aligned is what stops the arrival and departure
      -- narrowings from disagreeing about simultaneity.
      --
      -- Subtracting from the live read rather than reconstructing each group's
      -- graveyard: a card that was in a graveyard BEFORE the batch has no arrival
      -- event in the log this scan reads, so it is in no entry here and survives
      -- every subtraction, which is the answer the rule wants. A Set and a right
      -- scan for `laterGroups`' reasons, `drop 1` being the same alignment.
      --
      -- `arrivedInGraveyard` is NOT narrowed by this: it is already per event and
      -- already scoped by the arrival it answers for, so subtracting the arrival
      -- from its own event would delete the case that source exists for.
      arrivedLater = drop 1 (List.scanr (Set.union . arrivalsIn) Set.empty groups)
      -- CR 603.10a, the other half of that rule: for a LOOK-BACK condition the
      -- board that matters is "the appearance of objects immediately prior to the
      -- event", on which every permanent this same event removed was still
      -- standing. So a bearer that departed in the event's OWN group is offered
      -- alongside the strictly-later ones.
      --
      -- CR 603.10a's own Example is this and nothing else: an artifact watching
      -- creatures die "triggers twice, even though the artifact goes to its
      -- owner's graveyard at the same time as the creatures". Meren of Clan Nel
      -- Toth dying to Day of Judgment beside a Goblin Piker is that Example with
      -- an experience counter in place of the life, and Pawl.TriggerSpec proves
      -- it for BOTH object-id orders.
      --
      -- Narrowed to the conditions that rule lists, by `looksBack` below, and
      -- narrowed HERE rather than at the match: admitting the bearer for a
      -- CR 603.10 first-sentence condition would say a permanent existed
      -- immediately after the event that removed it. The other direction is the
      -- one that would answer the sequential case wrong -- a Meren destroyed by
      -- one part of a resolution must not see creatures buried by a LATER part,
      -- which is a different group and so absent from this entry.
      --
      -- Not deduplicated against the entry above: an id departs at exactly one
      -- group, so the two maps are disjoint, and `leftBattlefield`'s own offer of
      -- the event's own departure wins over this one by Map.unions' left bias.
      sameGroup = fmap (Map.map (fmap (filter (looksBack . TriggeredAbility.condition)))) perGroup
      -- CR 702.29c: the card that was just cycled, wherever it landed. The
      -- candidate source that is neither on the battlefield nor a permanent that
      -- left it -- which is exactly what that rule asks for:
      -- "these abilities trigger from whatever zone the card winds up in after
      -- it's cycled", the graveyard for every printing today.
      --
      -- Abilities come from the PRINTED card rather than a projection, no pool
      -- effect changing the TRIGGERED abilities of a card in a graveyard (#1859) --
      -- Teferi, Mage of Zhalfir's grant off the battlefield mints none. Rule 702's
      -- minted abilities are not consulted either -- none functions from a
      -- graveyard.
      --
      -- The controller is the OWNER, CR 113.8's second clause: a card in a
      -- graveyard has no controller (CR 108.4).
      --
      -- Scoped to the CYCLING cause, not every discard, rule 702.29c speaking
      -- about cycling specifically. An ordinary discard's card reaches the
      -- graveyard too and is offered by `inGraveyards` under CR 113.6k.
      cycledCard event = case event of
        GameEvent.Discarded (Discarded.MkDiscarded _ oid DiscardCause.ToPayCyclingCost) -> case Game.lookupObject oid gs of
          Nothing -> Map.empty
          Just obj -> case Game.faceOf oid gs of
            Nothing -> Map.empty
            Just face -> Map.singleton oid (Object.owner obj, Face.triggeredAbilities face)
        GameEvent.Discarded (Discarded.MkDiscarded _ _ DiscardCause.Ordinary) -> Map.empty
        -- A draw names no object either. The card it puts in a hand may well bear
        -- an ability that triggers from there -- CR 702.94a's miracle -- but that
        -- one fires on the REVEAL rather than on the draw, and `revealedInHand`
        -- below is its source.
        GameEvent.Drew {} -> Map.empty
        GameEvent.Moved {} -> Map.empty
        GameEvent.DamageDealt _ -> Map.empty
        GameEvent.DamagePrevented {} -> Map.empty
        GameEvent.StepBegan {} -> Map.empty
        GameEvent.SpellCast {} -> Map.empty
        GameEvent.BecameMonarch _ -> Map.empty
        GameEvent.TookInitiative _ -> Map.empty
        -- A reveal is not a cycling, whatever it showed. `revealedInHand` below is
        -- what hangs an ability on the card a reveal names.
        GameEvent.Revealed {} -> Map.empty
        GameEvent.AttackerDeclared {} -> Map.empty
        GameEvent.BecameBlocking {} -> Map.empty
        GameEvent.BlocksDeclared {} -> Map.empty
        GameEvent.AttackerBlocked {} -> Map.empty
        GameEvent.AttackerUnblocked _ -> Map.empty
        GameEvent.SpellCountered _ -> Map.empty
        GameEvent.AbilityCountered _ -> Map.empty
        GameEvent.HalfUnlocked {} -> Map.empty
        GameEvent.TurnedFaceUp _ -> Map.empty
        GameEvent.Transformed {} -> Map.empty
        GameEvent.BecameDesignated {} -> Map.empty
        GameEvent.Evolved _ -> Map.empty
        GameEvent.Mentored {} -> Map.empty
        GameEvent.Trained _ -> Map.empty
        GameEvent.PermanentSacrificed {} -> Map.empty
        GameEvent.AbilityTriggered {} -> Map.empty
        GameEvent.LoyaltyAbilityActivated _ -> Map.empty
        GameEvent.LifeLost {} -> Map.empty
        GameEvent.LifeGained {} -> Map.empty
        GameEvent.CountersPut {} -> Map.empty
        GameEvent.CountersRemoved {} -> Map.empty
        GameEvent.ControlChanged {} -> Map.empty
        GameEvent.VentureMarkerEntered {} -> Map.empty
        GameEvent.BecameTarget {} -> Map.empty
        GameEvent.BecameAttached {} -> Map.empty
        GameEvent.BecameUnattached {} -> Map.empty
        GameEvent.LeftTheGame _ -> Map.empty
        GameEvent.Milled {} -> Map.empty
        GameEvent.Scried _ -> Map.empty
        GameEvent.DungeonCompleted _ -> Map.empty
        GameEvent.Surveiled _ -> Map.empty
        GameEvent.DiceRolled _ -> Map.empty
        GameEvent.ClassLevelSet _ -> Map.empty
        GameEvent.Plotted _ -> Map.empty
        GameEvent.Explored _ -> Map.empty
        GameEvent.Exerted _ -> Map.empty
        GameEvent.BecameAttacked _ -> Map.empty
        GameEvent.AttackersDeclared _ -> Map.empty
        GameEvent.BecameTapped _ -> Map.empty
        GameEvent.BecameUntapped _ -> Map.empty
        GameEvent.TappedForMana _ -> Map.empty
        GameEvent.CoinFlipped {} -> Map.empty
        GameEvent.RingTempted _ -> Map.empty
        GameEvent.Blighted _ -> Map.empty
        GameEvent.CardArrived _ -> Map.empty
      -- CR 113.6k and CR 113.6m: every card in every graveyard carrying at least
      -- one ability those rules put there. The one source that widens the SCANNED
      -- ZONE rather than recovering an object an event names, which is why the
      -- walk itself happens once outside the event loop; what an individual event
      -- may see of the answer is `arrivedLater`'s subtraction below.
      --
      -- Narrow by construction, which keeps a large graveyard cheap: membership is
      -- decided by `functionsIn` -- a total case over a closed condition type and a
      -- walk of the ability's own effects, no projection and no board walk. Cards
      -- contributing nothing are dropped rather than carried as empty entries.
      --
      -- Abilities come from the PRINTED card and the controller is the OWNER, for
      -- `cycledCard`'s reasons.
      --
      -- CR 603.10a does not apply to what this serves -- a card ENTERING a
      -- graveyard is on none of its look-back list -- so CR 603.10's normal first
      -- sentence governs: an event is checked against the objects that existed
      -- immediately after IT, not against the board at the end of the batch. This
      -- read is the whole graveyard as it stands, so `arrivedLater` below narrows
      -- it per event group. The card that arrived in a graveyard and is gone again
      -- by the boundary is the one this read cannot reach; `arrivedInGraveyard`
      -- below is its source.
      --
      -- Its `_ -> Nothing` arm is what keeps the two disjoint: `Game.lookupObject`
      -- fails for an id that has ceased, and a ceased id is exactly the one the
      -- other source answers for.
      graveyardCandidate oid = case (Game.lookupObject oid gs, Game.faceOf oid gs) of
        (Just obj, Just face) ->
          case filter (functionsIn (TypeLine.subtypes (Face.typeLine face)) (Face.delayedAbilities face) Zone.Graveyard) (Face.triggeredAbilities face) of
            [] -> Nothing
            abilities -> Just (oid, (Object.owner obj, abilities))
        _ -> Nothing
      inGraveyards =
        Map.fromList
          (concatMap (Maybe.mapMaybe graveyardCandidate . Foldable.toList) (Map.elems (GameState.graveyard gs)))
      -- CR 603.10's first sentence again, for the card THIS event put into a
      -- graveyard that is gone by the CR 117.5 boundary: it existed in the
      -- graveyard immediately after the event, so its ability is checked, and
      -- CR 608.2h last known information is the only reading of it left. The
      -- graveyard twin of `leftBattlefield`, and per EVENT for the same reason --
      -- the arrival is what scopes it. Corpse Churn milling Narcomoeba and
      -- returning it in one resolution is the proving board, in
      -- Pawl.TriggerSpec's `graveyardTriggerSpec`.
      --
      -- Keyed by `ZoneChange.object`, where `leftBattlefield` keys by
      -- `ZoneChange.departed` -- the one place the graveyard source inverts the
      -- battlefield one. The bearer here is the CR 400.7 incarnation that ARRIVED
      -- in the graveyard, which is the id `matchesTriggerGiven` compares against
      -- for SelfPutIntoGraveyardFromLibrary and the id `inGraveyards` would have
      -- offered. It is also the id `lastKnown` files the later departure under,
      -- the graveyard card being what left; the two ids coincide by construction,
      -- and that is the hinge of this source.
      --
      -- ONLY the destination is gated. Which origins a condition accepts is the
      -- CONDITION's business -- SelfPutIntoGraveyardFromAnywhere is served from
      -- the same zone -- which is the posture `leftBattlefield`'s comment argues for.
      -- That gate is a regression fence rather than a proved behaviour: widening it
      -- to every non-battlefield destination leaves the suite green, since the key
      -- below is the ARRIVING id and only a graveyard arrival that has itself since
      -- departed has a `lastKnown` entry under it.
      --
      -- Abilities from the last known projection rather than a printed face: a
      -- ceased id has no face to look up, and `LastKnown` carries none. Identical
      -- to `inGraveyards`' printed read today, no pool effect changing the
      -- TRIGGERED abilities of a card in a graveyard (gap #1859). Not `abilitiesOf` either --
      -- nothing rule 702 or rule 310 mints functions from a graveyard, and
      -- `inGraveyards` does not consult them, so the two graveyard sources read
      -- alike.
      --
      -- The controller is `LastKnown.controller`, which for a graveyard card is
      -- the OWNER and so agrees with `inGraveyards` -- CR 113.8's second clause,
      -- CR 108.4 giving a card in a graveyard no controller. Not asserted by the
      -- read but true of the write: `changeZoneAttaching` computes it as
      -- Projection.controllerOf, which bottoms out at Object.enteredUnder and then
      -- the owner, and only a CR 613.1b layer-2 effect could move it -- those
      -- reach permanents (CR 110.2), never a card in a graveyard.
      --
      -- `functionsIn Zone.Graveyard` is CR 113.6k's own gate, as it is for
      -- `inGraveyards` and not for `leftBattlefield`: without it a departed Doomed
      -- Traveler would be offered its dies trigger from a graveyard, and a
      -- "whenever another creature dies" watcher would be offered ITS trigger for
      -- the very move that buried it -- CR 400.7 having minted a fresh id, the
      -- "another" test compares two different ids and passes. Proved by
      -- Pawl.ZoneTriggerSpec's "CR 113.6k a battlefield-only trigger on a card
      -- that arrived in a graveyard and left it is not offered": Come Back Wrong
      -- destroys a Meren of Clan Nel Toth and returns her in one resolution, and
      -- removing this filter hands her controller an experience counter for her
      -- own death.
      --
      -- Disjoint from `inGraveyards` by construction, not by Map.unions' bias: an
      -- id in `lastKnown` is one the same write deleted from GameState.objects,
      -- and CR 400.7 mints a fresh id per move, so `graveyardCandidate` drops it.
      -- A card that was in a graveyard before this batch is unreachable here too
      -- -- its arrival event is not in the log this scan reads.
      arrivedInGraveyard event = case movedOf event of
        Just zc
          | ZoneChange.to zc == Zone.Graveyard ->
              case Map.lookup (ZoneChange.object zc) (GameState.lastKnown gs) of
                Nothing -> Map.empty
                Just lk ->
                  case filter (functionsIn (PC.subtypes (LastKnown.characteristics lk)) (Game.delayedAbilitiesOf (ZoneChange.object zc) gs) Zone.Graveyard) (PC.triggeredAbilities (LastKnown.characteristics lk)) of
                    [] -> Map.empty
                    abilities -> Map.singleton (ZoneChange.object zc) (LastKnown.controller lk, abilities)
        _ -> Map.empty
      -- CR 113.6k's third zone, `inGraveyards` one zone over: every card in exile
      -- carrying an ability that functions from there. Rule 702.55c is the
      -- sentence it exists for -- "triggered abilities of cards with haunt that
      -- refer to the haunted creature can trigger in the exile zone" -- and CR
      -- 113.6k's own example is the card that bears one.
      --
      -- FILTERED BY `functionsIn`, unlike the command zone's source: there the rule
      -- at issue is CR 113.6p, which is about the OBJECT, and every emblem or
      -- vanguard ability would fail a condition test; here the rule at issue IS CR
      -- 113.6k, so the filter is the gate itself. Without it an exiled Doomed
      -- Traveler would be offered its dies trigger, and an exiled Desolation Twin
      -- its cast trigger, from a zone CR 113.6 says neither functions in.
      --
      -- ONE STANDING SCAN over the whole zone, computed outside the event loop for
      -- `inGraveyards`' reason: a haunting card sits in exile indefinitely and no
      -- event names it, so nothing narrower could find it.
      --
      -- Abilities come from the PRINTED card and the controller is the OWNER (CR
      -- 108.4a), also for `inGraveyards`' reasons: CR 108.4 gives a card in exile
      -- no controller at all, so Blind Hunter's "you gain 2 life" pays the player
      -- who owns the haunting card.
      -- Not implemented: a card exiled FACE DOWN is scanned here like any other,
      -- so its printed abilities are offered where CR 406.3a leaves it none
      -- (#1479).
      exileCandidate oid = case (Game.lookupObject oid gs, Game.faceOf oid gs) of
        (Just obj, Just face) ->
          case filter (functionsIn (TypeLine.subtypes (Face.typeLine face)) (Face.delayedAbilities face) Zone.Exile) (Face.triggeredAbilities face) of
            [] -> Nothing
            abilities -> Just (oid, (Object.owner obj, abilities))
        _ -> Nothing
      inExile = Map.fromList (Maybe.mapMaybe exileCandidate (Set.toAscList (GameState.exile gs)))
      -- CR 113.6k's other zone: the spell that just became cast, offered from the
      -- STACK, where CR 601.2a leaves it. Desolation Twin's "when you cast this
      -- spell" is borne by an object that is on nobody's battlefield and in
      -- nobody's graveyard, so no source above can reach it.
      --
      -- Scoped to the CAST EVENT rather than computed once over GameState.stack,
      -- which is `cycledCard`'s shape. Not for want of a controller: CR 405.4's
      -- caster is stamped into Object.enteredUnder by changeZoneCasting, and
      -- Projection.defaultControllerOf reads it back, so a standing scan would
      -- find the caster rather than falling back to the owner. The narrower scope
      -- is what the WORK asks for -- SelfCast is the only condition
      -- zonesTriggeredFrom puts on the stack and it matches no other event, so a
      -- standing scan of every spell would answer alike at more cost. A future
      -- condition that functions on the stack and watches some other event is
      -- what would widen this.
      --
      -- Abilities come from the PRINTED card, for `cycledCard`'s reason (#1859).
      spellCast event = case event of
        GameEvent.SpellCast (SpellWasCast.MkSpellWasCast caster spell _ _) -> case Game.faceOf spell gs of
          Nothing -> Map.empty
          Just face -> case filter (functionsIn (TypeLine.subtypes (Face.typeLine face)) (Face.delayedAbilities face) Zone.Stack) (Face.triggeredAbilities face) of
            [] -> Map.empty
            abilities -> Map.singleton spell (caster, abilities)
        GameEvent.Discarded {} -> Map.empty
        GameEvent.Drew {} -> Map.empty
        GameEvent.Moved {} -> Map.empty
        GameEvent.DamageDealt _ -> Map.empty
        GameEvent.DamagePrevented {} -> Map.empty
        GameEvent.StepBegan {} -> Map.empty
        GameEvent.BecameMonarch _ -> Map.empty
        GameEvent.TookInitiative _ -> Map.empty
        GameEvent.Revealed {} -> Map.empty
        GameEvent.AttackerDeclared {} -> Map.empty
        GameEvent.BecameBlocking {} -> Map.empty
        GameEvent.BlocksDeclared {} -> Map.empty
        GameEvent.AttackerBlocked {} -> Map.empty
        GameEvent.AttackerUnblocked _ -> Map.empty
        GameEvent.SpellCountered _ -> Map.empty
        GameEvent.AbilityCountered _ -> Map.empty
        GameEvent.HalfUnlocked {} -> Map.empty
        GameEvent.TurnedFaceUp _ -> Map.empty
        GameEvent.Transformed {} -> Map.empty
        GameEvent.BecameDesignated {} -> Map.empty
        GameEvent.Evolved _ -> Map.empty
        GameEvent.Mentored {} -> Map.empty
        GameEvent.Trained _ -> Map.empty
        GameEvent.PermanentSacrificed {} -> Map.empty
        GameEvent.AbilityTriggered {} -> Map.empty
        GameEvent.LoyaltyAbilityActivated _ -> Map.empty
        GameEvent.LifeLost {} -> Map.empty
        GameEvent.LifeGained {} -> Map.empty
        GameEvent.CountersPut {} -> Map.empty
        GameEvent.CountersRemoved {} -> Map.empty
        GameEvent.ControlChanged {} -> Map.empty
        GameEvent.VentureMarkerEntered {} -> Map.empty
        GameEvent.BecameTarget {} -> Map.empty
        GameEvent.BecameAttached {} -> Map.empty
        GameEvent.BecameUnattached {} -> Map.empty
        GameEvent.LeftTheGame _ -> Map.empty
        GameEvent.Milled {} -> Map.empty
        GameEvent.Scried _ -> Map.empty
        GameEvent.DungeonCompleted _ -> Map.empty
        GameEvent.Surveiled _ -> Map.empty
        GameEvent.DiceRolled _ -> Map.empty
        GameEvent.ClassLevelSet _ -> Map.empty
        GameEvent.Plotted _ -> Map.empty
        GameEvent.Explored _ -> Map.empty
        GameEvent.Exerted _ -> Map.empty
        GameEvent.BecameAttacked _ -> Map.empty
        GameEvent.AttackersDeclared _ -> Map.empty
        GameEvent.BecameTapped _ -> Map.empty
        GameEvent.BecameUntapped _ -> Map.empty
        GameEvent.TappedForMana _ -> Map.empty
        GameEvent.CoinFlipped {} -> Map.empty
        GameEvent.RingTempted _ -> Map.empty
        GameEvent.Blighted _ -> Map.empty
        GameEvent.CardArrived _ -> Map.empty
      -- CR 114.4 / CR 113.6p: "abilities of emblems function in the command zone".
      -- The third source that widens the SCANNED ZONE rather than recovering an
      -- object an event names, so it is computed once outside the event loop as
      -- `onBattlefield` and `inGraveyards` are.
      --
      -- NOT filtered through `functionsIn`, where `inGraveyards` is. CR 113.6k
      -- decides one CONDITION at a time and answers for the ability's usual zone;
      -- CR 114.4 is about the OBJECT, and says every ability of this one functions
      -- here. Filtering would drop them all: an emblem's "at the beginning of your
      -- upkeep" is a condition that triggers perfectly well from the battlefield,
      -- so CR 113.6's default sends it there and the emblem would never be asked.
      --
      -- EMBLEMS AND VANGUARDS, where a graveyard walk takes the whole zone, for
      -- the reason Pawl.Engine.CombatRestriction.inForce narrows the same walk:
      -- the command zone also holds a commander and a dungeon card, whose
      -- abilities CR 113.6 leaves functioning on the battlefield and CR 309.4c
      -- mints rather than prints. Vanguard.functionsFromCommandZone is that list
      -- (CR 113.6p), read the same way by all four walks over this zone, and
      -- reading the rulebook's own list is not casing on an effect's identity. CR
      -- 313.4 / CR 902.7 put the vanguard card on it in the same words -- "its
      -- triggered abilities may trigger" -- so it takes the same unfiltered walk
      -- and for the same reason.
      --
      -- The controller is the OWNER: CR 114.2 makes an emblem both owned and
      -- controlled by the player it was created for, and createEmblem leaves
      -- Object.enteredUnder empty so Projection.defaultControllerOf answers the
      -- same. CR 603.3a's control sample is not consulted, and cannot disagree:
      -- CR 110.2 gives a PERMANENT a controller, and CR 114.5 says an emblem is
      -- not one, so no layer-2 effect (CR 613.1b) has an emblem to move.
      --
      -- Abilities come from the emblem's own card, which is the whole of it (CR
      -- 114.3) -- no projection is involved, which is the posture
      -- Projection.gatherGiven's emblem walk takes for its static half: an emblem
      -- is not a creature, so the pool's CR 613.1f removers never reach it.
      -- CR 902.6 gives the vanguard card the same owner-is-controller answer CR
      -- 114.2 gives an emblem, so the two share this line as well: rule 902.6's
      -- "the controller of a face-up vanguard card is its owner".
      commandCandidate oid = case Game.lookupObject oid gs of
        Nothing -> Nothing
        Just obj ->
          if not (Vanguard.functionsFromCommandZone oid gs)
            then Nothing
            else case Game.faceOf oid gs of
              Nothing -> Nothing
              Just face -> case Face.triggeredAbilities face of
                [] -> Nothing
                abilities -> Just (oid, (Object.owner obj, abilities))
      inCommand =
        Map.fromList
          (Maybe.mapMaybe commandCandidate (Set.toAscList (GameState.command gs)))
      -- CR 113.6k's last zone: the card a player just revealed from their HAND as
      -- they drew it (CR 702.94a, CR 121.9). The ability is borne by an object that
      -- is on nobody's battlefield, in nobody's graveyard and on no stack -- rule
      -- 701.20b moved it nowhere -- so no source above can reach it.
      --
      -- Scoped to the reveal EVENT rather than computed once over every hand,
      -- which is `cycledCard`'s and `spellCast`'s shape rather than `inExile`'s.
      -- The reason is the rule: rule 702.94a's trigger fires on a reveal and
      -- nothing else, and no ability in the pool sits in a hand watching for
      -- something no event names -- where a haunting card sits in exile
      -- indefinitely and only a standing scan could find it. A standing walk of
      -- every hand would answer alike at the cost of a scan per event.
      --
      -- FILTERED BY `functionsIn`, like `inExile` and unlike the command zone's
      -- emblem source: the rule at issue here IS CR 113.6k. Without it a drawn
      -- Doomed Traveler would be offered its dies trigger from a hand.
      --
      -- The abilities are the PRINTED ones plus the ones rule 702 MINTS from the
      -- card's printed keywords -- and miracle's is entirely the latter, so
      -- dropping the mint would leave this source with nothing to find. Printed
      -- keywords rather than a projection's, for `cycledCard`'s reason (#1859).
      --
      -- The controller is the OWNER, CR 113.8's second clause, for `inGraveyards`'
      -- reason: CR 108.4 gives a card in a hand no controller. Rule 702.94a's
      -- reveal is one a player makes from their own hand, so the owner is also the
      -- revealer, and CR 109.5's "you" lands on the same seat either way.
      revealedInHand event = case event of
        GameEvent.Revealed (Revealed.MkRevealed _ oid RevealCause.ForMiracle _) -> case (Game.lookupObject oid gs, Game.faceOf oid gs) of
          (Just obj, Just face) ->
            case filter (functionsIn (TypeLine.subtypes (Face.typeLine face)) (Face.delayedAbilities face) Zone.Hand) (Face.triggeredAbilities face <> Keyword.printedTriggeredAbilitiesOf (Face.keywords face)) of
              [] -> Map.empty
              abilities -> Map.singleton oid (Object.owner obj, abilities)
          _ -> Map.empty
        GameEvent.Revealed (Revealed.MkRevealed _ _ RevealCause.Ordinary _) -> Map.empty
        GameEvent.Discarded {} -> Map.empty
        GameEvent.Drew {} -> Map.empty
        GameEvent.Moved {} -> Map.empty
        GameEvent.DamageDealt _ -> Map.empty
        GameEvent.DamagePrevented {} -> Map.empty
        GameEvent.StepBegan {} -> Map.empty
        GameEvent.SpellCast {} -> Map.empty
        GameEvent.BecameMonarch _ -> Map.empty
        GameEvent.TookInitiative _ -> Map.empty
        GameEvent.AttackerDeclared {} -> Map.empty
        GameEvent.BecameBlocking {} -> Map.empty
        GameEvent.BlocksDeclared {} -> Map.empty
        GameEvent.AttackerBlocked {} -> Map.empty
        GameEvent.AttackerUnblocked _ -> Map.empty
        GameEvent.SpellCountered _ -> Map.empty
        GameEvent.AbilityCountered _ -> Map.empty
        GameEvent.HalfUnlocked {} -> Map.empty
        GameEvent.TurnedFaceUp _ -> Map.empty
        GameEvent.Transformed {} -> Map.empty
        GameEvent.BecameDesignated {} -> Map.empty
        GameEvent.Evolved _ -> Map.empty
        GameEvent.Mentored {} -> Map.empty
        GameEvent.Trained _ -> Map.empty
        GameEvent.PermanentSacrificed {} -> Map.empty
        GameEvent.AbilityTriggered {} -> Map.empty
        GameEvent.LoyaltyAbilityActivated _ -> Map.empty
        GameEvent.LifeLost {} -> Map.empty
        GameEvent.LifeGained {} -> Map.empty
        GameEvent.CountersPut {} -> Map.empty
        GameEvent.CountersRemoved {} -> Map.empty
        GameEvent.ControlChanged {} -> Map.empty
        GameEvent.VentureMarkerEntered {} -> Map.empty
        GameEvent.BecameTarget {} -> Map.empty
        GameEvent.BecameAttached {} -> Map.empty
        GameEvent.BecameUnattached {} -> Map.empty
        GameEvent.LeftTheGame _ -> Map.empty
        GameEvent.Milled {} -> Map.empty
        GameEvent.Scried _ -> Map.empty
        GameEvent.DungeonCompleted _ -> Map.empty
        GameEvent.Surveiled _ -> Map.empty
        GameEvent.DiceRolled _ -> Map.empty
        GameEvent.ClassLevelSet _ -> Map.empty
        GameEvent.Plotted _ -> Map.empty
        GameEvent.Explored _ -> Map.empty
        GameEvent.Exerted _ -> Map.empty
        GameEvent.BecameAttacked _ -> Map.empty
        GameEvent.AttackersDeclared _ -> Map.empty
        GameEvent.BecameTapped _ -> Map.empty
        GameEvent.BecameUntapped _ -> Map.empty
        GameEvent.TappedForMana _ -> Map.empty
        GameEvent.CoinFlipped {} -> Map.empty
        GameEvent.RingTempted _ -> Map.empty
        GameEvent.Blighted _ -> Map.empty
        GameEvent.CardArrived _ -> Map.empty
      forOne event (oid, (ctrl, abilities)) =
        let -- The bearer's own slot environment, so a condition naming a slot
            -- (TriggerCondition.LoseControlOfBound) is read the same way here as it
            -- is for a CR 603.7 delayed entry. Empty for a bearer that has since
            -- left, there being no object to ask -- whether it arrived here out of
            -- last known information or out of a sample taken while it stood.
            bindings = maybe Map.empty Object.bindings (Game.lookupObject oid gs)
            fires ab = matchesTriggerGiven bindings gs oid ctrl (TriggeredAbility.condition ab) event
            pend ab = PendingTrigger.MkPendingTrigger (TriggerSource.OfObject oid) ctrl ab (eventBindings gs (Map.lookup oid becameInGraveyard) becameInGraveyard ctrl (TriggeredAbility.condition ab) event) Nothing
            -- CR 603.2c's key, for `oncePerBatch` below: which ability of which
            -- bearer this pending trigger came from, or Nothing when the condition
            -- is per-occurrence and every member of the batch is its own trigger
            -- event. Keyed by the ability's POSITION in the bearer's list rather
            -- than by the ability itself, so a permanent printing the same batch
            -- condition twice keeps both -- no equality on TriggeredAbility is
            -- needed and none is assumed.
            key (index, ab) =
              if batchScoped (TriggeredAbility.condition ab)
                then Just (oid, index :: Natural)
                else Nothing
            keyed indexed = (key indexed, pend (snd indexed))
         in fmap keyed (filter (fires . snd) (zip [0 ..] abilities))
      -- Map.unions is left-biased, so the battlefield reading wins over a
      -- last-known one, a cycled card and a graveyard reading. That rules out a
      -- double fire: one entry per id means one pass of `forOne` per id.
      --
      -- Two of the first four genuinely overlap, and there the bias is load-bearing
      -- rather than belt and braces: a permanent that departs at a LATER group was
      -- still standing when this group's sample was taken, so it is in both
      -- `onBattlefield` and `later`, and the sample's at-the-event reading is the
      -- one CR 603.10 asks for. `leftBattlefield` and `same` cannot collide with
      -- the sample -- both name a permanent that had already gone when the sample
      -- was taken -- nor with each other, an id departing at exactly one group.
      -- `inGraveyards` genuinely overlaps `cycledCard` on purpose -- a card
      -- cycled into a graveyard is honestly a member of both -- and the winner
      -- offers that card's printed abilities unfiltered, a superset either way.
      -- `arrivedInGraveyard` overlaps nothing: it answers only for an id
      -- `lastKnown` holds, which is one no longer in GameState.objects and so in
      -- no player's graveyard and no player's hand, so its position beside
      -- `inGraveyards` is documentation rather than arbitration.
      -- `spellCast` overlaps nothing: CR 601.2a keeps its object on the stack,
      -- which no other source reads. Neither does `inCommand`: CR 114.1 puts an
      -- emblem into the command zone, and no rule or effect in pawl moves one
      -- anywhere else -- Event.createEmblem is its only writer. Nor does
      -- `inExile`: CR 400.1 makes exile a zone of its own, and an id in it is in
      -- no other. Nor does `revealedInHand`: CR 701.20b leaves the revealed card
      -- in the hand, which no other source reads.
      candidates onBattlefield event later same arrivedAfter = Map.toAscList (Map.unions [onBattlefield, leftBattlefield event, later, same, cycledCard event, spellCast event, revealedInHand event, Map.withoutKeys inGraveyards arrivedAfter, arrivedInGraveyard event, inCommand, inExile])
      scanOne onBattlefield later same arrivedAfter event = concatMap (forOne event) (candidates onBattlefield event later same arrivedAfter)
      -- CR 603.2c's FIRST sentence, applied to ONE event group: a batch-scoped
      -- condition's trigger event is the whole group, which occurs once however
      -- many of the group's members matched, where a per-occurrence condition
      -- triggers once per member (the rule's second sentence and its own Example,
      -- the sweeper that fires a "whenever A land is put into a graveyard" ability
      -- once per land).
      --
      -- The FIRST match wins rather than the last, which keeps the canonical order
      -- below intact: this drops later duplicates and reorders nothing, so a batch
      -- trigger sits exactly where its earliest matching event would have put it.
      -- Which member won is unobservable anyway -- eventBindingSlots gives a
      -- batch-scoped condition no slots, so every duplicate carries identical
      -- bindings.
      --
      -- Per GROUP and never per scan: several groups can share one CR 117.5 scan
      -- (GameState.scannedThrough is not bumped until the scan ends), and CR 704.3
      -- makes each state-based-action pass its own single event.
      -- Pawl.ZoneTriggerSpec's "CR 704.3 two death groups in one trigger scan are
      -- two trigger events" is what tells the two readings apart.
      oncePerBatch seen entries = case entries of
        [] -> []
        (k, trigger) : rest -> case k of
          Nothing -> trigger : oncePerBatch seen rest
          Just batch
            | Set.member batch seen -> oncePerBatch seen rest
            | otherwise -> trigger : oncePerBatch (Set.insert batch seen) rest
      -- The battlefield reading is per GROUP and so is hoisted out of the block:
      -- every event in one group happened at the same time, so they share it. A
      -- group with no events cannot occur, which `eventGroups` states in the type.
      scanBlock block later same arrivedAfter = oncePerBatch Set.empty (concatMap (scanOne (onBattlefieldAt (LoggedEvent.group (NonEmpty.head block))) later same arrivedAfter . LoggedEvent.event) block)
   in concat (List.zipWith4 scanBlock groups laterGroups sameGroup arrivedLater)

-- CR 113.6m, read off a TRIGGERED ability: "an ability whose cost or effect
-- specifies that it moves the object it's on out of a particular zone functions
-- only in that zone". The rule says "an ability" -- Pawl.Engine.Activate's
-- namesake is the same sentence read off an activated one, and this is the
-- triggered half.
--
-- No COST half. CR 602.1 gives an activated ability "a cost and an effect";
-- CR 603.1 gives a triggered one "a trigger condition and an effect", and no
-- cost at all -- so Pawl.Engine.Cost, the other half of
-- Activate.zoneFunctionedFrom, has nothing to be asked here. The CONDITION is
-- read instead, and only for the rule's own exception: `enchantedObjectLeaves`
-- below.
--
-- ALL MODES, in printed order, for Activate.zoneFunctionedFrom's reason: CR
-- 700.2 makes a modal ability's modes alternatives, so a zone stated by any of
-- them is a zone the ability can move its object out of.
--
-- Not a case on an effect's identity: Pawl.Engine.EffectZone answers the one
-- question, and this folds its answer. The condition case below is not one
-- either -- TriggerCondition is a closed-half type like Phase or Keyword, which
-- design.md section 1 puts on the rulebook's side of the line.
--
-- CR 113.6m's "unless" clause is read here in the one half a trigger condition
-- can satisfy -- the Aura half, `enchantedObjectLeaves` below, gated on the
-- bearer being an Aura. Without it an Aura whose payload names its own source
-- ("when enchanted creature dies, return this card from your graveyard to the
-- battlefield") would be pinned to the graveyard, where its condition can never
-- be checked, and the card would do nothing.
--
-- The SUBTYPES are the rule's own "if the object is an Aura" (CR 205.3h puts
-- Aura among the enchantment types), and are projected rather than printed
-- wherever the caller has a projection -- an object that becomes an Aura in
-- layer 4 is one for CR 113.6m's purposes too.
--
-- Screams from Within, the pool's Aura with that text, is NOT what the clause
-- decides: CR 400.7 replaced its battlefield incarnation, so its payload names
-- Binding.became and Pawl.Engine.EffectZone.zoneFunctionedFrom reads a zone off
-- no slot but Binding.triggerSource -- the fold answers Nothing for it either
-- way. data/cards/synthetic-widowed-blade.json is the card that tells the
-- clause's two sides apart, and Pawl.ZoneTriggerSpec proves it.
--
-- The clause's "a previous part of its cost or effect specifies that the object
-- is put into that zone" half belongs to the fold below rather than here, and
-- the fold reads it by construction: Pawl.Engine.Activate.zoneFunctionedFrom
-- carries the argument, which is CR 400.7's fresh id -- a later part cannot
-- name the reserved source slot for an object an earlier part already moved.
-- See #2501.
--
-- CR 113.6m's FINAL sentence is read by the fold as well: the arm an
-- Effect.ArmDelayedTrigger names is looked up in the `delayed` map this carries,
-- and Pawl.Engine.EffectZone walks the ability it finds. Prized Amalgam is the
-- printing it decides -- "return this card from your graveyard to the battlefield
-- tapped at the beginning of the next end step" is its only zone-relevant
-- content, and without the sentence the Amalgam functions on the battlefield,
-- where its trigger can never do anything.
--
-- The `unless` clause governs that sentence too, and is read the same way: the
-- Aura half by `enchantedObjectLeaves` above this fold, the trigger-condition
-- half by `conditionPutsSelfInto` below -- so an ability whose own condition is
-- what put the card in the zone its (possibly delayed) effect names is not
-- pinned there, whether its return is immediate or delayed.
zoneFunctionedFrom :: Set.Set Subtype.Subtype -> Map.Map AbilityName.AbilityName (TriggeredAbility.TriggeredAbility Card (GrantedAbility.GrantedAbility Card)) -> TriggeredAbility.TriggeredAbility Card (GrantedAbility.GrantedAbility Card) -> Maybe Zone
zoneFunctionedFrom subtypes delayed ability =
  let condition = TriggeredAbility.condition ability
   in if Set.member Subtype.Aura subtypes && enchantedObjectLeaves condition
        then Nothing
        else case Maybe.listToMaybe (Maybe.mapMaybe (EffectZone.zoneFunctionedFrom delayed) (Modal.allEffects (TriggeredAbility.modal ability))) of
          Nothing -> Nothing
          Just zone -> if conditionPutsSelfInto condition zone then Nothing else Just zone

-- CR 113.6m's Aura clause, asked of a trigger condition: does it specify "that
-- the object it enchants leaves the battlefield"? CR 700.4 makes a death one, so
-- the "dies" wording every printing uses is inside the clause.
--
-- CR 603.1b: one ability may have several conditions, and the rule's "its
-- trigger condition" is satisfied by any of them -- the exception is about what
-- the ability can be made to watch, and one watching condition is enough.
--
-- The rule's "if the object is an Aura" is asked SEPARATELY, by the caller
-- above: this reads the condition alone, and `zoneFunctionedFrom` conjoins the
-- bearer's projected subtypes (CR 205.3) with it. CR 303.4m is why the split is
-- not a distinction without a difference -- an Equipment's "equipped creature"
-- is the same attachment link and the same TriggerCondition, and the exception
-- still does not reach it. Pawl.ZoneTriggerSpec's Synthetic Widowed Blade pair
-- proves it.
--
-- The `_` is a decision, not an omission: CR 113.6m's exception names exactly one
-- family of conditions, so a condition that says nothing about the enchanted
-- object's departure gets the rule's main sentence, which is what False means
-- here.
enchantedObjectLeaves :: TriggerCondition -> Bool
enchantedObjectLeaves condition = case condition of
  TriggerCondition.AttachedCreatureDies -> True
  -- False, stated rather than left to the wildcard: this watches the SAME
  -- attachment link, but for an event that leaves the enchanted permanent right
  -- where it was, so CR 113.6m's Aura clause has nothing to exempt.
  TriggerCondition.AttachedCreatureBecomesTapped -> False
  -- Not an attachment condition at all -- it reads the bearer -- so the
  -- Aura clause is not in question.
  TriggerCondition.SelfBecomesUntapped -> False
  -- False for the arm above's reason: the enchanted permanent stays exactly
  -- where it was, so CR 113.6m's Aura clause has nothing to exempt.
  TriggerCondition.AttachedPermanentTappedForMana -> False
  TriggerCondition.AnyOf conditions -> any enchantedObjectLeaves conditions
  _ -> False

-- CR 113.6m's general clause, asked of a trigger condition and the zone a fold
-- pinned: does the condition specify that the OBJECT THE ABILITY IS ON is put
-- into that zone? Endless Cockroaches, "when this creature dies, return it to
-- its owner's hand" -- dying is what puts it in the graveyard, so an effect that
-- named the graveyard would be exempted right back to the battlefield default.
--
-- Self-scoped conditions only: the rule's "its trigger condition" is the
-- ability's OWN watcher, distinct from the Aura disjunct beside it, which is
-- about a DIFFERENT object (the thing enchanted). `AttachedCreatureDies` answers
-- False here for that reason -- the enchanted creature dying does not put the
-- Aura itself anywhere.
--
-- A ZONE argument rather than a bare predicate, because the rule compares the
-- condition against the effect's OWN zone: `SelfDies` only ever puts the object
-- in the graveyard, so it answers True for that zone and False for any other a
-- (card-data-error) effect might have named.
--
-- The four graveyard-ARRIVAL arms (discarded, cycled, put into a graveyard)
-- are unobservable today: `zonesTriggeredFrom` already answers the graveyard for
-- each, so `functionsIn` reads the same either way. They are here so the rule is
-- stated once, not because a test can redden them.
--
-- Not implemented: a self condition spelled as a bystander constructor narrowed
-- by Filter.IsSource (PermanentSacrificed {You, IsSource}, Biolume Egg's shape)
-- -- the `_` answers False for it, and the ability is pinned to its own
-- graveyard (#3177). Nor the ability-wide reading of an AnyOf: one disjunct's
-- exemption unpins the whole ability, where CR 113.6k's second sentence would
-- let each disjunct function from its own zone (#3178).
--
-- The `_` is otherwise a decision, `enchantedObjectLeaves`'s reason: every other
-- condition says nothing about the object's own arrival anywhere, so the rule's
-- main sentence stands.
conditionPutsSelfInto :: TriggerCondition -> Zone -> Bool
conditionPutsSelfInto condition zone = case condition of
  -- CR 700.4's "dies": the object is put in the graveyard, nowhere else.
  TriggerCondition.SelfDies -> zone == Zone.Graveyard
  -- CR 603.6c's unnarrowed departure names NO destination, and the rule's
  -- exemption is for a condition that "specifies that the object is put into
  -- that zone" -- so the main sentence stands, and an ability of this shape
  -- whose effect names its graveyard functions only there, where a
  -- leaves-the-battlefield condition can never be met. No printing writes it;
  -- the printed shape is "dies".
  TriggerCondition.SelfLeavesTheBattlefield -> False
  -- CR 701.9a: discarding puts the object in the graveyard.
  TriggerCondition.SelfDiscarded -> zone == Zone.Graveyard
  -- CR 702.29c: cycling discards the object, same destination.
  TriggerCondition.SelfCycled -> zone == Zone.Graveyard
  -- CR 603.6's graveyard-arrival forms, either origin.
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> zone == Zone.Graveyard
  TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> zone == Zone.Graveyard
  TriggerCondition.AnyOf conditions -> any (`conditionPutsSelfInto` zone) conditions
  _ -> False

-- CR 113.6, asked of one zone and one triggered ability: does it function from
-- there? Three sentences of that rule in precedence order.
--
-- CR 113.6m first, because it is the only one that can name a zone the condition
-- knows nothing about -- Squee, Goblin Nabob's "at the beginning of your upkeep"
-- triggers perfectly well from the battlefield, and only "return this card from
-- your graveyard" says otherwise. "Functions ONLY in that zone" is what makes
-- this an override rather than an addition.
--
-- CR 113.6k next, for a condition that cannot trigger from the battlefield at
-- all -- Narcomoeba's "put into your graveyard from your library".
--
-- CR 113.6's own default last: "abilities of all other objects usually function
-- only while that object is on the battlefield".
--
-- The two rules cannot presently disagree: no printing states an origin zone on
-- an ability whose condition already answers CR 113.6k, and if one did they
-- would both say graveyard. The order is written down so a future card meets a
-- decision rather than an accident.
functionsIn :: Set.Set Subtype.Subtype -> Map.Map AbilityName.AbilityName (TriggeredAbility.TriggeredAbility Card (GrantedAbility.GrantedAbility Card)) -> Zone -> TriggeredAbility.TriggeredAbility Card (GrantedAbility.GrantedAbility Card) -> Bool
functionsIn subtypes delayed zone ability = case zoneFunctionedFrom subtypes delayed ability of
  Just named -> zone == named
  Nothing -> Set.member zone (zonesTriggeredFrom (TriggeredAbility.condition ability))

-- CR 113.6k, both sentences: "a trigger condition that can't trigger from the
-- battlefield functions in all zones it can trigger from. OTHER TRIGGER
-- CONDITIONS OF THE SAME TRIGGERED ABILITY MAY FUNCTION IN DIFFERENT ZONES."
-- Which is why the answer is a SET: rule 113.6k's own example is Absolver
-- Thrull's "when this creature enters or the creature it haunts dies", one
-- ability whose first condition functions from the battlefield and whose second
-- functions from exile, and no single zone describes it.
--
-- The graveyard, the stack, exile and the hand are the four non-battlefield
-- answers eventTriggers can act on, being the non-battlefield zones this rule
-- sends it to. The command zone IS scanned too, but by
-- a source CR 114.4 governs rather than this rule, so this function's one Command
-- answer -- CR 309.4c's room ability -- still goes unconsulted.
--
-- One of the three sentences `functionsIn` above reads, and the only one that
-- looks at the CONDITION -- so an ability whose effect already names its zone
-- never reaches this, and no arm below has to think about CR 113.6m.
--
-- A CLASSIFICATION of a trigger condition rather than an effect: it asks which
-- zone a rule 603 condition functions in and never reaches the ability's payload.
--
-- The default is `battlefield`, which is CR 113.6's own: abilities usually
-- function only while the object is on the battlefield. Every `battlefield` arm
-- below is that sentence, not an omission.
zonesTriggeredFrom :: TriggerCondition -> Set.Set Zone
zonesTriggeredFrom cond = case cond of
  -- CR 309.4c: "as long as a dungeon card is in the command zone, its abilities
  -- may trigger". The honest answer, and inert: eventTriggers' command-zone source
  -- is CR 114.4's and takes emblems alone, so nothing consults this arm --
  -- Pawl.Engine.Dungeon.roomPending is what gathers a room ability.
  TriggerCondition.RoomEntered _ -> Set.singleton Zone.Command
  -- CR 113.6's default for the three whose watcher is an ordinary permanent:
  -- Matoya, Archon Elder and Wildgrowth Walker are creatures, and neither the
  -- scry, the surveil nor the explore is a condition that cannot trigger from
  -- the battlefield, so CR 113.6k's exception does not apply.
  TriggerCondition.PlayerScries _ -> battlefield
  TriggerCondition.RingTemptsPlayer _ -> battlefield
  TriggerCondition.PlayerSurveils _ -> battlefield
  TriggerCondition.PermanentExplores _ -> battlefield
  -- CR 113.6's default again: Synthetic Blight Chronicler is an ordinary
  -- creature, and a
  -- blight is a condition a battlefield permanent can watch, so CR 113.6k's
  -- exception does not apply.
  TriggerCondition.PlayerBlights _ -> battlefield
  -- CR 113.6's default again, and NOT the graveyard, though Dungeon Crawler
  -- watches from there: completing a dungeon is a condition a battlefield
  -- permanent could watch perfectly well, so CR 113.6k's exception does not
  -- apply. What puts Dungeon Crawler's ability in the graveyard is CR 113.6m,
  -- read off its effect by `zoneFunctionedFrom` above -- Squee, Goblin Nabob's
  -- road, proved by Pawl.DungeonSpec's "CR 309.7 completing a dungeon triggers
  -- Dungeon Crawler out of the graveyard".
  TriggerCondition.PlayerCompletesDungeon _ -> battlefield
  -- CR 113.6's default again: Feywild Trickster is a creature, and nothing
  -- about rolling a die is a condition that cannot trigger from the
  -- battlefield.
  TriggerCondition.PlayerRollsDice _ -> battlefield
  TriggerCondition.PlayerWinsCoinFlip _ -> battlefield
  -- CR 113.6's default, and CR 701.43c makes it the only possible answer rather
  -- than a default: an object that isn't on the battlefield can't be exerted, so
  -- the bearer is standing there when its own exert is recorded.
  TriggerCondition.SelfExerted -> battlefield
  -- CR 113.6's default, and CR 701.3a makes it the only possible answer for the
  -- exert arm's reason: the host of an attachment is a permanent, so the bearer
  -- is on the battlefield whenever this can match.
  TriggerCondition.SelfBecomesAttachedBy _ -> battlefield
  TriggerCondition.SelfBecomesAttachedTo _ -> battlefield
  -- The battlefield too, even though CR 701.3d's own routes include the bearer
  -- leaving it: this names where the ability FUNCTIONS (CR 113.6), and CR 603.10c
  -- is what lets a bearer that has left be offered anyway.
  TriggerCondition.SelfBecomesUnattachedFrom _ -> battlefield
  -- EXILE, and this arm is CR 113.6k's exception rather than its default:
  -- CR 702.170b's special action exiles the card as it becomes plotted, so
  -- the object bearing Aloe Alchemist's "when this card becomes plotted" is
  -- in exile at the moment it fires and can never be on the battlefield for
  -- it. Answering `battlefield` here would leave the trigger unreachable --
  -- eventTriggers finds this bearer through its exile scan, which is gated on
  -- exactly this answer.
  TriggerCondition.SelfBecomesPlotted -> Set.singleton Zone.Exile
  -- CR 603.6a is an enters-the-battlefield ability; its bearer is on the
  -- battlefield when it fires.
  TriggerCondition.SelfEnters -> battlefield
  TriggerCondition.PermanentEnters _ -> battlefield
  TriggerCondition.StepBegins {} -> battlefield
  -- CR 709.5c makes an unlocked designation something a permanent ON THE
  -- BATTLEFIELD has, so this condition cannot trigger from a graveyard at all.
  TriggerCondition.SelfHalfUnlocked _ -> battlefield
  -- CR 709.5c again, one object over: the permanent that became fully unlocked is
  -- on the battlefield, and CR 113.6 leaves the WATCHER where it usually is.
  -- Balemurk Leech is a creature and does nothing from a graveyard.
  TriggerCondition.RoomFullyUnlocked _ -> battlefield
  -- THE UNION, which is CR 113.6k's second sentence read literally: each condition
  -- of a multi-condition ability functions where it functions, and the ability is
  -- offered from every zone any of them reaches. Blind Hunter's ability is the
  -- producer -- battlefield for "when this creature enters", exile for "or the
  -- creature it haunts dies" -- and it is the rule's own example.
  --
  -- Offering the ability in both zones cannot double-fire it: `matchesTrigger`
  -- still has to admit the bearer, and the two conditions are about different ids
  -- in different zones, so at most one of them matches any event.
  --
  -- The empty list falls to CR 113.6's default rather than to the empty union,
  -- which would say the ability functions nowhere. No card writes one.
  TriggerCondition.AnyOf conditions -> case conditions of
    [] -> battlefield
    _ -> Set.unions (fmap zonesTriggeredFrom conditions)
  -- CR 708.7 is about a PERMANENT being turned face up, and CR 110.1 puts
  -- permanents on the battlefield alone, so CR 113.6k never reaches this.
  TriggerCondition.SelfTurnedFaceUp -> battlefield
  -- CR 701.27a transforms a PERMANENT, which CR 110.1 puts on the battlefield
  -- alone, so CR 113.6k never reaches this either.
  TriggerCondition.SelfTransformedInto _ -> battlefield
  -- CR 113.6's default for the watcher's side, PermanentTurnedFaceUp's reason
  -- below: Cult of the Waxing Moon is a creature watching from the battlefield.
  TriggerCondition.PermanentTransforms _ -> battlefield
  -- CR 113.6's default, one object over: the WATCHER is an ordinary permanent
  -- doing its watching from the battlefield -- Aven Farseer is a creature -- so CR
  -- 113.6k's exception, which is for a condition that cannot trigger from the
  -- battlefield at all, does not apply.
  TriggerCondition.PermanentTurnedFaceUp _ -> battlefield
  -- The same default: CR 702.112b's "only permanents can be or become renowned"
  -- keeps the subject on the battlefield, and Valeron Wardens watches from it.
  TriggerCondition.PermanentBecomesDesignated {} -> battlefield
  -- The same default again: rule 702.100b's marker goes to a creature, and
  -- Renegade Krasis is the creature watching itself.
  TriggerCondition.SelfEvolves -> battlefield
  -- The same default a third time, from the Equipment's side: CR 301.5c unattaches
  -- an Equipment rather than moving it, so one that equips anything is on the
  -- battlefield, and Aegis of the Legion watches from there -- CR 113.6k's exception
  -- is for a condition that cannot trigger from the battlefield at all.
  TriggerCondition.AttachedCreatureMentors -> battlefield
  -- CR 113.6's default from the bearer's side, Aura or Equipment alike (CR
  -- 303.4m): an Aura is itself a permanent on the battlefield and CR 704.5n
  -- leaves an Equipment standing on it, so the bearer watches from there, and CR
  -- 113.6k's exception -- for a condition that cannot trigger from the
  -- battlefield at all -- does not apply. What DOES apply is CR 113.6m's Aura
  -- clause, read by zoneFunctionedFrom above, which is why this arm is reached
  -- for Screams from Within and Skullclamp but not for Synthetic Widowed Blade.
  TriggerCondition.AttachedCreatureDies -> battlefield
  -- The same default for the same reason, and more plainly: an Aura enchanting a
  -- permanent is itself a permanent on the battlefield, and CR 113.6k's exception
  -- is for a condition that cannot trigger from there at all.
  TriggerCondition.AttachedCreatureBecomesTapped -> battlefield
  -- CR 110.5 makes tapped a permanent's status, so only a permanent can
  -- become untapped and only the battlefield can hold one.
  TriggerCondition.SelfBecomesUntapped -> battlefield
  -- The same default and the same reason, one event over.
  TriggerCondition.AttachedPermanentTappedForMana -> battlefield
  -- CR 113.6's default once more: Autumn Willow, Harmony is a creature watching
  -- from the board it stands on, and CR 113.6k's exception -- for a condition
  -- that cannot trigger from the battlefield at all -- does not apply.
  TriggerCondition.PermanentTappedForMana {} -> battlefield
  -- The same default from the training creature's own side: rule 702.149a's ability
  -- fires on an attack, so its bearer is on the battlefield and CR 113.6k's
  -- exception -- for a condition that cannot trigger from there at all -- does not
  -- apply.
  TriggerCondition.SelfTrains -> battlefield
  -- CR 113.6's default: an ability of a permanent functions only while that
  -- permanent is on the battlefield. CR 113.6k's exception is for a trigger
  -- condition that CANNOT trigger from the battlefield, and this one plainly can
  -- -- Mayhem Devil watches every sacrifice from the board it stands on.
  TriggerCondition.PermanentSacrificed {} -> battlefield
  -- CR 603.8's state triggers are not event triggers, so this scan is not their
  -- reader in any zone; stateTriggers below gathers them from the battlefield.
  TriggerCondition.StateIs _ -> battlefield
  TriggerCondition.SelfDealsCombatDamageToPlayer -> battlefield
  -- CR 113.6's default again, and the match's own shape on top of it: this arm
  -- compares the bearer against the event's RECIPIENT, and CR 120.3's recipient is
  -- a player or a permanent -- so a bearer anywhere but the battlefield can never
  -- be the one damaged.
  TriggerCondition.SelfIsDealtDamage -> battlefield
  TriggerCondition.PermanentDealsCombatDamageToPlayer _ -> battlefield
  -- The batch reading watches from the battlefield too, and for the arm above's
  -- reason: its bearer is a bystander.
  TriggerCondition.PermanentsDealCombatDamageToPlayer _ -> battlefield
  TriggerCondition.CreatureDealtCombatDamageToMonarch -> battlefield
  TriggerCondition.CreaturesDealtCombatDamageToInitiative -> battlefield
  TriggerCondition.PlayerTookInitiative -> battlefield
  TriggerCondition.OpponentLostLifeDuringYourTurn -> battlefield
  -- CR 302.6 / 508.1a: only a permanent on the battlefield can be declared as an
  -- attacker, so CR 113.6k never reaches this.
  TriggerCondition.SelfAttacks _ -> battlefield
  TriggerCondition.SelfAttacksWithAnother _ -> battlefield
  TriggerCondition.CreatureAttacksAlone _ -> battlefield
  TriggerCondition.CreatureAttacksYou -> battlefield
  TriggerCondition.AttachedPlayerIsAttacked -> battlefield
  TriggerCondition.PlayerAttacks _ -> battlefield
  TriggerCondition.PlayerAttacksWith {} -> battlefield
  TriggerCondition.PlayerAttacksPlayer {} -> battlefield
  TriggerCondition.SelfAttacksPlayerWithMostLife -> battlefield
  TriggerCondition.SelfBlocks -> battlefield
  TriggerCondition.SelfBlocksCreature _ -> battlefield
  TriggerCondition.SelfBlocksAtLeast _ -> battlefield
  TriggerCondition.SelfBlocksOneOrMore _ -> battlefield
  TriggerCondition.SelfBecomesBlocked -> battlefield
  TriggerCondition.SelfBecomesBlockedBy _ -> battlefield
  TriggerCondition.PermanentBecomesBlockedBy _ -> battlefield
  TriggerCondition.SelfBecomesBlockedByOneOrMore _ -> battlefield
  TriggerCondition.CreatureBecomesBlockedByAtLeast {} -> battlefield
  TriggerCondition.SelfAttacksUnblocked -> battlefield
  -- CR 702.29c: a cycling ability triggers from whatever zone the card winds up
  -- in, the graveyard for every printing in this pool, and a cycled card cannot be
  -- on the battlefield. eventTriggers' `cycledCard` is what actually serves it.
  TriggerCondition.SelfCycled -> Set.singleton Zone.Graveyard
  -- CR 113.6k's fourth zone: rule 702.94a's reveal happens FROM a hand and rule
  -- 701.20b leaves the card there, so this condition cannot trigger from the
  -- battlefield at all and the hand is the one zone it can. eventTriggers'
  -- `revealedInHand` is what serves it.
  TriggerCondition.SelfRevealedForMiracle -> Set.singleton Zone.Hand
  -- CR 113.6k's exception again, on SelfCycled's argument: CR 701.9a discards a
  -- card from a HAND, so this condition can never trigger from the battlefield,
  -- and the graveyard rule 701.9a moves the card to is the one zone the scan
  -- meets it in.
  --
  -- eventTriggers' `inGraveyards` is what serves it, gated on exactly this
  -- answer -- Pawl.TriggerSpec's Bartered Cow cases go red if this arm answers
  -- the battlefield. No candidate source of its own is owed: `cycledCard`
  -- recovers the card the CYCLING cause named, which rule 702.29c makes narrower
  -- than this condition rather than a gap under it.
  TriggerCondition.SelfDiscarded -> Set.singleton Zone.Graveyard
  -- CR 113.6's default: the bearer watches from the battlefield, so a card in a
  -- graveyard does not see an opponent discard.
  TriggerCondition.PlayerDiscards _ -> battlefield
  -- CR 113.6's default too, and NOT the graveyard SelfCycled answers above: the
  -- bearer here is a permanent, not the card that was cycled, so the zone the
  -- cycled card winds up in (rule 702.29c's second sentence) is nothing to do
  -- with where this ability functions. eventTriggers' `cycledCard` serves the
  -- self-scoped condition only.
  TriggerCondition.PlayerCycles _ -> battlefield
  -- CR 113.6's default again: Erudite Wizard watches its controller's draws from
  -- the battlefield. CR 702.94a's miracle answers a hand below, and it is a
  -- different condition -- it watches the REVEAL, not the draw.
  TriggerCondition.PlayerDrawsNthCard {} -> battlefield
  -- The condition this predicate exists for: a card cannot be put into a graveyard
  -- from a library while on the battlefield, so this can never trigger from there
  -- and the graveyard it lands in is the one zone it can.
  TriggerCondition.SelfPutIntoGraveyardFromLibrary -> Set.singleton Zone.Graveyard
  -- The graveyard for a NEARER reason than the library condition's, and the one
  -- that matters: this condition CAN follow a battlefield-to-graveyard move, but CR
  -- 603.6c's last sentence denies it the leaves-the-battlefield look-back, so
  -- the bearer is never the permanent on the battlefield -- it is always the card
  -- that arrived in the graveyard. Nothing it can trigger from is the
  -- battlefield, so CR 113.6k puts it in every zone it can, and the graveyard is
  -- where the scan meets it whatever zone the card came from.
  TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> Set.singleton Zone.Graveyard
  -- The bystander reading takes CR 113.6's default instead: the watcher is a
  -- permanent on the battlefield (Planar Void), and nothing about the condition
  -- says otherwise.
  TriggerCondition.CardPutIntoGraveyard _ -> battlefield
  -- The mirror image, defaulting for a reason rather than by omission: a dies trigger CAN
  -- trigger from the battlefield, which CR 603.10a's look-back is what makes true
  -- of a permanent that is a graveyard card by the time the scan runs.
  -- `leftBattlefield` serves it from CR 608.2h; neither graveyard source may, or
  -- the ability would be read off the graveyard card and credited to its owner.
  TriggerCondition.SelfDies -> battlefield
  -- The same answer one step further: this condition's bearer is not the permanent
  -- that died at all, and watches from the battlefield.
  TriggerCondition.PermanentDies _ -> battlefield
  -- The batch reading watches from the battlefield too, and for the arm above's
  -- reason: its bearer is a bystander.
  TriggerCondition.PermanentsDie _ -> battlefield
  -- The same CR 603.10a answer as both dies conditions, and harder to miss here:
  -- the destination may be a hand or library, and an ability found in a GRAVEYARD
  -- could not be what fired for a permanent that went somewhere else.
  TriggerCondition.SelfLeavesTheBattlefield -> battlefield
  -- The same answer once more, and here it is the ONLY one CR 113.6k could give:
  -- the bearer is a bystander that never left the battlefield at all.
  TriggerCondition.PermanentLeavesTheBattlefield _ -> battlefield
  -- The same answer for the same reason: the bearer is a bystander that never
  -- left the battlefield.
  TriggerCondition.PermanentReturnedToHand _ -> battlefield
  TriggerCondition.PermanentsReturnedToHand _ -> battlefield
  -- CR 113.6's default once more: Kishla Skimmer is a creature watching its
  -- controller's graveyard from the battlefield, and CR 113.6k's exception is for
  -- a condition that cannot trigger from there at all.
  TriggerCondition.CardLeavesGraveyard {} -> battlefield
  -- CR 113.6k's third zone, and rule 702.55c states it outright: "triggered
  -- abilities of cards with haunt that refer to the haunted creature can trigger
  -- in the exile zone". A permanent on the battlefield haunts nothing -- only a
  -- card Effect.ExileHaunting put in exile is in GameState.haunting at all -- so
  -- this condition cannot trigger from the battlefield, and exile is the one zone
  -- it can trigger from. eventTriggers' `inExile` is what serves it.
  TriggerCondition.HauntedCreatureDies -> Set.singleton Zone.Exile
  -- CR 113.6's default again: the bearer watches from the battlefield.
  TriggerCondition.SpellOrAbilityCounters _ -> battlefield
  TriggerCondition.AbilityIsCountered -> battlefield
  -- The same default: Selfless Squire watches damage addressed to its controller from
  -- the battlefield, and a card in a graveyard sees nothing prevented.
  TriggerCondition.DamageToPlayerPrevented _ -> battlefield
  -- CR 113.6's default: the Vindicator's prevention ability functions on the
  -- battlefield, so the trigger paired with it watches from there too.
  TriggerCondition.SelfPreventsDamage _ -> battlefield
  -- CR 113.6's default once more: Ajani's Pridemate has to be on the battlefield
  -- to receive the counter its own ability puts on it.
  TriggerCondition.PlayerGainsLife _ -> battlefield
  -- CR 113.6's default, the arm above's: Synthetic Communal Vigil is an
  -- enchantment watching the table from the battlefield.
  TriggerCondition.PlayersGainLife _ -> battlefield
  -- And once more: Exquisite Blood is an enchantment, and CR 113.6 leaves its
  -- ability functioning only where the permanent is.
  TriggerCondition.PlayerLosesLife _ -> battlefield
  -- CR 122.1's first sentence puts counters on OBJECTS, and CR 714.3 keeps a
  -- Saga's lore counters on the permanent -- so CR 113.6's default holds and a
  -- chapter ability functions from the battlefield alone.
  TriggerCondition.SelfCountersReached {} -> battlefield
  TriggerCondition.SelfBecomesClassLevel _ -> battlefield
  TriggerCondition.SelfLastCounterRemoved _ -> battlefield
  TriggerCondition.SelfCountersRemoved _ -> battlefield
  -- The battlefield, every counter mirror's answer: a permanent takes CR 122.6
  -- counters there, and these conditions' bearers are bystanders watching from
  -- it.
  TriggerCondition.PermanentsGetCounters {} -> battlefield
  TriggerCondition.PermanentGetsCounters {} -> battlefield
  -- CR 113.6's default: Young Pyromancer watches the stack from the battlefield,
  -- and a card in a graveyard sees nothing cast.
  TriggerCondition.SpellCast {} -> battlefield
  -- CR 113.6k, the second zone it reaches: CR 601.2a moves the object to the stack
  -- to cast it and leaves it there, so at CR 601.2i it is on the stack and not on
  -- the battlefield -- this condition cannot trigger from there at all. The stack
  -- is the one zone it can, and eventTriggers' `spellCast` is what serves it.
  TriggerCondition.SelfCast -> Set.singleton Zone.Stack
  -- CR 113.6's default, unlike SelfCast just above: rule 702.21a prints ward on a
  -- permanent, so the bearer watches the announcement from the battlefield. A
  -- spell on the stack can become a target too, and no card in the pool is one.
  TriggerCondition.SelfBecomesTargeted _ -> battlefield
  -- CR 113.6's default again: Dormant Gomazoa is a creature and Amulet of
  -- Safekeeping an artifact, both watching their controller from the
  -- battlefield. Nothing on the stack reads its controller becoming a target.
  TriggerCondition.ControllerBecomesTarget {} -> battlefield
  -- CR 113.6's default a last time: Historian's Boon is an enchantment watching
  -- the battlefield's Sagas, and a card in a graveyard sees no chapter fire.
  TriggerCondition.SagaFinalChapterTriggers _ -> battlefield
  -- CR 113.6's default once more: Custodi Lich is a creature and watches the
  -- crown from the battlefield, so the card sees no crowning from a graveyard.
  TriggerCondition.PlayerBecomesMonarch _ -> battlefield
  -- CR 113.6's default, and never actually consulted: this condition's only carrier
  -- is a CR 603.7 delayed entry, which Event.delayedPending gathers out of
  -- GameState.delayedTriggers rather than out of a zone. The default is right
  -- anyway -- the event it matches happens on the battlefield.
  TriggerCondition.LoseControlOfBound _ -> battlefield
  -- Never consulted either, and for LoseControlOfBound's reason: rule 701.66a's
  -- delayed ability is a GameState.delayedTriggers entry, gathered out of the
  -- store rather than out of a zone.
  TriggerCondition.BoundDiesOrIsExiled _ -> battlefield
  -- Never consulted either, and for the same reason: CR 603.12 routes a
  -- reflexive through rule 603.7, so its only carrier is a delayed entry
  -- Event.delayedPending gathers out of GameState.delayedTriggers. EMPTY rather
  -- than CR 113.6's default, which is the honest answer here where it is not for
  -- the arm above: a reflexive is created by a RESOLVING spell or ability and
  -- watches no zone at all, its source having been able to leave before it fires.
  TriggerCondition.Reflexive -> Set.empty
  where
    battlefield = Set.singleton Zone.Battlefield

-- CR 603.8: state triggers. For every battlefield permanent, each StateIs ability
-- it bears whose condition is currently TRUE and which has no instance of ITSELF
-- already on the stack -- counted, so an object carrying the same state-triggered
-- ability twice arms both (CR 603.2 makes each of them an ability in its own
-- right).
--
-- Armedness is DERIVED, never stored: CR 603.8's three outcomes are all "no longer
-- on the stack", so an instance sitting there is the whole suppression rule and
-- there is no bookkeeping field to leak. No triggered-but-not-yet-placed window
-- either, Engine.placePendingTriggers acting within the same settle step.
--
-- A trigger whose modes are all unfillable would be removed from the stack (CR
-- 603.3c) and re-trigger on the next settle pass while its condition held, which
-- would not terminate. No card in the pool can do that, and the first that could
-- is the one that must revisit this.
--
-- Not implemented: a state trigger borne by an EMBLEM, which CR 114.4 would have
-- function in the command zone -- this scan reads the battlefield alone, where
-- eventTriggers reads both (#1400).
stateTriggers :: GameState -> [PendingTrigger]
stateTriggers gs
  -- A stack id whose object can't be found: fail CLOSED, not open. This runs
  -- inside the settleForPriority fixpoint, so a lost suppression loops forever
  -- -- a hang, not a wrong answer -- while failing closed costs at most one
  -- settle pass. Unreachable: Game.cease removes the stack entry and its object
  -- together. Hoisted to the whole function because that is what the per-ability
  -- check it replaces amounted to: one unreadable stack entry suppressed every
  -- ability of every source.
  | any (\sid -> Maybe.isNothing (Game.lookupObject sid gs)) (GameState.stack gs) = []
  | otherwise = concatMap forOne (Set.toAscList (GameState.battlefield gs))
  where
    -- The same hoist eventTriggers' `grants` binding makes.
    grants = Projection.controlGrants gs
    -- CR 603.8's suppression, COUNTED rather than tested. Scoped to (source,
    -- ability), so two permanents bearing the identical triggered ability
    -- suppress independently -- one instance per source, not one for the whole
    -- board.
    --
    -- A count rather than an "is there one?" because CR 603.2 makes each ability
    -- its own ability: one object may carry two identical state-triggered
    -- abilities, and CR 603.8 holds each back only until THAT ability's own
    -- instance leaves the stack. Object.source cannot tell those two instances
    -- apart -- they are equal values -- but it does not have to. Which of N
    -- identical abilities a given instance came from is unobservable, so N live
    -- copies minus K instances already on the stack is the exact answer: it
    -- reproduces the single-ability behavior at N = 1, and lets one of a twin
    -- pair re-arm while the other's instance still sits there
    -- (TriggerSpec, "one instance leaving re-arms ITS ability").
    instancesOnStack srcId ab =
      let isInstance sid = case fmap Object.source (Game.lookupObject sid gs) of
            -- The SOURCE and the ABILITY, rather than the whole record: CR
            -- 603.7a's creation moment also rides on that arm, and an instance
            -- of this ability is an instance of it however it got here.
            Just (Source.OfTrigger triggered) -> TriggeredAbilitySource.source triggered == srcId && TriggeredAbilitySource.ability triggered == ab
            _ -> False
       in length (filter isInstance (GameState.stack gs))
    forOne oid = case Projection.controllerOfGiven grants Set.empty oid gs of
      Nothing -> []
      -- CR 603.3a / 109.5: the ability's controller is its source's, and that is
      -- what "you" in the condition means. Outside the layer fold, so the ViewOf
      -- is the FULL projection rather than the layer-bounded one.
      Just ctrl ->
        let live ab = liveCondition (TriggeredAbility.condition ab)
            liveCondition condition = case condition of
              TriggerCondition.StateIs cond ->
                Condition.holds (Projection.fullView gs) (Filter.contextFor (Game.teams gs) (Just ctrl) (Just oid)) gs oid cond
              TriggerCondition.SelfEnters -> False
              -- CR 309.4c is an EVENT trigger too: the marker MOVING into the room
              -- is what fires it, not the marker sitting there.
              TriggerCondition.RoomEntered _ -> False
              -- CR 603.2 again: CR 309.7's completion is the dungeon card's
              -- removal happening, not a state that could be true standing still.
              TriggerCondition.PlayerCompletesDungeon _ -> False
              -- CR 603.2 event triggers, all four: a scry, a surveil, a card
              -- becoming plotted and an explore are things that HAPPEN, each with
              -- its own log entry, and none of them is a CR 603.8 state that could
              -- be true standing still.
              TriggerCondition.PlayerScries _ -> False
              TriggerCondition.RingTemptsPlayer _ -> False
              TriggerCondition.PlayerSurveils _ -> False
              TriggerCondition.SelfBecomesPlotted -> False
              TriggerCondition.PermanentExplores _ -> False
              -- CR 603.2 once more: a blight is something that HAPPENS, with
              -- its own log entry, never a CR 603.8 state standing still.
              TriggerCondition.PlayerBlights _ -> False
              -- CR 603.2 once more: a die roll is something that HAPPENS, with its own log
              -- entry, never a CR 603.8 state that could be true standing still.
              TriggerCondition.PlayerRollsDice _ -> False
              TriggerCondition.PlayerWinsCoinFlip _ -> False
              -- CR 603.2 again: being exerted is something that happens, with its
              -- own log entry, and CR 701.43b makes "already exerted" no bar to
              -- exerting again -- so there is no standing state to be true.
              TriggerCondition.SelfExerted -> False
              -- CR 603.2 once more: becoming attached is something that HAPPENS,
              -- with its own log entry. Standing attached is a state, but no
              -- condition here asks about it.
              TriggerCondition.SelfBecomesAttachedBy _ -> False
              TriggerCondition.SelfBecomesAttachedTo _ -> False
              -- And becoming unattached likewise. Standing UNattached is a state
              -- CR 704.5m acts on, but it is a state-based action rather than a
              -- CR 603.8 trigger, and no condition here asks about it.
              TriggerCondition.SelfBecomesUnattachedFrom _ -> False
              -- CR 603.6a is an EVENT trigger, matched against the log; nothing
              -- about it is a CR 603.8 state.
              TriggerCondition.PermanentEnters _ -> False
              TriggerCondition.StepBegins {} -> False
              TriggerCondition.SelfDealsCombatDamageToPlayer -> False
              TriggerCondition.SelfIsDealtDamage -> False
              TriggerCondition.PermanentDealsCombatDamageToPlayer _ -> False
              TriggerCondition.PermanentsDealCombatDamageToPlayer _ -> False
              TriggerCondition.CreatureDealtCombatDamageToMonarch -> False
              TriggerCondition.CreaturesDealtCombatDamageToInitiative -> False
              TriggerCondition.PlayerTookInitiative -> False
              TriggerCondition.OpponentLostLifeDuringYourTurn -> False
              TriggerCondition.SelfAttacks _ -> False
              TriggerCondition.SelfAttacksWithAnother _ -> False
              TriggerCondition.CreatureAttacksAlone _ -> False
              TriggerCondition.CreatureAttacksYou -> False
              TriggerCondition.AttachedPlayerIsAttacked -> False
              TriggerCondition.PlayerAttacks _ -> False
              TriggerCondition.PlayerAttacksWith {} -> False
              TriggerCondition.PlayerAttacksPlayer {} -> False
              TriggerCondition.SelfAttacksPlayerWithMostLife -> False
              TriggerCondition.SelfBlocks -> False
              TriggerCondition.SelfBlocksCreature _ -> False
              TriggerCondition.SelfBlocksAtLeast _ -> False
              TriggerCondition.SelfBlocksOneOrMore _ -> False
              TriggerCondition.SelfBecomesBlocked -> False
              TriggerCondition.SelfBecomesBlockedBy _ -> False
              TriggerCondition.PermanentBecomesBlockedBy _ -> False
              TriggerCondition.SelfBecomesBlockedByOneOrMore _ -> False
              TriggerCondition.CreatureBecomesBlockedByAtLeast {} -> False
              TriggerCondition.SelfAttacksUnblocked -> False
              TriggerCondition.SelfCycled -> False
              TriggerCondition.SelfRevealedForMiracle -> False
              TriggerCondition.SelfDiscarded -> False
              TriggerCondition.PlayerDiscards _ -> False
              TriggerCondition.PlayerCycles _ -> False
              TriggerCondition.PlayerDrawsNthCard {} -> False
              TriggerCondition.SelfPutIntoGraveyardFromLibrary -> False
              TriggerCondition.SelfPutIntoGraveyardFromAnywhere -> False
              TriggerCondition.CardPutIntoGraveyard _ -> False
              TriggerCondition.SelfDies -> False
              TriggerCondition.PermanentDies _ -> False
              TriggerCondition.PermanentsDie _ -> False
              TriggerCondition.SelfLeavesTheBattlefield -> False
              TriggerCondition.PermanentLeavesTheBattlefield _ -> False
              TriggerCondition.PermanentReturnedToHand _ -> False
              TriggerCondition.PermanentsReturnedToHand _ -> False
              TriggerCondition.CardLeavesGraveyard {} -> False
              TriggerCondition.HauntedCreatureDies -> False
              TriggerCondition.SpellOrAbilityCounters _ -> False
              TriggerCondition.AbilityIsCountered -> False
              TriggerCondition.DamageToPlayerPrevented _ -> False
              TriggerCondition.SelfPreventsDamage _ -> False
              TriggerCondition.PlayerGainsLife _ -> False
              TriggerCondition.PlayersGainLife _ -> False
              TriggerCondition.PlayerLosesLife _ -> False
              -- CR 603.3b's condition is an EVENT trigger too, and the event is
              -- another ability triggering: nothing about it is a state a settle
              -- could re-read.
              TriggerCondition.SagaFinalChapterTriggers _ -> False
              -- CR 603.2 event trigger, not a CR 603.8 state: this fires on a
              -- player BECOMING the monarch, and a settle re-reading "is the
              -- monarch" would fire it again every time until the crown moved.
              TriggerCondition.PlayerBecomesMonarch _ -> False
              -- CR 603.2 event trigger too, and not a state: it fires on control
              -- CHANGING, and a settle re-reading "somebody else controls it" would
              -- fire it again on every pass thereafter.
              TriggerCondition.LoseControlOfBound _ -> False
              -- CR 603.2 event trigger too: it fires on the land BEING put into a
              -- graveyard or into exile, not on its being there.
              TriggerCondition.BoundDiesOrIsExiled _ -> False
              -- CR 603.12 sends a reflexive through rule 603.7, and CR 603.8's
              -- state triggers are a different family: nothing about "when you
              -- do" is a state that could be standing true.
              TriggerCondition.Reflexive -> False
              -- CR 709.5h is an EVENT trigger: it fires on the permanent BEING
              -- GIVEN the designation, which CR 709.5c leaves it holding
              -- thereafter, so a state read would fire it again every time the
              -- board settles.
              TriggerCondition.SelfHalfUnlocked _ -> False
              -- CR 708.7 is an EVENT trigger for the same reason: it fires on the
              -- permanent BEING turned face up, and CR 708.8 leaves it face up
              -- thereafter -- so a state read would fire it again every settle,
              -- for as long as the permanent stayed on the battlefield.
              TriggerCondition.SelfTurnedFaceUp -> False
              -- CR 701.27a is an EVENT trigger for that reason exactly: CR
              -- 712.18 leaves the permanent on its new face thereafter, so a
              -- state read would fire it again on every settle.
              TriggerCondition.SelfTransformedInto _ -> False
              -- And the bystander's form of it is an EVENT trigger for the
              -- same reason: a board showing a permanent on its back face
              -- says nothing about when it turned over.
              TriggerCondition.PermanentTransforms _ -> False
              -- And the watcher's form is an EVENT trigger for the same reason,
              -- more plainly still: a board on which some permanent is face up
              -- says nothing about which of them was ever TURNED over, so there
              -- is no state here to read at all.
              TriggerCondition.PermanentTurnedFaceUp _ -> False
              -- CR 702.112b's designation is exactly that shape once more: the
              -- permanent keeps it, so a state read would fire every settle.
              TriggerCondition.PermanentBecomesDesignated {} -> False
              -- CR 702.100b is an EVENT trigger and leaves no state at all behind:
              -- the counters it put are indistinguishable from any others.
              TriggerCondition.SelfEvolves -> False
              -- CR 702.134c likewise, and one step further removed: what it fires
              -- on is a resolution, and the counter that resolution put is a
              -- counter like any other, so the board afterwards says nothing about
              -- which creature mentored which.
              TriggerCondition.AttachedCreatureMentors -> False
              -- CR 700.4's death is an EVENT, and the board afterwards cannot
              -- say which permanent an Aura in a graveyard used to enchant.
              TriggerCondition.AttachedCreatureDies -> False
              -- CR 701.26a's tap is an EVENT too. A tapped enchanted permanent is
              -- a state the board can read, which is exactly why this must be
              -- False: CR 603.2e says a "becomes" condition does not retrigger
              -- while the state persists, and a state trigger would do nothing but.
              TriggerCondition.AttachedCreatureBecomesTapped -> False
              -- CR 701.26b's untap is an EVENT for the identical reason,
              -- read off the bearer instead of its host.
              TriggerCondition.SelfBecomesUntapped -> False
              -- CR 106.12a is an EVENT too, and more plainly than the arm
              -- above: a mana ability having resolved is nothing a later
              -- board read can recover.
              TriggerCondition.AttachedPermanentTappedForMana -> False
              -- The same resolution read by a bystander, so the same answer.
              TriggerCondition.PermanentTappedForMana {} -> False
              -- CR 702.149c the same: it fires on a resolution, and the counter
              -- that resolution put is a counter like any other, so the board
              -- afterwards says nothing about which creature trained.
              TriggerCondition.SelfTrains -> False
              -- CR 701.21a is a game ACTION, so this is an event trigger too: it
              -- fires on the moment the permanent is sacrificed, and the board
              -- afterwards holds no state a read could recover.
              TriggerCondition.PermanentSacrificed {} -> False
              -- CR 714.2b is an EVENT trigger too: it fires on the moment counters
              -- are PUT ON, not on the count standing at or above N -- which is
              -- exactly the difference CR 603.8 draws, and the reason a Saga does
              -- not re-run its final chapter for as long as it sits there.
              TriggerCondition.SelfCountersReached {} -> False
              TriggerCondition.SelfBecomesClassLevel _ -> False
              TriggerCondition.SelfLastCounterRemoved _ -> False
              TriggerCondition.SelfCountersRemoved _ -> False
              TriggerCondition.PermanentsGetCounters {} -> False
              TriggerCondition.PermanentGetsCounters {} -> False
              TriggerCondition.SpellCast {} -> False
              TriggerCondition.SelfCast -> False
              TriggerCondition.SelfBecomesTargeted _ -> False
              TriggerCondition.ControllerBecomesTarget {} -> False
              -- CR 709.5i is an EVENT trigger, for CR 709.5h's reason one arm up:
              -- it fires on the LAST designation arriving, and CR 709.5c leaves
              -- the permanent holding both thereafter, so a state read would fire
              -- it again on every settle.
              TriggerCondition.RoomFullyUnlocked _ -> False
              -- `any`, which is matchesTrigger's AnyOf arm read into this scan:
              -- an ability with a CR 603.8 clause is a state trigger, whatever
              -- else it also has. Never True today -- Pawl.CardSpec's lint
              -- forbids a StateIs inside an AnyOf, precisely so that an ability
              -- cannot be gathered by this scan and by the event scan at once --
              -- so what this arm really says is that the classification stays
              -- coherent if that lint is ever relaxed.
              TriggerCondition.AnyOf conditions -> any liveCondition conditions
            lives = filter live (Projection.triggeredAbilitiesOf oid gs)
            -- Each live copy against the copies of itself that came earlier in
            -- the list, which gives it a 1-based ordinal among its equals: the
            -- j-th copy is armed exactly when fewer than j instances of it are
            -- already on the stack. That is the N-minus-K subtraction
            -- instancesOnStack describes, written without ever needing an Ord on
            -- a triggered ability.
            armed (before, ab) = 1 + length (filter (ab ==) before) > instancesOnStack oid ab
            pend ab = PendingTrigger.MkPendingTrigger (TriggerSource.OfObject oid) ctrl ab Map.empty Nothing
         in fmap (pend . snd) (filter armed (zip (List.inits lives) lives))

-- CR 603.7: delayed abilities whose trigger event is among these events. An entry
-- that TRIGGERS is REMOVED from the store (CR 603.7b) unless it carries a stated
-- duration, which is that rule's own exception -- one of Expiry's sweeps ends
-- those instead. The survivors are returned so the caller can store them back. CR
-- 603.7d-f: the controller travels with the entry, so a delayed ability resolves
-- under whoever controlled the spell that created it even once that spell's source
-- is gone.
--
-- `matching` matches its condition only against EVENTS, never live game state -- the
-- turn number `armed` reads is CR 603.7a's arming gate, which can only withhold a
-- match. So a stored entry whose condition is StateIs would never fire, and
-- without a stated duration would never leave the store. Not a live gap: no card
-- in this pool arms a delayed ability with a StateIs condition.
--
-- CR 603.4 is applied HERE rather than in gatherTriggers, because the surviving
-- store depends on it: "the ability triggers only if [the condition] is [true];
-- otherwise it does nothing", and CR 603.7b bounds how many times the ability
-- TRIGGERS, not how many occurrences of its event it watches. So an entry whose
-- intervening "if" is false at the occurrence has not triggered, nothing is spent
-- against 603.7b's one shot, and it stays armed for the next occurrence. No other
-- 603.7 subrule evicts an entry, so triggering and a stated duration remain its
-- only two exits.
delayedPending :: [LoggedEvent.LoggedEvent] -> GameState -> Game ([PendingTrigger], Seq.Seq DelayedTrigger)
delayedPending grouped gs =
  let -- CR 603.7a's floor is the watermark's job, and is all an ordinary entry
      -- needs. This is the card's OWN further restriction: an ability printed "on
      -- your next turn" fires on that one turn and no other, whatever its
      -- condition matches. Read against the LIVE turn number, so an entry with no
      -- onset is untouched.
      armed entry = case DelayedTrigger.window entry of
        TurnWindow.AnyTurn -> True
        -- The named turn has not begun, so no occurrence counts -- including one
        -- in the turn that armed the ability, which is why the onset exists.
        TurnWindow.ControllersNextTurn -> False
        -- EQUALITY, not a floor: CR 603.7a is a claim about ONE named turn, so the
        -- window has an upper end and not merely a lower one.
        TurnWindow.OnTurn n -> n == GameState.turnNumber gs
      -- WHICH events fired the entry, rather than merely whether one did: the
      -- payload reads CR 603.2's event through eventBindings below, so the match
      -- has to hand each event forward.
      matching entry =
        let cond = TriggeredAbility.condition (DelayedTrigger.ability entry)
         in -- The entry's own bindings, which is CR 603.7c's captured environment:
            -- TriggerCondition.LoseControlOfBound asks about an object named as the
            -- arming spell resolved, and the store is the only thing that still
            -- remembers it.
            if armed entry
              then filter (matchesTriggerGiven (DelayedTrigger.bindings entry) gs (DelayedTrigger.source entry) (DelayedTrigger.controller entry) cond . LoggedEvent.event) grouped
              else []
      -- CR 603.2c's FIRST sentence on the CR 603.7 path, which is eventTriggers'
      -- `oncePerBatch` asked of a delayed entry: a batch-scoped condition names the
      -- whole Pawl.Types.EventGroup as its trigger event, so the group is ONE
      -- occurrence however many of its members matched, where a per-occurrence
      -- condition's trigger event is each member and the whole match list stands.
      --
      -- Per GROUP and never per scan, `oncePerBatch`'s reason exactly: CR 704.3
      -- makes each state-based-action pass its own single event, and several
      -- groups can reach one CR 117.5 scan. Collapsing the scan would be the same
      -- bug in the other direction.
      --
      -- matchesTriggerGiven cannot do this itself: it sees one event at a time and
      -- so answers alike for both readings (`batchScoped` above states that
      -- contract), which is why every gatherer owes the predicate a consultation;
      -- see #2384.
      occurrences entry
        | batchScoped (TriggeredAbility.condition (DelayedTrigger.ability entry)) = fmap NonEmpty.head (eventGroups (matching entry))
        | otherwise = matching entry
      -- Which entries CR 603.7b's second sentence actually ASKS, answered without
      -- asking: the CR 101.4c ordering below has to know before the first
      -- question is raised. The three gates are firedBy's own, read through this
      -- one predicate rather than restated -- a stated duration lifts the rule
      -- entirely, a reflexive entry never reaches firedBy, and one candidate is
      -- no choice.
      asks entry =
        not (reflexive entry)
          && Maybe.isNothing (DelayedTrigger.expiry entry)
          && case eventGroups (occurrences entry) of
            block : _ -> not (null (NonEmpty.tail block))
            [] -> False
      -- CR 603.7b's exception, read through CR 603.2c. A stated duration lifts the
      -- one shot, and 603.2c then applies unmodified -- "it can trigger repeatedly
      -- if one event contains multiple occurrences" -- so every occurrence in the
      -- batch fires the entry once. Centaur Peacemaker's "each player gains 4 life"
      -- is that batch for False Cure, and Pawl.EventTriggerSpec's three-seat board
      -- proves the count. What an OCCURRENCE is is `occurrences` above: three for
      -- False Cure's per-seat condition on that batch, one for Forth Eorlingas!'s
      -- batch condition on a combat damage step where two of its tokens connect.
      --
      -- Without a duration, CR 603.7b's two sentences read as two steps over the
      -- GROUPED batch. The first sentence -- "the next time its trigger event
      -- occurs" -- commits the entry to the EARLIEST Pawl.Types.EventGroup that
      -- holds an occurrence, so taking the first block stays right. The second
      -- then decides WITHIN that group: occurrences sharing one group are
      -- simultaneous (CR 608.2f), and "the controller of the delayed triggered
      -- ability chooses which event causes the ability to trigger". A batch of
      -- simultaneous occurrences is producible: Pawl.Engine.Resolve's life arms
      -- share one EventGroup across the seats they name, which is what Synthetic
      -- Singular Cure reads in Pawl.EventTriggerSpec.
      --
      -- That second sentence turns on "its trigger event occurs MORE THAN ONCE",
      -- so a batch-scoped condition never reaches it: `occurrences` has already
      -- left one event per group, the block below is a singleton, and the prompt
      -- is not raised. Asking would be a question the rule does not authorise.
      --
      -- Adjacency AND tag equality, which coincide by construction: `eventGroups`
      -- above is where that argument lives, and both scans read that one function
      -- rather than each cutting the log its own way.
      --
      -- One candidate is not a choice, so no prompt is raised -- which is every
      -- board but a PER-OCCURRENCE condition's batch. A batch-scoped one is never
      -- a choice, `occurrences` having left the block a singleton.
      firedBy entry
        | Maybe.isJust (DelayedTrigger.expiry entry) = pure (fmap LoggedEvent.event (occurrences entry))
        | otherwise = case eventGroups (occurrences entry) of
            [] -> pure []
            block : _ ->
              let candidates = fmap LoggedEvent.event block
               in if not (asks entry)
                    then pure [NonEmpty.head candidates]
                    else do
                      let controller = DelayedTrigger.controller entry
                      -- Filtered rather than trusted: every candidate offered
                      -- already matched the entry's condition, and an answer off
                      -- the end falls back to the earliest.
                      answer <- Game.choose (Prompt.ChooseDelayedTriggerEvent (Decide.deciderFor controller gs) controller (DelayedTrigger.source entry) candidates)
                      pure [Replacement.at (NonEmpty.toList candidates) answer (NonEmpty.head candidates)]
      pend entry event =
        PendingTrigger.MkPendingTrigger
          (TriggerSource.OfObject (DelayedTrigger.source entry))
          (DelayedTrigger.controller entry)
          (DelayedTrigger.ability entry)
          -- CR 603.2's event slots over CR 603.7c's captured environment, the way an
          -- object's trigger gets them in eventTriggers -- False Cure's "that
          -- player" is the seat that just gained, not one the arming spell named.
          -- Map.union is left-biased, so a name the arming environment happens to
          -- share is read as THIS firing's, which is what the printed word means.
          -- Nothing for CR 400.7f's bearer arrival: this scan looks for events a
          -- delayed entry watches, not for a bearer's own departure, and the
          -- entry's captured environment (CR 603.7c) is where what it knows about
          -- its own object comes from. The empty arrival table beside it for the
          -- same reason -- this scan gathers no batch -- which is why CR 400.7e's
          -- new object is claimed by eventBindingSlotsSometimes and not by the
          -- floor wherever eventBindings reads that table.
          (Map.union (eventBindings gs Nothing Map.empty (DelayedTrigger.controller entry) (TriggeredAbility.condition (DelayedTrigger.ability entry)) event) (DelayedTrigger.bindings entry))
          -- CR 603.7a: what tells the ability this becomes apart from one its
          -- source simply has, once it is on the stack.
          (Just (DelayedTrigger.createdAt entry))
      store = GameState.delayedTriggers gs
      -- CR 603.12's exception to all of the above, and the ONE place the reflexive
      -- form differs from an ordinary CR 603.7 entry: it is "checked immediately
      -- after being created" and triggers "based on whether the trigger event or
      -- events occurred earlier during the resolution of the spell or ability
      -- that created them" -- which is a question about the resolution that is
      -- over, not about this batch's log. Pawl.Engine.Resolve appends such an
      -- entry only where that question was answered yes -- from the CR 118.12
      -- pay-gate branch that actually ran, and from
      -- Resolve.applyClauseEffects, which skips the arm when the preceding
      -- instruction recorded no event (CR 603.12, 701.28e) -- so the entry's
      -- EXISTENCE is the affirmative answer and no event is needed. It
      -- therefore fires at the first gather after it was armed, which CR 603.3
      -- makes the next time a player would receive priority.
      --
      -- `armed` still gates it, so the data cannot say two things at once: an
      -- onset would name a turn CR 603.12's "immediately" has already denied, and
      -- the entry would sit unfired rather than firing early. No card can reach
      -- that -- CardSpec's onset lint needs a controller-scoped condition and this
      -- one is not -- so the guard is a fence, not a live branch.
      reflexive entry = TriggeredAbility.condition (DelayedTrigger.ability entry) == TriggerCondition.Reflexive
      -- One firing, whatever the batch holds. CR 603.12a's second sentence wants
      -- exactly that for a cost payable several times, and `spent` below retires
      -- the entry immediately after, an expiry-less entry having CR 603.7b's one
      -- shot. Not implemented: that rule's FIRST sentence, once per occurrence
      -- (#2121).
      bare entry =
        PendingTrigger.MkPendingTrigger
          (TriggerSource.OfObject (DelayedTrigger.source entry))
          (DelayedTrigger.controller entry)
          (DelayedTrigger.ability entry)
          (DelayedTrigger.bindings entry)
          -- CR 603.12: a reflexive ability follows CR 603.7, so it carries the
          -- creation moment too. No card exercises the pairing -- nothing
          -- reflexive transforms -- so this is CR 701.27f as the rule states it
          -- rather than a behaviour a test pins.
          (Just (DelayedTrigger.createdAt entry))
      -- CR 603.2 plus CR 603.4: the event matched AND the intervening "if" held,
      -- which together are what "triggered" means. Per occurrence, since CR 603.4
      -- asks about the moment the event occurs. AFTER firedBy rather than inside
      -- it, so an entry with no stated duration still commits -- to the earliest
      -- group, and within it to the occurrence CR 603.7b's chooser named -- and
      -- then triggers or does not: this docstring's CR 603.4 paragraph has the
      -- entry survive an occurrence whose "if" was false, not skip past it to a
      -- later one in the same batch. Nor is the choice narrowed to the
      -- occurrences whose "if" holds: CR 603.7b gives the controller every event
      -- that OCCURRED, and CR 603.4 then answers for the one they picked.
      triggered entry
        | reflexive entry = pure (filter (interveningHolds gs) [bare entry | armed entry])
        | otherwise = fmap (filter (interveningHolds gs) . fmap (pend entry)) (firedBy entry)
      -- Triggering spends the one shot only for an entry with no stated duration.
      spent (entry, fired) = not (null fired) && Maybe.isNothing (DelayedTrigger.expiry entry)
   in do
        -- One pass, because `triggered` can ASK: the prompt must be raised once
        -- per entry, and `spent` reads the answer rather than recomputing it.
        --
        -- CR 101.4: two entries with different controllers matched by one batch
        -- are choices made at the same time, so the active player is asked first
        -- and the nonactive players follow in turn order. CR 101.4b is what makes
        -- that order load-bearing rather than cosmetic -- the later chooser knows
        -- what the earlier one named. The seat is the ENTRY's controller (CR
        -- 603.7d-f), which travels with the entry, and not Decide.deciderFor's
        -- decider, whom a control-changing effect can make someone else.
        --
        -- Replacement.seatOf is the shared answer to "how far down APNAP order",
        -- including its fallback: a controller off the seating roster sorts last.
        -- A STABLE sort, so entries sharing a controller reach `ordering` in
        -- arming order, which is where CR 101.4c takes over.
        --
        -- CR 101.4c: within one controller's share, the order the questions are
        -- asked in is itself that player's choice, so it is asked as CR 603.3b's
        -- Prompt.OrderTriggers over the entries that will ask. Only where TWO or
        -- more of theirs ask -- one question has no order to pick, and the rules
        -- authorise no prompt then -- and only over those, an entry that raises
        -- no question having nothing to order. The rest follow in arming order;
        -- their position is unobservable, `asks` being false exactly where
        -- `triggered` cannot ask.
        --
        -- Per controller and not per phase, so that a player makes ALL of their
        -- choices before the next player makes any (CR 101.4, CR 101.4b).
        --
        -- The ASKING is all that is reordered. `outcomes` is reassembled in store
        -- order, because CR 101.4 governs neither of the two things it feeds: the
        -- pending list is CR 603.3b's to order, which Engine.orderPending does,
        -- and the surviving store's order is the arming order every later pass
        -- reads. Pawl.EventTriggerSpec's "the store still holds it" is what pins
        -- that second half.
        let seated = List.sortOn (Replacement.seatOf gs . DelayedTrigger.controller . snd) (zip [0 :: Int ..] (Foldable.toList store))
            controllers = List.nub (fmap (DelayedTrigger.controller . snd) seated)
            -- CR 603.3b's entry, built from a STORE entry: the question is asked
            -- before the entry has fired, so the PendingTrigger that
            -- Pawl.Engine.Engine.entryOf reads does not exist yet.
            entryOf entry = TriggerEntry.MkTriggerEntry (TriggerSource.OfObject (DelayedTrigger.source entry)) (DelayedTrigger.ability entry)
            ordering pid =
              let mine = filter ((pid ==) . DelayedTrigger.controller . snd) seated
                  asking = filter (asks . snd) mine
               in if length asking < 2
                    then pure mine
                    else do
                      -- Filtered rather than trusted: Game.permute keeps the
                      -- offered order for an answer that is not a permutation.
                      answer <- Game.choose (Prompt.OrderTriggers (Decide.deciderFor pid gs) pid (fmap (entryOf . snd) asking))
                      pure (Game.permute asking answer <> filter (not . asks . snd) mine)
            asked (index, entry) = fmap (\fired -> (index, (entry, fired))) (triggered entry)
        answered <- fmap concat (traverse (ordering Monad.>=> traverse asked) controllers)
        let outcomes = Seq.fromList (fmap snd (List.sortOn fst answered))
        pure (concatMap snd outcomes, fmap fst (Seq.filter (not . spent) outcomes))

-- CR 603.4: the ability doesn't trigger at all when its intervening "if" is false
-- as the trigger event occurs. Checked at the gather rather than at placement,
-- because "doesn't trigger" must be indistinguishable from "no ability existed",
-- including to the CR 117.5 settle loop's re-run flag.
--
-- A SOURCELESS pending trigger never reaches this -- gatherTriggers and
-- delayedPending are the only callers, and all three gatherers hang their
-- triggers on an object, the inherent ones being merged in afterwards by
-- Pawl.Engine.Engine. The arm answers True
-- rather than failing because an inherent ability's own gatherer owns CR 603.4:
-- rule 725.2's pair has no intervening "if" at all, and CR 702.179d's does,
-- checked inside Pawl.Engine.Speed.inherentPending. EVERY such gatherer owns its
-- own check -- Pawl.Engine.Monarch, Pawl.Engine.Initiative, Pawl.Engine.Speed and
-- Pawl.Engine.Rad each have one, and a further one would too; there is no subject
-- object to hand this function, so routing one here
-- would mean giving Condition.holds the ability object Pawl.Engine.Stack's CR
-- 608.2a re-check uses, which does not exist until placement.
--
-- CR 608.2h supplies the view rather than fullView, which for a look-back trigger
-- is the difference between reading the clause and reading nothing: CR 603.10a
-- makes the source the permanent as it was immediately before the event, whose id
-- CR 400.7 has since deleted -- and fullView describes a deleted id as an object
-- with no characteristics, quietly answering False to every clause. Stack's CR
-- 608.2a re-check reads the same way, and the two must agree or a trigger would be
-- placed and then removed for disagreeing with itself.
--
-- The trigger's own bindings ride in as the context's slot objects, which is
-- what lets CR 702.100a's "if THAT CREATURE's power is greater" read the
-- entrant through Quantity.AgainstSlot rather than the bearer: an intervening
-- "if" may be about the event's object and not only about the source. Stack's
-- re-check reads the same slots off the placed ability, for the reason the view
-- above must match.
--
-- Which is why the view is the UNSCOPED viewWithLastKnownAnywhere: CR 608.2h is
-- owed to every object the clause reads, and a slot naming an object that has
-- since left would otherwise be described as one with no characteristics.
--
-- Nothing OBSERVES that at this end of the rule, and scoping the view back to the
-- source here leaves the suite green: evolve and Breathless Knight are the two
-- abilities whose "if" reads a slot, and both entrants are on the battlefield by
-- construction while their own entry is being gathered. So this is a fence keeping
-- the two checks reading alike, not a proved behaviour -- the proved one is
-- Stack's re-check.
interveningHolds :: GameState -> PendingTrigger -> Bool
interveningHolds gs pending =
  case (TriggeredAbility.intervening (PendingTrigger.ability pending), PendingTrigger.source pending) of
    (Nothing, _) -> True
    (Just _, TriggerSource.Sourceless) -> True
    (Just cond, TriggerSource.OfObject oid) ->
      Condition.holds
        (Projection.viewWithLastKnownAnywhere gs)
        -- CR 303.4b's host rides in beside the slots, for the reason they do:
        -- Ray of Frost's "if enchanted creature is red" is about the SOURCE's
        -- attachment rather than about the event, and Stack's CR 608.2a re-check
        -- supplies the same field so the two checks cannot disagree.
        ((Filter.contextWithSlots (Game.teams gs) (Just (PendingTrigger.controller pending)) (Just oid) (Binding.slotObjects (PendingTrigger.bindings pending))) {Filter.sourceAttachedTo = Projection.hostOf oid gs})
        gs
        oid
        cond
