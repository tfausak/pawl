-- | CR 310, battles: the defense a battle enters with (CR 310.4b), the protector
-- designated as it enters (CR 310.8a / 310.11a) together with the state-based
-- actions that repair that designation (CR 704.5w / 704.5x), the intrinsic ability
-- rule 310.11b gives a Siege -- both halves of its sentence -- and the state-based
-- action rule 310.7 / 704.5v performs on a battle at defense 0.
--
-- Pawl.Engine.Saga's sibling, and kept apart from Pawl.Engine.Sba for the reason
-- that module gives: Sba owns WHEN a state-based action is checked, not what any
-- one of them means.
--
-- Casing on Subtype.Siege here is casing on the RULEBOOK, exactly as
-- Pawl.Engine.Saga cases on Subtype.Saga: rule 310.11 is as much a part of the
-- comprehensive rules as rule 704. Nothing here asks which EFFECT a battle's text
-- carries -- only which battle type its type line prints.
--
-- Imports no Pawl.Engine.Projection: every function here takes the projection it
-- needs as an argument, so a caller that has already projected does not pay for a
-- second one. NOT Saga's reason -- rule 714's module is imported BY Projection,
-- which calls Saga.entryReplacementsOf, so a dependency the other way really would
-- be a cycle there. Nothing in Projection calls this module: CR 310.4b's clause in
-- intrinsicReplacementsOf tests `Set.member CardType.Battle` inline, exactly as CR
-- 306.5b's planeswalker clause beside it tests its own card type.
--
-- What a protector is FOR lives in Pawl.Engine.Combat, not here: CR 310.5's
-- attackable battle (Combat.attackableBattles), CR 310.8b's "any attacking player
-- for whom its protector is a defending player", and CR 310.8d with CR 508.5
-- (Combat.defendingPlayerOf). This module owns the designation; that one owns what
-- reads it.
--
-- CR 310.6's damage removing defense counters is NOT here: rule 120.3h is one arm
-- of CR 120.3 among several, and it lives beside the others in Pawl.Engine.Damage.
-- What this module owns is what the rest of rule 310 makes of the result.
--
-- CR 310.11b's sentence is whole here, but only its first half is rule 310: the
-- second half is three rules this module names and does not own -- CR 608.2g's
-- cast during a resolution, CR 712.11a's transformed face and CR 118.9's
-- alternative cost -- carried as an Effect.OfferCast the DSL states and
-- Pawl.Engine.Resolve performs.
module Pawl.Engine.Battle where

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Types.AttackTarget as AttackTarget
import Pawl.Types.Card (Card)
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.CastOffer as CastOffer
import qualified Pawl.Types.Clause as Clause
import qualified Pawl.Types.Combat as Combat
import qualified Pawl.Types.CounterKind as CounterKind
import qualified Pawl.Types.Effect as Effect
import qualified Pawl.Types.EntryRiders as EntryRiders
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.LibraryPlacement as LibraryPlacement
import qualified Pawl.Types.Modal as Modal
import qualified Pawl.Types.Mode as Mode
import qualified Pawl.Types.ModeSelection as ModeSelection
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.ObjectRef as ObjectRef
import qualified Pawl.Types.Optionality as Optionality
import qualified Pawl.Types.PlayerId as PlayerId
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.Subtype as Subtype
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import Pawl.Types.TriggeredAbility (TriggeredAbility)
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.Zone as Zone

-- CR 310: is this object a battle? Read off the PROJECTED card types, so a
-- permanent that became one is judged as one and a paper-only one is not -- the
-- posture Projection.intrinsicReplacementsOf takes for CR 306.5b.
isBattle :: PC.ProjectedCharacteristics -> Bool
isBattle = Set.member CardType.Battle . PC.cardTypes

-- CR 310.8 / 310.8e: the player designated as this battle's protector, or Nothing
-- when nobody is -- which CR 310.10 makes a state-based action rather than a
-- steady state, so a Nothing here is a board mid-repair or an object that is no
-- battle at all.
--
-- STORED and read live rather than derived, for the reason Object.protector gives:
-- CR 310.8a's choice is made once, as the battle enters, and CR 310.8f is what
-- moves it afterwards. Every reader asks it fresh, so a designation that moves
-- while an attack stands moves the defending player with it (CR 310.8d).
protectorOf :: ObjectId.ObjectId -> GameState -> Maybe PlayerId.PlayerId
protectorOf oid gs = Object.protector =<< Game.lookupObject oid gs

-- CR 310.5 / CR 704.5w: is any attacking creature currently attacking this battle?
--
-- The LIVE record and not CR 508.8's historical Combat.attacked, because 704.5w
-- asks "currently": a creature removed from combat (CR 506.4) is deleted from this
-- map and stops attacking the battle, while the historical set keeps the entry
-- forever. That difference is the whole content of the rider.
--
-- Reads Pawl.Types.Combat directly rather than going through Pawl.Engine.Combat,
-- which imports THIS module for CR 310.5's attackable battles.
isBeingAttacked :: ObjectId.ObjectId -> GameState -> Bool
isBeingAttacked oid gs =
  List.elem (AttackTarget.OfBattle oid) (Map.elems (Combat.attackers (GameState.combat gs)))

-- CR 205.3q: the battle types this battle has, which is not the same question as
-- which subtypes it has -- a permanent that is a battle and a creature has
-- creature types too, and CR 310.8a asks only about the battle ones.
--
-- Siege is the whole list because CR 205.3q's list of battle types is ("that
-- battle type is Siege"); CR 310.11 says the different and weaker thing that every
-- battle currently existing HAS that subtype. A second battle type is a rulebook
-- change to 205.3q, and lands here.
battleTypes :: PC.ProjectedCharacteristics -> Set.Set Subtype.Subtype
battleTypes = Set.intersection (Set.singleton Subtype.Siege) . PC.subtypes

-- CR 310.8a: which players may be chosen as this battle's protector, "determined
-- by its battle type (see rule 310.11)".
--
-- Two clauses, and the fallback is the rule's own: a battle with NO battle types
-- has its controller become its protector (CR 310.8a's last sentence), and a Siege
-- must take an opponent of its controller (CR 310.11a, "only an opponent of a
-- Siege's controller can be its protector").
--
-- `playing` is the players still in the game, which the caller supplies as
-- Game.stillPlaying -- CR 704.5w asks for "no player IN THE GAME designated as its
-- protector", so a departed protector is not a candidate and not a legal one to
-- keep. The controller is filtered against it too, so a battle whose controller
-- has left offers nothing rather than offering a ghost.
--
-- Order is the caller's seating order, and the head is what an interpreter that
-- declines to answer gets (Replay.defaultAnswer). Nothing here is a choice the
-- engine makes: an empty list means the rules leave no legal protector, which CR
-- 310.10, CR 704.5w and CR 704.5x all answer by putting the battle into its
-- owner's graveyard.
protectorCandidates ::
  PC.ProjectedCharacteristics ->
  PlayerId.PlayerId ->
  [PlayerId.PlayerId] ->
  [PlayerId.PlayerId]
protectorCandidates pc controller playing
  | Set.null (battleTypes pc) = filter (== controller) playing
  | otherwise = filter (/= controller) playing

-- CR 704.5w / CR 704.5x: does this battle's protector designation need repairing?
--
-- CR 310.10 states both as one sentence -- "if a battle that isn't being attacked
-- has no player designated as its protector, OR ITS PROTECTOR IS A PLAYER WHO
-- CAN'T BE ITS PROTECTOR BASED ON ITS BATTLE TYPE, its controller chooses an
-- appropriate player to be its protector" -- and rule 704 splits them, which is
-- what forces the two arms below rather than one "the designated player is not
-- among protectorCandidates".
--
-- The split is the RIDER, and the two rules put it in different places. CR 704.5w
-- says "no player IN THE GAME designated as its protector AND no attacking
-- creatures are currently attacking that battle"; CR 704.5x, the Siege whose
-- controller is its own protector, carries no rider at all. Rule 704 governs where
-- it disagrees with 310.10's shorter statement, since 310.10's own last sentence
-- defers to it ("This is a state-based action (see rule 704)"). The disagreement
-- is unreachable besides: 704.5x needs a control-change effect that can name a
-- battle, and no card in the pool has one (#853).
--
-- `attacked` is Battle.isBeingAttacked at the call. The rider suspends the
-- re-choice rather than cancelling it: Gatherer's ruling is that the controller
-- chooses a new protector once no creatures are attacking it, and that meanwhile
-- the battle "continues to be attacked and can be dealt combat damage as normal".
-- Suspension is exactly what a state-based action re-asked every check does.
--
-- Between them CR 310.10 is still the wider condition, and that width lands in the
-- second arm: an illegal protector who is neither absent nor the controller is
-- named by neither 704 rule, and protectorCandidates answers it.
needsProtector ::
  PC.ProjectedCharacteristics ->
  PlayerId.PlayerId ->
  [PlayerId.PlayerId] ->
  Bool ->
  Maybe PlayerId.PlayerId ->
  Bool
needsProtector pc controller playing attacked designated = case designated of
  -- CR 704.5w, both conjuncts.
  Nothing -> not attacked
  Just pid
    -- CR 704.5w again: a designated player who has LEFT is no player in the game
    -- designated as its protector, so the rider covers this too.
    | notElem pid playing -> not attacked
    -- CR 704.5x, and CR 310.10's second clause. No rider.
    | otherwise -> notElem pid (protectorCandidates pc controller playing)

-- CR 310.8a: ask the battle's controller who protects it, and answer with their
-- pick. Nothing means the rules offer no legal protector, which both callers
-- answer by putting the battle into its owner's graveyard (CR 704.5w, CR 704.5x).
--
-- The one place the question is asked, shared by CR 310.8a's as-enters route
-- (Pawl.Engine.Event's EntryRewrite.ChooseProtector arm) and CR 704.5w/704.5x's
-- state-based re-choice (Pawl.Engine.Sba). Sharing it is what keeps the candidate
-- rule in one place: a re-choice must offer exactly what the entry choice offered.
--
-- Elided at one candidate, and the ANSWER is still the same: one candidate is one
-- outcome, so the options are indistinguishable and the engine decides nothing by
-- not asking. See Prompt.ChooseProtector.
--
-- Filters rather than trusts the answer, the posture Combat.chooseDefender and
-- Sba.chooseLegendVictims both take: an interpreter that names a player who is not
-- a candidate gets the head of the list instead of an illegal designation.
designateProtector ::
  PC.ProjectedCharacteristics ->
  PlayerId.PlayerId ->
  ObjectId.ObjectId ->
  Game (Maybe PlayerId.PlayerId)
designateProtector pc controller oid = do
  gs <- State.get
  case NonEmpty.nonEmpty (protectorCandidates pc controller (Game.stillPlaying gs)) of
    Nothing -> pure Nothing
    Just candidates
      | null (NonEmpty.tail candidates) -> pure (Just (NonEmpty.head candidates))
      | otherwise -> do
          let decider = Decide.deciderFor controller gs
          answer <- Game.choose (Prompt.ChooseProtector decider controller oid candidates)
          pure . Just $
            if List.elem answer (NonEmpty.toList candidates)
              then answer
              else NonEmpty.head candidates

-- CR 310.11b: "Sieges have the intrinsic ability 'When the last defense counter is
-- removed from this permanent, exile it, then you may cast it transformed without
-- paying its mana cost.'"
--
-- MINTED here and handed to the ordinary CR 603 machinery, exactly as
-- Pawl.Engine.Keyword mints rule 702.70a's poisonous and rule 702.91a's battle cry:
-- the rules give this ability, no card prints it, and every reader downstream gets
-- an ordinary TriggeredAbility that never learns rule 310 produced it. That is what
-- keeps CR 704.5v's "isn't the source of an ability that has triggered but not yet
-- left the stack" honest -- the exemption is about a real ability on a real stack,
-- not about a special case in the state-based action.
--
-- Gated on the battle TYPE and not on the card type: rule 310.11b says "Sieges",
-- and a battle with no battle types (CR 310.8a's other branch) has no such ability.
-- `battleTypes` above is where that gate is stated once.
--
-- Single mode, one Mandatory clause, no targets, ChooseExactly 1: rule 310.11b's
-- exile is not optional. The "you may" governs only the casting that follows it,
-- and it is the OFFER's own prompt (Prompt.OfferedCast) rather than CR 603.5
-- optionality because this "may" is the RULE's, printed on no card -- so there
-- is no printed clause for it to ride. Splitting the exile and the cast into two
-- clauses would be the wrong shape for the same reason.
--
-- TWO effects for one sentence, joined by a slot, because CR 400.7 makes the two
-- "it"s two objects: the permanent that was on the battlefield is exiled, and
-- what may then be cast is the card the move minted in exile. Binding.became is
-- that name -- the reserved slot for the incarnation a card became -- and
-- Pawl.Engine.Resolve reads it live, since a resolving ability's target map is
-- fixed before its effect fold begins.
siegeDefeat :: TriggeredAbility Card
siegeDefeat =
  TriggeredAbility.MkTriggeredAbility
    { TriggeredAbility.condition = TriggerCondition.SelfLastCounterRemoved CounterKind.Defense,
      TriggeredAbility.modal =
        Modal.MkModal
          (Seq.singleton (Mode.MkMode (Seq.singleton (Clause.MkClause Nothing Optionality.Mandatory Nothing (Seq.fromList [exile, offer]))) Map.empty))
          (ModeSelection.ChooseExactly 1),
      TriggeredAbility.intervening = Nothing
    }
  where
    -- "Exile it": the permanent the ability triggered from, which CR 113.7 binds
    -- under Binding.triggerSource. No entry riders and no stated origin zone --
    -- the ability functions on the battlefield, where the permanent already is --
    -- and Binding.became for the exiled incarnation the next effect names.
    exile =
      Effect.MoveToZone
        (ObjectRef.InSlot Binding.triggerSource)
        Zone.Exile
        EntryRiders.MkEntryRiders {EntryRiders.tapped = TapState.Untapped, EntryRiders.attacking = False, EntryRiders.transformed = False}
        (Just Binding.became)
        Nothing
        LibraryPlacement.defaultValue
    -- "then you may cast it transformed without paying its mana cost": CR 608.2g's
    -- cast during a resolution, with CR 712.11a's face rider and CR 118.9's
    -- alternative cost. Both riders come from the OFFER, so nothing downstream
    -- learns that rule 310.11b is what wrote them.
    offer =
      Effect.OfferCast
        Binding.became
        CastOffer.MkCastOffer {CastOffer.transformed = True, CastOffer.withoutPayingManaCost = True}

-- The intrinsic triggered abilities rule 310 gives a permanent, read off the
-- finished projection. Pawl.Engine.Keyword.triggeredAbilitiesOf's sibling, and
-- consumed at the same one place: Pawl.Engine.Event's event scan.
--
-- Off the PROJECTION rather than off the printed type line, so a permanent that
-- became a Siege has the ability and a Siege that stopped being one does not (CR
-- 613.1d, CR 613.1f). That differs from CR 310.4b's intrinsic replacement, which
-- Projection mints after the layer fold and so puts out of LoseAllAbilities' reach:
-- rule 310.4b hangs off the CARD TYPE, where 310.11b hangs off an ability the rules
-- grant, and layer 6 removes abilities.
triggeredAbilitiesOf :: PC.ProjectedCharacteristics -> [TriggeredAbility Card]
triggeredAbilitiesOf pc = [siegeDefeat | Set.member Subtype.Siege (battleTypes pc)]

-- CR 122.1g / 310.4c: the defense counters on a permanent. Zero for an object the
-- game does not hold, the answer Saga.loreOn gives for rule 714.3.
defenseOn :: ObjectId.ObjectId -> GameState -> Natural
defenseOn oid gs = case Game.lookupObject oid gs of
  Nothing -> 0
  Just obj -> Map.findWithDefault 0 CounterKind.Defense (Object.counters obj)

-- CR 704.5v's exemption: is this battle "the source of an ability that has
-- triggered but not yet left the stack"?
--
-- Saga.awaitingChapter's twin, and TWO halves for its reason: the rule says
-- "triggered" and not "is on the stack", and the engine has a window where exactly
-- that is true -- Engine.performSettle runs the CR 704.5 pass BEFORE
-- placePendingTriggers, so a battle taken to defense 0 by combat damage meets this
-- check with CR 310.11b's ability still in the unscanned event log. Without the
-- second half the battle would be buried first and its own defeat ability would
-- resolve without it, sending it to the GRAVEYARD where rule 310.11b exiles it.
-- That is the observable wrong answer this rider exists to prevent.
--
-- The stack half compares OBJECT IDS, for awaitingChapter's reason: CR 400.7 mints
-- a fresh id on every zone change, so a battle that flickered is not the source of
-- the old one's ability.
--
-- The stack half is rule 704.5v's own width -- ANY ability, where rule 704.5s says
-- "a chapter ability". The PENDING half is narrower than that: it recognizes only
-- CR 310.11b's own trigger, because reading a general "would any of this
-- permanent's abilities fire on any unscanned event" means the CR 603 matcher,
-- which lives above this module. NOT IMPLEMENTED: a battle at defense 0 owing some
-- OTHER triggered ability that has fired and not yet been placed (#902).
--
-- The unscanned events arrive as an ARGUMENT for awaitingChapter's reason:
-- Pawl.Engine.Event owns the watermark and this module sits below it.
awaitingAbility :: [GameEvent.GameEvent] -> GameState -> ObjectId.ObjectId -> Bool
awaitingAbility events gs oid =
  let onStack sid = case fmap Object.source (Game.lookupObject sid gs) of
        Just (Source.OfTrigger srcId _) -> srcId == oid
        _ -> False
      -- CR 310.11b's condition, matched exactly as Event.matchesTrigger matches it:
      -- an unscanned removal on this permanent that took its last defense counter.
      pending event = case event of
        GameEvent.CountersRemoved target CounterKind.Defense _ after -> target == oid && after == 0
        _ -> False
   in any onStack (GameState.stack gs) || any pending events

-- CR 310.7 / CR 704.5v: the battles put into their owners' graveyards -- defense 0,
-- and not the source of an ability still owed a resolution. The state-based
-- action's CLASSIFIER half, taking the pre-pass projection so Pawl.Engine.Sba can
-- judge it against the same board as every other CR 704.5 clause (CR 704.3).
--
-- The card-type guard is load-bearing in Sba.zeroLoyalty's direction rather than in
-- zeroToughness's: Object.counters is keyed by kind for EVERY permanent, so absent
-- it every creature on the battlefield would read as defense 0. CR 122.1g confines
-- the reading to battles.
--
-- A battle that never received counters reads 0 here and is buried, which is the
-- rule and not an accident: CR 310.4b gives a battle its counters as it enters.
--
-- For a SIEGE this is normally unreachable, and that is CR 704.5v's whole design:
-- the counters hitting 0 fires CR 310.11b, the exemption holds the battle on the
-- battlefield while that ability resolves, and the ability exiles it. What reaches
-- this clause is a battle with no defeat ability to fire -- one with no battle types
-- (CR 310.8a's other branch), or a Siege whose ability layer 6 removed.
--
-- Ascending, for Saga.sacrificing's reason.
defeated :: Map.Map ObjectId.ObjectId PC.ProjectedCharacteristics -> [GameEvent.GameEvent] -> GameState -> [ObjectId.ObjectId]
defeated pcs events gs =
  let gone oid = case Map.lookup oid pcs of
        Nothing -> Nothing
        Just pc
          | isBattle pc,
            defenseOn oid gs == 0,
            not (awaitingAbility events gs oid) ->
              Just oid
          | otherwise -> Nothing
   in Maybe.mapMaybe gone (Set.toAscList (GameState.battlefield gs))
