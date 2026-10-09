-- | Whether two objects are the same option -- the question a prompt has to
-- answer before it may offer one of them in place of both.
--
-- The engine never makes a player's choice, so an elision is legitimate only
-- where the options are genuinely indistinguishable. CR 732.2a puts a shortcut
-- in the hands of the player with priority rather than the game, so nothing in
-- the rules AUTHORISES this; what it does say is that a shortcut is sound only
-- when its results are predictable, which is the bar met here.
module Pawl.Engine.Interchangeable where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Pawl.Engine.Filter as Filter.Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Interchangeable.Mentions as Mentions
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Projection.View as Projection.View
import qualified Pawl.Types.AbilityTriggered as AbilityTriggered
import qualified Pawl.Types.ActiveActivationProhibition as ActiveActivationProhibition
import qualified Pawl.Types.ActiveAttackProhibition as ActiveAttackProhibition
import qualified Pawl.Types.ActiveAttackRequirement as ActiveAttackRequirement
import qualified Pawl.Types.ActiveBlockProhibition as ActiveBlockProhibition
import qualified Pawl.Types.ActiveBlockRequirement as ActiveBlockRequirement
import qualified Pawl.Types.ActiveCopy as ActiveCopy
import qualified Pawl.Types.ActiveEvasion as ActiveEvasion
import qualified Pawl.Types.ActivePlayerEffect as ActivePlayerEffect
import qualified Pawl.Types.ActiveUnregeneratable as ActiveUnregeneratable
import qualified Pawl.Types.ActiveUntapProhibition as ActiveUntapProhibition
import qualified Pawl.Types.Affected as Affected
import qualified Pawl.Types.AimedAt as AimedAt
import qualified Pawl.Types.AimedPlayers as AimedPlayers
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.AttackerDeclared as AttackerDeclared
import qualified Pawl.Types.Binding as Binding
import qualified Pawl.Types.Card as Card
import qualified Pawl.Types.CardIdentity as CardIdentity
import qualified Pawl.Types.Combat as Combat
import qualified Pawl.Types.ContinuousEffect as ContinuousEffect
import qualified Pawl.Types.ExileLink as ExileLink
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.IgnoredAbility as IgnoredAbility
import qualified Pawl.Types.MonarchWatch as MonarchWatch
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.ObjectSnapshot as ObjectSnapshot
import qualified Pawl.Types.PastActivation as PastActivation
import qualified Pawl.Types.PhasedOut as PhasedOut
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.RestrictedCreatures as RestrictedCreatures
import qualified Pawl.Types.ReturnWatch as ReturnWatch
import qualified Pawl.Types.Timestamp as Timestamp
import qualified Pawl.Types.TriggerSource as TriggerSource

-- | Whether choosing between these two objects is choosing between options the
-- game cannot tell apart. Takes the projection rather than computing it, so a
-- caller enumerating candidates pays for one Pawl.Engine.Projection.projectAll
-- and not one per pair.
--
-- CONSERVATIVE IN FOUR WAYS, because a wrong answer here silently makes a
-- player's choice for them.
--
--   * The objects are compared WHOLE, by Object's derived Eq over every field
--     but the timestamp and an identity's serial. A field added to that record
--     is therefore compared by default, and can only ever make this answer
--     False -- the opposite posture to a hand-kept list of fields that must
--     agree, which a new field would leave unread. The timestamp and the serial
--     are the two fields two distinct objects never share; what reads the
--     timestamp is layeredBetween's question below.
--   * The projections must agree, which is what an Equipment or an Aura shows up
--     in -- Bonesplitter's +2/+0 makes one Llanowar Elves a 3/1 and the other a
--     1/1. So must what their two Filter views read off the turn's log (CR
--     608.2i: attacked, dealt damage, entered this turn), so no Filter anywhere
--     -- printed or stored -- answers a look-back atom differently of the two.
--   * Nothing else may name either object: no other object (namedByAnother),
--     no id-keyed relation (namedByRelation), and no stored row or combat
--     assignment (namedByStored). Each of those SEARCHES for the two ids, so a
--     row about some third object leaves the pair alike.
--   * The board must be QUIET: the entry bookkeeping, which no traversal here
--     reads, is required EMPTY instead.
objects :: Map.Map ObjectId PC.ProjectedCharacteristics -> GameState -> ObjectId -> ObjectId -> Bool
objects pcs gs a b =
  let sameObject = case (Map.lookup a (GameState.objects gs), Map.lookup b (GameState.objects gs)) of
        -- An identity's serial is bookkeeping no rule reads; its starting owner
        -- is not, since Ante.ownershipChanges tells two cards apart by it.
        (Just x, Just y) ->
          x {Object.timestamp = Object.timestamp y, Object.identity = Object.identity y} == y
            && fmap CardIdentity.startingOwner (Object.identity x) == fmap CardIdentity.startingOwner (Object.identity y)
        _ -> False
      grants = Projection.View.controlGrants gs
      viewOf oid = Projection.viewOfObjectGiven pcs grants oid gs
      -- Every View field Pawl.Engine.Projection.View fills from the turn's log
      -- (CR 608.2i). Hand-kept: a new look-back field owes a line here.
      lookBack view =
        ( Filter.Engine.attackedThisTurn view,
          Filter.Engine.milledThisTurn view,
          Filter.Engine.dealtDamageThisTurn view,
          Filter.Engine.enteredThisTurn view,
          Filter.Engine.crewedThisTurn view,
          Filter.Engine.convokedThisTurn view,
          Filter.Engine.saddledThisTurn view
        )
      sameView = lookBack (viewOf a) == lookBack (viewOf b)
      -- Keyed by every permanent alike, so compared rather than searched.
      sameSample m = Map.lookup a m == Map.lookup b m
      sameSamples =
        sameSample (GameState.controlSample gs)
          && all sameSample (Map.elems (GameState.battlefieldWhenTriggered gs))
   in a == b
        || ( quiet gs
               && Map.lookup a pcs == Map.lookup b pcs
               && sameObject
               && sameView
               && sameSamples
               && not (namedByRelation a gs)
               && not (namedByRelation b gs)
               && not (namedByAnother a gs)
               && not (namedByAnother b gs)
               && not (namedByStored a gs)
               && not (namedByStored b gs)
               && not (layeredBetween a b gs)
           )

-- | One candidate per interchangeability class, keeping the FIRST of each in the
-- order it was offered. Deterministic, because Pawl.Engine.Replay.defaultAnswer
-- and several callers read the head of a candidate list.
representatives :: Map.Map ObjectId PC.ProjectedCharacteristics -> GameState -> NonEmpty.NonEmpty ObjectId -> NonEmpty.NonEmpty ObjectId
representatives pcs gs candidates =
  let keep kept oid = if any (objects pcs gs oid) kept then kept else kept <> [oid]
   in case List.foldl' keep [] (NonEmpty.toList candidates) of
        -- Unreachable: keep drops nothing it has not already got a class for.
        [] -> candidates
        first : rest -> first NonEmpty.:| rest

-- Whether the board stores no entry bookkeeping, the one thing this module
-- does not search.
--
-- The lists here, in namedByRelation and in namedByStored are HAND-KEPT, and
-- nothing forces a new GameState field into any of them, so the account below
-- covers the rest of the record. The fields NOT read anywhere in this module,
-- and why each is safe to leave unread:
--
--   * The zones and GameState.objects hold every object symmetrically, and what
--     one object says about another is namedByAnother's question.
--   * GameState.players, GameState.manaPool, GameState.pendingControl,
--     GameState.control (keyed by a player, and every PlayerControl on the stack
--     names a Decider, a lifetime and rule 723.7's restriction -- no object),
--     GameState.monarch, GameState.initiative, GameState.drewFromEmpty,
--     GameState.loopInvolvement, GameState.extraTurns and the per-player
--     counters (landsPlayed, drawsThisTurn, departedThisTurn,
--     spellsCastLastTurn, castsLastTurn, resolvedNames) are keyed by or valued
--     at a PLAYER and name no object.
--   * GameState.lastKnown is about objects that have LEFT, and so is
--     GameState.castsBeforeThisTurn: its spells were cast on an earlier turn,
--     and CR 500.2 ends no step while the stack holds one.
--     GameState.stackArchive is keyed by stack objects that have left too, and
--     its bindings are read only on behalf of a stack object or a delayed
--     trigger (CR 608.2h), which namedByAnother and namedByStored already
--     cover.
--   * GameState.events is read here through the two Filter views objects
--     compares, which is where a Filter's look-back atoms read it; an event
--     the choice itself writes names whichever object was chosen, alike.
--     GameState.controlSample (CR 603.2's control diff) and
--     GameState.battlefieldWhenTriggered (CR 603.10's look-back) are keyed by
--     every permanent alike, so objects COMPARES the two entries in each
--     rather than searching for the ids.
--   * GameState.outsideObjects (CR 729.4) is keyed by ids belonging to a game
--     this one is nested inside, and its value names no object -- an owner, a
--     printing and a face-up/face-down status (CR 110.5); the id supplies of
--     the two games are disjoint (Setup.funnelBack takes the max), so no key of
--     it can be a candidate.
--   * GameState.ambientAmounts is keyed by slot and valued at a number, and
--     GameState.referenceNames is keyed by a Filter and valued at names.
--   * The turn, phase, priority, result, daytime, signal, supply, printing,
--     event-group, scan-cursor, subgame and die-roll fields describe the GAME
--     rather than any object.
--
-- GameState.evasions is searched (namedByStored) although its rows name a
-- class rather than an object (CR 611.2c): the class is a Filter, and
-- filterNames answers for it.
--
-- GameState.enteringCounters, GameState.copyExceptionCounters,
-- GameState.enteringTogether, GameState.arrivals, GameState.detachedBindings
-- and GameState.broughtIn are listed below because each holds ObjectIds, and
-- NOT because any board reaches this with one of them non-empty: the first
-- two are empty outside an entry loop, the next two outside a CR 608.2f action
-- or an arrival, and the last two are CR 729's subgame bookkeeping. They are
-- in the list rather than in the account above because requiring a field empty
-- is the direction that cannot make a player's choice, so an unreachable row
-- is the cheap side to be wrong on.
quiet :: GameState -> Bool
quiet gs =
  Seq.null (GameState.broughtIn gs)
    && Set.null (GameState.enteringBeside gs)
    && Set.null (GameState.enteringSubjects gs)
    && Map.null (GameState.enteringPending gs)
    && Maybe.isNothing (GameState.refusedEntries gs)
    && Map.null (GameState.enteringCounters gs)
    && Map.null (GameState.copyExceptionCounters gs)
    && Map.null (GameState.detachedBindings gs)
    && Maybe.isNothing (GameState.enteringTogether gs)
    && Maybe.isNothing (GameState.arrivals gs)

-- Whether a stored row, or the combat in progress, MIGHT name this object.
-- "Might", because a slot read with no environment to search answers True --
-- the side that cannot make a player's choice.
--
-- Every row type is matched POSITIONALLY, so a field added to one is an arity
-- error here rather than an ObjectId this silently stops reading.
--
-- A row's SOURCE is read alongside its subject, since the source frames the
-- row's Filters (IsSource, the source comparisons) and a row whose source is
-- one of the two treats the pair differently through it.
--
-- The trees a row holds -- an effect, an ability, a condition, a cost -- are
-- read through Pawl.Engine.Interchangeable.Mentions. A replacement, a delayed
-- trigger and the three pending queues store the environment their slots read
-- (a pending entry effect reads none), which the same traversal searches, so
-- they are asked as Mentions.Searched;
-- every other row stores none, so a slot it reads might name anything
-- (Mentions.Unread).
--
-- Pawl.ManaSourceSpec proves the replacement arm (Mending Hands, through
-- DamagePattern's recipient) and the delayed-trigger arm (Salt Road Skirmish,
-- through its bindings). The pending queues are regression fences: no board
-- in the suite reaches a mana-source window with one non-empty.
namedByStored :: ObjectId -> GameState -> Bool
namedByStored oid gs =
  combatNames oid (GameState.combat gs)
    || any (continuousNames oid gs) (GameState.continuousEffects gs)
    || any (copyNames oid) (GameState.copyEffects gs)
    || any (playerEffectRowNames oid) (GameState.playerEffects gs)
    || any (blockRequirementNames oid) (GameState.blockRequirements gs)
    || any (attackRequirementNames oid) (GameState.attackRequirements gs)
    || any (unregeneratableNames oid) (GameState.unregeneratables gs)
    || any (blockProhibitionNames oid) (GameState.blockProhibitions gs)
    || any (attackProhibitionNames oid) (GameState.attackProhibitions gs)
    || any (activationProhibitionNames oid) (GameState.activationProhibitions gs)
    || any (untapProhibitionNames oid) (GameState.untapProhibitions gs)
    || any (evasionNames oid) (GameState.evasions gs)
    || any (ignoredNames oid) (GameState.ignoredAbilities gs)
    || any (Mentions.activeReplacementNames (searched oid)) (GameState.replacements gs)
    || any (Mentions.delayedTriggerNames (searched oid)) (GameState.delayedTriggers gs)
    || any (Mentions.preventionNames (searched oid)) (GameState.pendingPreventionRiders gs)
    || any (Mentions.pendingDamageEffectNames (searched oid)) (GameState.pendingDamageEffects gs)
    || any (Mentions.pendingEntryEffectNames (searched oid)) (GameState.pendingEntryEffects gs)

-- The question asked of a row that stores the environment its slots read --
-- searched alongside the row's trees, so a slot read is answered there.
searched :: ObjectId -> Mentions.Asking
searched oid = Mentions.MkAsking oid Mentions.Searched

-- The question asked of a row that stores no environment, so every slot read
-- might name the object.
unread :: ObjectId -> Mentions.Asking
unread oid = Mentions.MkAsking oid Mentions.Unread

-- CR 613.7a: an object's static abilities apply at its own timestamp, so a
-- stored layered row stamped strictly BETWEEN the two can apply after one's
-- abilities and before the other's -- and which of the two is left once the
-- other is gone can then change what the board projects. Conservative: asked
-- whether or not either object has a static ability at all.
layeredBetween :: ObjectId -> ObjectId -> GameState -> Bool
layeredBetween a b gs = case (Map.lookup a (GameState.objects gs), Map.lookup b (GameState.objects gs)) of
  (Just x, Just y) ->
    let lo = min (Object.timestamp x) (Object.timestamp y)
        hi = max (Object.timestamp x) (Object.timestamp y)
        between ts = lo < ts && ts < hi
     in any (between . ContinuousEffect.timestamp) (GameState.continuousEffects gs)
          || any (between . ActiveCopy.timestamp) (GameState.copyEffects gs)
  _ -> True

-- Whether the combat in progress names this object, as an attacker, a blocker
-- or an attacked planeswalker or battle (CR 506.4, 508, 509). Positional, so
-- a new Combat field is an arity error here.
combatNames :: ObjectId -> Combat.Combat -> Bool
combatNames oid combat = case combat of
  Combat.MkCombat attackers blockers struckFirst joinedUnder attackedUnder attackedControlledBy attacked declaredAttacked declaredAttackedBy declaredAttackedThisStep declaredAttackers declaredBlockers _blockersDeclared attackingNothing blockingNothing removedDefending _defenders _barred ->
    Map.member oid attackers
      || any (attackTargetNames oid) (Map.elems attackers)
      || Map.member oid blockers
      || any (Set.member oid) (Map.elems blockers)
      || maybe False (Set.member oid) struckFirst
      || Map.member oid joinedUnder
      || Map.member oid attackedUnder
      || Map.member oid attackedControlledBy
      || any (attackTargetNames oid) attacked
      || any (attackTargetNames oid) declaredAttacked
      || any (any (attackTargetNames oid)) (Map.elems declaredAttackedBy)
      || any (attackTargetNames oid) declaredAttackedThisStep
      || Set.member oid declaredAttackers
      || Set.member oid declaredBlockers
      || Set.member oid attackingNothing
      || Set.member oid blockingNothing
      || Map.member oid removedDefending

attackTargetNames :: ObjectId -> AttackTarget.AttackTarget -> Bool
attackTargetNames oid target = case target of
  AttackTarget.OfPlayer _player -> False
  AttackTarget.OfPlaneswalker walker -> walker == oid
  AttackTarget.OfBattle battle -> battle == oid

continuousNames :: ObjectId -> GameState -> ContinuousEffect.ContinuousEffect Card.Card -> Bool
continuousNames oid gs row = case row of
  ContinuousEffect.MkContinuousEffect source _timestamp expiry modification affected ->
    source == oid || Mentions.expiryNames (unread oid) expiry || Mentions.modificationNames (unread oid) (Mentions.grantedAbilityNames (unread oid) (const False)) modification || affectedNames oid source gs affected

-- CR 707.2: the snapshot is copiable values, which hold printed text and no
-- runtime id, exactly as a printed card does.
copyNames :: ObjectId -> ActiveCopy.ActiveCopy -> Bool
copyNames oid row = case row of
  ActiveCopy.MkActiveCopy source _timestamp expiry copied _snapshot ->
    source == oid || Mentions.expiryNames (unread oid) expiry || Set.member oid copied

playerEffectRowNames :: ObjectId -> ActivePlayerEffect.ActivePlayerEffect -> Bool
playerEffectRowNames oid row = case row of
  ActivePlayerEffect.MkActivePlayerEffect source _controller _choices _timestamp expiry _scope effect ->
    source == oid || Mentions.expiryNames (unread oid) expiry || Mentions.playerEffectNames (unread oid) effect

blockRequirementNames :: ObjectId -> ActiveBlockRequirement.ActiveBlockRequirement -> Bool
blockRequirementNames oid row = case row of
  ActiveBlockRequirement.MkActiveBlockRequirement source _timestamp expiry blocker attacker ->
    source == oid || Mentions.expiryNames (unread oid) expiry || blocker == oid || attacker == oid

attackRequirementNames :: ObjectId -> ActiveAttackRequirement.ActiveAttackRequirement -> Bool
attackRequirementNames oid row = case row of
  ActiveAttackRequirement.MkActiveAttackRequirement source _controller _timestamp expiry attacker defender ->
    source == oid || Mentions.expiryNames (unread oid) expiry || restrictedNames oid attacker || attackTargetNames oid defender

unregeneratableNames :: ObjectId -> ActiveUnregeneratable.ActiveUnregeneratable -> Bool
unregeneratableNames oid row = case row of
  ActiveUnregeneratable.MkActiveUnregeneratable source _timestamp expiry object ->
    source == oid || Mentions.expiryNames (unread oid) expiry || object == oid

blockProhibitionNames :: ObjectId -> ActiveBlockProhibition.ActiveBlockProhibition -> Bool
blockProhibitionNames oid row = case row of
  ActiveBlockProhibition.MkActiveBlockProhibition source _timestamp expiry object ->
    source == oid || Mentions.expiryNames (unread oid) expiry || object == oid

-- AimedAt names players and target kinds, never an object.
attackProhibitionNames :: ObjectId -> ActiveAttackProhibition.ActiveAttackProhibition -> Bool
attackProhibitionNames oid row = case row of
  ActiveAttackProhibition.MkActiveAttackProhibition source _controller _timestamp expiry affected aimedAt ->
    source == oid || Mentions.expiryNames (unread oid) expiry || restrictedNames oid affected || maybe False aimedAtNames aimedAt

aimedAtNames :: AimedAt.AimedAt -> Bool
aimedAtNames aimed = case aimed of
  AimedAt.MkAimedAt defenders _kinds -> case defenders of
    AimedPlayers.Scoped _scope -> False
    AimedPlayers.EachInSlot _slot -> False
    AimedPlayers.BoundPlayer _player -> False

activationProhibitionNames :: ObjectId -> ActiveActivationProhibition.ActiveActivationProhibition -> Bool
activationProhibitionNames oid row = case row of
  ActiveActivationProhibition.MkActiveActivationProhibition source _timestamp expiry object ->
    source == oid || Mentions.expiryNames (unread oid) expiry || object == oid

untapProhibitionNames :: ObjectId -> ActiveUntapProhibition.ActiveUntapProhibition -> Bool
untapProhibitionNames oid row = case row of
  ActiveUntapProhibition.MkActiveUntapProhibition source _timestamp expiry object ->
    source == oid || Mentions.expiryNames (unread oid) expiry || object == oid

evasionNames :: ObjectId -> ActiveEvasion.ActiveEvasion -> Bool
evasionNames oid row = case row of
  ActiveEvasion.MkActiveEvasion source _controller _timestamp expiry affected ->
    source == oid || Mentions.expiryNames (unread oid) expiry || Mentions.filterNames (unread oid) affected

-- CR 116.2d: the ignored ability is its source's, so the source is the subject.
ignoredNames :: ObjectId -> IgnoredAbility.IgnoredAbility -> Bool
ignoredNames oid row = case row of
  IgnoredAbility.MkIgnoredAbility _player source _ability expiry ->
    source == oid || Mentions.expiryNames (unread oid) expiry

restrictedNames :: ObjectId -> RestrictedCreatures.RestrictedCreatures ObjectId -> Bool
restrictedNames oid restricted = case restricted of
  RestrictedCreatures.Named named -> named == oid
  RestrictedCreatures.Matching criterion -> Mentions.filterNames (unread oid) criterion

-- CR 611.2c: who a stored continuous effect applies to. A fixed set is
-- searched; a predicate is filterNames' question; CR 303.4m's "enchanted"
-- reads the source's host, which namedByAnother also reads from the other end.
affectedNames :: ObjectId -> ObjectId -> GameState -> Affected.Affected -> Bool
affectedNames oid source gs affected = case affected of
  Affected.TheseObjects held -> Set.member oid held
  Affected.Matching criterion -> Mentions.filterNames (unread oid) criterion
  Affected.MatchingAnywhere criterion -> Mentions.filterNames (unread oid) criterion
  Affected.MatchingOffBattlefield criterion -> Mentions.filterNames (unread oid) criterion
  Affected.Attached -> Game.hostOf source gs == Just oid
  Affected.AttachedPlayerControls criterion -> Mentions.filterNames (unread oid) criterion
  -- CR 611.3d: applied by storing a TheseObjects row (Event.permissionRiders),
  -- and Pawl.Engine.Projection applies this template to nothing.
  Affected.PlayedThisWay _duration -> False

-- Whether one of the board's ID-KEYED RELATIONS names this object. Each is
-- a Map from an object to what the game remembers about it, so the ids a row
-- names are its KEY plus whatever ids its value holds -- and both are read here,
-- which is what makes searching them exact rather than merely likely.
--
--   * GameState.phasedOut (CR 702.26b), keyed by the phased-out permanent, its
--     value the player it phased out under.
--   * GameState.exiledUntilMonarch (CR 725), keyed by the exiled incarnation,
--     its value the watching player and whether the crown has moved.
--   * GameState.movedUntilSourceLeaves (CR 610.3), keyed by the object a move
--     with a duration put in another zone, its value the source whose leaving
--     the battlefield brings it back plus the zone it came from.
--   * GameState.haunting (CR 702.55b), keyed by the haunting card in exile, its
--     value the object that card haunts.
--   * GameState.encoded (CR 702.99b), keyed by the card with cipher in exile,
--     its value the creature it is encoded on.
--   * GameState.exiledWith (CR 607.2), keyed by the exiled card, its value
--     naming the object CR 607.2a's or CR 607.2b's link names.
--   * GameState.exilePiles (CR 406.4), keyed by the card in exile face down, its
--     value the stamp of the pile it is in.
--   * GameState.enteredWith (CR 400.7), keyed by the permanent, its value the
--     source whose effect put it onto the battlefield.
--   * The per-object budgets and notes: GameState.activatedThisTurn (CR
--     602.5b), castPermissionsUsedThisTurn (CR 601.3), rollModifiersUsedThisTurn
--     (CR 706.2), namedCopyChoices (CR 707.13), notedCards (CR 707.14),
--     notedMana (CR 607.2e), keptFaceDown (CR 121.8) and outsideCopies (CR
--     400.11), each keyed by the object it is about; and the logs
--     triggeredThisGame (a "triggers only once" rider's spent triggerings),
--     activationsThisTurn (CR 602.2) and attacksInOwnLastTurn (CR 508.1a), by
--     the object each entry names.
--
-- phasedOut and exiledUntilMonarch can never name a CANDIDATE through the one
-- caller (Pawl.Engine.Cost's mana-source window): both key on an object not on
-- the battlefield, and neither value holds an object at all. The CR 610.3 watch
-- is the opposite case on its value side and the reason it is searched rather
-- than assumed inert: the source it names IS on the battlefield while the watch
-- stands, so a permanent that owes an exiled card its return is not
-- interchangeable with one that owes nothing. They are searched
-- rather than required empty all the same, because "this row is about some other
-- object" is the honest reading of the rule and requiring emptiness makes an
-- unrelated phased-out permanent decide a pair it says nothing about.
--
-- The haunting arm's VALUE side is the proved one: Pawl.ManaSourceSpec's "an
-- Elf a haunting card in exile haunts is a candidate of its own" is a haunt row
-- naming one of three otherwise identical Elves, beside "a haunt row that names
-- none of them leaves the elision standing", which is the same board with the
-- row's value moved.
--
-- The KEY side is a regression fence rather than a proof, and so is exiledWith's
-- value side, encoded's whole arm and exilePiles' whole arm. Every one of these relations keys on an
-- object that is not on the battlefield -- an exiled incarnation, or a permanent
-- GameState.battlefield excludes (CR 702.26b) -- so no key can ever be a
-- mana-source candidate, and neutering `key == oid` below leaves the whole suite
-- green. The line stays because a row does name its key; do not read the green as
-- coverage. The per-object budgets, notes and logs are fences too: no board in
-- the suite gives one of two otherwise identical mana sources such a row.
-- notedMana's cannot: Ice Cauldron's noted mana pays only for the card it
-- exiled, so a Cauldron whose mana any payment admits is already named by that
-- card's exiledWith row.
namedByRelation :: ObjectId -> GameState -> Bool
namedByRelation oid gs =
  let relates names = any (\(key, value) -> key == oid || Set.member oid (names value)) . Map.toList
      keyed = Map.member oid
   in relates phasedOutNames (GameState.phasedOut gs)
        || relates monarchWatchNames (GameState.exiledUntilMonarch gs)
        || relates returnWatchNames (GameState.movedUntilSourceLeaves gs)
        || relates Set.singleton (GameState.haunting gs)
        || relates Set.singleton (GameState.encoded gs)
        || relates (Set.singleton . ExileLink.source) (GameState.exiledWith gs)
        || relates pileNames (GameState.exilePiles gs)
        || relates Set.singleton (GameState.enteredWith gs)
        -- CR 305.1 / 601.2a: keyed by the spell or permanent a play made, too.
        || keyed (GameState.cardsPlayed gs)
        || keyed (GameState.activatedThisTurn gs)
        || keyed (GameState.castPermissionsUsedThisTurn gs)
        || keyed (GameState.rollModifiersUsedThisTurn gs)
        || keyed (GameState.namedCopyChoices gs)
        || keyed (GameState.notedCards gs)
        || keyed (GameState.notedMana gs)
        || maybe False keyed (GameState.keptFaceDown gs)
        || Set.member oid (GameState.outsideCopies gs)
        || any (triggeredNames oid) (GameState.triggeredThisGame gs)
        || any (activationNames oid) (GameState.activationsThisTurn gs)
        || any (any (attackedLastTurnNames oid)) (Map.elems (GameState.attacksInOwnLastTurn gs))

-- The objects a GameState.phasedOut row names BEYOND its key: none, since CR
-- 702.26a's stored value is the player the permanent phased out under.
--
-- Total over PhasedOut's constructors and POSITIONAL, so a field added to any of
-- them is an arity error here rather than an ObjectId this silently stops
-- reading -- the failure mode a `_` arm or a `{}` pattern would hide.
phasedOutNames :: PhasedOut.PhasedOut -> Set.Set ObjectId
phasedOutNames row = case row of
  PhasedOut.Directly _under -> Set.empty
  PhasedOut.Indirectly _under -> Set.empty
  PhasedOut.Orphaned _under -> Set.empty

-- The objects a GameState.exiledUntilMonarch row names beyond its key: none. Its
-- value is a player and an event group. Positional for phasedOutNames' reason.
monarchWatchNames :: MonarchWatch.MonarchWatch -> Set.Set ObjectId
monarchWatchNames watch = case watch of
  MonarchWatch.MkMonarchWatch _controller _due -> Set.empty

-- The object a GameState.movedUntilSourceLeaves row names beyond its key: CR
-- 610.3's source, whose leaving the battlefield ends the move. Positional for
-- phasedOutNames' reason.
returnWatchNames :: ReturnWatch.ReturnWatch -> Set.Set ObjectId
returnWatchNames watch = case watch of
  ReturnWatch.MkReturnWatch source _zone -> Set.singleton source

-- The objects a GameState.exilePiles row names beyond its key: none. Its value is
-- CR 406.4's pile stamp, which names a pile rather than an object. Positional for
-- phasedOutNames' reason.
pileNames :: Timestamp.Timestamp -> Set.Set ObjectId
pileNames stamp = case stamp of
  Timestamp.MkTimestamp _n -> Set.empty

-- Whether a spent "triggers only once" triggering names this object as its
-- source. Positional for phasedOutNames' reason.
triggeredNames :: ObjectId -> AbilityTriggered.AbilityTriggered -> Bool
triggeredNames oid entry = case entry of
  AbilityTriggered.MkAbilityTriggered source _controller _ability -> case source of
    TriggerSource.OfObject object -> object == oid
    TriggerSource.Sourceless -> False

-- Whether a CR 602.2 activation this turn names this object, as its source or
-- among its targets. Positional for phasedOutNames' reason.
activationNames :: ObjectId -> PastActivation.PastActivation -> Bool
activationNames oid entry = case entry of
  PastActivation.MkPastActivation _activator source _keyword _kind targets ->
    any (\snapshot -> ObjectSnapshot.object snapshot == oid) (source : targets)

-- Whether an attack declared in its player's last turn (CR 508.1a) names this
-- object, as the attacker or the planeswalker or battle attacked. Positional
-- for phasedOutNames' reason.
attackedLastTurnNames :: ObjectId -> AttackerDeclared.AttackerDeclared -> Bool
attackedLastTurnNames oid entry = case entry of
  AttackerDeclared.MkAttackerDeclared attacker _defender target _count _attackingPlayer ->
    attacker == oid || attackTargetNames oid target

-- Whether any OTHER object names this one: an Aura or Equipment attached to it,
-- which CR 303.4 stores on the rider rather than on the host, and a spell or
-- ability on the stack that took it as a target (CR 601.2c) or that named it as
-- something an instruction produced. Binding's other three fields carry no
-- object -- an amount, a Seq of mode indices, and a copiable-values snapshot.
--
-- A spell's targets reach Object.bindings only as CR 601.2i finishes the cast,
-- so the window CR 601.2g opens for that same cast cannot see them; every
-- earlier spell on the stack is visible. Pawl.ManaSourceSpec's "an Elf a spell on the
-- stack targets is a candidate of its own" case is the two-step proof.
--
-- The ATTACHMENT arm is proved rather than a fence, and Betrayal ({U} Aura,
-- "Whenever enchanted creature becomes tapped, you draw a card") is what proves
-- it: it changes nothing about its host, so the enchanted permanent projects
-- exactly like the one beside it, and only this line tells the two apart.
-- Pawl.ManaSourceSpec's "an Elf enchanted by an Aura that changes nothing about it is
-- still a candidate of its own" is the case, and it asserts the identical
-- projection alongside the offer so the reason is pinned as well as the answer.
namedByAnother :: ObjectId -> GameState -> Bool
namedByAnother oid gs =
  let names (other, obj) =
        other /= oid
          && ( (Object.attachedTo obj >>= Recipient.objectOf) == Just oid
                 || any (bindingNames oid) (Map.elems (Object.bindings obj))
             )
   in any names (Map.toList (GameState.objects gs))

-- Whether one slot's binding names this object.
bindingNames :: ObjectId -> Binding.Binding -> Bool
bindingNames oid binding =
  any (\recipient -> Recipient.objectOf recipient == Just oid) (Maybe.fromMaybe Set.empty (Binding.targets binding))
    || elem oid (Maybe.fromMaybe Seq.empty (Binding.objects binding))
