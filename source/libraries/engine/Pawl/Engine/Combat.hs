module Pawl.Engine.Combat where

import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.AttackCost as AttackCost
import qualified Pawl.Engine.AttackRequirement as AttackRequirement
import qualified Pawl.Engine.Battle as Battle
import qualified Pawl.Engine.BlockCost as BlockCost
import qualified Pawl.Engine.BlockPermission as BlockPermission
import qualified Pawl.Engine.BlockRequirement as BlockRequirement
import qualified Pawl.Engine.CombatRestriction as CombatRestriction
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Defender as Defender
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Summoning as Summoning
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.Extra.Natural as Natural
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.AttackerBlocked as AttackerBlocked
import qualified Pawl.Types.AttackerDeclared as AttackerDeclared
import qualified Pawl.Types.BlockerDeclared as BlockerDeclared
import qualified Pawl.Types.BlocksDeclared as BlocksDeclared
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import Pawl.Types.Combat (Combat)
import qualified Pawl.Types.Combat as Combat
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.OptionalDecision as OptionalDecision
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.TapState as TapState

emptyCombat :: Combat
emptyCombat =
  Combat.MkCombat
    { Combat.attackers = Map.empty,
      Combat.blockers = Map.empty,
      Combat.struckFirst = Nothing,
      Combat.joinedUnder = Map.empty,
      Combat.attacked = Set.empty,
      Combat.declaredAttacked = Set.empty,
      Combat.declaredAttackedThisStep = Set.empty,
      Combat.blockersDeclared = False,
      Combat.defender = Nothing
    }

-- CR 511.3: as the end of combat step ends, everything is removed from combat.
-- Combat.defender is reset alongside on CR 506.2's authority (the designation is
-- scoped to the combat phase) and Combat.blockersDeclared on CR 506.7c's (the
-- next combat phase asks CR 506.7b's question afresh).
--
-- Engine.runStep calls this as the step ENDS, not from runTurnBasedActions: CR
-- 511.1 gives this step no turn-based action. CR 724.1d and CR 724.2d ask for
-- the same removal at a different moment, so Resolve's Effect.EndTurn arm calls
-- it too, guarded on the turn having been ended during a combat phase, and its
-- Effect.EndCombatPhase arm inside CR 724.2g's guard.
clearCombat :: GameState -> GameState
clearCombat gs = gs {GameState.combat = emptyCombat}

-- CR 508.6 on CR 500.1's span: "you've been attacked this step" is a question
-- about one step, so the record answering it ends with the step. Engine.runStep
-- calls this as EVERY step ends, clearCombat's sibling in placement and its
-- opposite in reach -- CR 500.11 lets any step be skipped, so a reset that ran
-- only in the combat steps (or only in the end of combat step, where CR 511.3
-- puts clearCombat) would strand the record into a later turn whenever the step
-- carrying it never happened.
clearAttackedThisStep :: GameState -> GameState
clearAttackedThisStep gs =
  gs {GameState.combat = (GameState.combat gs) {Combat.declaredAttackedThisStep = Set.empty}}

-- CR 508.8: if no creatures were declared as attackers or put onto the
-- battlefield attacking, skip the declare blockers and combat damage steps.
-- Engine.runStepThatBegan asks it as the declare attackers step ENDS.
--
-- Combat.attacked and NOT Map.null on Combat.attackers: CR 506.4's removal takes
-- away the attacking, never the declaration, so asking the map skipped both steps
-- for a lone attacker a Ray of Command took during the step.
skipEmptyCombat :: GameState -> GameState
skipEmptyCombat gs =
  if Set.null (Combat.attacked (GameState.combat gs))
    then gs {GameState.remaining = Turn.dropSkippedCombatSteps (GameState.phase gs) (GameState.remaining gs)}
    else gs

-- CR 506.2a: the candidates the attacking player chooses from (CR 102.1, CR
-- 806.1). Not implemented: CR 102.3's teammates, pawl having no teams (#175).
--
-- SEATING order (CR 103.1), not player-id order, so the first candidate is the
-- next seat rather than the lowest id.
attackableOpponents :: GameState -> [PlayerId]
attackableOpponents gs = filter (/= GameState.activePlayer gs) (Game.stillPlayingInOrder gs)

-- CR 508.1b: what the active player may announce a chosen creature is attacking --
-- which player, planeswalker or battle. CR 506.2's second sentence is the same
-- list scoped to a two-player game.
--
-- The defending player is FIRST, the one candidate that exists on every board, so
-- an interpreter taking the head gets what Replay.defaultAnswer's fallback gives.
-- Derived at DECLARATION and again at damage assignment (stillAttacked below):
-- re-asking this filter IS CR 506.4's removal.
--
-- Not implemented: CR 802's attack-multiple-players option, which would put a
-- second player on this list (#175).
attackTargets :: PlayerId -> GameState -> NonEmpty.NonEmpty AttackTarget.AttackTarget
attackTargets defender gs =
  AttackTarget.OfPlayer defender
    NonEmpty.:| fmap AttackTarget.OfPlaneswalker (attackablePlaneswalkers defender gs)
      <> fmap AttackTarget.OfBattle (attackableBattles defender gs)

-- CR 306.6 / CR 508.1b: the planeswalkers a defending player controls, in
-- ascending id order. PROJECTED rather than printed: CR 613.1d puts card types in
-- layer 4.
attackablePlaneswalkers :: PlayerId -> GameState -> [ObjectId]
attackablePlaneswalkers defender gs =
  filter (\oid -> Projection.isPlaneswalkerOf oid gs) (Projection.controls defender gs)

-- CR 310.5 / CR 508.1b: the battles a defending player PROTECTS, in ascending id
-- order.
--
-- Protects, not controls (CR 310.9b), which is what admits the active player's own
-- Siege -- CR 310.12a puts its protector among its controller's opponents -- and
-- why this walks the whole battlefield. CR 310.9b's first sentence needs no check:
-- the argument is the DEFENDING player. PROJECTED card types (CR 613.1d), the
-- protector surviving a permanent ceasing to be a battle (CR 310.9g), with the
-- designation asked FIRST so a board with no battle projects nothing.
attackableBattles :: PlayerId -> GameState -> [ObjectId]
attackableBattles defender gs =
  let protects oid = Battle.protectorOf oid gs == Just defender
      isOne oid = Battle.isBattle (Projection.project oid gs)
   in filter (\oid -> protects oid && isOne oid) (Set.toAscList (GameState.battlefield gs))

-- CR 506.4: is this planeswalker still one that is being attacked -- or has it
-- been removed from combat since the declaration?
--
-- Asked where the answer is USED (Damage.combatRecipient, at CR 510.1's
-- assignment) rather than sampled into the record, because CR 506.4c is emphatic
-- that Combat.attackers' KEYS must not change. Keeping the entry naming a departed
-- planeswalker is required rather than merely harmless: CR 702.19e is an exception
-- to CR 506.4c, so the entry is what lets Damage.combatRecipient tell "was
-- attacking a planeswalker that is gone" from "was never attacking anything"
-- (Thrasta, Tempest's Roar).
--
-- Not implemented: CR 508.3b's planeswalker and battle subjects. The event that
-- would carry them is there -- GameEvent.BecameAttacked names the permanent --
-- and no trigger condition asks; Pawl.Types.TriggerCondition's
-- AttachedPlayerIsAttacked records the sweep behind that (#538).
stillAttacked :: ObjectId -> GameState -> Bool
stillAttacked oid gs = case Combat.defender (GameState.combat gs) of
  -- No defending player is no attack (see Pawl.Types.Combat's defender field).
  Nothing -> False
  Just defender -> List.elem oid (attackablePlaneswalkers defender gs)

-- CR 506.4 for a battle: is this one still being attacked, or has it left the
-- battlefield since the declaration? stillAttacked's twin, built out of the same
-- candidate list CR 508.1b drew the declaration from -- so CR 613.1d's type change
-- and CR 506.4's "leaves the battlefield" both fall out of the list.
--
-- The list also asks who protects the battle, so a protector moved to a third
-- player mid-combat (CR 310.9f) reads here as removed from combat, which is what
-- rule 506.4 says. Not implemented: any effect that moves a designation (#853).
stillAttackedBattle :: ObjectId -> GameState -> Bool
stillAttackedBattle oid gs = case Combat.defender (GameState.combat gs) of
  Nothing -> False
  Just defender -> List.elem oid (attackableBattles defender gs)

isCreatureObject :: ObjectId -> GameState -> Bool
isCreatureObject = isCreatureObjectGiven Map.empty

isCreatureObjectGiven :: Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> GameState -> Bool
isCreatureObjectGiven = Projection.isCreatureGiven

-- CR 508.1a: an attacking creature must be untapped, controlled by the active
-- player, and not summoning sick (CR 302.6), plus the PER-CREATURE half of CR
-- 508.1c: a creature failing one of those is in no legal declaration at all.
--
-- CR 508.1c's SET-SHAPED half (Bonded Construct's "can't attack alone") is
-- attackDeclarationAllowed's, since such a creature is still a candidate -- taking
-- it off the list would forbid the declaration CR 508.1c's own Example calls
-- legal.
--
-- Not implemented: the PAIRWISE shape, a restriction naming what the attack is
-- aimed at (Blazing Archon), which has no carrier (#1686); and CR 508.1a's "they
-- can't also be battles", which the creature test below already covers (#898).
--
-- canAttackGiven is the half a LOOP wants: `grants`, `pcs` and `restricted` are
-- each one battlefield-wide walk, taken once per declaration pass. An
-- absent projection is a cache miss the projection recovers from, while an absent
-- restriction set is a wrong answer -- which is why canAttack computes one.
canAttack :: PlayerId -> ObjectId -> GameState -> Bool
canAttack pid oid gs = canAttackGiven (Projection.controlGrants gs) Map.empty (CombatRestriction.cantAttack [oid] gs) pid oid gs

canAttackGiven :: [Projection.ControlGrant] -> Map ObjectId PC.ProjectedCharacteristics -> Set ObjectId -> PlayerId -> ObjectId -> GameState -> Bool
canAttackGiven grants pcs restricted pid oid gs = case Game.lookupObject oid gs of
  Nothing -> False
  Just obj ->
    Projection.controllerOfGiven grants Set.empty oid gs == Just pid
      && GameState.activePlayer gs == pid
      -- CR 506.3 wants a permanent, so the test is battlefield MEMBERSHIP and not
      -- Object.zone: a phased-out permanent is one the game treats as not
      -- existing (CR 702.26b) whose zone still reads Zone.Battlefield (CR 702.26d).
      && Set.member oid (GameState.battlefield gs)
      && Object.tapped obj == TapState.Untapped
      -- CR 302.6, relaxed by CR 702.10b: a creature with haste can attack even if
      -- it hasn't been controlled continuously since its controller's most recent
      -- turn began.
      && Summoning.settledOrHastyGiven pcs pid oid gs
      && isCreatureObjectGiven pcs oid gs
      -- CR 508.1c through CR 702.3b: a creature with defender can't attack. It may
      -- still block -- 702.3b says nothing about blocking.
      && not (Projection.hasKeywordGiven pcs Keyword.Defender oid gs)
      -- CR 508.1c: every PRINTED attacking restriction in force (Pacifism).
      && not (Set.member oid restricted)

legalAttackers :: PlayerId -> GameState -> [ObjectId]
legalAttackers pid gs =
  let grants = Projection.controlGrants gs
      pcs = Projection.projectAll gs
      controlled = Projection.controlsGiven grants pid gs
      restricted = CombatRestriction.cantAttack controlled gs
   in filter (\oid -> canAttackGiven grants pcs restricted pid oid gs) controlled

-- CR 506.5: a creature attacks alone if it is the only creature DECLARED as an
-- attacker during the declare attackers step. `alone` is the set of candidates a
-- CR 508.1c restriction forbids that of (Bonded Construct), so a declaration is
-- disallowed exactly when it is the singleton of such a creature.
--
-- Read off the DECLARATION and not off Combat.attackers, CR 506.5's own word being
-- "declared": a creature put onto the battlefield attacking never was declared, so
-- it is not company (CR 508.4c). Two restricted creatures attacking TOGETHER is
-- legal, which is CR 508.1c's own Example, and is why this asks the declaration's
-- size rather than each creature for an unrestricted companion.
aloneAllows :: Set ObjectId -> Set ObjectId -> Bool
aloneAllows alone declaration = case Set.toList declaration of
  [only] -> not (Set.member only alone)
  _ -> True

-- CR 508.1c: if any restriction is disobeyed the DECLARATION is illegal.
-- blockDeclarationAllowed's attacking twin, and the seam a set-shaped attacking
-- restriction is added at.
--
-- Only the SET-SHAPED restrictions are asked here; the per-creature ones keep a
-- creature off `candidates` entirely. Both conjuncts are gathered by the caller so
-- the declaration check and the ceiling cannot judge different boards.
attackDeclarationAllowed :: Maybe Natural -> Set ObjectId -> Set ObjectId -> Bool
attackDeclarationAllowed limit alone declaration =
  aloneAllows alone declaration
    && withinLimit limit (Set.size declaration)

-- CR 508.1c / CR 509.1b read through Silent Arbiter: is a declaration of this
-- SIZE one the bound in force allows? "No more than one" is `<=`, so the empty
-- declaration is within every bound and declining to attack stays legal.
withinLimit :: Maybe Natural -> Int -> Bool
withinLimit limit size = case limit of
  Nothing -> True
  Just n -> toInteger size <= toInteger n

-- Every declaration CR 508.1a and CR 508.1b let the active player write down:
-- each candidate either does not attack, or attacks one of the targets
-- `announceable` admits for it. candidateBlockDeclarations' attacking twin, and
-- EXPONENTIAL for its reason -- O((1 + targets) ^ candidates), where the blocking
-- twin's base is the attackers a blocker may be assigned to.
--
-- Map.empty comes FIRST and every declaration precedes both its own supersets and
-- the same declaration with a LATER target for the creature just added, which
-- attackCeiling's tie-breaking fold relies on: the winner is one no proper
-- sub-declaration of which obeys as many requirements, and its targets are the
-- earliest that attain the maximum. Combat.attackTargets puts the defending
-- player first, so a tie is broken towards attacking the player.
candidateAttackDeclarations :: (ObjectId -> [AttackTarget.AttackTarget]) -> [ObjectId] -> [Map ObjectId AttackTarget.AttackTarget]
candidateAttackDeclarations announceable candidates =
  let extend acc oid = concatMap (\declaration -> declaration : fmap (\target -> Map.insert oid target declaration) (announceable oid)) acc
   in List.foldl' extend [Map.empty] candidates

-- CR 508.1d's two halves, computed together because neither is usable alone: the
-- requirement instances in force, and a declaration obeying the maximum number of
-- them that could be obeyed without disobeying any restriction. blockCeiling's
-- twin, deliberately the same shape.
--
-- A DECLARATION here is CR 508.1a's set and CR 508.1b's announcement together --
-- Map ObjectId AttackTarget, the shape Combat.attackers itself takes -- because
-- CR 508.1d's requirements name both axes: Alluring Siren's "attacks you if able"
-- is obeyed by attacking that player and not by attacking their planeswalker.
-- Nothing else in the maximization changed with that widening; a requirement that
-- names no object mints a pair per target and is obeyed by any announcement.
--
-- TWO SEARCHES answering the same question. The CLOSED FORM gives each required
-- creature the target that obeys the most of its instances, taken independently
-- per creature; it is exact only when no set-shaped restriction (CR 508.1c)
-- reaches a candidate, since canAttack has already applied every per-creature one
-- to `candidates` and nothing in pawl restricts WHICH target a creature may be
-- announced against -- so with the per-creature choices independent, the
-- per-creature maximum sums to the maximum. Multiplicity leaves it alone.
-- Otherwise the ENUMERATION runs, at blockCeiling's exponential cost, uncapped
-- and unsampled (#714).
--
-- Both range over the announcements that can be made FREELY, which is CR 508.1d's
-- cost clause: a player is never required to pay to attack. The clause is applied
-- per (creature, target) PAIR rather than per creature, which is what CR 508.1b's
-- announcement makes of it -- a Ghostly Prison taxes attacking its controller and
-- not attacking a planeswalker they control, so a requirement to attack the
-- PLAYER is excused while a requirement to attack the planeswalker stands.
attackCeiling :: [ObjectId] -> GameState -> (Map (ObjectId, AttackTarget.AttackTarget) Natural, Map ObjectId AttackTarget.AttackTarget)
attackCeiling candidates gs =
  attackCeilingGiven (CombatRestriction.attackLimit gs) (CombatRestriction.cantAttackAlone candidates gs) candidates gs

-- attackCeiling against the restrictions the caller already gathered: each caller
-- also asks attackDeclarationAllowed of the player's own declaration, and the two
-- must be judging the same board.
attackCeilingGiven :: Maybe Natural -> Set ObjectId -> [ObjectId] -> GameState -> (Map (ObjectId, AttackTarget.AttackTarget) Natural, Map ObjectId AttackTarget.AttackTarget)
attackCeilingGiven limit alone candidates gs =
  let targets = declarableTargets gs
      required = AttackRequirement.instances candidates targets gs
      -- CR 508.1d's cost clause. AttackCost.costsOn is asked of the ANNOUNCEMENT,
      -- which is the question that rule's cards ask.
      freely oid target = null (AttackCost.costsOn oid target gs)
      announceable oid = filter (freely oid) targets
      better best declaration =
        if attackRequirementsMet required declaration > attackRequirementsMet required best
          then declaration
          else best
      -- The fold's seed is the EMPTY declaration, always legal under restrictions
      -- alone (CR 508.1c only ever forbids attacking), which is what makes the
      -- answer total without a partial function.
      enumerated =
        List.foldl'
          better
          Map.empty
          (filter (attackDeclarationAllowed limit alone . Map.keysSet) (candidateAttackDeclarations announceable candidates))
      -- The best announcement for ONE creature, in `targets` order so a tie goes
      -- to the defending player, and Nothing when no free announcement obeys
      -- anything -- which is a creature the cost clause excuses entirely.
      bestFor oid =
        let scored = fmap (\target -> (target, Map.findWithDefault 0 (oid, target) required)) (announceable oid)
            top = maximum (0 : fmap snd scored)
         in if top == 0 then Nothing else fmap fst (List.find (\pair -> snd pair == top) scored)
      closed = Map.fromList (Maybe.mapMaybe (\oid -> fmap ((,) oid) (bestFor oid)) (List.nub (fmap fst (Map.keys required))))
   in ( required,
        -- Map.size and not the multiplicity total, because what `limit` bounds is
        -- a declaration's SIZE (CR 508.1c counts creatures). No board tells the
        -- two apart, so this is argued from the rule rather than fenced by a test.
        if (Set.null alone && withinLimit limit (Map.size closed)) || Map.null required
          then closed
          else enumerated
      )

-- CR 508.1b's announcement list for the combat in progress, empty when no
-- defending player has been chosen -- which is a combat no creature may attack
-- in, so no requirement can be instantiated and none can be disobeyed.
declarableTargets :: GameState -> [AttackTarget.AttackTarget]
declarableTargets gs = case Combat.defender (GameState.combat gs) of
  Nothing -> []
  Just defender -> NonEmpty.toList (attackTargets defender gs)

-- How many of `required` this declaration obeys (CR 508.1d): a requirement
-- instance is obeyed exactly when the declaration attacks with its creature AND
-- announces its target for that creature. Summing multiplicities rather than
-- counting keys, because CR 508.1d counts REQUIREMENTS: two naming one pair are
-- both obeyed by making that announcement.
attackRequirementsMet :: Map (ObjectId, AttackTarget.AttackTarget) Natural -> Map ObjectId AttackTarget.AttackTarget -> Natural
attackRequirementsMet required declaration =
  sum (Map.filterWithKey (\(oid, target) _ -> Map.lookup oid declaration == Just target) required)

-- CR 508.1d asked of a declaration that has already passed CR 508.1a and CR
-- 508.1c: does it obey at least as many requirements as the maximum? Split out so
-- declareAttackers can ask it against a ceiling it computed once, and so the two
-- cannot drift.
obeysAttackRequirements :: (Map (ObjectId, AttackTarget.AttackTarget) Natural, Map ObjectId AttackTarget.AttackTarget) -> Map ObjectId AttackTarget.AttackTarget -> Bool
obeysAttackRequirements (required, best) chosen =
  attackRequirementsMet required chosen >= attackRequirementsMet required best

-- CR 508.1: is this declaration one the active player may make? All three checks
-- the rules ask for, in the order they ask them: CR 508.1a's chosen-from set, CR
-- 508.1c's restrictions, then CR 508.1d's requirements.
--
-- CR 508.1c's PER-CREATURE restrictions are not a separate conjunct: being a
-- candidate IS obeying every restriction of that shape (canAttack). CR 508.1d is a
-- MAXIMIZATION rather than a check, and is what makes declaring no attackers at
-- all illegal under a Curse of the Nightly Hunt.
--
-- The creature list is CR 508.1b's announcement when the rule calls for none:
-- every creature attacks the defending player, which is the announcement pawl
-- makes without asking on a board with no planeswalker and no battle. A
-- declaration that names its targets goes through legalAttackDeclarationAs.
legalAttackDeclaration :: PlayerId -> [ObjectId] -> GameState -> Bool
legalAttackDeclaration pid chosen gs = case declarableTargets gs of
  -- No defending player: CR 508.1b has nothing to announce, so no creature can
  -- attack and only the empty declaration is a declaration at all. Its
  -- requirements are empty too, every instance being minted against a target.
  [] -> null chosen
  target : _ -> legalAttackDeclarationAs pid (fmap (\oid -> (oid, target)) chosen) gs

-- legalAttackDeclaration with CR 508.1b's announcement spelled out, which is the
-- form a board with a planeswalker or a battle on it needs.
legalAttackDeclarationAs :: PlayerId -> [(ObjectId, AttackTarget.AttackTarget)] -> GameState -> Bool
legalAttackDeclarationAs pid chosen gs = legalAttackDeclarationGiven (legalAttackers pid gs) (Map.fromList chosen) gs

legalAttackDeclarationGiven :: [ObjectId] -> Map ObjectId AttackTarget.AttackTarget -> GameState -> Bool
legalAttackDeclarationGiven candidates chosen gs =
  -- Both gathered ONCE and shared with the ceiling: the restriction check and the
  -- maximization have to be judging one board.
  let alone = CombatRestriction.cantAttackAlone candidates gs
      limit = CombatRestriction.attackLimit gs
   in all (\oid -> List.elem oid candidates) (Map.keys chosen)
        -- CR 508.1b: an announcement outside the list is not a declaration at all.
        -- Vacuous on the empty declaration, which is what keeps declining legal in
        -- a combat with no defending player.
        && all (\target -> List.elem target (declarableTargets gs)) (Map.elems chosen)
        && attackDeclarationAllowed limit alone (Map.keysSet chosen)
        && obeysAttackRequirements (attackCeilingGiven limit alone candidates gs) chosen

-- A declaration that is always legal: one attaining CR 508.1d's maximum, which
-- with no requirement in force is the empty one (declining to attack). CR 508.1's
-- preamble asks for a fresh declaration instead, so this is reached only when an
-- interpreter REPEATS a declaration that was already rewound. It obeys CR
-- 508.1c as well as CR 508.1d, on both of attackCeiling's paths. Ordered by
-- `candidates` rather than by Map.toList, so it comes back in the order the player
-- was offered its creatures.
forcedAttackDeclaration :: (Map (ObjectId, AttackTarget.AttackTarget) Natural, Map ObjectId AttackTarget.AttackTarget) -> [ObjectId] -> [(ObjectId, AttackTarget.AttackTarget)]
forcedAttackDeclaration (_, best) =
  Maybe.mapMaybe (\oid -> fmap ((,) oid) (Map.lookup oid best))

-- CR 509.1a: a blocking creature must be untapped and controlled by the
-- defending player. Plus CR 509.1b's PER-CREATURE restrictions, which are the
-- last conjunct, on canAttackGiven's terms and for its reason.
--
-- Only the per-creature ones. CR 509.1b's restrictions are mostly PAIRWISE
-- (flying, fear), which live in pairAllowed, and CR 702.111b's menace is
-- SET-SHAPED, which lives in blockDeclarationAllowed. Summoning sickness is NOT a
-- blocking restriction: CR 302.6 says nothing about blocking.
--
-- canBlockGiven/legalBlockersGiven are canAttackGiven's pair, hoisted for the
-- same reason and with the same snapshot argument.
canBlock :: PlayerId -> ObjectId -> GameState -> Bool
canBlock pid oid gs = canBlockGiven (Projection.controlGrants gs) Map.empty (CombatRestriction.cantBlock [oid] gs) pid oid gs

canBlockGiven :: [Projection.ControlGrant] -> Map ObjectId PC.ProjectedCharacteristics -> Set ObjectId -> PlayerId -> ObjectId -> GameState -> Bool
canBlockGiven grants pcs restricted pid oid gs = case Game.lookupObject oid gs of
  Nothing -> False
  Just obj ->
    Projection.controllerOfGiven grants Set.empty oid gs == Just pid
      -- Battlefield MEMBERSHIP, for canAttackGiven's reason above.
      && Set.member oid (GameState.battlefield gs)
      && Object.tapped obj == TapState.Untapped
      && isCreatureObjectGiven pcs oid gs
      -- CR 509.1b: every per-creature blocking restriction in force, printed
      -- (Pacifism) or minted by rule 702 from a keyword (unleash, CR 702.98a).
      && not (Set.member oid restricted)

legalBlockers :: PlayerId -> GameState -> [ObjectId]
legalBlockers pid gs = legalBlockersGiven (Projection.controlGrants gs) (Projection.projectAll gs) pid gs

-- The restriction walk is taken HERE rather than handed in: nothing but this
-- filter reads it, where the grant list and the projection are shared with the
-- whole blocking search.
legalBlockersGiven :: [Projection.ControlGrant] -> Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> GameState -> [ObjectId]
legalBlockersGiven grants pcs pid gs =
  let controlled = Projection.controlsGiven grants pid gs
      restricted = CombatRestriction.cantBlock controlled gs
   in filter (\oid -> canBlockGiven grants pcs restricted pid oid gs) controlled

-- CR 702.9b: a creature with flying can't be blocked except by creatures with
-- flying and/or reach (CR 702.17b).
--
-- Note the asymmetry, easy to get backwards: 702.9b's second sentence says a
-- creature with flying CAN block a creature with or without flying, so flying
-- restricts being blocked, never blocking. The question is asked of the ATTACKER
-- first, and only then of the blocker.
evasionAllows :: ObjectId -> ObjectId -> GameState -> Bool
evasionAllows = evasionAllowsGiven Map.empty

evasionAllowsGiven :: Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> ObjectId -> GameState -> Bool
evasionAllowsGiven pcs blocker attacker gs =
  not (Projection.hasKeywordGiven pcs Keyword.Flying attacker gs)
    || Projection.hasKeywordGiven pcs Keyword.Flying blocker gs
    || Projection.hasKeywordGiven pcs Keyword.Reach blocker gs

-- CR 702.36b: a creature with fear can't be blocked except by artifact creatures
-- and/or black creatures.
--
-- evasionAllows' asymmetry. Both halves of the exception read the PROJECTION: a
-- creature made black by a CR 613 layer-5 effect blocks legally, and a devoid
-- creature with a black mana cost does not.
fearAllows :: ObjectId -> ObjectId -> GameState -> Bool
fearAllows = fearAllowsGiven Map.empty

fearAllowsGiven :: Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> ObjectId -> GameState -> Bool
fearAllowsGiven pcs blocker attacker gs =
  not (Projection.hasKeywordGiven pcs Keyword.Fear attacker gs)
    || Set.member CardType.Artifact (Projection.cardTypesGiven pcs blocker gs)
    || Set.member Color.Black (Projection.colorsGiven pcs blocker gs)

-- CR 702.13b: a creature with intimidate can't be blocked except by artifact
-- creatures and/or creatures that SHARE A COLOR WITH IT.
--
-- Fear's shape with the colour half generalised to an OVERLAP, since 702.13b
-- compares the two creatures where 702.36b names a fixed colour. Both sides
-- projected, for fearAllowsGiven's reason; evasionAllows' asymmetry; membership
-- and never a count (CR 702.13c).
--
-- The overlap test is what gets a COLOURLESS attacker right: CR 105.2c says a
-- colourless object has no color, so such an intimidator shares a colour with
-- nobody and only artifact creatures may block it.
intimidateAllows :: ObjectId -> ObjectId -> GameState -> Bool
intimidateAllows = intimidateAllowsGiven Map.empty

intimidateAllowsGiven :: Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> ObjectId -> GameState -> Bool
intimidateAllowsGiven pcs blocker attacker gs =
  not (Projection.hasKeywordGiven pcs Keyword.Intimidate attacker gs)
    || Set.member CardType.Artifact (Projection.cardTypesGiven pcs blocker gs)
    || not
      ( Set.disjoint
          (Projection.colorsGiven pcs blocker gs)
          (Projection.colorsGiven pcs attacker gs)
      )

-- CR 702.28b: a creature with shadow can't be blocked by creatures without
-- shadow, and a creature without shadow can't be blocked by creatures with
-- shadow.
--
-- The ONE evasion gate that is not evasionAllows' asymmetry: 702.28b's second
-- sentence restricts BLOCKING, so the two sentences together are exactly "the two
-- agree", written as an equality that cannot drift into stating only one of them.
-- A separate conjunct in pairAllowedGiven rather than folded into
-- evasionAllowsGiven, since CR 509.1b checks EVERY restriction in force.
-- Membership and never a count (CR 702.28c).
shadowAllows :: ObjectId -> ObjectId -> GameState -> Bool
shadowAllows = shadowAllowsGiven Map.empty

shadowAllowsGiven :: Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> ObjectId -> GameState -> Bool
shadowAllowsGiven pcs blocker attacker gs =
  Projection.hasKeywordGiven pcs Keyword.Shadow attacker gs
    == Projection.hasKeywordGiven pcs Keyword.Shadow blocker gs

-- CR 702.31b: a creature with horsemanship can't be blocked by creatures without
-- horsemanship.
--
-- Shadow's first sentence without shadow's second, the omission being the rule's
-- own: 702.31b's second sentence permits blocking either way. So this is
-- evasionAllows' asymmetry, and shadow's equality would be wrong here, barring a
-- horseman from blocking a groundling -- the falsifier in Pawl.CombatSpec.
-- Membership and never a count (CR 702.31c).
horsemanshipAllows :: ObjectId -> ObjectId -> GameState -> Bool
horsemanshipAllows = horsemanshipAllowsGiven Map.empty

horsemanshipAllowsGiven :: Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> ObjectId -> GameState -> Bool
horsemanshipAllowsGiven pcs blocker attacker gs =
  not (Projection.hasKeywordGiven pcs Keyword.Horsemanship attacker gs)
    || Projection.hasKeywordGiven pcs Keyword.Horsemanship blocker gs

-- CR 702.118b: a creature with skulk can't be blocked by creatures with GREATER
-- POWER.
--
-- evasionAllows' asymmetry; membership and never a count (CR 702.118c).
--
-- Both powers come off the PROJECTION (CR 613.4c layer 7c), read at declaration
-- time; nothing re-checks them, and CR 509.1h keeps the attacker blocked whatever
-- happens to the blocker later.
skulkAllows :: ObjectId -> ObjectId -> GameState -> Bool
skulkAllows = skulkAllowsGiven Map.empty

skulkAllowsGiven :: Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> ObjectId -> GameState -> Bool
skulkAllowsGiven pcs blocker attacker gs =
  not (Projection.hasKeywordGiven pcs Keyword.Skulk attacker gs)
    || case (Projection.powerGiven pcs blocker gs, Projection.powerGiven pcs attacker gs) of
      (Just b, Just a) -> b <= a
      -- Unreachable: CR 208.5 leaves every creature with a power, and both
      -- arguments are creatures by the time pairAllowedGiven asks. Permissive,
      -- since a restriction that cannot be evaluated forbids nothing.
      _ -> True

-- CR 702.14c: a creature with landwalk can't be blocked as long as the defending
-- player controls at least one land matching the specified criterion.
--
-- The BLOCKER is not an argument, which is CR 702.14d stated in the type: landwalk
-- abilities don't cancel one another, so a player who controls a snow Forest AND a
-- creature with snow forestwalk still may not block a snow-forestwalker.
--
-- evasionAllows' asymmetry; membership over the keyword map and never its counts
-- (CR 702.14e). The MAP rather than hasKeywordGiven, because CR 702.14a's [type]
-- rides the constructor, so there is no single Keyword value to ask about. All
-- four of CR 702.14c's clauses, the keyword carrying a Filter.
landwalkAllows :: ObjectId -> GameState -> Bool
landwalkAllows attacker gs = landwalkAllowsGiven (Projection.controlGrants gs) Map.empty attacker gs

landwalkAllowsGiven :: [Projection.ControlGrant] -> Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> GameState -> Bool
landwalkAllowsGiven grants pcs attacker gs =
  let -- A wildcard rather than an exhaustive case: this asks about ONE named
      -- constructor rather than classifying every keyword.
      landCriterionOf keyword = case keyword of
        Keyword.Landwalk criterion -> Just criterion
        _ -> Nothing
      walked = Maybe.mapMaybe landCriterionOf (Map.keys (Projection.keywordsGiven pcs attacker gs))
      -- CR 508.5, in Pawl.Engine.Defender: landwalk is an ability of an attacking
      -- creature referring to a defending player, so the player is read off the
      -- ATTACK and not off the blocker's controller -- CR 310.9d breaks the two
      -- apart once a battle is attacked. Nothing means the object is not attacking
      -- at all; an attacker whose battle has left still reads CR 506.2's defending
      -- player, which Pawl.BattleSpec's departed-Siege trio proves.
      defendingPlayer = Defender.playerOfAttacker attacker gs
      -- CR 702.14c's lands of the defending player. Lazy, and load-bearing: this
      -- walks the whole battlefield, and `any` below never forces it for an
      -- attacker without landwalk.
      defendersLands = foldMap (\pid -> Projection.controlsGiven grants pid gs) defendingPlayer
      -- CR 109.5's "you" for the criterion is the ATTACKER's controller and the
      -- source is the attacker, the pairing every keyword-borne Filter takes.
      -- Hoisted, since it does not vary per candidate.
      context = Filter.contextFor (Projection.controllerOfGiven grants Set.empty attacker gs) (Just attacker)
      -- The land-ness is asked HERE and never by the criterion: every clause of CR
      -- 702.14c reads "at least one LAND". Load-bearing where the criterion names
      -- no land type at all -- Vectis Gloves' artifact landwalk, Dryad
      -- Sophisticate's nonbasic landwalk -- and redundant where it names one,
      -- since CR 205.3d is enforced at the grant (Pawl.Engine.Projection).
      -- ONE projection per candidate: Filter.cardTypes is the very set
      -- Projection.cardTypesGiven would rebuild.
      matchesCriterion criterion oid =
        let view = Projection.viewOfObjectGiven pcs grants oid gs
         in Set.member CardType.Land (Filter.cardTypes view) && Filter.matches context view criterion
   in not (any (\criterion -> any (matchesCriterion criterion) defendersLands) walked)

-- CR 702.111b: a creature with menace can't be blocked except by two or more
-- creatures.
--
-- The blocking side's SET-SHAPED restriction, aloneAllows' twin, and the reason
-- this takes the whole declaration where its siblings above take a pair.
--
-- "EXCEPT BY two or more", not "must be blocked by two or more": an attacker
-- nobody blocked is not blocked at all, which is why this folds over the attackers
-- the declaration MENTIONS rather than over every attacker in combat.
--
-- evasionAllows' asymmetry, so a creature with menace blocking alone is legal.
-- Membership rather than a per-keyword count (CR 702.111c), so a creature with two
-- instances still needs two blockers rather than four.
menaceAllows :: Map ObjectId (Set ObjectId) -> GameState -> Bool
menaceAllows = menaceAllowsGiven Map.empty

menaceAllowsGiven :: Map ObjectId PC.ProjectedCharacteristics -> Map ObjectId (Set ObjectId) -> GameState -> Bool
menaceAllowsGiven pcs declaration gs =
  let -- blocker -> attackers inverted into attacker -> how many blockers. One
      -- blocker declared against two attackers counts once for each of them and
      -- never twice: CR 702.111b counts CREATURES blocking, not blocks.
      blockerCounts = Map.fromListWith (+) (fmap (\attacker -> (attacker, 1 :: Int)) (concatMap Set.toList (Map.elems declaration)))
      -- The count first, so a comfortably blocked attacker never pays for a
      -- keyword read inside candidateBlockDeclarations' exponential filter.
      allowed (attacker, count) = count >= 2 || not (Projection.hasKeywordGiven pcs Keyword.Menace attacker gs)
   in all allowed (Map.toList blockerCounts)

-- CR 509.1b asked of ONE (blocker, attacker) pair: may this creature block that
-- one at all? This is also what CR 509.1c's requirements mean by "able to block"
-- (Lure), which is why it is a named function.
--
-- A conjunction of independent checks, because CR 509.1b says different evasion
-- abilities are cumulative: an attacker with flying AND shadow admits only
-- blockers that answer both. Every restriction asked here is at most pairwise;
-- menace (CR 702.111b) constrains the SET blocking one attacker, so it is asked in
-- blockDeclarationAllowed.
pairAllowed :: [ObjectId] -> [ObjectId] -> ObjectId -> ObjectId -> GameState -> Bool
pairAllowed candidates attackers blocker attacker gs =
  pairAllowedGiven (Projection.controlGrants gs) Map.empty (CombatRestriction.cantBeBlockedBy candidates attackers gs) candidates attackers blocker attacker gs

-- pairAllowed against a pre-projected board: this is asked once per (blocker,
-- attacker) PAIR, so each evasion read would otherwise be a fresh gather in a
-- doubly nested loop. An EMPTY pcs is a cache miss the projection recovers
-- from, but an empty grant list is a wrong answer.
--
-- `barred` is CR 509.1b's PAIRWISE restrictions stated on the attacker (CR
-- 701.54c), decided for every pair by CombatRestriction.cantBeBlockedBy. An EMPTY
-- set is a board stating no such restriction rather than a cache miss.
pairAllowedGiven :: [Projection.ControlGrant] -> Map ObjectId PC.ProjectedCharacteristics -> Set (ObjectId, ObjectId) -> [ObjectId] -> [ObjectId] -> ObjectId -> ObjectId -> GameState -> Bool
pairAllowedGiven grants pcs barred candidates attackers blocker attacker gs =
  -- CR 509.1a: the blocker must be one this player could block with at all, and
  -- the attacker must actually be attacking.
  List.elem blocker candidates
    && List.elem attacker attackers
    && evasionAllowsGiven pcs blocker attacker gs
    && fearAllowsGiven pcs blocker attacker gs
    && intimidateAllowsGiven pcs blocker attacker gs
    && shadowAllowsGiven pcs blocker attacker gs
    && horsemanshipAllowsGiven pcs blocker attacker gs
    && skulkAllowsGiven pcs blocker attacker gs
    && landwalkAllowsGiven grants pcs attacker gs
    && not (Set.member (blocker, attacker) barred)

-- CR 509.1b: the defending player checks each creature for RESTRICTIONS, and if
-- any are disobeyed the DECLARATION is illegal.
--
-- The unit of legality is the whole declaration, not the pair: menace (CR 702.111b)
-- constrains the SET blocking an attacker, which no per-pair predicate can express.
--
-- All three shapes of restriction are asked here, one conjunct each: pairAllowed
-- over the pairs, menaceAllows over the creatures blocking each attacker, and the
-- bound over the declaration's SIZE. CR 509.1a's per-creature ARITY is a fourth
-- conjunct -- not a restriction, but a fact about the declaration. This is also
-- the seam blockCeiling's enumeration is filtered through, and it takes a
-- projected board and a hoisted `limit` because it is that enumeration's
-- exponential filter's body (#342).
blockDeclarationAllowed :: Maybe Natural -> (ObjectId -> Maybe Natural) -> Map ObjectId PC.ProjectedCharacteristics -> (ObjectId -> ObjectId -> Bool) -> Map ObjectId (Set ObjectId) -> GameState -> Bool
blockDeclarationAllowed limit arity pcs able declaration gs =
  -- CR 509.1b restriction-checks every creature against every creature it is
  -- declared against: a blocker may block a flier and a non-flier only if it could
  -- block each alone.
  all (\(blocker, attackers) -> all (able blocker) (Set.toList attackers)) (Map.toList declaration)
    -- withinLimit and not a bare comparison, so Palace Guard's unbounded arity is
    -- the same Nothing Silent Arbiter's absent bound already is.
    && all (\(blocker, attackers) -> withinLimit (arity blocker) (Set.size attackers)) (Map.toList declaration)
    && menaceAllowsGiven pcs declaration gs
    -- Silent Arbiter's second sentence counts BLOCKING CREATURES, so an entry
    -- naming two attackers is still one creature. Empty entries are dropped.
    && withinLimit limit (Map.size (Map.filter (not . Set.null) declaration))

-- How many of `requirements` this declaration obeys (CR 509.1c): a requirement
-- instance is obeyed exactly when the declaration has its blocker blocking its
-- attacker. Summing multiplicities rather than counting keys, because CR 509.1c
-- counts REQUIREMENTS and BlockRequirement.instances is a multiset over pairs.
requirementsMet :: Map (ObjectId, ObjectId) Natural -> Map ObjectId (Set ObjectId) -> Natural
requirementsMet requirements declaration =
  sum (Map.filterWithKey (\(blocker, attacker) _ -> Set.member attacker (Map.findWithDefault Set.empty blocker declaration)) requirements)

-- Every declaration CR 509.1a lets the defending player write down, given the
-- pairs CR 509.1b allows: each candidate blocker independently blocks nothing, or
-- blocks up to its own arity's worth of attackers it may block.
--
-- EXPONENTIAL: O((attackers + 1) ^ blockers) at arity one, a blocker at arity k
-- widening ITS OWN factor to the subsets of size at most k. Nothing caps it and
-- nothing samples it -- a cap would answer CR 509.1c's maximum with a number that
-- is not the maximum. blockCeiling's guard keeps it off the hot path (#342).
candidateBlockDeclarations :: (ObjectId -> Maybe Natural) -> (ObjectId -> ObjectId -> Bool) -> [ObjectId] -> [ObjectId] -> [Map ObjectId (Set ObjectId)]
candidateBlockDeclarations arity able candidates attackers =
  let extend acc blocker =
        let options = choicesUpTo (arity blocker) (filter (able blocker) attackers)
            apply declaration chosen =
              if Set.null chosen then declaration else Map.insert blocker chosen declaration
         in concatMap (\declaration -> fmap (apply declaration) options) acc
   in List.foldl' extend [Map.empty] candidates

-- Every subset of `attackers` of size at most `n`, the empty one first, or every
-- subset at all when `n` is Nothing. Declining to block is always among them,
-- which is the seed blockCeiling's fold relies on.
choicesUpTo :: Maybe Natural -> [ObjectId] -> [Set ObjectId]
choicesUpTo n attackers =
  let extend acc attacker =
        acc <> [Set.insert attacker chosen | chosen <- acc, withinLimit n (Set.size chosen + 1)]
   in List.foldl' extend [Set.empty] attackers

-- CR 509.1c's two halves, computed together because neither is usable alone: the
-- requirement instances in force, and a declaration obeying the maximum number of
-- them that could be obeyed without disobeying any restriction.
--
-- Map.empty when no requirement is in force, WITHOUT enumerating anything: the
-- maximum is zero and every declaration obeys zero. That guard needs nothing added
-- for a SIZE BOUND, where attackCeilingGiven's twin does -- this shortcut's answer
-- is the empty declaration, which is within every bound.
--
-- The fold's Map.empty seed is always legal under restrictions alone, which is
-- what makes the answer total. blockCeilingGiven is the half legalBlockDeclaration
-- reaches, so the two share one grant walk and one whole-board projection.
--
-- The enumeration ranges over the creatures that block FREELY, which is CR
-- 509.1c's cost clause: a player is never required to pay to block, so `best` is
-- drawn from the untaxed creatures while `requirements` stays every instance in
-- force. attackCeilingGiven's twin, and the placement is the whole of the rule --
-- a taxed creature is still a CANDIDATE and still a legal blocker, it is only
-- never one the defending player must reach for.
blockCeiling :: PlayerId -> GameState -> (Map (ObjectId, ObjectId) Natural, Map ObjectId (Set ObjectId))
blockCeiling pid gs = blockCeilingGiven (Projection.controlGrants gs) (Projection.projectAll gs) pid gs

blockCeilingGiven :: [Projection.ControlGrant] -> Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> GameState -> (Map (ObjectId, ObjectId) Natural, Map ObjectId (Set ObjectId))
blockCeilingGiven grants pcs pid gs =
  let attackers = Map.keys (Combat.attackers (GameState.combat gs))
      candidates = legalBlockersGiven grants pcs pid gs
      -- One walk for the whole search: CR 509.1b's pairwise restrictions are
      -- decided once here and read by every pair `able` judges.
      barred = CombatRestriction.cantBeBlockedBy candidates attackers gs
      able blocker attacker = pairAllowedGiven grants pcs barred candidates attackers blocker attacker gs
      limit = CombatRestriction.blockLimit gs
      arity = blockArityGiven candidates gs
      requirements = BlockRequirement.instances able candidates attackers gs
      -- CR 509.1c's cost clause is a filter on the CREATURE, never on its
      -- requirements: a creature is excused wholly or not at all, and what it
      -- would have blocked never enters the question.
      freely blocker = BlockCost.blocksFreely blocker gs
      better best declaration =
        if requirementsMet requirements declaration > requirementsMet requirements best
          then declaration
          else best
      legal = filter (\declaration -> blockDeclarationAllowed limit arity pcs able declaration gs) (candidateBlockDeclarations arity able (filter freely candidates) attackers)
   in ( requirements,
        if Map.null requirements
          then Map.empty
          else List.foldl' better Map.empty legal
      )

-- CR 509.1: is this declaration one the defending player may make? Both checks
-- the rule asks for, in the order it asks them: CR 509.1b's restrictions, then CR
-- 509.1c's requirements -- a MAXIMIZATION rather than a check, which is what makes
-- declaring no blockers at all illegal while a Lure is on the battlefield.
--
-- CR 509.1c's cost clause rides the ceiling rather than this function: a
-- declaration that costs mana is legal, it is only never the one the maximization
-- demands. CR 509.1d-509.1f's determination and payment are declareBlockers'.
legalBlockDeclaration :: PlayerId -> Map ObjectId (Set ObjectId) -> GameState -> Bool
legalBlockDeclaration pid declaration gs =
  let grants = Projection.controlGrants gs
      pcs = Projection.projectAll gs
      attackers = Map.keys (Combat.attackers (GameState.combat gs))
      candidates = legalBlockersGiven grants pcs pid gs
      -- One walk for the whole search: CR 509.1b's pairwise restrictions are
      -- decided once here and read by every pair `able` judges.
      barred = CombatRestriction.cantBeBlockedBy candidates attackers gs
      able blocker attacker = pairAllowedGiven grants pcs barred candidates attackers blocker attacker gs
      limit = CombatRestriction.blockLimit gs
      arity = blockArityGiven candidates gs
      (requirements, best) = blockCeilingGiven grants pcs pid gs
   in blockDeclarationAllowed limit arity pcs able declaration gs
        && requirementsMet requirements declaration >= requirementsMet requirements best

-- CR 509.1a: how many attacking creatures each of `candidates` may be declared
-- blocking -- the rule's one, plus whatever Pawl.Engine.BlockPermission adds, and
-- Nothing for a card's "any number of creatures" (Palace Guard).
blockArityGiven :: [ObjectId] -> GameState -> ObjectId -> Maybe Natural
blockArityGiven candidates gs =
  let extra = BlockPermission.additionalBlocks candidates gs
   in \blocker -> fmap (1 +) (Map.findWithDefault (Just 0) blocker extra)

-- A declaration that is always legal: one attaining CR 509.1c's maximum, which
-- with no requirement in force is the empty one (declining to block). CR 509.1's
-- preamble asks for a fresh declaration instead, so this is reached only when an
-- interpreter REPEATS a declaration that was already rewound.
forcedBlockDeclaration :: PlayerId -> GameState -> Map ObjectId (Set ObjectId)
forcedBlockDeclaration pid gs = snd (blockCeiling pid gs)

-- Who is CURRENTLY blocking this attacker -- not whether it is blocked. The two
-- come apart, so a reader that wants blocked-ness asks isBlocked rather than
-- testing this for emptiness.
blockersOf :: ObjectId -> GameState -> Set ObjectId
blockersOf oid gs = Map.findWithDefault Set.empty oid (Combat.blockers (GameState.combat gs))

-- CR 509.1h: an attacking creature with one or more blockers declared for it
-- becomes blocked, and remains blocked even if all of them are removed from
-- combat.
--
-- So blocked-ness is a STATUS conferred once, and the map's KEY is that status.
-- The SET behind it is CR 510.1c's separate question of who is currently blocking,
-- and it can empty out while the key stays -- a regenerated blocker (CR 701.19a)
-- is deleted from it. Testing the set for emptiness instead let a creature blocked
-- by a regenerated Drudge Skeletons become unblocked.
isBlocked :: ObjectId -> GameState -> Bool
isBlocked oid gs = Map.member oid (Combat.blockers (GameState.combat gs))

-- CR 509.1h's escape clause performed: an effect SAYS an attacking creature
-- becomes blocked (Effect.BecomesBlocked, Curtain of Light). An EMPTY set behind
-- the key, which is the rule rather than a placeholder: a creature blocked this
-- way is blocked by nothing, so CR 510.1c gives it no combat damage to assign and
-- nothing to take.
--
-- Two guards, both CR 509.1h's own words: only an ATTACKING creature has the
-- status to change, and one already blocked is untouched (CR 509.3c). Both are
-- regression fences -- neutralizing either leaves the suite green.
--
-- The event is declareBlockers' AttackerBlocked, CR 509.3c firing the same trigger
-- on the effect as on the declaration. NOT BlockerDeclared, which is CR 509.3d.
becomeBlocked :: ObjectId -> GameState -> GameState
becomeBlocked oid gs =
  let c = GameState.combat gs
   in if not (Map.member oid (Combat.attackers c)) || isBlocked oid gs
        then gs
        else
          let blocked = gs {GameState.combat = c {Combat.blockers = Map.insert oid Set.empty (Combat.blockers c)}}
           in -- The status is conferred either way: with no defending player
              -- there is nobody for a CR 509.3c trigger to bind, and CR 509.1h
              -- still says the creature is blocked.
              case Defender.playerOfAttacker oid gs Applicative.<|> Combat.defender c of
                Nothing -> blocked
                Just defending -> Event.recordEvent (GameEvent.AttackerBlocked (AttackerBlocked.MkAttackerBlocked oid defending)) blocked

-- Every creature currently IN combat: the attackers, plus everything still
-- blocking one of them. Not the keys of Combat.joinedUnder, which can outlive the
-- record it was taken for.
combatants :: Combat -> Set ObjectId
combatants c = Set.union (Map.keysSet (Combat.attackers c)) (Set.unions (Map.elems (Combat.blockers c)))

-- CR 506.4's two clauses whose trigger is DERIVED state -- a combatant's
-- controller changing, and an attacking or blocking creature stopping being a
-- creature -- which is why this is a sampler and not a hook. settleForPriority
-- runs it at every point the board can change, CR 117.5 making a priority grant
-- the coarsest moment anything observes the board.
--
-- It only ever REMOVES, which is what makes the sampling sound: putting a creature
-- BACK would invent a CR 506.4 the rules do not have.
--
-- Battlefield-scoped, so this stays these two clauses: an object that has LEFT the
-- battlefield is a separate clause of CR 506.4, in Pawl.Engine.Departure and
-- Pawl.Engine.Damage. Creatures only, which is what `combatants` gathers -- an
-- ATTACKED planeswalker or battle is answered at stillAttacked and
-- stillAttackedBattle, CR 506.4d falls out of that split (Pawl.CombatSpec's
-- CreaturePlaneswalkerInCombat is the proof), and the phases-out clause is
-- Pawl.Engine.Phasing.phaseOut's. Not implemented: CR 506.4e and the
-- becomes-a-battle clause (#981).
--
-- The gather is NOT shared with Sba.performStateBasedActions' earlier one: a
-- state-based action can change the board between the two.
removeChanged :: GameState -> GameState
removeChanged gs =
  let c = GameState.combat gs
      inCombat = combatants c
      grants = Projection.controlGrants gs
      cands = Projection.gather gs
      controlChanged oid = case Map.lookup oid (Combat.joinedUnder c) of
        Nothing -> False
        Just who -> Projection.controllerOfGiven grants Set.empty oid gs /= Just who
      stoppedBeingCreature oid = not (Projection.isCreatureFrom cands oid gs)
      onBattlefield oid = Set.member oid (GameState.battlefield gs)
      changed oid = controlChanged oid || stoppedBeingCreature oid
      leaving = filter (\oid -> onBattlefield oid && changed oid) (Set.toList inCombat)
   in if Set.null inCombat
        then gs
        else List.foldl' (flip Game.removeFromCombat) gs leaving

-- CR 507.1 / CR 703.4h: immediately after the beginning of combat step begins,
-- the active player chooses one of their opponents, and that player becomes the
-- defending player. The turn-based action does not use the stack (CR 507.1), which
-- is why this is a plain Game () and not an ability. It runs before any trigger by
-- construction: Engine.runStep calls runTurnBasedActions before priorityLoop.
--
-- Not prompted with one candidate: CR 507.1's condition is a multiplayer
-- game, and a two-player game's defending player is CR 506.2's nonactive player.
-- No candidates leaves Combat.defender Nothing, which declareAttackers reads as no
-- attack being possible; unreachable in a running game (CR 104.2a).
--
-- An answer outside the candidate list is a broken interpreter, not a game state,
-- and degrades to the first candidate -- the same value Replay.defaultAnswer gives
-- this prompt. The two must agree, since neither path can observe the other.
chooseDefender :: Game ()
chooseDefender = do
  gs <- State.get
  let pid = GameState.activePlayer gs
  -- Not implemented: CR 800.4h's handing of this choice to the next player in turn
  -- order on a turn whose active player has left (CR 800.4j) (#181).
  --
  -- Engine.runTurnBasedActions binds the identical test before calling this, so on
  -- the engine's path this guard is redundant. Do NOT delete it: a direct caller
  -- -- a spec, or a second combat phase spliced by an effect -- depends on it.
  Monad.when (List.elem pid (Game.stillPlaying gs)) $
    case NonEmpty.nonEmpty (attackableOpponents gs) of
      Nothing -> pure ()
      Just candidates -> do
        chosen <- case candidates of
          only NonEmpty.:| [] -> pure only
          _ -> do
            let decider = Decide.deciderFor pid gs
            answer <- Game.choose (Prompt.ChooseDefender decider pid candidates)
            pure $
              if List.elem answer (NonEmpty.toList candidates)
                then answer
                else NonEmpty.head candidates
        State.modify' $ \g ->
          g {GameState.combat = (GameState.combat g) {Combat.defender = Just chosen}}

-- CR 508.1b's announcement for ONE creature, and CR 508.4's choice for one
-- creature put onto the battlefield attacking -- the same question, so one
-- function and one prompt (Prompt.ChooseAttackTarget says why).
--
-- Not prompted with one candidate, which is CR 508.1b's own condition: the rule
-- calls for an announcement only when the defending player controls a planeswalker
-- or battle, or the game allows attacking multiple players. An out-of-list answer
-- degrades to the first candidate, the defending player -- chooseDefender's
-- posture and Replay.defaultAnswer's value for this prompt.
announceAttackTarget :: PlayerId -> ObjectId -> NonEmpty.NonEmpty AttackTarget.AttackTarget -> Game AttackTarget.AttackTarget
announceAttackTarget pid oid options = case options of
  only NonEmpty.:| [] -> pure only
  _ -> do
    gs <- State.get
    let decider = Decide.deciderFor pid gs
    answer <- Game.choose (Prompt.ChooseAttackTarget decider pid oid options)
    pure $
      if List.elem answer (NonEmpty.toList options)
        then answer
        else NonEmpty.head options

-- CR 508.1: the active player chooses which creatures attack (CR 508.1a), the
-- declaration is judged against CR 508.1c's restrictions and CR 508.1d's
-- requirements, and then they become tapped and attacking (CR 508.1f). The steps
-- run here are CR 508.1a-d and 508.1f-k, in the rule's own order, plus the event
-- CR 508.1m's triggers watch.
--
-- No legal attackers means no prompt: declining is then the only legal answer, CR
-- 508.1d's instances being minted from the candidate list.
--
-- CR 508.1's preamble costs this function its shape: a declaration the active
-- player cannot comply with is illegal and the game returns to the moment before
-- it, and the declaration is still owed -- so it is made again, which is
-- attemptAttackDeclaration's recursion.
declareAttackers :: PlayerId -> Game ()
declareAttackers pid = do
  gs <- State.get
  case Combat.defender (GameState.combat gs) of
    -- Nothing means no attack is possible: either the beginning of combat step's
    -- turn-based action has not run, or it ran on a turn with no active player
    -- (CR 800.4j), or it found no opponents. Never a place to recompute one.
    Nothing -> pure ()
    Just defender -> attemptAttackDeclaration pid defender Set.empty

-- One attempt at CR 508.1's declaration, plus the preamble's retry. Two steps can
-- make the attempt fail: CR 508.1c/508.1d judge the declaration illegal, and CR
-- 508.1j's payment cannot be made. The preamble treats them the same way -- both
-- clauses end "the declaration ... is illegal" -- so one loop covers both, and the
-- rewind that CR 733.1 spells out is followed by a fresh declaration.
--
-- NOT CR 733.2, which is written for spells and abilities: it hands the redo to
-- the player who had priority, and a turn-based action has no priority holder. The
-- authority for asking again is CR 508.1's own preamble.
--
-- `rejected` is the set of declarations already rewound, and is what bounds the
-- recursion: a pure `Prompt r -> r` returns the identical answer, so a repeat is
-- not rewound a second time but degrades to forcedAttackDeclaration. Such an
-- interpreter therefore costs exactly one extra prompt and ends on the ceiling's
-- declaration. An interpreter that keeps proposing FRESH failing declarations
-- terminates on the finite set of declarations over `candidates`.
--
-- No legal attackers means no prompt: declining is then the only legal answer, CR
-- 508.1d's instances being minted from the candidate list.
attemptAttackDeclaration :: PlayerId -> PlayerId -> Set (Map ObjectId AttackTarget.AttackTarget) -> Game ()
attemptAttackDeclaration pid defender rejected = do
  gs <- State.get
  let candidates = legalAttackers pid gs
  Monad.unless (null candidates) $ do
    let decider = Decide.deciderFor pid gs
    chosen <- Game.choose (Prompt.DeclareAttackers decider pid candidates)
    -- Filtered, not trusted: an interpreter cannot attack with a creature
    -- that is not legally an attacker.
    let isCandidate oid = List.elem oid candidates
        -- Deduplicated too: CR 508.1a's declaration is a SET, and the event
        -- fold below would otherwise record a creature's declaration twice and
        -- make CR 506.5's count disagree with it.
        offered = List.nub (filter isCandidate chosen)
        -- CR 508.1b's candidates, taken ONCE and from the state the
        -- declaration is judged against.
        targets = attackTargets defender gs
    -- CR 508.1b: the announcement, one question per chosen creature, in the
    -- rule's own order -- BEFORE CR 508.1c's restrictions and CR 508.1d's
    -- requirements, which is what a requirement naming its object (Alluring
    -- Siren) forces: whether a declaration obeys the maximum is a question
    -- about the announcements, so they have to exist before it can be asked.
    -- The price is that a creature the CR 508.1d degradation below then drops
    -- was asked about; only an interpreter that repeats a rewound declaration
    -- reaches that path.
    announced <- Monad.mapM (\oid -> fmap ((,) oid) (announceAttackTarget pid oid targets)) offered
    let -- CR 508.1c's set-shaped restrictions and CR 508.1d's maximization,
        -- taken ONCE for all three questions below, so the ceiling and the
        -- check beside it cannot judge different boards.
        alone = CombatRestriction.cantAttackAlone candidates gs
        limit = CombatRestriction.attackLimit gs
        bound = attackCeilingGiven limit alone candidates gs
        -- CR 508.1a-d's declaration, announcements included, as a map -- the
        -- key `rejected` is taken on, since it is the declaration the preamble
        -- rewinds rather than the interpreter's raw answer.
        proposal = Map.fromList announced
        -- Declining to attack is NOT always legal: under a CR 508.1d
        -- requirement (Curse of the Nightly Hunt) "no attacks" can itself be
        -- the illegal answer.
        allowed = attackDeclarationAllowed limit alone (Map.keysSet proposal) && obeysAttackRequirements bound proposal
        -- Whether the preamble's rewind still has a fresh declaration to ask
        -- for. False once this exact declaration has already been rewound,
        -- which is what makes the recursion terminate.
        again = not (Set.member proposal rejected)
    -- CombatEffectSpec's "CR 508.1d an illegal declaration is rewound and asked
    -- again, not replaced by the ceiling's" is the proof.
    if not allowed && again
      then attemptAttackDeclaration pid defender (Set.insert proposal rejected)
      else do
        -- A declaration already rewound once and offered again degrades to
        -- forcedAttackDeclaration. The whole answer is replaced rather than
        -- repaired, declareBlockers' posture: unioning the missing creatures in
        -- could manufacture an attacks-alone illegality. Where several
        -- declarations attain the maximum this takes the SMALLEST, a real
        -- choice among distinguishable declarations.
        --
        -- The ANNOUNCEMENTS are replaced along with the creatures, and are not
        -- re-asked: the ceiling's declaration already names a target per
        -- creature, and re-prompting a player whose answer was just discarded
        -- would ask CR 508.1b about a declaration they did not make.
        let settled = if allowed then announced else forcedAttackDeclaration bound candidates
            attacking = fmap fst settled
            recorded = Map.fromList settled
            -- CR 508.1f: declaring an attacker taps it -- unless it has vigilance
            -- (CR 702.20b), which does not change WHETHER it attacks, only what
            -- attacking does to it.
            tapIt g oid =
              if Projection.hasKeyword Keyword.Vigilance oid g
                then g
                else g {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) oid (GameState.objects g)}
            -- CR 506.4's comparand, taken where the creature joins combat. `pid`
            -- and not a fresh controllerOf: canAttack already required it.
            joined = Map.fromList (fmap (\oid -> (oid, pid)) attacking)
        -- UNIONED into the record, not written over it: "the record is mine alone"
        -- is exactly the assumption CR 508.8's second clause breaks, and replacing
        -- the map would silently remove such a creature from combat.
        let attach g =
              g
                { GameState.combat =
                    (GameState.combat g)
                      { Combat.attackers = Map.union recorded (Combat.attackers (GameState.combat g)),
                        Combat.joinedUnder = Map.union joined (Combat.joinedUnder (GameState.combat g)),
                        -- CR 508.8's first clause. Never cleared, so a CR 506.4
                        -- removal later in the step cannot un-declare these
                        -- creatures, and never narrowed to the targets still being
                        -- attacked (see Pawl.Types.Combat's `attacked` field).
                        Combat.attacked =
                          Set.union (Set.fromList (Map.elems recorded)) (Combat.attacked (GameState.combat g)),
                        -- CR 508.3b / 508.4's narrower record, written HERE and
                        -- only here: these creatures were DECLARED, which is what
                        -- those rules distinguish from being put onto the
                        -- battlefield attacking.
                        Combat.declaredAttacked =
                          Set.union (Set.fromList (Map.elems recorded)) (Combat.declaredAttacked (GameState.combat g)),
                        -- CR 508.6's same record on CR 500.1's narrower span,
                        -- written here for the reason above and emptied as the
                        -- step ends (Pawl.Engine.Engine.runStepThatBegan).
                        Combat.declaredAttackedThisStep =
                          Set.union (Set.fromList (Map.elems recorded)) (Combat.declaredAttackedThisStep (GameState.combat g))
                      }
                }
        -- CR 508.1's preamble, captured here: everything from this line on is
        -- undone together if the payment below cannot be made.
        before <- State.get
        -- CR 508.1f, and ONLY 508.1f: 508.1f taps, 508.1h-j determine and pay, and
        -- only 508.1k makes the creatures attacking. The order is observable -- a
        -- Birds of Paradise just declared as an attacker is tapped, so it is no
        -- longer a mana source for the very cost its attack incurred.
        State.modify' (\g -> List.foldl' tapIt g attacking)
        -- CR 508.1g: the OPTIONAL costs to attack, after CR 508.1f's tapping and
        -- before CR 508.1h's determination. Asked per creature, and of `attacking`
        -- rather than `chosen` so a creature the CR 508.1d degradation dropped is
        -- never offered one. Read off the PROJECTION rather than the printed face.
        --
        -- CR 701.43a's keyword action is the write itself: `pid` joins
        -- Object.exertedBy, which Pawl.Engine.Engine.untapAll applies at that
        -- seat's untap step and CR 701.43b expires. NOT Effect.DoesNotUntapNext,
        -- which is a resolving effect; exerting is a cost payment. The PLAYER is
        -- recorded because rule 701.43a names one. The step sits after `before`, so
        -- CR 508.1's preamble undoes an exert along with the declaration, and
        -- nothing is added to CR 508.1h's total -- exert's cost is not mana.
        --
        -- Not implemented: CR 702.154's enlist, rule 508.1g's other optional cost
        -- to attack, whose cost is tapping a filtered untapped creature rather than
        -- a yes-or-no and whose trigger reads that creature's power (#877).
        --
        Monad.forM_ attacking $ \oid -> do
          gsExert <- State.get
          Monad.when (Projection.hasKeyword Keyword.Exert oid gsExert) $ do
            let exertDecider = Decide.deciderFor pid gsExert
                exert g =
                  Event.recordEvent
                    (GameEvent.Exerted oid)
                    g {GameState.objects = Map.adjust (\o -> o {Object.exertedBy = Set.insert pid (Object.exertedBy o)}) oid (GameState.objects g)}
            answer <- Game.choose (Prompt.ChooseExert exertDecider pid oid)
            Monad.when (answer == OptionalDecision.Exercises) (State.modify' exert)
        gs1 <- State.get
        -- CR 508.1h: the total cost to attack is determined once and then LOCKED
        -- IN -- this `let`. Asking AttackCost.totalCost a second time is what the
        -- rule forbids, which is why that function leaves locking to its caller.
        let owed = AttackCost.totalCost recorded gs1
        -- CR 508.1i's mana-ability window and CR 508.1j's all-costs-or-nothing
        -- payment are both Cost.payToll, which restores the entry state rather than
        -- spending half of it. Skipped outright when nothing is owed.
        --
        -- NO "will you pay?" prompt, and that is a rules reading rather than an
        -- elision: CR 508.1j is unconditional once the creatures are chosen, and CR
        -- 508.1d's excuse from paying is exercised one step earlier, by NOT
        -- DECLARING the creature.
        paid <-
          if null owed
            then pure True
            else Cost.payToll pid owed
        if not paid
          then do
            -- CR 508.1's preamble: the declaration is illegal and the game returns
            -- to the moment before it. Reachable by an ordinary player who
            -- declares more attackers than they can pay for.
            State.put before
            -- And then the declaration is made again, normally a smaller attack
            -- the player can afford. CombatEffectSpec's "CR 508.1 the rewound
            -- declaration is made again: two Pikers under a Ghostly Prison
            -- become one" is the proof; the case beside it is what a repeated
            -- answer does instead.
            Monad.when again (attemptAttackDeclaration pid defender (Set.insert proposal rejected))
          else
            -- CR 508.1k: each chosen creature becomes an attacking creature. After
            -- the payment, which is the rules' own order.
            do
              State.modify' attach
              -- CR 508.2b: the declaration is what abilities trigger on, and CR
              -- 508.3a scopes them to a creature DECLARED as an attacker -- one
              -- event per creature chosen HERE, and none for one put onto the
              -- battlefield attacking. Recorded after the record is written, so a
              -- trigger's intervening-if clause (CR 603.4) reads these creatures
              -- attacking.
              --
              -- The event also carries CR 506.5's count of this declaration, the
              -- same on every event of the batch, and CR 508.5's defending player,
              -- computed per creature because CR 508.5a resolves the phrase through
              -- what each creature is attacking. Nothing reaches the `defender`
              -- fallback: every target here came from attackTargets, and each of
              -- the three arms answers a player for one of those.
              --
              -- Not implemented: CR 508.3a's attacks-a-permanent form and CR
              -- 508.3e, neither of which the two events below can be matched on --
              -- the first needs the target beside the ATTACKER's identity, the
              -- second the attacking player beside the target (#538).
              State.modify'
                ( \g ->
                    let defendingFor oid = Maybe.fromMaybe defender ((\t -> Defender.playerOf t g) =<< Map.lookup oid recorded)
                        declared = Natural.length attacking
                     in List.foldl' (\h oid -> Event.recordEvent (GameEvent.AttackerDeclared (AttackerDeclared.MkAttackerDeclared oid (defendingFor oid) declared)) h) g attacking
                )
              -- CR 508.3b's arity, which is the declaration's rather than the
              -- creature's: one event per DISTINCT target, so a player five
              -- creatures were sent at was attacked once. A Set is what makes that
              -- structural, and it orders the batch deterministically besides.
              --
              -- After the per-creature batch above, since CR 508.2b puts every
              -- trigger from this declaration on the stack together and the order
              -- they triggered in does not matter.
              State.modify'
                ( \g ->
                    let attacked = Set.fromList (Maybe.mapMaybe (\oid -> Map.lookup oid recorded) attacking)
                     in List.foldl' (\h t -> Event.recordEvent (GameEvent.BecameAttacked t) h) g (Set.toList attacked)
                )
              -- CR 508.3d's arity, which is neither of the two above: the
              -- DECLARATION's, so one event however many creatures were named and
              -- however many things they were sent at. `pid` is rule 508.1's
              -- declaring player, whom rule 508.3d's "[a player]" names.
              --
              -- Only for a non-empty declaration, which is rule 508.3d's "one or
              -- more creatures": a player who declined has not attacked, and this
              -- block is reached with `attacking` empty whenever they did.
              --
              -- Last of the three, the batch above's reason: CR 508.2b puts every
              -- trigger from this declaration on the stack together.
              Monad.unless (null attacking) (State.modify' (Event.recordEvent (GameEvent.AttackersDeclared pid)))

-- CR 508.4: a creature put onto the battlefield attacking has its controller
-- choose what it is attacking as it enters. Resolve calls this for each permanent
-- an effect's EntryRiders say is attacking.
--
-- The creature was never DECLARED, and this function's whole difference from
-- declareAttackers follows: neither GameEvent.AttackerDeclared nor
-- GameEvent.BecameAttacked, so CR 508.3a's exclusion and rule 508.3b's last
-- sentence both hold by construction; no tapping, CR 508.1f tapping what is
-- declared;
-- and none of canAttack's questions, per CR 508.4c.
--
-- The guards are the ways the rules say the creature enters WITHOUT being an
-- attacking creature -- CR 506.3a, CR 506.3b, CR 506.3c / CR 508.4a, and a Nothing
-- Combat.defender -- each a silent no-op, which is what those rules say. CR
-- 508.4a's remaining clauses need no check, attackTargets deriving the offer from
-- the board AT THIS MOMENT, and CR 508.4d holds by construction: the creature gets
-- no key in Combat.blockers.
--
-- CR 508.4's CHOICE is prompted per permanent over CR 508.1b's candidates, which
-- Hanweir Garrison's and Meandering Towershell's rulings both require; elided at
-- one candidate.
putOntoBattlefieldAttacking :: ObjectId -> Game ()
putOntoBattlefieldAttacking oid = do
  gs <- State.get
  let c = GameState.combat gs
  case (Combat.defender c, Projection.controllerOf oid gs) of
    (Just defender, Just controller)
      | Set.member oid (GameState.battlefield gs),
        -- CR 506.3a
        isCreatureObject oid gs,
        -- CR 506.3b / CR 506.2: the attacking player is the active player
        controller == GameState.activePlayer gs,
        -- CR 506.3c / CR 508.4a
        List.elem defender (Game.stillPlaying gs) -> do
          -- CR 508.4's chooser is the creature's controller, whom the guard above
          -- makes the attacking player -- which is what lets CR 508.1b's
          -- announcement and this one share a prompt.
          target <- announceAttackTarget controller oid (attackTargets defender gs)
          State.put
            gs
              { GameState.combat =
                  c
                    { Combat.attackers = Map.insert oid target (Combat.attackers c),
                      -- CR 506.4's comparand: this is where the creature joins
                      -- combat.
                      Combat.joinedUnder = Map.insert oid controller (Combat.joinedUnder c),
                      -- CR 508.8's SECOND clause, written inside the guards rather
                      -- than in Resolve's Create arm: CR 506.3a-c and CR 508.4a
                      -- each let the permanent enter without ever becoming an
                      -- attacking creature.
                      Combat.attacked = Set.insert target (Combat.attacked c)
                    }
              }
    _ -> pure ()

-- CR 509.4: a creature put onto the battlefield blocking. The ATTACKER is a
-- parameter rather than a prompt, because CR 509.4's parenthetical is the case
-- every printing of this shape is in -- "unless the effect that put it onto the
-- battlefield specifies what it's blocking" -- and Resolve reads that attacker
-- out of the slot the effect named (EntryRiders.blocking). CR 509.4's main
-- clause, where the controller chooses instead, has no printing (#2089).
--
-- putOntoBattlefieldAttacking's twin, and its difference from
-- attemptBlockDeclaration is the same shape as that function's from
-- declareAttackers. The creature was never DECLARED, so:
--
--   * neither GameEvent.BlockerDeclared nor GameEvent.BlocksDeclared is
--     recorded, which is CR 509.3a's and CR 509.3b's last sentence in both cases
--     ("It won't trigger if the creature is put onto the battlefield blocking");
--   * no CR 509.1b restriction and no CR 509.1c requirement is checked, and
--     canBlock is never asked, per CR 509.4b in as many words -- so a Saproling
--     put onto the battlefield blocking a flier really does block it (CR
--     702.9b);
--   * nothing is tapped and nothing is required to be untapped, CR 509.1a's
--     condition belonging to the declaration CR 509.4b exempts this creature
--     from.
--
-- GameEvent.AttackerBlocked IS recorded, and that is CR 509.3c's third producer:
-- "It will also trigger if that creature becomes blocked by an effect or by a
-- creature that's put onto the battlefield as a blocker, but only if the
-- attacking creature was an unblocked creature at that time." The clause after
-- the comma is `wasBlocked`, read off Combat.blockers before the write. CR
-- 509.3c and not CR 509.1h is the authority: rule 509.1h is worded about
-- creatures DECLARED as blockers, which this creature is not.
--
-- The guards are the ways the rules say the creature enters WITHOUT ever being a
-- blocking creature, each a silent no-op because that is what those rules say:
-- CR 506.3a (not a creature), CR 509.4a's first clause (the named creature is no
-- longer attacking), and CR 506.3e / CR 509.4a's second clause -- which is
-- Defender.playerOfAttacker, that function answering CR 508.5's three cases and
-- so exactly rule 506.3e's "attacking the entering creature's controller, a
-- planeswalker that player controls, or a battle that player protects".
--
-- CR 506.3f (a creature that's also a battle) is not guarded, as
-- putOntoBattlefieldAttacking does not guard it either: no printing is both.
--
-- The last two guards are REGRESSION FENCES rather than proved behaviour:
-- dropping both leaves the whole suite green. The pool's one producer is Flash
-- Foliage, whose target slot admits only an attacking creature an opponent
-- controls and is re-read at CR 608.2b, so no board reaches this function with
-- an attacker that has left combat or one attacking somebody else. The second
-- becomes reachable under CR 802's attack-multiple-players (#175); the first
-- needs a card that removes the target from combat between targeting and
-- resolution.
putOntoBattlefieldBlocking :: ObjectId -> ObjectId -> Game ()
putOntoBattlefieldBlocking oid attacker = do
  gs <- State.get
  let c = GameState.combat gs
  case Projection.controllerOf oid gs of
    Just controller
      | Set.member oid (GameState.battlefield gs),
        -- CR 506.3a
        isCreatureObject oid gs,
        -- CR 509.4a's first clause
        Map.member attacker (Combat.attackers c),
        -- CR 506.3e / CR 509.4a's second clause
        Defender.playerOfAttacker attacker gs == Just controller -> do
          -- CR 509.3c's "was an unblocked creature at that time", read BEFORE the
          -- write below.
          let wasBlocked = Map.member attacker (Combat.blockers c)
          State.put
            gs
              { GameState.combat =
                  c
                    { -- insertWith and not adjust: the attacker this creature is
                      -- put onto the battlefield blocking need not have been
                      -- blocked already, so the key may be absent.
                      Combat.blockers = Map.insertWith Set.union attacker (Set.singleton oid) (Combat.blockers c),
                      -- CR 506.4's comparand: this is where the creature joins
                      -- combat.
                      Combat.joinedUnder = Map.insert oid controller (Combat.joinedUnder c)
                    }
              }
          -- CR 509.3c: the attacker became a blocked creature. The defending
          -- player rides the event as it does off the declaration; the guard
          -- above has already settled that it is this creature's controller.
          Monad.unless wasBlocked $
            State.modify' (Event.recordEvent (GameEvent.AttackerBlocked (AttackerBlocked.MkAttackerBlocked attacker controller)))
    _ -> pure ()

-- CR 509.1: the defending player declares blockers -- singular. The loop is over
-- at most one player, and Maybe.maybeToList is what makes "nobody is being
-- attacked" and "one player is" one code path.
--
-- Not implemented: CR 802.4's APNAP declaration by several defending players and
-- CR 802.4a's restriction, both needing the attack-multiple-players option (#175).
--
-- CR 310.9c needs no clause of its own: this asks the defending player and nobody
-- else, and attackableBattles admits a battle only when that player is its
-- protector. Pawl.BattleSpec's pair of CR 310.9c cases is the proof.
--
-- No still-playing guard: at three or more seats CR 800.4a has removed a departed
-- player's objects, so legalBlockers finds nothing. At two seats CR 800.4a never
-- runs (CR 800.1), but CR 104.2a has ended the game, which Engine.playGame's loop
-- reads before calling this again.
declareBlockers :: Game ()
declareBlockers = do
  -- CR 506.7b's boundary, raised BEFORE the short-circuit below and before any
  -- prompt, the rule opening the window "regardless of whether any blockers are
  -- actually declared". Turn.afterBlockersDeclared is the reader.
  --
  -- The PLACEMENT is a regression fence rather than proved behaviour: moving this
  -- line inside the guard below leaves the suite green, no board in the pool
  -- reaching this step with nothing attacking.
  State.modify' $ \g -> g {GameState.combat = (GameState.combat g) {Combat.blockersDeclared = True}}
  start <- State.get
  let attacking = Map.keys (Combat.attackers (GameState.combat start))
  Monad.unless (null attacking) $ do
    Monad.forM_ (Maybe.maybeToList (Combat.defender (GameState.combat start))) $ \pid ->
      attemptBlockDeclaration pid attacking Set.empty
    -- CR 509.1h's other half, performed once CR 509.1g has assigned the blockers:
    -- every attacking creature the declaration named no blockers for became an
    -- UNBLOCKED creature. TriggerCondition.SelfAttacksUnblocked is the reader.
    --
    -- OUTSIDE the loop above, and the placement is the point: that loop is guarded
    -- three times over -- `attacking`, the defending player, and
    -- attemptBlockDeclaration's own candidate check -- and a board where nobody
    -- can block trips all three --
    -- exactly the board on which every attacker is unblocked. Rule 509.1h carries
    -- no such condition.
    --
    -- Recorded ONCE and never sampled again -- the rule's last sentence keeps an
    -- attacker blocked after every creature blocking it leaves combat, and
    -- TriggerSpec's "losing every blocker does not make the Eternal unblocked"
    -- proves that timing. Testing the KEY rather than the set is isBlocked's own
    -- reading, but a regression fence here: swapping the two leaves the suite
    -- green.
    State.modify' $ \g ->
      let c = GameState.combat g
          unblocked = filter (\oid -> not (Map.member oid (Combat.blockers c))) (Map.keys (Combat.attackers c))
       in List.foldl' (\h oid -> Event.recordEvent (GameEvent.AttackerUnblocked oid) h) g unblocked

-- One attempt at CR 509.1's declaration, plus the preamble's retry --
-- attemptAttackDeclaration's twin, and for the same reason: CR 509.1's preamble
-- is word for word CR 508.1's, and CR 509.1b/509.1c both end "the declaration of
-- blockers is illegal", so an illegal declaration and one CR 509.1f cannot pay for
-- take the same rewind. The game returns to the moment before the declaration and
-- the defending player declares again.
--
-- `rejected` holds the raw answers already rewound -- there is no announcement
-- step here, so the interpreter's own map IS the declaration -- and bounds the
-- recursion exactly as it does for attackers: a repeat degrades to
-- forcedBlockDeclaration rather than being rewound a second time.
attemptBlockDeclaration :: PlayerId -> [ObjectId] -> Set (Map ObjectId (Set ObjectId)) -> Game ()
attemptBlockDeclaration pid attacking rejected = do
  gs <- State.get
  let candidates = legalBlockers pid gs
  Monad.unless (null candidates) $ do
    let decider = Decide.deciderFor pid gs
    chosen <- Game.choose (Prompt.DeclareBlockers decider pid candidates attacking)
    -- CR 509.1b: an illegal declaration is illegal AS A WHOLE. NOT filtered
    -- down to its legal entries, which is unsound rather than merely inelegant
    -- -- under menace, dropping one blocker from a pair leaves an illegal
    -- single block, so the filter would manufacture the illegality it was meant
    -- to remove.
    --
    -- Declining to block is NOT always legal: under a CR 509.1c requirement
    -- (Lure) "no blocks" can itself be the illegal answer.
    gs1 <- State.get
    let allowed = legalBlockDeclaration pid chosen gs1
        -- Whether the preamble's rewind still has a fresh declaration to ask for.
        again = not (Set.member chosen rejected)
    -- CombatEffectSpec's "CR 509.1c an illegal declaration is rewound and asked
    -- again, not replaced by the ceiling's" is the proof.
    if not allowed && again
      then attemptBlockDeclaration pid attacking (Set.insert chosen rejected)
      else do
        -- A declaration already rewound once and offered again degrades to
        -- forcedBlockDeclaration -- always legal, and equal to "no blocks"
        -- whenever no requirement is in force. Replay.defaultAnswer's "no blocks"
        -- for this prompt routes through here too, so the two cannot disagree.
        let legal = if allowed then chosen else forcedBlockDeclaration pid gs1
        -- CR 509.1d: the total cost to block is determined once and then LOCKED IN
        -- -- this `let`. Asking BlockCost.totalCost a second time is what the rule
        -- forbids, which is why that function leaves locking to its caller.
        let owed = BlockCost.totalCost legal gs1
        -- CR 509.1e's mana-ability window and CR 509.1f's all-costs-or-nothing
        -- payment are both Cost.payToll, which restores the entry state rather
        -- than spending half of it. Skipped outright when nothing is owed, which
        -- is every board with no cost to block on it.
        --
        -- NO "will you pay?" prompt, declareAttackers' reading of the same pair of
        -- sentences: CR 509.1f is unconditional once the creatures are chosen, and
        -- CR 509.1c's excuse from paying is exercised one step earlier, at the
        -- Prompt.DeclareBlockers above, by NOT DECLARING the creature.
        --
        -- BEFORE the record is written, which is the rules' own order and the
        -- reverse of declareAttackers': CR 508.1f taps the chosen creatures before
        -- their cost is determined, while CR 509.1g makes the chosen creatures
        -- blocking only after CR 509.1f's payment.
        paid <-
          if null owed
            then pure True
            else Cost.payToll pid owed
        -- CR 509.1's preamble: a declaration the defending player cannot pay for
        -- is illegal and the game returns to the moment before it. Nothing needs
        -- undoing -- the record is written below, and Cost.payToll restores what a
        -- half-paid toll spent -- so the rewind is the declaration being made
        -- again. CombatEffectSpec's "CR 509.1 the rewound declaration is made
        -- again: the taxed blocker is dropped and the free one blocks" is the
        -- proof; the case beside it is what a repeated answer does instead.
        gs2 <- State.get
        if not paid && again
          then attemptBlockDeclaration pid attacking (Set.insert chosen rejected)
          else do
            let declaration = if paid then legal else forcedBlockDeclaration pid gs2
            -- The pairs the declaration states, blocker-major, which is the order
            -- CR 509.1a writes it in and the order the events below are recorded in.
            let pairs = [(blocker, attacker) | (blocker, attackers) <- Map.toList declaration, attacker <- Set.toList attackers]
            Monad.unless (null pairs) $ do
              let add m (b, a) = Map.insertWith Set.union a (Set.singleton b) m
                  merged = List.foldl' add (Combat.blockers (GameState.combat gs2)) pairs
                  -- CR 506.4's comparand for the blockers, alongside the attackers'.
                  -- `pid` for the same reason it is there: every blocker here is one
                  -- legalBlockers offered, which is controllerOf == Just pid (CR
                  -- 509.1a). Unioned, the attackers' entries being already in this map.
                  joined = Map.union (Map.fromList (fmap (\(b, _) -> (b, pid)) pairs)) (Combat.joinedUnder (GameState.combat gs2))
              State.modify' $ \g -> g {GameState.combat = (GameState.combat g) {Combat.blockers = merged, Combat.joinedUnder = joined}}
              -- CR 509.1i: the declaration is a trigger event, and CR 509.2a puts what
              -- it fires onto the stack before the active player gets priority.
              -- Recorded AFTER the state is written, and over `declaration` rather
              -- than `merged`, since CR 509.4 makes a creature put onto the
              -- battlefield blocking never "blocked".
              --
              -- TWO events per declaration, split by CR 509.3a against CR 509.3b: one
              -- BlockerDeclared per PAIR, and one BlocksDeclared per BLOCKING CREATURE.
              -- The count it carries is CR 509.3e's.
              State.modify' $ \g -> List.foldl' (\h (blocker, attacker) -> Event.recordEvent (GameEvent.BlockerDeclared (BlockerDeclared.MkBlockerDeclared blocker attacker)) h) g pairs
              State.modify' $ \g -> List.foldl' (\h (blocker, attackers) -> Event.recordEvent (GameEvent.BlocksDeclared (BlocksDeclared.MkBlocksDeclared blocker (Natural.length attackers))) h) g (filter (not . Set.null . snd) (Map.toList declaration))
              -- CR 509.1h: the same declaration makes each attacker it named a BLOCKED
              -- creature. One event per attacker rather than per pair, which is CR
              -- 509.3c's "only once each combat for that creature" -- a real dedup,
              -- since the defending player can put two blockers on one attacker.
              --
              -- CR 509.3c's "only if the attacking creature was an unblocked creature
              -- at that time" is the difference: an attacker already in Combat.blockers
              -- does not become blocked a second time. A regression fence rather than a
              -- proof, Engine running this once per combat.
              --
              -- Over `declaration` and not `merged`, and that stays exact now that
              -- putOntoBattlefieldBlocking exists: an attacker whose only blocker was
              -- put onto the battlefield blocking became blocked THERE, where that
              -- function records this same event, and CR 509.3c's "only once each
              -- combat" is what `wasBlocked` keeps true across the two writers.
              --
              -- The defending player rides the event as it rides AttackerDeclared (CR
              -- 702.130a's afflict is the reader), with `pid` as the fallback.
              let wasBlocked = Map.keysSet (Combat.blockers (GameState.combat gs2))
                  becameBlocked = Set.difference (Set.fromList (fmap snd pairs)) wasBlocked
              State.modify'
                ( \g ->
                    let defendingFor oid = Maybe.fromMaybe pid (Defender.playerOfAttacker oid g)
                     in List.foldl' (\h attacker -> Event.recordEvent (GameEvent.AttackerBlocked (AttackerBlocked.MkAttackerBlocked attacker (defendingFor attacker))) h) g (Set.toList becameBlocked)
                )
