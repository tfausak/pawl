module Pawl.Engine.Combat where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pawl.Engine.AttackCost as AttackCost
import qualified Pawl.Engine.AttackRequirement as AttackRequirement
import qualified Pawl.Engine.BlockRequirement as BlockRequirement
import qualified Pawl.Engine.CombatRestriction as CombatRestriction
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Mana as Mana
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Summoning as Summoning
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.Types.AttackTarget as AttackTarget
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.Color as Color
import Pawl.Types.Combat (Combat)
import qualified Pawl.Types.Combat as Combat
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.ManaCost as ManaCost
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.Program as Program
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

emptyCombat :: Combat
emptyCombat =
  Combat.MkCombat
    { Combat.attackers = Map.empty,
      Combat.blockers = Map.empty,
      Combat.struckFirst = Nothing,
      Combat.joinedUnder = Map.empty,
      Combat.attacked = Set.empty,
      Combat.declaredAttacked = Set.empty,
      Combat.defender = Nothing
    }

-- CR 511.3: as soon as the end of combat step ends, all creatures, battles and
-- planeswalkers are removed from combat -- which by CR 506.4 is what stops them
-- being attacking and blocking creatures. Resetting `defender` alongside them is
-- CR 506.2, not CR 511.3: the designation is scoped to the combat phase, so it
-- cannot survive the phase ending.
--
-- Engine.runStep calls this as the end of combat step ENDS, alongside CR 500.5's
-- mana emptying -- not from runTurnBasedActions, which is a step's opening and
-- which CR 511.1 says this step has none of. Neighbours in time only: CR 703.4q
-- makes that emptying a turn-based action, and this is not one.
clearCombat :: GameState -> GameState
clearCombat gs = gs {GameState.combat = emptyCombat}

-- CR 508.8: "If no creatures are declared as attackers or put onto the
-- battlefield attacking, skip the declare blockers and combat damage steps."
--
-- BOTH of that rule's clauses are the same question of Combat.attacked, because
-- both things that can make one true write that set: declareAttackers
-- below, and putOntoBattlefieldAttacking. Engine.runStepThatBegan asks it as the
-- declare attackers step ENDS -- after the priority round in which an attack
-- trigger resolves -- rather than the moment the turn-based action finishes,
-- which is what made the second clause unrepresentable before.
--
-- That set and NOT Map.null on Combat.attackers, which is the same question only
-- while nothing leaves combat. CR 508.8 asks whether a creature WAS declared or
-- put onto the battlefield attacking, and CR 508.1k makes that a different
-- question from whether one is attacking now: a declared creature "remains an
-- attacking creature until it's removed from combat", and CR 506.4's removal
-- takes away the attacking, never the declaration. Asking the map skipped both
-- steps for a lone attacker that a Ray of Command took during the step, which is
-- TurnSpec's proving test; Pawl.Engine.Replacement's CR 701.19a regeneration reaches the
-- same Game.removeFromCombat door.
--
-- A creature put onto the battlefield attacking LATER than that step cannot
-- un-skip anything, and does not need to: the steps this drops are the only ones
-- of THIS combat phase it could enter during, so if they were dropped there is
-- no such moment, and if they were not then nothing is skipped anyway. A CR
-- 500.8 additional combat phase later in the turn is not this call's business
-- and Turn.dropSkippedCombatSteps does not touch it -- that phase reaches its
-- own declare attackers step and asks this question again for itself.
--
-- The second clause on its own -- a creature put onto the battlefield attacking
-- while NOTHING was declared -- is proved at gameplay level by Meandering
-- Towershell, in CombatSpec's MeanderingTowershell group: its delayed ability
-- returns it attacking on a later turn, on which its controller declares
-- nothing. TurnSpec's direct call to putOntoBattlefieldAttacking states the same
-- clause with no card in the way, and both are kept.
skipEmptyCombat :: GameState -> GameState
skipEmptyCombat gs =
  if Set.null (Combat.attacked (GameState.combat gs))
    then gs {GameState.remaining = Turn.dropSkippedCombatSteps (GameState.phase gs) (GameState.remaining gs)}
    else gs

-- CR 506.2a: the candidates the attacking player chooses from. Read only by
-- chooseDefender; the CHOSEN one lives in Combat.defender.
--
-- CR 506.2a says the attacking player chooses one of their opponents, and three
-- rules get from "opponents" to this list. CR 102.1: a player is one of the
-- people IN THE GAME, so someone who has left is not a player and cannot be an
-- opponent. CR 806.1: in a free-for-all the players compete as individuals, so
-- every other player is an opponent. CR 102.3 is the one reading this is wrong
-- for -- a teammate is not an opponent -- and pawl has no teams to express
-- (#175). Same argument Count.playersFor's PlayerRelation.Opponent arm carries,
-- phrased the same way on purpose. Filter.matches has an Opponent arm too, but it
-- carries no such note and is deliberately not cited here as though it did.
--
-- SEATING order (Game.stillPlayingInOrder), not player-id order: the seating
-- roster is the game's own ordering for anything player-shaped (CR 103.1: "the
-- game's default turn order begins with the starting player and proceeds
-- clockwise"; CR 800.5 only says the seating itself is agreed at the table), and
-- Game.stillPlaying's order is an artifact of reading the players map. It
-- makes the first candidate the next seat rather than the lowest id, which is
-- what an interpreter that takes the head should get.
attackableOpponents :: GameState -> [PlayerId]
attackableOpponents gs = filter (/= GameState.activePlayer gs) (Game.stillPlayingInOrder gs)

-- CR 508.1b: what the active player may announce a chosen creature is attacking
-- -- "which player, planeswalker, or battle". CR 506.2's second sentence is the
-- same list scoped to a two-player game: "that player, planeswalkers they
-- control, and battles they protect may be attacked."
--
-- The defending player is FIRST, which is not cosmetic: it is the candidate that
-- exists on every board, so an interpreter that takes the head gets the answer
-- every attack in a battle-less, planeswalker-less pool used to get, and
-- Replay.defaultAnswer's fallback is the same one for the same reason.
--
-- Battles are absent because there is no battle card type (#302). CR 802's
-- attack-multiple-players option would put a SECOND player on this list, and
-- pawl has no options concept to read it from (#175) -- the defending player is
-- the argument, so this function needs no change when it arrives, only a longer
-- caller.
--
-- Read at DECLARATION and again at damage assignment (stillAttacked below), and
-- both readings are derived rather than stored on purpose: every clause of CR
-- 506.4 that stops a planeswalker being attacked -- it leaves the battlefield,
-- its controller changes, it stops being a planeswalker -- is a change to
-- exactly what this filter asks about, so re-asking IS performing the removal.
attackTargets :: PlayerId -> GameState -> NonEmpty.NonEmpty AttackTarget.AttackTarget
attackTargets defender gs =
  AttackTarget.OfPlayer defender
    NonEmpty.:| fmap AttackTarget.OfPlaneswalker (attackablePlaneswalkers defender gs)

-- CR 306.6 / CR 508.1b: the planeswalkers a defending player controls, in
-- ascending id order (Projection.controls walks the battlefield, which is a Set).
--
-- Battlefield-scoped by construction, since that is where Projection.controls
-- looks, and PROJECTED rather than printed for isPlaneswalkerOf's own reason:
-- CR 613.1d puts card types in layer 4.
attackablePlaneswalkers :: PlayerId -> GameState -> [ObjectId]
attackablePlaneswalkers defender gs =
  filter (\oid -> Projection.isPlaneswalkerOf oid gs) (Projection.controls defender gs)

-- CR 506.4: is this planeswalker still one that is being attacked -- or has it
-- been removed from combat since the declaration?
--
-- Asked where the answer is USED (Damage.combatRecipient) rather than sampled
-- into the combat record by Engine.settleForPriority the way removeChanged
-- samples its two clauses, and the difference is invisible: an attack target is
-- read in exactly two places, and both re-ask.
--
--   * Damage.combatRecipient, at CR 510.1's assignment -- whose own rule
--     (CR 510.1b) is phrased for precisely this case: "If it isn't currently
--     attacking anything (if, for example, it was attacking a planeswalker that
--     has left the battlefield), it assigns no combat damage."
--   * landwalkAllowsGiven, for CR 508.5's "defending player", which reads the
--     planeswalker's controller and never whether it is still attacked.
--
-- Every other reader of Combat.attackers takes its KEYS (Projection's
-- Filter.IsAttacking, blockCeiling, declareBlockers, Damage.dealCombatDamage),
-- and CR 506.4c is emphatic that the keys must NOT change here: "removing that
-- planeswalker or battle from combat doesn't remove that creature from combat.
-- It continues to be an attacking creature, although it is not attacking any
-- player, planeswalker, or battle. It may be blocked."
--
-- So the state pawl stores -- an attacker whose recorded target is no longer
-- attackable -- and the state the rules describe -- an attacking creature
-- attacking nothing -- are observationally the same board, and stay so until
-- something asks a question that separates them. The candidate for that is a
-- card whose text reads WHAT a creature is attacking (CR 508.3b's "whenever
-- [a planeswalker] is attacked", CR 702.19c's trample over planeswalkers); none
-- is in the pool (#537).
stillAttacked :: ObjectId -> GameState -> Bool
stillAttacked oid gs = case Combat.defender (GameState.combat gs) of
  -- No defending player is no attack (see Pawl.Types.Combat's defender field), so
  -- nothing of theirs is being attacked either.
  Nothing -> False
  Just defender -> List.elem oid (attackablePlaneswalkers defender gs)

isCreatureObject :: ObjectId -> GameState -> Bool
isCreatureObject = isCreatureObjectGiven Map.empty

isCreatureObjectGiven :: Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> GameState -> Bool
isCreatureObjectGiven = Projection.isCreatureGiven

-- CR 508.1a: an attacking creature must be untapped, controlled by the active
-- player, and not summoning sick (CR 302.6). Together with CR 508.1c's
-- restrictions, which are the last two conjuncts: the rules ask them as separate
-- steps, and pawl answers both here because every restriction it can state is per
-- creature, so failing one is indistinguishable from never having been a
-- candidate. A restriction on a SET of creatures could not be answered here
-- (#533).
--
-- canAttackGiven is the half a LOOP wants: `grants` is one control-grant walk
-- (Projection.controlGrants), `pcs` one whole-board projection
-- (Projection.projectAll), and `restricted` one battlefield walk for CR 508.1c
-- (CombatRestriction.cantAttack), each taken once per declaration pass by
-- legalAttackers below rather than once per candidate -- the questions this asks
-- are otherwise as many as three fresh gathers (haste, creature-ness, defender), a
-- fresh grant walk and a fresh restriction walk apiece, which made the pass
-- quadratic in the battlefield (#200). Same hoist Sba.performStateBasedActions
-- takes for the CR 704.3 sweep and Projection.controls takes for the grant list;
-- Projection.projectGiven carries the argument for why a shared board is the same
-- answer, and for why it is valid only within one pure pass over one GameState.
--
-- canAttack itself passes Map.empty for the projection, so a lone query projects
-- per read exactly as it always did. It does NOT pass an empty restriction set:
-- an absent projection is a cache miss the projection recovers from, while an
-- absent restriction is a wrong answer, which is the distinction
-- pairAllowedGiven's grant list already draws.
canAttack :: PlayerId -> ObjectId -> GameState -> Bool
canAttack pid oid gs = canAttackGiven (Projection.controlGrants gs) Map.empty (CombatRestriction.cantAttack [oid] gs) pid oid gs

canAttackGiven :: [Projection.ControlGrant] -> Map ObjectId PC.ProjectedCharacteristics -> Set ObjectId -> PlayerId -> ObjectId -> GameState -> Bool
canAttackGiven grants pcs restricted pid oid gs = case Game.lookupObject oid gs of
  Nothing -> False
  Just obj ->
    Projection.controllerOfGiven grants Set.empty oid gs == Just pid
      && GameState.activePlayer gs == pid
      && Object.zone obj == Zone.Battlefield
      && Object.tapped obj == TapState.Untapped
      -- CR 302.6, relaxed by CR 702.10b: a creature with haste can attack even if
      -- it hasn't been controlled continuously since its controller's most recent
      -- turn began.
      && Summoning.settledOrHastyGiven pcs pid oid gs
      && isCreatureObjectGiven pcs oid gs
      -- CR 508.1c through CR 702.3b: a creature with defender can't attack. It may
      -- still block -- 702.3b says nothing about blocking. A KEYWORD and not a
      -- CombatRestriction, because rule 702 is part of the rulebook: casing on a
      -- keyword is the closed half reading its own rules, where a printed "can't
      -- attack" is open-half card data.
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

-- CR 508.1d's two halves, computed together because neither is usable alone: the
-- requirement instances in force, and a declaration obeying the "maximum possible
-- number of requirements that could be obeyed without disobeying any
-- restrictions". blockCeiling's twin, and the pair is deliberately the same shape
-- so the two rules read the same way at their call sites.
--
-- The maximum is answered in CLOSED FORM -- the best declaration is the instance
-- set, minus whichever instances CR 508.1d's cost clause excuses (below) -- where
-- blockCeiling enumerates every candidate declaration and folds. That is not an
-- optimization of the blocking search; it is a different search, and it is exact
-- only because of what pawl cannot yet print:
--
--   * every attacking RESTRICTION pawl models is per creature (CR 508.1a's own
--     clauses, CR 702.3b's defender, and CR 508.1c's printed
--     CombatRestriction.CantAttack -- Pacifism), and canAttack has already applied
--     all of them to `candidates`, which AttackRequirement.instances then prunes
--     its instances by;
--   * so declaring every required creature at once disobeys nothing -- attacking
--     with one creature cannot make another's attack illegal -- which makes that
--     declaration legal AND maximal by construction. A cost to attack does not
--     break this: CR 508.1h totals the whole declaration at once, so no creature's
--     presence can make another's attack ILLEGAL, only dearer.
--
-- Both bullets fail the moment a set-shaped ATTACKING restriction lands (Silent
-- Arbiter's "no more than one creature can attack each combat", Bonded
-- Construct's "can't attack alone"), and the replacement is blockCeiling's
-- enumeration with blockCeiling's exponential cost (#533). The closed form
-- therefore rests on a missing capability rather than on a claim about Magic.
-- The BLOCKING side's set-shaped restriction has landed -- CR 702.111b's menace,
-- in declarationAllowed -- and reaches nothing here: no restriction on who may
-- block bounds who may attack.
--
-- So nothing here is exponential, and #342's warning about the blocking search has
-- no counterpart: the work is one battlefield walk plus, per requirement, one
-- Projection.affects per candidate. That last read is QUADRATIC in the battlefield
-- rather than linear -- Projection.affects's AttachedPlayerControls arm takes a
-- projection and a control-grant walk per candidate, which is #435's shape and is
-- documented at that arm. Measured on a board of N Goblin Pikers under one Curse:
-- 5 MB allocated at N=100, 15 MB at N=200, 53 MB and 0.02s at N=400. The cost
-- filter below adds one more battlefield walk per REQUIRED creature per attack
-- target, and each stops at the first permanent printing no cost to attack --
-- which on a board with no Ghostly Prison is all of them.
--
-- CR 508.1d's COST CLAUSE is the second component's filter, and it is a modifier
-- on the maximization rather than a check of its own: "if a creature can't attack
-- unless a player pays a cost, that player is not required to pay that cost, even
-- if attacking with that creature would increase the number of requirements being
-- obeyed". A requirement whose creature cannot attack anything without its
-- controller paying is therefore not one the maximum reaches for, and declining
-- to attack with it stays legal.
--
-- Which is why the two components come apart here for the first time. `required`
-- stays every instance in force, because that is what a declaration's obedience
-- is counted against -- attacking with a taxed creature obeys its requirement
-- perfectly well, and paying is then mandatory (CR 508.1j). `best` is the
-- untaxed subset, and it is what makes "no attacks" legal under a Curse of the
-- Nightly Hunt while a Ghostly Prison is out. Both readings of the pair below
-- (obeysAttackRequirements, forcedAttackDeclaration) want exactly that split.
--
-- AttackCost.attacksFreely is asked per required creature against CR 508.1b's
-- whole candidate list of targets, so a creature that could attack a planeswalker
-- for nothing keeps its requirement -- Ghostly Prison's own ruling ("a creature
-- that can't attack you can still attack a planeswalker you control"), and the
-- player's CR 508.1b announcement then decides whether they end up paying.
--
-- No defending player means no target at all, so nothing attacks freely and
-- `best` is empty. Not a fallback: with no defender there is no attack to make
-- (declareAttackers returns before ever prompting), so a requirement that cannot
-- be obeyed is one CR 508.1d's "if able" never reaches.
--
-- Nothing is forced when `required` is empty, which is every board without a
-- Curse on it: Set.filter never calls the predicate, so no cost walk and no
-- target list is built.
--
-- (empty, empty) when no requirement is in force: the maximum is zero, every
-- declaration obeys zero, and CR 508.1d has nothing to say.
attackCeiling :: [ObjectId] -> GameState -> (Set ObjectId, Set ObjectId)
attackCeiling candidates gs =
  let required = AttackRequirement.instances candidates gs
      targets = case Combat.defender (GameState.combat gs) of
        Nothing -> []
        Just defender -> NonEmpty.toList (attackTargets defender gs)
   in (required, Set.filter (\oid -> AttackCost.attacksFreely oid targets gs) required)

-- How many of `required` this declaration obeys -- CR 508.1d's "the number of
-- requirements that are being obeyed". A requirement instance is obeyed exactly
-- when the declaration attacks with its creature. requirementsMet's twin, on a set
-- of creatures rather than a map of pairs.
attackRequirementsMet :: Set ObjectId -> Set ObjectId -> Int
attackRequirementsMet required declaration = Set.size (Set.intersection required declaration)

-- CR 508.1d asked of a declaration that has already passed CR 508.1a and CR
-- 508.1c: does it obey at least as many requirements as the maximum? Split out of
-- legalAttackDeclaration so that declareAttackers can ask it against a ceiling it
-- computed once, rather than paying for a second one -- and so that the two of
-- them cannot drift, since the caller's check is built from this same expression.
obeysAttackRequirements :: (Set ObjectId, Set ObjectId) -> [ObjectId] -> Bool
obeysAttackRequirements (required, best) chosen =
  attackRequirementsMet required (Set.fromList chosen) >= attackRequirementsMet required best

-- CR 508.1: is this declaration one the active player may make? Both checks the
-- rules ask for, in the order they ask them: CR 508.1a's chosen-from set together
-- with CR 508.1c's restrictions, then CR 508.1d's requirements.
--
-- CR 508.1c's restrictions are not a separate conjunct because they are not a
-- separate set: canAttack is the whole of what pawl can say a creature "can't
-- attack" for -- CR 508.1a's own clauses, CR 702.3b's defender, and every printed
-- CombatRestriction.CantAttack -- so being a candidate IS obeying every
-- restriction it knows. That collapse holds only while every restriction is per
-- creature (#533).
--
-- CR 508.1d is not a check but a MAXIMIZATION -- "if the number of requirements
-- that are being obeyed is fewer than the maximum possible number of requirements
-- that could be obeyed without disobeying any restrictions, the declaration of
-- attackers is illegal" -- so it cannot be asked of the declaration alone. It is
-- what makes declaring no attackers at all illegal while a Curse of the Nightly
-- Hunt is on the enchanted player's battlefield.
legalAttackDeclaration :: PlayerId -> [ObjectId] -> GameState -> Bool
legalAttackDeclaration pid chosen gs = legalAttackDeclarationGiven (legalAttackers pid gs) chosen gs

legalAttackDeclarationGiven :: [ObjectId] -> [ObjectId] -> GameState -> Bool
legalAttackDeclarationGiven candidates chosen gs =
  all (\oid -> List.elem oid candidates) chosen
    && obeysAttackRequirements (attackCeiling candidates gs) chosen

-- A declaration that is always legal: one attaining CR 508.1d's maximum, which
-- with no requirement in force is the empty one (declining to attack). Not an
-- answer the engine ever prefers to the active player's own -- declareAttackers
-- reaches for it only when an interpreter hands back a declaration the rules
-- forbid.
--
-- Taken as a filter over `candidates` rather than as Set.toList, so the forced
-- declaration comes back in the order the player was offered its creatures.
forcedAttackDeclaration :: (Set ObjectId, Set ObjectId) -> [ObjectId] -> [ObjectId]
forcedAttackDeclaration (_, best) = filter (\oid -> Set.member oid best)

-- CR 509.1a: a blocking creature must be untapped and controlled by the
-- defending player. Plus CR 509.1b's PER-CREATURE restrictions, which are the
-- last conjunct, on canAttackGiven's terms and for its reason.
--
-- Only the per-creature ones. CR 509.1b's restrictions are mostly PAIRWISE
-- (flying, fear) and cannot be decided about a blocker alone -- those live in
-- pairAllowed, which is asked of a (blocker, attacker) pair -- and CR 702.111b's
-- menace is SET-SHAPED, which lives in declarationAllowed. So the rule is
-- answered in three places, one per shape of restriction, and this is the
-- narrowest.
--
-- Summoning sickness is NOT a blocking restriction. CR 302.6 restricts attacking
-- and activated abilities with the tap or untap symbol, and says nothing about
-- blocking.
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
      && Object.zone obj == Zone.Battlefield
      && Object.tapped obj == TapState.Untapped
      && isCreatureObjectGiven pcs oid gs
      -- CR 509.1b: every PRINTED per-creature blocking restriction in force
      -- (Pacifism).
      && not (Set.member oid restricted)

legalBlockers :: PlayerId -> GameState -> [ObjectId]
legalBlockers pid gs = legalBlockersGiven (Projection.controlGrants gs) (Projection.projectAll gs) pid gs

-- The restriction walk is taken HERE rather than handed in, where the grant list
-- and the projection are both parameters: those two are shared with the whole
-- blocking search (blockCeilingGiven's pairs, legalBlockDeclaration's checks),
-- while the restricted set is read by nothing but this filter, and threading it
-- through every caller would buy one battlefield walk that short-circuits on the
-- first permanent of almost every board.
legalBlockersGiven :: [Projection.ControlGrant] -> Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> GameState -> [ObjectId]
legalBlockersGiven grants pcs pid gs =
  let controlled = Projection.controlsGiven grants pid gs
      restricted = CombatRestriction.cantBlock controlled gs
   in filter (\oid -> canBlockGiven grants pcs restricted pid oid gs) controlled

-- CR 702.9b: a creature with flying can't be blocked except by creatures with
-- flying and/or reach (CR 702.17b).
--
-- Note the asymmetry, which is easy to get backwards: 702.9b's second sentence
-- says a creature with flying CAN block a creature with or without flying.
-- Flying restricts being blocked, never blocking. The question is asked of the
-- ATTACKER first, and only then of the blocker.
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
-- The same asymmetry as flying (see evasionAllows): fear restricts being BLOCKED,
-- never blocking, so the question is asked of the ATTACKER first. Both halves of
-- the exception read the PROJECTION -- a creature made black by a CR 613 layer-5
-- effect blocks legally, and a devoid creature with a black mana cost does not.
fearAllows :: ObjectId -> ObjectId -> GameState -> Bool
fearAllows = fearAllowsGiven Map.empty

fearAllowsGiven :: Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> ObjectId -> GameState -> Bool
fearAllowsGiven pcs blocker attacker gs =
  not (Projection.hasKeywordGiven pcs Keyword.Fear attacker gs)
    || Set.member CardType.Artifact (Projection.cardTypesGiven pcs blocker gs)
    || Set.member Color.Black (Projection.colorsGiven pcs blocker gs)

-- CR 702.14c: "A creature with landwalk can't be blocked as long as the
-- defending player controls at least one land with the specified land type (as
-- in 'islandwalk')."
--
-- The BLOCKER is not an argument, and that is CR 702.14d stated in the type.
-- "Landwalk abilities don't 'cancel' one another": its example is a player who
-- controls a snow Forest AND a creature with snow forestwalk, and who still may
-- not block a snow-forestwalker. Landwalk is a property of the defending
-- player's LANDS, never a comparison between the two creatures -- unlike
-- protection -- so a signature that could read the blocker is a signature that
-- could answer 702.14d wrong.
--
-- The same asymmetry the other two evasion gates have (see evasionAllows):
-- landwalk restricts being BLOCKED, so the question is asked of the ATTACKER.
--
-- Membership over the projection's keyword map, never its counts: CR 702.14e
-- says "multiple instances of the same kind of landwalk on the same creature are
-- redundant". The MAP rather than hasKeywordGiven, because CR 702.14a's "[type]"
-- rides the constructor -- there is no single Keyword value to ask about, which
-- is Projection.totalToxic's situation and takes its shape.
--
-- All four of CR 702.14c's clauses, because the keyword carries a Filter: "with
-- the specified land type (as in 'islandwalk'), with the specified type or
-- supertype (as in 'artifact landwalk'), without the specified type or supertype
-- (as in 'nonbasic landwalk'), or with both the specified type or supertype and
-- the specified subtype (as in 'snow swampwalk')". All four have a printing in
-- the pool: Bog Wraith the first, Vectis Gloves the second -- the only paper
-- source of artifact landwalk, and it GRANTS the keyword rather than printing it
-- on a creature -- Dryad Sophisticate the third and Legions of Lim-Dûl the
-- fourth.
landwalkAllows :: ObjectId -> GameState -> Bool
landwalkAllows attacker gs = landwalkAllowsGiven (Projection.controlGrants gs) Map.empty attacker gs

landwalkAllowsGiven :: [Projection.ControlGrant] -> Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> GameState -> Bool
landwalkAllowsGiven grants pcs attacker gs =
  let -- A wildcard rather than an exhaustive case, the Keyword.flashbackCost
      -- precedent: this asks about ONE named constructor rather than classifying
      -- every keyword, so a new arm has nothing to say here.
      landCriterionOf keyword = case keyword of
        Keyword.Landwalk criterion -> Just criterion
        _ -> Nothing
      walked = Maybe.mapMaybe landCriterionOf (Map.keys (Projection.keywordsGiven pcs attacker gs))
      -- CR 508.5: "If an ability of an attacking creature refers to a defending
      -- player ... the defending player it's referring to is the player that
      -- creature is attacking, the controller of the planeswalker that creature
      -- is attacking, or the protector of the battle that creature is attacking."
      -- Landwalk is exactly such an ability, so this is that rule's own case
      -- split, read off the attack itself rather than off the blocker's
      -- controller -- those two coincide only while there is exactly one
      -- defending player (CR 802, #175). Nothing means the object is not
      -- attacking, so no landwalk of its can restrict anything.
      --
      -- CR 508.5's second sentence -- the planeswalker's controller "before it
      -- was removed from combat", once the creature is no longer attacking -- is
      -- last known information, and this reads the controller LIVE instead.
      -- Unreachable in the pool, which has no card that can remove an attacked
      -- planeswalker from combat and change who controlled it (#537), and
      -- unobservable besides: no attacker in the pool has both landwalk and a
      -- reason to attack a planeswalker.
      defenderOf target = case target of
        AttackTarget.OfPlayer pid -> Just pid
        AttackTarget.OfPlaneswalker oid -> Projection.controllerOfGiven grants Set.empty oid gs
      defendingPlayer = defenderOf =<< Map.lookup attacker (Combat.attackers (GameState.combat gs))
      -- CR 702.14c's "the defending player controls at least one land ...".
      --
      -- Lazy, and load-bearing: this walks the whole battlefield, and `any` below
      -- never forces it for an attacker without landwalk, which is every attacker
      -- in almost every combat (#200).
      defendersLands = foldMap (\pid -> Projection.controlsGiven grants pid gs) defendingPlayer
      -- The land-ness is asked HERE and never by the criterion: every clause of
      -- CR 702.14c reads "at least one LAND with/without ...", so it belongs to
      -- the rule rather than to the card's parameter, and a printing cannot omit
      -- it. The criterion answers the "[type]" half alone.
      --
      -- CR 205.3d ("an object can't gain a subtype that doesn't correspond to
      -- one of that object's types") is what makes the card-type test all but
      -- redundant for the two clauses whose criterion NAMES a land type --
      -- islandwalk's and snow swampwalk's -- and "all but" is why it is still
      -- asked even for them: nothing in the projection enforces 205.3d, so a
      -- Modification.AddLandSubtype aimed at a non-land would otherwise be walked
      -- on. For the other two it is not redundant at all: their criteria name no
      -- land type, so "nonbasic landwalk" would match every nonbasic PERMANENT
      -- and "artifact landwalk" every artifact.
      --
      -- CR 109.5's "you" for the criterion is the ATTACKER's controller, and the
      -- source is the attacker -- the same pairing every keyword-borne Filter
      -- takes. No landwalk in the pool reads either (all four clauses are type,
      -- supertype and subtype tests), so the context is well-defined rather than
      -- exercised. Hoisted, since it does not vary per candidate.
      context = Filter.MkContext (Projection.controllerOfGiven grants Set.empty attacker gs) (Just attacker)
      -- ONE projection per candidate: Filter.cardTypes is the very set
      -- Projection.cardTypesGiven would rebuild, so the land test reads it off
      -- the view rather than projecting the object a second time. The comment
      -- above about walking the whole battlefield is why that matters (#200).
      matchesCriterion criterion oid =
        let view = Projection.viewOfObjectGiven pcs grants oid gs
         in Set.member CardType.Land (Filter.cardTypes view) && Filter.matches context view criterion
   in not (any (\criterion -> any (matchesCriterion criterion) defendersLands) walked)

-- CR 702.111b: "A creature with menace can't be blocked except by two or more
-- creatures."
--
-- The first restriction of the SET shape #533 named -- its blocking half; the
-- attacking half is still open, at attackCeiling -- and the reason this takes
-- the whole declaration where its three siblings above take a pair: "two or more
-- creatures" is a fact about how many blockers were assigned to one attacker,
-- which no predicate on a single (blocker, attacker) pair can state. Splitting
-- the declaration into pairs loses exactly the information the rule reads.
--
-- "EXCEPT BY two or more", not "must be blocked by two or more". An attacker
-- nobody blocked is not blocked at all, so 702.111b has nothing to say about it
-- -- and that is why this folds over the attackers the declaration MENTIONS
-- (Map.elems) rather than over every attacker in combat. Declining to block is
-- always legal under restrictions alone, which is the seed blockCeiling's fold
-- relies on.
--
-- The same asymmetry the other three evasion gates have (see evasionAllows): the
-- keyword is read off the ATTACKER. A creature with menace blocking alone is
-- legal, since 702.111b restricts being blocked and says nothing about blocking.
--
-- Membership rather than the projection's per-keyword count, on
-- landwalkAllowsGiven's terms: CR 702.111c says "multiple instances of menace on
-- the same creature are redundant", so a creature with two of them still needs
-- two blockers rather than four.
menaceAllows :: Map ObjectId ObjectId -> GameState -> Bool
menaceAllows = menaceAllowsGiven Map.empty

menaceAllowsGiven :: Map ObjectId PC.ProjectedCharacteristics -> Map ObjectId ObjectId -> GameState -> Bool
menaceAllowsGiven pcs declaration gs =
  let -- blocker -> attacker inverted into attacker -> how many blockers, which is
      -- the only reading of a declaration 702.111b cares about.
      blockerCounts = Map.fromListWith (+) (fmap (\attacker -> (attacker, 1 :: Int)) (Map.elems declaration))
      -- The count first, so an attacker that is comfortably blocked never pays
      -- for a keyword read (#200's posture, in the one place a declaration check
      -- sits inside candidateDeclarations' exponential filter).
      allowed (attacker, count) = count >= 2 || not (Projection.hasKeywordGiven pcs Keyword.Menace attacker gs)
   in all allowed (Map.toList blockerCounts)

-- CR 509.1b asked of ONE (blocker, attacker) pair: may this creature block that
-- one at all? This is also what CR 509.1c's requirements mean by "able to block"
-- (Lure), which is why it is a named function and not a lambda inside the
-- declaration check.
--
-- A conjunction of independent restriction checks, because CR 509.1b says
-- different evasion abilities are cumulative: an attacker with flying AND shadow
-- admits only blockers that answer both.
--
-- Every restriction ASKED HERE is at most pairwise, and CR 702.14c's landwalk is
-- less than that: it does not read the blocker at all. Menace (CR 702.111b) is
-- not pairwise -- it constrains the SET blocking one attacker -- so it is asked
-- in declarationAllowed, of the whole declaration, and never here. The two are
-- cumulative rather than alternative, which is CR 509.1b's own "different evasion
-- abilities are cumulative" read across the shapes: a menace attacker that also
-- has fear needs two blockers AND needs each of them to pass 702.36b.
pairAllowed :: [ObjectId] -> [ObjectId] -> ObjectId -> ObjectId -> GameState -> Bool
pairAllowed candidates attackers blocker attacker gs =
  pairAllowedGiven (Projection.controlGrants gs) Map.empty candidates attackers blocker attacker gs

-- pairAllowed against a pre-projected board, which is what the callers below
-- pass: this question is asked once per (blocker, attacker) PAIR, so each of its
-- evasion reads was a fresh gather in a doubly nested loop (#200). The grant list
-- is threaded for the same reason and on canBlockGiven's terms: an EMPTY pcs is a
-- cache miss the projection recovers from, but an empty grant list is a wrong
-- answer, so callers pass the real one.
pairAllowedGiven :: [Projection.ControlGrant] -> Map ObjectId PC.ProjectedCharacteristics -> [ObjectId] -> [ObjectId] -> ObjectId -> ObjectId -> GameState -> Bool
pairAllowedGiven grants pcs candidates attackers blocker attacker gs =
  -- CR 509.1a: the blocker must be one this player could block with at all, and
  -- the attacker must actually be attacking.
  List.elem blocker candidates
    && List.elem attacker attackers
    && evasionAllowsGiven pcs blocker attacker gs
    && fearAllowsGiven pcs blocker attacker gs
    && landwalkAllowsGiven grants pcs attacker gs

-- CR 509.1b: the defending player checks each creature for RESTRICTIONS, and if
-- any are disobeyed the DECLARATION is illegal.
--
-- The unit of legality is the whole declaration, not the pair, and that is not a
-- stylistic choice. Menace (CR 702.111b) says a creature can't be blocked except
-- by TWO OR MORE creatures -- a constraint on the SET blocking an attacker, which
-- no per-pair predicate can express. Every other evasion ability the pool has --
-- flying, reach, fear, landwalk -- is pairwise or narrower; designing to them
-- would be designing to the case that misleads. See the M2a spec, section 3.
--
-- So the two shapes of restriction are both asked here, one conjunct each:
-- pairAllowed over the pairs, and menaceAllows over the whole map. This is also
-- the seam blockCeiling's enumeration is filtered through, so CR 509.1c's
-- "maximum possible number of requirements that could be obeyed without
-- disobeying any restrictions" maximizes over declarations menace already allows.
--
-- Takes the projected board rather than projecting per read, because the
-- set-shaped conjunct reads a keyword and this sits inside candidateDeclarations'
-- exponential filter (#342). There is no per-read twin the way pairAllowed has
-- one: both callers are already inside a hoisted pass.
declarationAllowed :: Map ObjectId PC.ProjectedCharacteristics -> (ObjectId -> ObjectId -> Bool) -> Map ObjectId ObjectId -> GameState -> Bool
declarationAllowed pcs able declaration gs =
  all (uncurry able) (Map.toList declaration)
    && menaceAllowsGiven pcs declaration gs

-- How many of `requirements` this declaration obeys -- CR 509.1c's "the number of
-- requirements that are being obeyed". A requirement instance is obeyed exactly
-- when the declaration has its blocker blocking its attacker.
requirementsMet :: Set (ObjectId, ObjectId) -> Map ObjectId ObjectId -> Int
requirementsMet requirements declaration =
  Set.size (Set.filter (\(blocker, attacker) -> Map.lookup blocker declaration == Just attacker) requirements)

-- Every declaration CR 509.1a lets the defending player write down, given the
-- pairs CR 509.1b allows: each candidate blocker independently either blocks
-- nothing or blocks one attacker it may block.
--
-- EXPONENTIAL, and honestly so: the list has product over candidates of
-- (1 + how many attackers that candidate may block) entries, so it is
-- O((attackers + 1) ^ blockers) in the worst case. Nothing caps it and nothing
-- samples it -- a cap would answer CR 509.1c's "maximum possible number" with a
-- number that is not the maximum, which is worse than being slow. What keeps it
-- off the hot path is blockCeiling's guard: this is never called unless some
-- requirement is actually in force, which needs a card like Lure on the
-- battlefield (#342).
candidateDeclarations :: (ObjectId -> ObjectId -> Bool) -> [ObjectId] -> [ObjectId] -> [Map ObjectId ObjectId]
candidateDeclarations able candidates attackers =
  let extend acc blocker =
        let options = Nothing : fmap Just (filter (\attacker -> able blocker attacker) attackers)
            apply declaration option = case option of
              Nothing -> declaration
              Just attacker -> Map.insert blocker attacker declaration
         in concatMap (\declaration -> fmap (apply declaration) options) acc
   in List.foldl' extend [Map.empty] candidates

-- CR 509.1c's two halves, computed together because neither is usable alone: the
-- requirement instances in force, and a declaration obeying the "maximum possible
-- number of requirements that could be obeyed without disobeying any
-- restrictions".
--
-- Map.empty when no requirement is in force, WITHOUT enumerating anything. That
-- is not an optimization of the common case so much as the whole of it: with no
-- requirement the maximum is zero, every declaration obeys zero, and CR 509.1c
-- has nothing to say. A board without Lure never pays a search.
--
-- The maximum is taken by folding rather than by `maximum`, and the fold's seed is
-- Map.empty -- which is always a legal declaration under restrictions alone, since
-- declining to block disobeys no restriction -- so the answer is total and needs
-- no partial function. Ties go to the EARLIER declaration in enumeration order;
-- which one is picked matters only to forcedBlockDeclaration's broken-interpreter
-- path, never to legality, which compares counts.
--
-- One grant walk and one whole-board projection for the whole search, threaded
-- into the candidate list and into every pair `able` judges -- see canAttackGiven
-- and Projection.projectGiven. Nothing between the projection and its uses can
-- move: this is a pure function of one GameState. blockCeilingGiven is the half
-- legalBlockDeclaration reaches, so that the two of them share one board rather
-- than taking one apiece.
blockCeiling :: PlayerId -> GameState -> (Set (ObjectId, ObjectId), Map ObjectId ObjectId)
blockCeiling pid gs = blockCeilingGiven (Projection.controlGrants gs) (Projection.projectAll gs) pid gs

blockCeilingGiven :: [Projection.ControlGrant] -> Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> GameState -> (Set (ObjectId, ObjectId), Map ObjectId ObjectId)
blockCeilingGiven grants pcs pid gs =
  let attackers = Map.keys (Combat.attackers (GameState.combat gs))
      candidates = legalBlockersGiven grants pcs pid gs
      able blocker attacker = pairAllowedGiven grants pcs candidates attackers blocker attacker gs
      requirements = BlockRequirement.instances able candidates attackers gs
      better best declaration =
        if requirementsMet requirements declaration > requirementsMet requirements best
          then declaration
          else best
      legal = filter (\declaration -> declarationAllowed pcs able declaration gs) (candidateDeclarations able candidates attackers)
   in ( requirements,
        if Set.null requirements
          then Map.empty
          else List.foldl' better Map.empty legal
      )

-- CR 509.1: is this declaration one the defending player may make? Both checks
-- the rule asks for, in the order it asks them: CR 509.1b's restrictions, then CR
-- 509.1c's requirements.
--
-- CR 509.1c is not a check but a MAXIMIZATION -- "if the number of requirements
-- that are being obeyed is fewer than the maximum possible number of requirements
-- that could be obeyed without disobeying any restrictions, the declaration of
-- blockers is illegal" -- so it cannot be asked of the declaration alone. It is
-- what makes declaring no blockers at all illegal while a Lure is on the
-- battlefield.
--
-- CR 509.1c's cost clause ("if a creature can't block unless a player pays a
-- cost, that player is not required to pay that cost") and CR 509.1d's cost
-- locking are not implemented: no card in the pool makes blocking cost anything
-- (#343).
legalBlockDeclaration :: PlayerId -> Map ObjectId ObjectId -> GameState -> Bool
legalBlockDeclaration pid declaration gs =
  -- Hoisted exactly as blockCeiling hoists, and for the same reason.
  let grants = Projection.controlGrants gs
      pcs = Projection.projectAll gs
      attackers = Map.keys (Combat.attackers (GameState.combat gs))
      candidates = legalBlockersGiven grants pcs pid gs
      able blocker attacker = pairAllowedGiven grants pcs candidates attackers blocker attacker gs
      (requirements, best) = blockCeilingGiven grants pcs pid gs
   in declarationAllowed pcs able declaration gs
        && requirementsMet requirements declaration >= requirementsMet requirements best

-- A declaration that is always legal: one attaining CR 509.1c's maximum, which
-- with no requirement in force is the empty one (declining to block). Not an
-- answer the engine ever prefers to the defending player's own -- declareBlockers
-- reaches for it only when an interpreter hands back a declaration the rules
-- forbid.
forcedBlockDeclaration :: PlayerId -> GameState -> Map ObjectId ObjectId
forcedBlockDeclaration pid gs = snd (blockCeiling pid gs)

-- Who is CURRENTLY blocking this attacker -- not whether it is blocked. The two
-- questions come apart (see isBlocked), and a reader that wants blocked-ness must
-- ask isBlocked rather than test this for emptiness.
blockersOf :: ObjectId -> GameState -> Set ObjectId
blockersOf oid gs = Map.findWithDefault Set.empty oid (Combat.blockers (GameState.combat gs))

-- CR 509.1h: "An attacking creature with one or more creatures declared as
-- blockers for it becomes a blocked creature ... A creature remains blocked even
-- if all the creatures blocking it are removed from combat."
--
-- So blocked-ness is a STATUS that the declaration confers once, not a running
-- count of who is still blocking. The map's KEY is that status -- declareBlockers
-- creates it and only Game.removeFromCombat's Map.delete arm (the attacker itself
-- leaving combat, CR 506.4) and Combat.clearCombat ever drop it. The SET behind
-- the key is the separate CR 510.1c question of who is currently blocking, and it
-- can empty out while the key stays: a regenerated blocker (CR 701.19a) is deleted
-- from it, and a blocker that merely died is filtered out at assignment time.
--
-- Testing the set for emptiness instead is the bug this replaced: a Goblin Piker
-- blocked by a Drudge Skeletons that regenerated before the combat damage step
-- became "unblocked" and hit the defending player for 2. DamageSpec's
-- "Blocked stays blocked" group is what proves it, both ways the set can empty.
isBlocked :: ObjectId -> GameState -> Bool
isBlocked oid gs = Map.member oid (Combat.blockers (GameState.combat gs))

-- Every creature currently IN combat: the attackers, plus everything still
-- blocking one of them. Not the keys of Combat.joinedUnder, which can outlive the
-- record it was taken for -- Pawl.Engine.Departure edits Combat.attackers directly, and
-- deliberately leaves Combat.blockers alone (CR 509.1h).
combatants :: Combat -> Set ObjectId
combatants c = Set.union (Map.keysSet (Combat.attackers c)) (Set.unions (Map.elems (Combat.blockers c)))

-- CR 506.4: "A permanent is removed from combat if ... its controller changes ...
-- or if it's an attacking or blocking creature that ... stops being a creature."
--
-- The two clauses of that rule whose trigger is DERIVED state, which is why this
-- is a sampler and not a hook. Neither has an event to hang a removal on: a
-- control-granting static ability (Control Magic's SetControllerToSource) is
-- re-read live by the projection, and even a stored SetController is installed by
-- a resolution that never announces "control changed" (#198); creature-ness is
-- the same, a CR 613 layer-4 answer that changes the moment the effect producing
-- it appears or ends. The same shape, and the same argument, as
-- Engine.checkControlContinuity's CR 302.6 scan; Engine's settleForPriority runs
-- both, at every point the board can change.
--
-- The TIMING that costs: the rules remove the permanent the instant the
-- characteristic changes, and this notices at the next settle. Nothing can see
-- the difference. CR 117.5 makes "whenever a player would get priority" the
-- coarsest moment anything observes the board, and the two readers of the combat
-- record -- the CR 510 damage steps and Filter.IsAttacking at targeting -- both
-- sit behind a priority grant, which settles first. The window that would open it
-- is a single resolution that changes control or card types and then reads combat
-- status in a LATER effect of the same resolution; no card in the pool has one,
-- and the settle loop is where such a card's fix would go.
--
-- It only ever REMOVES. That asymmetry is what makes the sampling sound, exactly
-- as it is there: a discrepancy proves the characteristic changed, so removing is
-- always right, while putting a creature BACK when control returns or a new
-- animation starts would invent a CR 506.4 the rules do not have -- removal
-- from combat is permanent for that combat phase (the glossary: "has no further
-- involvement in that combat phase").
--
-- Battlefield-scoped, so this stays these two clauses and nothing else. CR 110.1
-- makes a permanent something on the battlefield, and an object that has LEFT it
-- was already removed by that separate clause of CR 506.4 -- whose implementation
-- is elsewhere (Pawl.Engine.Departure, and Pawl.Engine.Damage's liveness filters for the
-- CR 509.1h key this must not disturb). Without the gate, an object gone from
-- GameState.objects would fail both tests here -- no controller to match, and no
-- card types to find a creature in -- and be swept up under the wrong clause.
--
-- Removal goes through Game.removeFromCombat, so a removed ATTACKER takes its
-- blocked-ness with it (Map.delete) while a removed BLOCKER leaves the attacker
-- blocked with nothing blocking it (Set.delete inside a surviving key) --
-- CR 509.1h's last sentence, argued in full at that function.
--
-- Creatures only, which is what `combatants` gathers: an ATTACKED planeswalker
-- is not in that set, and CR 506.4's clauses about one are answered where its
-- target is read instead (stillAttacked, whose own haddock argues why the two
-- are the same board). CR 506.4d/e are about a permanent that is both an
-- attacked planeswalker and a blocking creature, or both a planeswalker and a
-- battle, and nothing can be either: nothing in the pool prints two of those card
-- types (#503) and there is no battle card type (#302). CR 506.4's "becomes a
-- battle" clause is unreachable for that same reason, and "phases out" for
-- phasing's (#154).
--
-- A combatant with no entry in Combat.joinedUnder is left alone by the CONTROL
-- clause, because there is nothing to compare it against and this only ever
-- removes. Unreachable through the engine: declareAttackers and declareBlockers
-- write the snapshot in the same update that puts the creature into the record.
-- The types clause needs no such comparand -- CR 506.3 lets only a creature be
-- declared, so every combatant was one, and "is it one now" is the whole test.
--
-- Nobody in combat short-circuits, which is most of the game: the grant list and
-- the gathered candidate list each cost a whole-battlefield scan
-- (Projection.controlGrants, Projection.gather), both hoisted out of the
-- per-combatant loop, and this runs on every settle pass alongside
-- checkControlContinuity's own. The gather is NOT shared with the one
-- Sba.performStateBasedActions makes earlier in the same settle pass: a
-- state-based action can change the board between the two, and a sample has to
-- gather against the state it is judging (Projection.projectGiven's own caveat).
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
-- is why this is a plain Game () and not an ability.
--
-- Runs before any trigger, by construction rather than by arrangement:
-- Engine.runStep calls runTurnBasedActions before priorityLoop, and it is
-- priorityLoop's settleForPriority that first places a triggered ability.
--
-- Not prompted with one candidate (#169): CR 507.1's condition is a multiplayer
-- game, and a two-player game's defending player is CR 506.2's nonactive player
-- with nothing to ask.
--
-- No candidates means the action does not happen and Combat.defender stays
-- Nothing, which declareAttackers reads as no attack being possible. Unreachable
-- in a running game -- the last opponent leaving ends it (CR 104.2a) -- and
-- NonEmpty is what makes the prompt's fallback total rather than this branch.
--
-- An answer outside the candidate list is a broken interpreter, not a game state,
-- and degrades to the first candidate: always legal, least eventful, and the same
-- SHAPE (degrade totally rather than fail) Setup.subgameStateFrom uses for an
-- out-of-order starting player -- and the same VALUE Replay.defaultAnswer gives
-- this very prompt (NonEmpty.head candidates). The two must agree: a diverging
-- fallback here would be an invisible bug, since neither path can observe the
-- other.
chooseDefender :: Game ()
chooseDefender = do
  gs <- State.get
  let pid = GameState.activePlayer gs
  -- CR 800.4j: a turn whose active player has left continues without one, so the
  -- action the rules assign to the active player has no subject. CR 800.4j is a
  -- priority rule and stops there; CR 800.4h is the one that would hand this
  -- choice -- required of the active player by CR 507.1 and CR 703.4h -- to the
  -- next player in turn order. pawl skips it, which is an unobservable divergence
  -- rather than a vacuous case (#181); the argument is on Pawl.Types.Combat's
  -- defender field and is not repeated here.
  --
  -- Engine.runTurnBasedActions binds the identical test (hasActive) before
  -- calling this, so on the engine's path the two guards are the same value BY
  -- EQUIVALENCE -- the mechanism (what runs between the bind and the call, and
  -- why the predicate here is the same expression) is argued at that site, not
  -- here. Redundant on that path, and declined for a reason that is not a green
  -- suite: this is the copy a DIRECT caller depends on -- a spec, or a second
  -- combat phase spliced by an effect, neither of which goes through Engine's
  -- guard -- and CombatSpec's direct-call case is the test that discriminates
  -- it. Do not delete it as redundant; Engine.runTurnBasedActions's comment
  -- states this same argument from the other end.
  Monad.when (List.elem pid (Game.stillPlaying gs)) $
    case NonEmpty.nonEmpty (attackableOpponents gs) of
      Nothing -> pure ()
      Just candidates -> do
        chosen <- case candidates of
          only NonEmpty.:| [] -> pure only
          _ -> do
            let decider = Decide.deciderFor pid gs
            answer <- Trans.lift (Program.prompt (Prompt.ChooseDefender decider pid candidates))
            pure $
              if List.elem answer (NonEmpty.toList candidates)
                then answer
                else NonEmpty.head candidates
        State.modify' $ \g ->
          g {GameState.combat = (GameState.combat g) {Combat.defender = Just chosen}}

-- CR 508.1b's announcement for ONE creature, and CR 508.4's choice for one
-- creature put onto the battlefield attacking -- the same question, so the same
-- function and the same prompt (Prompt.ChooseAttackTarget says why one and not
-- two).
--
-- Not prompted with one candidate, which is CR 508.1b's own condition: the rule
-- calls for an announcement only "if the defending player controls any
-- planeswalkers, is the protector of any battles, or the game allows the active
-- player to attack multiple other players", and attackTargets returns a lone
-- defending player exactly when none of those holds. Where the rules leave
-- nothing to ask, don't prompt.
--
-- An answer outside the candidate list is a broken interpreter, not a game
-- state, and degrades to the first candidate -- the defending player, always a
-- legal thing to attack and the least eventful answer. chooseDefender's posture
-- and Replay.defaultAnswer's value for this prompt, which must agree with it for
-- chooseDefender's reason: neither path can observe the other.
announceAttackTarget :: PlayerId -> ObjectId -> NonEmpty.NonEmpty AttackTarget.AttackTarget -> Game AttackTarget.AttackTarget
announceAttackTarget pid oid options = case options of
  only NonEmpty.:| [] -> pure only
  _ -> do
    gs <- State.get
    let decider = Decide.deciderFor pid gs
    answer <- Trans.lift (Program.prompt (Prompt.ChooseAttackTarget decider pid oid options))
    pure $
      if List.elem answer (NonEmpty.toList options)
        then answer
        else NonEmpty.head options

-- CR 508.1: the active player chooses which creatures attack (CR 508.1a), the
-- declaration is judged against CR 508.1c's restrictions and CR 508.1d's
-- requirements, and then they become tapped and attacking (CR 508.1f).
--
-- No legal attackers means no prompt: declining is then the only legal answer,
-- and asking would be inventing a decision. Same reasoning as CR 510.1c's single
-- blocker, and a requirement cannot make it wrong -- CR 508.1d's instances are
-- minted from the candidate list, so with no candidate there is nothing to
-- require.
--
-- The steps run here are CR 508.1a, 508.1b, 508.1c, 508.1d, 508.1f, 508.1h,
-- 508.1i, 508.1j and 508.1k, in the rule's own order, plus the event CR 508.1m's
-- triggers watch. CR 508.1g's OPTIONAL costs to attack -- "costs a player may pay
-- 'as' a creature attacks", which CR 701.43d and CR 702.154b name exert and
-- enlist as -- are not implemented (#597).
--
-- CR 508.1's preamble is the one clause that costs this function its shape: "if
-- at any point during the declaration of attackers, the active player is unable
-- to comply with any of the steps listed below, the declaration is illegal; the
-- game returns to the moment before the declaration". A cost to attack is the
-- first step pawl can fail to comply with AFTER the board has been written to, so
-- the entry state is captured and restored. See the payment below.
declareAttackers :: PlayerId -> Game ()
declareAttackers pid = do
  gs <- State.get
  let candidates = legalAttackers pid gs
  case Combat.defender (GameState.combat gs) of
    -- Nothing means no attack is possible, and that is the right answer rather
    -- than a fallback: either the beginning of combat step's turn-based action
    -- has not run, or it ran on a turn with no active player (CR 800.4j), or it
    -- found no opponents. Never a place to recompute a defender -- doing so is
    -- the head-of-list behaviour this replaced.
    Nothing -> pure ()
    Just defender ->
      Monad.unless (null candidates) $ do
        let decider = Decide.deciderFor pid gs
        chosen <- Trans.lift (Program.prompt (Prompt.DeclareAttackers decider pid candidates))
        -- Filtered, not trusted: an interpreter cannot attack with a creature
        -- that is not legally an attacker.
        let isCandidate oid = List.elem oid candidates
            offered = filter isCandidate chosen
            -- CR 508.1d's maximization, taken ONCE for both questions below --
            -- one battlefield walk, against the same candidate list the prompt
            -- was built from.
            bound = attackCeiling candidates gs
            -- Declining to attack is NOT always legal: with a CR 508.1d
            -- requirement on the board (Curse of the Nightly Hunt), "no attacks"
            -- can itself be the illegal answer, so the filtered answer is not a
            -- state this can always accept. It degrades to forcedAttackDeclaration
            -- instead -- always legal, and EQUAL to the filtered answer whenever
            -- no requirement is in force, so no board that had a behaviour before
            -- has a different one now.
            --
            -- The whole answer is replaced rather than repaired, which is
            -- declareBlockers' posture and rests on its argument: a declaration is
            -- illegal AS A WHOLE (CR 508.1's own "the declaration is illegal"), and
            -- unioning the missing creatures into the player's answer would be
            -- sound only while every restriction stays per-creature (#533). Nor is
            -- it re-prompted -- a pure `Prompt r -> r` returns the identical wrong
            -- answer -- and this is NOT CR 733's rewind, which is about human
            -- error at a table rather than an engine check.
            --
            -- It is not the engine choosing for the player: an enforcing
            -- interpreter never arrives here, and the player's answer was taken and
            -- rejected before this ran. Where a requirement leaves exactly one
            -- legal declaration, this hands back the one the rules already forced.
            -- Where it leaves several -- any SUPERSET of the required creatures
            -- also attains the maximum, since attacking with more is always legal
            -- -- this takes the smallest, which is the least the rules can be said
            -- to have forced. That is a real choice among distinguishable
            -- declarations, and it is why nothing but a broken interpreter may
            -- reach it; the same is true of forcedBlockDeclaration.
            attacking =
              if obeysAttackRequirements bound offered
                then offered
                else forcedAttackDeclaration bound candidates
            -- CR 508.1f: declaring an attacker taps it -- unless it has vigilance
            -- (CR 702.20b), which does not change WHETHER it attacks, only what
            -- attacking does to it.
            tapIt g oid =
              if Projection.hasKeyword Keyword.Vigilance oid g
                then g
                else g {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) oid (GameState.objects g)}
            -- CR 506.4's comparand, taken here because here is where the creature
            -- joins combat. `pid` and not a fresh Projection.controllerOf call:
            -- canAttack has already required controllerOf == Just pid of every
            -- creature in `attacking` (CR 508.1a), so the two are the same value
            -- and this one cannot disagree with the legality check.
            joined = Map.fromList (fmap (\oid -> (oid, pid)) attacking)
            -- CR 508.1b's candidates, taken ONCE for the whole declaration and
            -- from the state it is judged against: a pure `Prompt r -> r` cannot
            -- change the board between two announcements, so per-creature
            -- derivation would be the same battlefield walk for the same answer.
            targets = attackTargets defender gs
        -- CR 508.1b: the announcement, one question per chosen creature. Taken
        -- here rather than beside the CR 508.1a prompt because that is the rule's
        -- own order, and asked of `attacking` rather than of `chosen` so that a
        -- creature the CR 508.1d degradation dropped is never announced -- on
        -- every path but a broken interpreter's the two lists are equal anyway.
        recorded <- fmap Map.fromList (Monad.mapM (\oid -> fmap ((,) oid) (announceAttackTarget pid oid targets)) attacking)
        -- UNIONED into the record, not written over it. Nothing in the pool
        -- can have joined combat before this runs -- putOntoBattlefieldAttacking
        -- is reachable only from a resolution, and the earliest one is the
        -- priority round after this action -- but "the record is mine alone"
        -- is exactly the assumption CR 508.8's second clause breaks, and
        -- replacing the map would silently remove such a creature from combat.
        let attach g =
              g
                { GameState.combat =
                    (GameState.combat g)
                      { Combat.attackers = Map.union recorded (Combat.attackers (GameState.combat g)),
                        Combat.joinedUnder = Map.union joined (Combat.joinedUnder (GameState.combat g)),
                        -- CR 508.8's first clause, recorded here because here is
                        -- where the declaration happens. Never cleared, so a
                        -- CR 506.4 removal later in the step cannot un-declare
                        -- these creatures, and never narrowed to the targets still
                        -- being attacked -- Pawl.Types.Combat's `attacked` field
                        -- says why its reader wants the historical answer.
                        Combat.attacked =
                          Set.union (Set.fromList (Map.elems recorded)) (Combat.attacked (GameState.combat g)),
                        -- CR 508.3b / 508.4's narrower record, written HERE and
                        -- only here: these creatures were DECLARED, which is
                        -- exactly what those two rules distinguish from being put
                        -- onto the battlefield attacking.
                        Combat.declaredAttacked =
                          Set.union (Set.fromList (Map.elems recorded)) (Combat.declaredAttacked (GameState.combat g))
                      }
                }
        -- CR 508.1's preamble, captured here: everything from this line on is
        -- undone together if the payment below cannot be made.
        before <- State.get
        -- CR 508.1f, and ONLY 508.1f. Tapping is split from the record-writing it
        -- used to share a modify' with, because the rules put a step between them:
        -- 508.1f taps, 508.1h-j determine and pay, and only 508.1k makes the
        -- creatures attacking. The order is observable rather than pedantic -- a
        -- Birds of Paradise that was just declared as an attacker is tapped, so it
        -- is no longer a mana source for the very cost its attack incurred.
        State.modify' (\g -> List.foldl' tapIt g attacking)
        gs1 <- State.get
        -- CR 508.1h: "the active player determines the total cost to attack ...
        -- Once the total cost is determined, it becomes 'locked in'. If effects
        -- would change the total cost after this time, ignore this change."
        --
        -- LOCKED IN is this `let`, and nothing more elaborate is needed: the total
        -- is computed once from the finished declaration and from the board as of
        -- CR 508.1f, and nothing at all runs between that determination and the
        -- payment it is handed to. Asking AttackCost.totalCost a second time, at
        -- payment time or after it, is exactly what the rule forbids -- which is
        -- why that function's own haddock says the locking is the caller's.
        let owed = AttackCost.totalCost recorded gs1
        -- CR 508.1i ("if any of the costs require mana, the active player then has
        -- a chance to activate mana abilities") and CR 508.1j ("once the player has
        -- enough mana in their mana pool, they pay all costs in any order. Partial
        -- payments are not allowed") are Mana.payCost, which is both: it prompts
        -- for which source to tap until the pool covers the cost, and restores the
        -- entry state rather than spending half of it.
        --
        -- Skipped outright at {0}, which is every board with no cost to attack on
        -- it. Not an optimization of Mana.payCost -- it answers True on an empty
        -- cost without tapping anything -- but a statement that a combat with no
        -- Ghostly Prison in it reaches no mana code at all.
        --
        -- NO "will you pay?" PROMPT, and that is a rules reading rather than an
        -- elision. CR 508.1j is unconditional once the creatures are chosen -- "they
        -- pay all costs" -- and CR 508.1d's "that player is not required to pay that
        -- cost" is exercised one step earlier, by NOT DECLARING the creature, which
        -- is what attackCeiling's cost clause keeps legal even under an attacking
        -- requirement. So declining IS reachable, at the CR 508.1a prompt where the
        -- rules put it, and CombatSpec's "a Curse of the Nightly Hunt does not force
        -- an attack a Ghostly Prison taxes" is the case that proves it.
        --
        -- The same shape a cast has, and the precedent is exact: Cast.castSpell does
        -- not ask whether the caster wants to pay after they have announced the
        -- spell, because announcing it was the choosing. What the player is still
        -- asked here is WHICH sources to tap -- Mana.payCost's own prompt, CR
        -- 508.1i's window -- so no source is committed for them either.
        paid <-
          if null (ManaCost.unwrap owed)
            then pure True
            else Mana.payCost pid owed
        if not paid
          then
            -- CR 508.1's preamble: "the declaration is illegal; the game returns to
            -- the moment before the declaration". The restore IS that sentence, and
            -- it is reachable by an ordinary player -- declaring more attackers
            -- than they can pay for is a mistake the rules catch here.
            --
            -- What the rules then expect, and pawl cannot do, is a fresh
            -- declaration: a pure `Prompt r -> r` returns the identical answer, so
            -- re-prompting would loop. The active player therefore attacks with
            -- nothing, which can leave a CR 508.1d requirement unobeyed that a
            -- smaller declaration would have obeyed (#600).
            State.put before
          else
            -- CR 508.1k: "each chosen creature still controlled by the active
            -- player becomes an attacking creature." After the payment, which is
            -- the rules' own order.
            do
              State.modify' attach
              -- CR 508.2b: the declaration is what abilities trigger on, and CR
              -- 508.3a scopes them to a creature that "is declared as an attacker"
              -- -- so one event per creature chosen HERE, and none at all for a
              -- creature put onto the battlefield attacking. Recorded after the
              -- record is written, so the board a trigger's intervening-if clause
              -- reads (CR 603.4) already has these creatures attacking.
              --
              -- The event names the creature and not what it was announced as
              -- attacking, so CR 508.3a's "attacks [a player, planeswalker, or
              -- battle]", CR 508.3b and CR 508.3e are unavailable (#538).
              State.modify' (\g -> List.foldl' (\h oid -> Event.recordEvent (GameEvent.AttackerDeclared oid) h) g attacking)

-- CR 508.4: "If a creature is put onto the battlefield attacking, its controller
-- chooses which defending player, planeswalker a defending player controls, or
-- battle a defending player protects it's attacking as it enters the
-- battlefield." Resolve calls this for each permanent an effect's EntryRiders say
-- is attacking -- a token its Create arm minted (Hanweir Garrison's), or a card
-- its MoveToZone arm returned to the battlefield (Meandering Towershell's);
-- nothing else does.
--
-- The creature was never DECLARED, and this function's whole difference from
-- declareAttackers is what follows from that. It records no
-- GameEvent.AttackerDeclared, so CR 508.3a's "such abilities won't trigger if a
-- creature is put onto the battlefield attacking" holds by construction rather
-- than by a filter. It taps nothing, because CR 508.1f taps what is declared
-- (the tokens' own tapped status is the creating effect's, applied at entry). And
-- it asks none of canAttack's questions -- CR 508.4c: "a creature that's put onto
-- the battlefield attacking ... isn't affected by requirements or restrictions
-- that apply to the declaration of attackers" -- so summoning sickness (CR 302.6,
-- whose sentence is "a creature can't ATTACK unless...") and defender (CR 702.3b)
-- do not reach it. CombatSpec's "the tokens are attacking, and the attack trigger
-- fired only for the Garrison" proves the trigger half.
--
-- What it does check is the three ways the rules say the creature enters WITHOUT
-- being an attacking creature. CR 506.3a: a noncreature permanent "does enter the
-- battlefield but it's never considered to be an attacking or blocking
-- permanent". CR 506.3b: the same for a creature entering "under the control of
-- any player except an attacking player", which by CR 506.2's first sentence is
-- the active player.
-- CR 506.3c and CR 508.4a: the same for one attacking "a player not in the game".
-- Each is a silent no-op rather than a failure -- the permanent is already on the
-- battlefield and stays there, which is precisely what those rules say.
--
-- Combat.defender being Nothing is the fourth way, and it is CR 506.3c's clause
-- again rather than a fallback: outside the combat phase there is no defending
-- player at all (see Pawl.Types.Combat's defender field), so there is nobody for
-- the creature to be attacking.
--
-- CR 508.4's CHOICE is prompted per permanent, over the same candidates CR
-- 508.1b's declaration offers, and two rulings say it must be: Hanweir
-- Garrison's ("You choose which player, planeswalker, or battle each token is
-- attacking as you create the tokens ... the tokens don't both have to attack
-- the same one") and Meandering Towershell's ("you choose which opponent or
-- opposing planeswalker it's attacking. It doesn't have to attack the same
-- opponent ... that it was when it was exiled"). It is elided at one candidate,
-- which is every board with no planeswalker on the defending player's side --
-- so a pool without one behaves exactly as it did before the prompt existed.
--
-- CR 508.4a's remaining clauses -- a planeswalker "no longer on the battlefield,
-- ... no longer a planeswalker or battle, [or] a planeswalker that is no longer
-- controlled by a defending player" -- need no check of their own: attackTargets
-- derives the offer from the board AT THIS MOMENT, so a candidate it lists
-- satisfies all three, and the degradation for an out-of-list answer is the
-- defending player, whom the guard below has already checked is in the game.
--
-- CR 508.4d ("a creature that's put onto the battlefield attacking during the
-- declare blockers step, combat damage step, or end of combat step enters the
-- battlefield as an unblocked creature") is not implemented: every source in the
-- pool enters during the declare attackers step, before blockers exist (#368).
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
          -- CR 508.4: "its controller chooses", and by the guard above that
          -- controller is the attacking player -- so the chooser is the same
          -- player CR 508.1b asks, which is what lets the two share one prompt.
          target <- announceAttackTarget controller oid (attackTargets defender gs)
          State.put
            gs
              { GameState.combat =
                  c
                    { Combat.attackers = Map.insert oid target (Combat.attackers c),
                      -- CR 506.4's comparand, for the same reason declareAttackers
                      -- takes one: this is where the creature joins combat.
                      Combat.joinedUnder = Map.insert oid controller (Combat.joinedUnder c),
                      -- CR 508.8's SECOND clause -- "or put onto the battlefield
                      -- attacking". Written inside the guards, and so here rather
                      -- than in Resolve's Create arm: CR 506.3a-c and CR 508.4a
                      -- each let the permanent enter while it is "never
                      -- considered to be an attacking creature", and one that
                      -- never became an attacker cannot answer this rule.
                      Combat.attacked = Set.insert target (Combat.attacked c)
                    }
              }
    _ -> pure ()

-- CR 509.1: the defending player declares blockers -- singular. The loop is over
-- at most one player, and Maybe.maybeToList is what makes "nobody is being
-- attacked" and "one player is" the same code path.
--
-- CR 802.4 is the rule that has each of several defending players declare blocks
-- in APNAP order, and CR 802.4a restricts each to blocking creatures attacking
-- them. Both need the attack-multiple-players option, which pawl has no options
-- concept to read (#175).
--
-- No still-playing guard: at three or more seats, a defending player who left
-- the game has had every object they owned removed by CR 800.4a, so
-- legalBlockers finds nothing for them and the inner Monad.unless
-- short-circuits. At two seats CR 800.4a never runs at all -- CR 800.1 gates it
-- on "more than two players" (Departure.continuesAfterDeparture) -- but CR
-- 104.2a ends the game the instant a player's last opponent leaves, which
-- Sba.checkSba records as GameState.result and Engine.playGame's loop reads
-- before ever calling declareBlockers again; see GameSpec.hs's
-- CR 800.4j/703.4i test for the state that leaves unreachable in play.
declareBlockers :: Game ()
declareBlockers = do
  start <- State.get
  let attacking = Map.keys (Combat.attackers (GameState.combat start))
  Monad.unless (null attacking)
    . Monad.forM_ (Maybe.maybeToList (Combat.defender (GameState.combat start)))
    $ \pid -> do
      gs <- State.get
      let candidates = legalBlockers pid gs
      Monad.unless (null candidates) $ do
        let decider = Decide.deciderFor pid gs
        chosen <- Trans.lift (Program.prompt (Prompt.DeclareBlockers decider pid candidates attacking))
        -- CR 509.1b: an illegal declaration is illegal AS A WHOLE. It is NOT
        -- filtered down to its legal entries -- that is unsound, not merely
        -- inelegant: under menace, dropping one blocker from a pair leaves an
        -- illegal single block, so the filter would manufacture the illegality it
        -- was meant to remove. M1b settled the identical question for CR 510.1e:
        -- "checks the assignment AS A WHOLE, so this cannot be repaired by
        -- filtering the way a discard can."
        --
        -- This is NOT CR 733's rewind. An enforcing engine never offers an
        -- illegal declaration, so only a broken interpreter arrives here, and
        -- re-prompting a pure `Prompt r -> r` returns the identical wrong answer.
        --
        -- Declining to block is NOT always legal: with a CR 509.1c requirement on
        -- the board (Lure), "no blocks" can itself be the illegal answer, so
        -- doing nothing is not a state this can fall back to. It degrades to
        -- forcedBlockDeclaration instead -- always legal, and equal to "no
        -- blocks" whenever no requirement is in force, so this is the same
        -- fallback as before on every board that had one.
        --
        -- The same posture chooseDefender takes for an out-of-candidates answer:
        -- degrade TOTALLY rather than fail, and never re-prompt. It is not the
        -- engine choosing for the player -- the player's answer was taken and
        -- rejected -- and where a requirement leaves exactly one legal
        -- declaration, this hands back the one the rules already forced.
        -- Replay.defaultAnswer's "no blocks" for this prompt routes through here
        -- too, so the two cannot disagree about what an illegal answer becomes.
        gs1 <- State.get
        let declaration = if legalBlockDeclaration pid chosen gs1 then chosen else forcedBlockDeclaration pid gs1
        Monad.unless (Map.null declaration) $ do
          let add m (b, a) = Map.insertWith Set.union a (Set.singleton b) m
              merged = List.foldl' add (Combat.blockers (GameState.combat gs1)) (Map.toList declaration)
              -- CR 506.4's comparand for the blockers, alongside the attackers'
              -- (declareAttackers). `pid` for the same reason it is there: every
              -- blocker here is one legalBlockers offered, which is
              -- controllerOf == Just pid (CR 509.1a) -- required by
              -- legalBlockDeclaration on the accepted path, and true by
              -- construction on the forcedBlockDeclaration one, whose candidates
              -- come from that same list. Unioned rather than replacing, since
              -- the attackers' entries are already in this map.
              joined = Map.union (Map.fromList (fmap (\b -> (b, pid)) (Map.keys declaration))) (Combat.joinedUnder (GameState.combat gs1))
          State.modify' $ \g -> g {GameState.combat = (GameState.combat g) {Combat.blockers = merged, Combat.joinedUnder = joined}}
