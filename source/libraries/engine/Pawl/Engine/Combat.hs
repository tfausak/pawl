module Pawl.Engine.Combat where

import qualified Control.Monad as Monad
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
import qualified Pawl.Engine.Battle as Battle
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

-- CR 508.8: if no creatures were declared as attackers or put onto the
-- battlefield attacking, skip the declare blockers and combat damage steps.
--
-- BOTH of that rule's clauses are the same question of Combat.attacked, because
-- both things that can make one true write that set: declareAttackers below, and
-- putOntoBattlefieldAttacking. Engine.runStepThatBegan asks it as the declare
-- attackers step ENDS -- after the priority round in which an attack trigger
-- resolves -- rather than the moment the turn-based action finishes.
--
-- That set and NOT Map.null on Combat.attackers, which is the same question only
-- while nothing leaves combat. CR 508.8 asks whether a creature WAS declared or
-- put onto the battlefield attacking, and CR 508.1k makes that a different
-- question from whether one is attacking now: CR 506.4's removal takes away the
-- attacking, never the declaration. Asking the map skipped both steps for a lone
-- attacker that a Ray of Command took during the step.
--
-- A creature put onto the battlefield attacking LATER than that step cannot
-- un-skip anything, and does not need to: the steps this drops are the only ones
-- of THIS combat phase it could enter during. A CR 500.8 additional combat phase
-- later in the turn reaches its own declare attackers step and asks this
-- question again for itself.
skipEmptyCombat :: GameState -> GameState
skipEmptyCombat gs =
  if Set.null (Combat.attacked (GameState.combat gs))
    then gs {GameState.remaining = Turn.dropSkippedCombatSteps (GameState.phase gs) (GameState.remaining gs)}
    else gs

-- CR 506.2a: the candidates the attacking player chooses from. Read only by
-- chooseDefender; the CHOSEN one lives in Combat.defender.
--
-- Three rules get from CR 506.2a's "one of their opponents" to this list. CR
-- 102.1: a player is one of the people IN THE GAME, so someone who has left is
-- not a player and cannot be an opponent. CR 806.1: in a free-for-all the players
-- compete as individuals, so every other player is an opponent. CR 102.3 is the
-- one reading this is wrong for -- a teammate is not an opponent -- and pawl has
-- no teams to express (#175). Same argument Count.playersFor's
-- PlayerRelation.Opponent arm carries.
--
-- SEATING order (Game.stillPlayingInOrder), not player-id order: the seating
-- roster is the game's own ordering for anything player-shaped (CR 103.1), while
-- Game.stillPlaying's order is an artifact of reading the players map. It makes
-- the first candidate the next seat rather than the lowest id, which is what an
-- interpreter that takes the head should get.
attackableOpponents :: GameState -> [PlayerId]
attackableOpponents gs = filter (/= GameState.activePlayer gs) (Game.stillPlayingInOrder gs)

-- CR 508.1b: what the active player may announce a chosen creature is attacking
-- -- which player, planeswalker or battle. CR 506.2's second sentence is the
-- same list scoped to a two-player game.
--
-- The defending player is FIRST, which is not cosmetic: it is the candidate that
-- exists on every board, so an interpreter that takes the head gets the answer
-- every attack in a battle-less, planeswalker-less pool used to get, and
-- Replay.defaultAnswer's fallback is the same one for the same reason.
--
-- CR 802's attack-multiple-players option would put a SECOND player on this
-- list, and pawl has no options concept to read it from (#175) -- the defending
-- player is the argument, so this function needs no change when it arrives.
--
-- Read at DECLARATION and again at damage assignment (stillAttacked below), and
-- both readings are derived rather than stored on purpose: every clause of CR
-- 506.4 that stops a planeswalker being attacked is a change to exactly what
-- this filter asks about, so re-asking IS performing the removal.
attackTargets :: PlayerId -> GameState -> NonEmpty.NonEmpty AttackTarget.AttackTarget
attackTargets defender gs =
  AttackTarget.OfPlayer defender
    NonEmpty.:| fmap AttackTarget.OfPlaneswalker (attackablePlaneswalkers defender gs)
      <> fmap AttackTarget.OfBattle (attackableBattles defender gs)

-- CR 306.6 / CR 508.1b: the planeswalkers a defending player controls, in
-- ascending id order (Projection.controls walks the battlefield, which is a Set).
--
-- Battlefield-scoped by construction, since that is where Projection.controls
-- looks, and PROJECTED rather than printed for isPlaneswalkerOf's own reason:
-- CR 613.1d puts card types in layer 4.
attackablePlaneswalkers :: PlayerId -> GameState -> [ObjectId]
attackablePlaneswalkers defender gs =
  filter (\oid -> Projection.isPlaneswalkerOf oid gs) (Projection.controls defender gs)

-- CR 310.5 / CR 508.1b: the battles a defending player PROTECTS, in ascending id
-- order.
--
-- Protects, not controls -- and that is the whole rule rather than a nicety. CR
-- 310.8b: "A battle can be attacked by any attacking player for whom its protector
-- is a defending player. Notably, a Siege battle can be attacked by its own
-- controller." Since CR 310.11a puts a Siege's protector among its controller's
-- opponents, filtering by the protector is what admits the active player's OWN
-- battle to this list, which filtering by the controller (attackablePlaneswalkers'
-- rule, CR 306.6) would never do. So this walks the whole battlefield rather than
-- Projection.controls' one player's slice.
--
-- CR 310.8b's first sentence -- "a battle's protector can never attack it" -- needs
-- no check of its own here: the argument is the DEFENDING player, whom CR 506.2a
-- draws from the active player's opponents, so a battle on this list is protected
-- by someone the attacking player is not.
--
-- PROJECTED card types (Battle.isBattle) for attackablePlaneswalkers' reason: CR
-- 613.1d puts card types in layer 4, so a permanent that became a battle is one and
-- a battle that stopped being one is not. The protector survives that (CR 310.8g),
-- which is exactly why the type has to be re-asked rather than assumed from the
-- designation being present.
--
-- The designation is asked FIRST and the projection only of what survives it,
-- which is what keeps this off the hot path: Object.protector is Nothing for every
-- permanent that is not a battle, so on a board with no battle nothing is
-- projected at all and this costs one Map lookup per permanent (#200).
attackableBattles :: PlayerId -> GameState -> [ObjectId]
attackableBattles defender gs =
  let protects oid = Battle.protectorOf oid gs == Just defender
      isOne oid = Battle.isBattle (Projection.project oid gs)
   in filter (\oid -> protects oid && isOne oid) (Set.toAscList (GameState.battlefield gs))

-- CR 508.5: the defending player an attacking creature's ability refers to --
-- "the player that creature is attacking, the controller of the planeswalker that
-- creature is attacking, or the protector of the battle that creature is
-- attacking". One arm per AttackTarget arm, because that rule's case split IS this
-- type's.
--
-- CR 310.8d is the battle arm's other half, and it is wider than CR 508.5: while a
-- battle is being attacked, EVERY rule and effect that refers to the defending
-- player relative to it means the protector. Reading it off the protector here
-- rather than off Combat.defender is what makes that true of a battle whose
-- controller is the attacking player -- the case CR 310.8b's "notably" creates.
--
-- Nothing means the target names no player: a planeswalker or battle that has left
-- the battlefield, or a battle mid-repair with no designation (CR 310.10).
--
-- CR 508.5's second sentence -- the defending player of a creature that is no
-- longer attacking, read off what it WAS attacking before it left combat -- is last
-- known information, and this reads live instead. Unreachable in the pool, which
-- has no card that can remove an attacked permanent from combat and change who
-- defends through it (#537), and unobservable besides.
defendingPlayerOf :: [Projection.ControlGrant] -> AttackTarget.AttackTarget -> GameState -> Maybe PlayerId
defendingPlayerOf grants target gs = case target of
  AttackTarget.OfPlayer pid -> Just pid
  AttackTarget.OfPlaneswalker oid -> Projection.controllerOfGiven grants Set.empty oid gs
  AttackTarget.OfBattle oid -> Battle.protectorOf oid gs

-- "only if you've been attacked this step", asked of the player a printed clause
-- says "you" about.
--
-- TWO readers, one question: Pawl.Types.CastingRestriction.AttackedThisStep
-- (Rally the Troops) and Pawl.Types.ActivationRestriction.AttackedThisStep
-- (Kongming's Contraptions) spell the same clause, and CR 307.5's ban on the two
-- gates agreeing is about casting PROHIBITIONS, not about a fact of the combat
-- record. So the record is read once, here, and each gate conjoins it itself.
--
-- CR 506.2 (CR 507.1 where the seat count makes it a choice) settles who the
-- defending player is, so the question left is whether any creature was DECLARED
-- attacking THIS PLAYER rather than a planeswalker of theirs -- exactly
-- membership in Combat.declaredAttacked. DECLARED, and not "or put onto the
-- battlefield attacking": CR 508.4 says such creatures never "attacked", and CR
-- 508.3b spells out the player side. So this reads Combat.declaredAttacked and
-- NOT Combat.attacked, CR 508.8's wider set; the two fields exist because the two
-- rules disagree (see Pawl.Types.Combat). Eightfold Maze's ruling pins the
-- reading: a creature needs to have attacked YOU, which is why this cannot be
-- emptiness of the record, and CR 306.6 is what made it observable.
--
-- Membership in the HISTORICAL set rather than a search of Combat.attackers,
-- because CR 506.4 removing the lone attacker from combat does not un-attack
-- anybody. No separate Combat.defender test either: only a defending player can
-- be attacked, so an OfPlayer entry naming this player IS that conjunct.
--
-- "THIS STEP" is read off the combat record, which CR 511.3 scopes to the whole
-- combat PHASE. The two spans coincide for every card in the pool because this
-- set is written ONLY by declareAttackers below, CR 508.1's turn-based action.
-- That is a fact about the pool rather than a rule (#447): what remains open is a
-- second declaration inside one phase.
attackedThisStep :: PlayerId -> GameState -> Bool
attackedThisStep pid gs =
  Set.member (AttackTarget.OfPlayer pid) (Combat.declaredAttacked (GameState.combat gs))

-- CR 506.4: is this planeswalker still one that is being attacked -- or has it
-- been removed from combat since the declaration?
--
-- Asked where the answer is USED rather than sampled into the combat record the
-- way removeChanged samples its two clauses, and the difference is invisible: an
-- attack target is read in exactly three places, and all three re-ask --
-- Damage.combatRecipient at CR 510.1's assignment, whose own CR 510.1b is
-- phrased for precisely this case, defendingPlayerOf for CR 508.5's defending
-- player, which reads the planeswalker's controller and never whether it is
-- still attacked, and Battle.isBeingAttacked for CR 704.5w's rider, which asks
-- only whether a battle is named at all.
--
-- Every other reader of Combat.attackers takes its KEYS (Projection's
-- Filter.IsAttacking, blockCeiling, declareBlockers, Damage.dealCombatDamage),
-- and CR 506.4c is emphatic that the keys must NOT change here: the creature
-- continues to be an attacking creature, attacking nothing, and may be blocked.
--
-- So the state pawl stores -- an attacker whose recorded target is no longer
-- attackable -- and the state the rules describe are observationally the same
-- board, and stay so until something asks a question that separates them. The
-- candidate for that is a card whose text reads WHAT a creature is attacking (CR
-- 508.3b, CR 702.19c's trample over planeswalkers); none is in the pool (#537).
stillAttacked :: ObjectId -> GameState -> Bool
stillAttacked oid gs = case Combat.defender (GameState.combat gs) of
  -- No defending player is no attack (see Pawl.Types.Combat's defender field), so
  -- nothing of theirs is being attacked either.
  Nothing -> False
  Just defender -> List.elem oid (attackablePlaneswalkers defender gs)

-- CR 506.4 for a battle: is this one still being attacked, or has it left the
-- battlefield since the declaration? stillAttacked's twin, asked at the same one
-- place -- Damage.combatRecipient's CR 510.1b assignment -- and built the same way,
-- out of the candidate list CR 508.1b drew the declaration from.
--
-- Reusing attackableBattles rather than testing the zone directly is what keeps
-- the two readings in step: a battle that stopped being a battle (CR 613.1d) is off
-- the list for the same reason a planeswalker that stopped being one is, and CR
-- 506.4's "leaves the battlefield" falls out of the list being battlefield-scoped.
--
-- It is one clause WIDER than CR 506.4, because the list also asks who protects
-- the battle: a protector moved to a third player mid-combat (CR 310.8f) would read
-- here as removed from combat, where rule 506.4 lists no such clause. No effect in
-- the pool can move a designation (#853), and CR 310.8d is why the extra clause is
-- the conservative direction anyway -- the defending player would have moved with
-- it.
stillAttackedBattle :: ObjectId -> GameState -> Bool
stillAttackedBattle oid gs = case Combat.defender (GameState.combat gs) of
  Nothing -> False
  Just defender -> List.elem oid (attackableBattles defender gs)

isCreatureObject :: ObjectId -> GameState -> Bool
isCreatureObject = isCreatureObjectGiven Map.empty

isCreatureObjectGiven :: Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> GameState -> Bool
isCreatureObjectGiven = Projection.isCreatureGiven

-- CR 508.1a: an attacking creature must be untapped, controlled by the active
-- player, and not summoning sick (CR 302.6). Together with the PER-CREATURE half
-- of CR 508.1c's restrictions, which are the last two conjuncts: the rules ask
-- them as separate steps, and pawl answers both here because a creature failing
-- one of these is in no legal declaration at all, so failing it is
-- indistinguishable from never having been a candidate.
--
-- Only that half. CR 508.1c's other shape is SET-SHAPED -- Bonded Construct's
-- "can't attack alone" is a fact about the whole declaration -- and a creature
-- carrying one is still a candidate, since some declaration containing it is
-- legal. That shape is asked in attackDeclarationAllowed and never here; taking
-- it off the candidate list would forbid the declaration CR 508.1c's own Example
-- calls legal. So CR 508.1c is answered in two places, one per shape -- the
-- split canBlockGiven describes for CR 509.1b, without its middle PAIRWISE case,
-- which on the attacking side is the missing object axis (#620).
--
-- NOT IMPLEMENTED: CR 508.1a's "they can't also be battles". The creature test
-- below already excludes every battle in the pool, since none is also a creature
-- (#898).
--
-- canAttackGiven is the half a LOOP wants: `grants`, `pcs` and `restricted` are
-- each one battlefield-wide walk, taken once per declaration pass by
-- legalAttackers below rather than once per candidate, which is what kept the
-- pass from being quadratic in the battlefield (#200).
-- Projection.projectGiven carries the argument for why a shared board is the
-- same answer, and for why it is valid only within one pure pass over one
-- GameState.
--
-- canAttack itself passes Map.empty for the projection, so a lone query projects
-- per read. It does NOT pass an empty restriction set: an absent projection is a
-- cache miss the projection recovers from, while an absent restriction is a
-- wrong answer.
canAttack :: PlayerId -> ObjectId -> GameState -> Bool
canAttack pid oid gs = canAttackGiven (Projection.controlGrants gs) Map.empty (CombatRestriction.cantAttack [oid] gs) pid oid gs

canAttackGiven :: [Projection.ControlGrant] -> Map ObjectId PC.ProjectedCharacteristics -> Set ObjectId -> PlayerId -> ObjectId -> GameState -> Bool
canAttackGiven grants pcs restricted pid oid gs = case Game.lookupObject oid gs of
  Nothing -> False
  Just obj ->
    Projection.controllerOfGiven grants Set.empty oid gs == Just pid
      && GameState.activePlayer gs == pid
      -- CR 506.3 wants a permanent, so the test is battlefield MEMBERSHIP and not
      -- Object.zone: CR 702.26b makes a phased-out permanent one the game treats
      -- as not existing, and its zone still reads Zone.Battlefield (CR 702.26d).
      -- legalAttackers below never offers one, since it filters
      -- Projection.controlsGiven, which walks the same set -- this conjunct is what
      -- makes the predicate agree when asked about an id off that menu.
      && Set.member oid (GameState.battlefield gs)
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

-- CR 506.5: a creature attacks alone if it is the only creature DECLARED as an
-- attacker during the declare attackers step. `alone` is the set of candidates a
-- CR 508.1c restriction forbids that of (Bonded Construct), so a declaration is
-- disallowed exactly when it is the singleton of such a creature.
--
-- Read off the DECLARATION and not off Combat.attackers, which is CR 506.5's own
-- word "declared": a creature put onto the battlefield attacking earlier in the
-- combat phase never was declared, so it is not company, and CR 508.4c says such
-- a creature is not affected by this declaration's restrictions either.
--
-- The pool's one printing restricts ITSELF, and nothing here assumes that: the
-- test is on the lone creature's membership in `alone`, so an Aura printing
-- "enchanted creature can't attack alone" would work unchanged.
--
-- Two restricted creatures attacking TOGETHER is legal, which is CR 508.1c's own
-- Example, and is why this asks the declaration's size rather than asking each
-- creature for an unrestricted companion.
aloneAllows :: Set ObjectId -> Set ObjectId -> Bool
aloneAllows alone declaration = case Set.toList declaration of
  [only] -> not (Set.member only alone)
  _ -> True

-- CR 508.1c: the active player checks each creature for RESTRICTIONS, and if any
-- are disobeyed the DECLARATION is illegal. blockDeclarationAllowed's attacking
-- twin, and the seam a set-shaped attacking restriction is added at.
--
-- Only the SET-SHAPED restrictions are asked here. The per-creature ones are
-- canAttackGiven's, and a creature failing one is not on `candidates` at all, so
-- the caller's own membership test is already CR 508.1c for that shape --
-- exactly the collapse legalAttackDeclarationGiven describes. The blocking side
-- keeps both conjuncts in one function because its per-pair shape has no
-- candidate list to hide in.
attackDeclarationAllowed :: Set ObjectId -> Set ObjectId -> Bool
attackDeclarationAllowed = aloneAllows

-- Every declaration CR 508.1a lets the active player write down: each candidate
-- independently attacks or does not. candidateBlockDeclarations' attacking twin,
-- and EXPONENTIAL for its reason -- O(2 ^ candidates) here rather than
-- O((attackers + 1) ^ blockers), because an attacker's only choice is whether to
-- attack. CR 508.1b's announcement of WHAT it attacks is a later step and no part
-- of the legality this list is searched for.
--
-- Set.empty comes FIRST and every declaration precedes its own supersets, which
-- attackCeiling's tie-breaking fold relies on: ties go to the earlier entry, so
-- the declaration that wins is one no PROPER SUBSET of which obeys as many
-- requirements.
candidateAttackDeclarations :: [ObjectId] -> [Set ObjectId]
candidateAttackDeclarations candidates =
  let extend acc oid = concatMap (\declaration -> [declaration, Set.insert oid declaration]) acc
   in List.foldl' extend [Set.empty] candidates

-- CR 508.1d's two halves, computed together because neither is usable alone: the
-- requirement instances in force, and a declaration obeying the maximum number of
-- them that could be obeyed without disobeying any restriction. blockCeiling's
-- twin, deliberately the same shape so the two rules read the same way at their
-- call sites.
--
-- TWO SEARCHES, and which one runs is decided by `alone` -- the set-shaped
-- restrictions in force over the candidates (CR 508.1c). Both answer the same
-- question; the closed form is a shortcut that only some boards admit.
--
-- The CLOSED FORM is the instance set, minus whichever instances CR 508.1d's cost
-- clause excuses. It is exact when no set-shaped restriction reaches a candidate,
-- because then every restriction pawl models is per creature (CR 508.1a's own
-- clauses, CR 702.3b's defender, CR 508.1c's printed
-- CombatRestriction.CantAttack), and canAttack has already applied all of them to
-- `candidates` -- so declaring every required creature at once disobeys nothing
-- and is maximal by construction. A cost to attack does not break that: CR 508.1h
-- totals the whole declaration at once, so no creature's presence can make
-- another's attack ILLEGAL, only dearer.
--
-- That argument is what a Bonded Construct falsifies: it makes a declaration
-- illegal for its SIZE, so "all of the required creatures at once" can be a
-- declaration no player may make, and the maximum stops being the instance set.
-- The ENUMERATION is then blockCeiling's search, at blockCeiling's exponential
-- cost -- #342's shape on the attacking side, where nothing is capped and
-- nothing is sampled for #342's reason (#714).
--
-- Keeping the closed form on the boards that admit it is an optimization and NOT
-- a second rules reading: `alone` empty means no declaration is disallowed by
-- anything the enumeration would filter on, so the winning declaration would be
-- the set of required creatures that attack freely -- element for element what
-- Set.filter builds. It matters because the guard is what keeps every board
-- without such a card off the exponential path, including every board in the pool
-- that has a Curse of the Nightly Hunt on it.
--
-- The enumeration is over the creatures that attack FREELY, never over all the
-- candidates, and that is CR 508.1d's cost clause rather than a cheat: a player
-- is not required to pay a cost to attack, so a declaration that costs something
-- cannot be the bar another declaration is judged against. Excluding those
-- creatures from the search is the same act the closed form performs with
-- Set.filter.
--
-- CR 508.1d's COST CLAUSE is a modifier on the maximization rather than a check
-- of its own: a player is never required to pay a cost to attack, even where
-- attacking would obey one more requirement. So a requirement whose creature
-- cannot attack anything without its controller paying is not one the maximum
-- reaches for, and declining to attack with it stays legal -- which is why the
-- two components come apart. `required` stays every instance in force, because
-- that is what a declaration's obedience is counted against, and paying is
-- mandatory once a taxed creature does attack -- CR 508.1j allows no partial
-- payment and CR 508.1's preamble makes a declaration the player cannot comply
-- with illegal; `best` is drawn from
-- the untaxed creatures alone, and it is what makes "no attacks" legal under a
-- Curse of the Nightly Hunt while a Ghostly Prison is out.
-- AttackCost.attacksFreely is asked against CR 508.1b's whole target list, so a
-- creature that could attack a planeswalker for nothing keeps its requirement.
--
-- No defending player means no target at all, so nothing attacks freely and
-- `best` is empty. Not a fallback: with no defender there is no attack to make,
-- so a requirement that cannot be obeyed is one CR 508.1d's "if able" never
-- reaches. (empty, empty) when no requirement is in force -- the maximum is zero,
-- every declaration obeys zero -- and that case takes the closed form whatever
-- `alone` says, so a board carrying a set-shaped restriction and no requirement
-- enumerates nothing and no cost walk is taken.
attackCeiling :: [ObjectId] -> GameState -> (Set ObjectId, Set ObjectId)
attackCeiling candidates gs = attackCeilingGiven (CombatRestriction.cantAttackAlone candidates gs) candidates gs

-- attackCeiling against a restriction set the caller already gathered, which is
-- what both callers below want: each has to ask attackDeclarationAllowed of the
-- player's own declaration as well, and the two must be judging the same board.
attackCeilingGiven :: Set ObjectId -> [ObjectId] -> GameState -> (Set ObjectId, Set ObjectId)
attackCeilingGiven alone candidates gs =
  let required = AttackRequirement.instances candidates gs
      targets = case Combat.defender (GameState.combat gs) of
        Nothing -> []
        Just defender -> NonEmpty.toList (attackTargets defender gs)
      freely oid = AttackCost.attacksFreely oid targets gs
      better best declaration =
        if attackRequirementsMet required declaration > attackRequirementsMet required best
          then declaration
          else best
      -- The fold's seed is the EMPTY declaration, always legal under restrictions
      -- alone: CR 508.1c's restrictions only ever forbid attacking, so declining
      -- disobeys none of them. blockCeiling's seed, for blockCeiling's reason --
      -- it is what makes the answer total without a partial function.
      enumerated =
        List.foldl'
          better
          Set.empty
          (filter (attackDeclarationAllowed alone) (candidateAttackDeclarations (filter freely candidates)))
   in ( required,
        if Set.null alone || Set.null required
          then Set.filter freely required
          else enumerated
      )

-- How many of `required` this declaration obeys (CR 508.1d): a requirement
-- instance is obeyed exactly when the declaration attacks with its creature.
-- requirementsMet's twin, on a set of creatures rather than a map of pairs.
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

-- CR 508.1: is this declaration one the active player may make? All three checks
-- the rules ask for, in the order they ask them: CR 508.1a's chosen-from set, CR
-- 508.1c's restrictions, then CR 508.1d's requirements.
--
-- CR 508.1c's PER-CREATURE restrictions are not a separate conjunct because they
-- are not a separate set: canAttack is the whole of what pawl can say a creature
-- can't attack for on its own -- CR 508.1a's own clauses, CR 702.3b's defender,
-- and every printed CombatRestriction.CantAttack -- so being a candidate IS
-- obeying every restriction of that shape. Its SET-SHAPED restrictions are the
-- conjunct, because no candidate list can carry them (see aloneAllows).
--
-- CR 508.1d is not a check but a MAXIMIZATION, so it cannot be asked of the
-- declaration alone. It is what makes declaring no attackers at all illegal while
-- a Curse of the Nightly Hunt is on the enchanted player's battlefield.
legalAttackDeclaration :: PlayerId -> [ObjectId] -> GameState -> Bool
legalAttackDeclaration pid chosen gs = legalAttackDeclarationGiven (legalAttackers pid gs) chosen gs

legalAttackDeclarationGiven :: [ObjectId] -> [ObjectId] -> GameState -> Bool
legalAttackDeclarationGiven candidates chosen gs =
  -- Gathered ONCE and shared with the ceiling, on blockCeilingGiven's terms: the
  -- restriction check and the maximization have to be judging one board, and a
  -- second walk would be paid for nothing.
  let alone = CombatRestriction.cantAttackAlone candidates gs
   in all (\oid -> List.elem oid candidates) chosen
        && attackDeclarationAllowed alone (Set.fromList chosen)
        && obeysAttackRequirements (attackCeilingGiven alone candidates gs) chosen

-- A declaration that is always legal: one attaining CR 508.1d's maximum, which
-- with no requirement in force is the empty one (declining to attack). Not an
-- answer the engine ever prefers to the active player's own -- declareAttackers
-- reaches for it only when an interpreter hands back a declaration the rules
-- forbid.
--
-- It obeys CR 508.1c as well as CR 508.1d, on both of attackCeiling's paths: the
-- enumeration draws `best` from declarations attackDeclarationAllowed kept, and
-- the closed form runs only where nothing set-shaped reaches a candidate.
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
-- pairAllowed -- and CR 702.111b's menace is SET-SHAPED, which lives in
-- blockDeclarationAllowed. So the rule is answered in three places, one per
-- shape of restriction, and this is the narrowest.
--
-- Summoning sickness is NOT a blocking restriction: CR 302.6 restricts attacking
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
      -- Battlefield MEMBERSHIP, for canAttackGiven's reason above.
      && Set.member oid (GameState.battlefield gs)
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

-- CR 702.14c: a creature with landwalk can't be blocked as long as the defending
-- player controls at least one land matching the specified criterion.
--
-- The BLOCKER is not an argument, and that is CR 702.14d stated in the type:
-- landwalk abilities don't cancel one another, so a player who controls a snow
-- Forest AND a creature with snow forestwalk still may not block a
-- snow-forestwalker. Landwalk is a property of the defending player's LANDS,
-- never a comparison between the two creatures -- unlike protection -- so a
-- signature that could read the blocker could answer 702.14d wrong.
--
-- The same asymmetry the other two evasion gates have (see evasionAllows):
-- landwalk restricts being BLOCKED, so the question is asked of the ATTACKER.
--
-- Membership over the projection's keyword map, never its counts (CR 702.14e).
-- The MAP rather than hasKeywordGiven, because CR 702.14a's [type] rides the
-- constructor -- there is no single Keyword value to ask about, which is
-- Projection.totalToxic's situation and takes its shape.
--
-- All four of CR 702.14c's clauses, because the keyword carries a Filter, and all
-- four have a printing in the pool: Bog Wraith, Vectis Gloves (the only paper
-- source of artifact landwalk, and it GRANTS the keyword rather than printing it
-- on a creature), Dryad Sophisticate and Legions of Lim-Dûl.
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
      -- CR 508.5, in defendingPlayerOf above: landwalk is exactly an ability of an
      -- attacking creature that refers to a defending player, so the player is
      -- read off the ATTACK rather than off the blocker's controller -- those two
      -- coincide only while there is exactly one defending player (CR 802, #175),
      -- and CR 310.8d breaks them apart at two seats as soon as a battle is
      -- attacked. Nothing means the object is not attacking, so no landwalk of its
      -- can restrict anything.
      defendingPlayer = (\t -> defendingPlayerOf grants t gs) =<< Map.lookup attacker (Combat.attackers (GameState.combat gs))
      -- CR 702.14c's lands of the defending player. Lazy, and load-bearing: this
      -- walks the whole battlefield, and `any` below never forces it for an
      -- attacker without landwalk, which is every attacker in almost every combat
      -- (#200).
      defendersLands = foldMap (\pid -> Projection.controlsGiven grants pid gs) defendingPlayer
      -- The land-ness is asked HERE and never by the criterion: every clause of
      -- CR 702.14c reads "at least one LAND", so it belongs to the rule rather
      -- than to the card's parameter. The criterion answers the type half alone.
      --
      -- CR 205.3d makes the card-type test all but redundant for the two clauses
      -- whose criterion NAMES a land type, and "all but" is why it is still asked
      -- even for them: nothing in the projection enforces 205.3d, so a
      -- Modification.AddLandSubtype aimed at a non-land would otherwise be walked
      -- on. For the other two it is not redundant at all -- their criteria name no
      -- land type, so nonbasic landwalk would match every nonbasic PERMANENT and
      -- artifact landwalk every artifact.
      --
      -- CR 109.5's "you" for the criterion is the ATTACKER's controller, and the
      -- source is the attacker -- the same pairing every keyword-borne Filter
      -- takes. No landwalk in the pool reads either, so the context is
      -- well-defined rather than exercised. Hoisted, since it does not vary per
      -- candidate.
      context = Filter.MkContext (Projection.controllerOfGiven grants Set.empty attacker gs) (Just attacker)
      -- ONE projection per candidate: Filter.cardTypes is the very set
      -- Projection.cardTypesGiven would rebuild, so the land test reads it off
      -- the view rather than projecting the object a second time (#200).
      matchesCriterion criterion oid =
        let view = Projection.viewOfObjectGiven pcs grants oid gs
         in Set.member CardType.Land (Filter.cardTypes view) && Filter.matches context view criterion
   in not (any (\criterion -> any (matchesCriterion criterion) defendersLands) walked)

-- CR 702.111b: a creature with menace can't be blocked except by two or more
-- creatures.
--
-- The blocking side's SET-SHAPED restriction, aloneAllows' twin, and the reason
-- this takes the whole declaration where its three siblings above take a pair:
-- how many blockers were assigned to one attacker is not something a predicate on
-- a single (blocker, attacker) pair can state.
--
-- "EXCEPT BY two or more", not "must be blocked by two or more". An attacker
-- nobody blocked is not blocked at all, so 702.111b has nothing to say about it
-- -- which is why this folds over the attackers the declaration MENTIONS rather
-- than over every attacker in combat. Declining to block is always legal under
-- restrictions alone, which is the seed blockCeiling's fold relies on.
--
-- The same asymmetry the other three evasion gates have (see evasionAllows): the
-- keyword is read off the ATTACKER. A creature with menace blocking alone is
-- legal, since 702.111b restricts being blocked and says nothing about blocking.
--
-- Membership rather than the projection's per-keyword count, on
-- landwalkAllowsGiven's terms: CR 702.111c makes multiple instances redundant, so
-- a creature with two of them still needs two blockers rather than four.
menaceAllows :: Map ObjectId ObjectId -> GameState -> Bool
menaceAllows = menaceAllowsGiven Map.empty

menaceAllowsGiven :: Map ObjectId PC.ProjectedCharacteristics -> Map ObjectId ObjectId -> GameState -> Bool
menaceAllowsGiven pcs declaration gs =
  let -- blocker -> attacker inverted into attacker -> how many blockers, which is
      -- the only reading of a declaration 702.111b cares about.
      blockerCounts = Map.fromListWith (+) (fmap (\attacker -> (attacker, 1 :: Int)) (Map.elems declaration))
      -- The count first, so an attacker that is comfortably blocked never pays
      -- for a keyword read (#200's posture, in the one place a declaration check
      -- sits inside candidateBlockDeclarations' exponential filter).
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
-- less than that: it does not read the blocker at all. Menace (CR 702.111b)
-- constrains the SET blocking one attacker, so it is asked in
-- blockDeclarationAllowed and never here. The two are cumulative rather than
-- alternative: a menace attacker that also has fear needs two blockers AND needs
-- each of them to pass 702.36b.
pairAllowed :: [ObjectId] -> [ObjectId] -> ObjectId -> ObjectId -> GameState -> Bool
pairAllowed candidates attackers blocker attacker gs =
  pairAllowedGiven (Projection.controlGrants gs) Map.empty candidates attackers blocker attacker gs

-- pairAllowed against a pre-projected board, which is what the callers below
-- pass: this question is asked once per (blocker, attacker) PAIR, so each evasion
-- read would otherwise be a fresh gather in a doubly nested loop (#200). The
-- grant list is threaded on canBlockGiven's terms: an EMPTY pcs is a cache miss
-- the projection recovers from, but an empty grant list is a wrong answer.
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
-- stylistic choice: menace (CR 702.111b) constrains the SET blocking an attacker,
-- which no per-pair predicate can express. Every other evasion ability the pool
-- has -- flying, reach, fear, landwalk -- is pairwise or narrower; designing to
-- them would be designing to the case that misleads.
--
-- So the two shapes of restriction are both asked here, one conjunct each:
-- pairAllowed over the pairs, and menaceAllows over the whole map. This is also
-- the seam blockCeiling's enumeration is filtered through, so CR 509.1c's maximum
-- is taken over declarations menace already allows.
--
-- Takes the projected board rather than projecting per read, because the
-- set-shaped conjunct reads a keyword and this sits inside
-- candidateBlockDeclarations' exponential filter (#342). There is no per-read
-- twin the way pairAllowed has one: both callers are already inside a hoisted
-- pass.
blockDeclarationAllowed :: Map ObjectId PC.ProjectedCharacteristics -> (ObjectId -> ObjectId -> Bool) -> Map ObjectId ObjectId -> GameState -> Bool
blockDeclarationAllowed pcs able declaration gs =
  all (uncurry able) (Map.toList declaration)
    && menaceAllowsGiven pcs declaration gs

-- How many of `requirements` this declaration obeys (CR 509.1c): a requirement
-- instance is obeyed exactly when the declaration has its blocker blocking its
-- attacker.
requirementsMet :: Set (ObjectId, ObjectId) -> Map ObjectId ObjectId -> Int
requirementsMet requirements declaration =
  Set.size (Set.filter (\(blocker, attacker) -> Map.lookup blocker declaration == Just attacker) requirements)

-- Every declaration CR 509.1a lets the defending player write down, given the
-- pairs CR 509.1b allows: each candidate blocker independently either blocks
-- nothing or blocks one attacker it may block.
--
-- EXPONENTIAL, and honestly so: O((attackers + 1) ^ blockers) in the worst case.
-- Nothing caps it and nothing samples it -- a cap would answer CR 509.1c's
-- maximum with a number that is not the maximum, which is worse than being slow.
-- What keeps it off the hot path is blockCeiling's guard: this is never called
-- unless some requirement is actually in force, which needs a card like Lure on
-- the battlefield (#342).
candidateBlockDeclarations :: (ObjectId -> ObjectId -> Bool) -> [ObjectId] -> [ObjectId] -> [Map ObjectId ObjectId]
candidateBlockDeclarations able candidates attackers =
  let extend acc blocker =
        let options = Nothing : fmap Just (filter (\attacker -> able blocker attacker) attackers)
            apply declaration option = case option of
              Nothing -> declaration
              Just attacker -> Map.insert blocker attacker declaration
         in concatMap (\declaration -> fmap (apply declaration) options) acc
   in List.foldl' extend [Map.empty] candidates

-- CR 509.1c's two halves, computed together because neither is usable alone: the
-- requirement instances in force, and a declaration obeying the maximum number of
-- them that could be obeyed without disobeying any restriction.
--
-- Map.empty when no requirement is in force, WITHOUT enumerating anything: the
-- maximum is zero, every declaration obeys zero, and CR 509.1c has nothing to
-- say, so a board without Lure never pays a search.
--
-- The maximum is taken by folding rather than by `maximum`, and the fold's seed
-- is Map.empty -- always a legal declaration under restrictions alone, since
-- declining to block disobeys no restriction -- so the answer is total and needs
-- no partial function. Ties go to the EARLIER declaration in enumeration order;
-- which one is picked matters only to forcedBlockDeclaration's
-- broken-interpreter path, never to legality, which compares counts.
--
-- One grant walk and one whole-board projection for the whole search, threaded
-- into the candidate list and into every pair `able` judges (see canAttackGiven
-- and Projection.projectGiven). blockCeilingGiven is the half
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
      legal = filter (\declaration -> blockDeclarationAllowed pcs able declaration gs) (candidateBlockDeclarations able candidates attackers)
   in ( requirements,
        if Set.null requirements
          then Map.empty
          else List.foldl' better Map.empty legal
      )

-- CR 509.1: is this declaration one the defending player may make? Both checks
-- the rule asks for, in the order it asks them: CR 509.1b's restrictions, then CR
-- 509.1c's requirements.
--
-- CR 509.1c is not a check but a MAXIMIZATION, so it cannot be asked of the
-- declaration alone. It is what makes declaring no blockers at all illegal while
-- a Lure is on the battlefield.
--
-- CR 509.1c's cost clause and CR 509.1d's cost locking are not implemented: no
-- card in the pool makes blocking cost anything (#343).
legalBlockDeclaration :: PlayerId -> Map ObjectId ObjectId -> GameState -> Bool
legalBlockDeclaration pid declaration gs =
  -- Hoisted exactly as blockCeiling hoists, and for the same reason.
  let grants = Projection.controlGrants gs
      pcs = Projection.projectAll gs
      attackers = Map.keys (Combat.attackers (GameState.combat gs))
      candidates = legalBlockersGiven grants pcs pid gs
      able blocker attacker = pairAllowedGiven grants pcs candidates attackers blocker attacker gs
      (requirements, best) = blockCeilingGiven grants pcs pid gs
   in blockDeclarationAllowed pcs able declaration gs
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

-- CR 509.1h: an attacking creature with one or more blockers declared for it
-- becomes blocked, and remains blocked even if all of them are removed from
-- combat.
--
-- So blocked-ness is a STATUS the declaration confers once, not a running count
-- of who is still blocking. The map's KEY is that status -- declareBlockers
-- creates it and only Game.removeFromCombat's Map.delete arm (the attacker itself
-- leaving combat, CR 506.4) and Combat.clearCombat ever drop it. The SET behind
-- the key is the separate CR 510.1c question of who is currently blocking, and it
-- can empty out while the key stays: a regenerated blocker (CR 701.19a) is
-- deleted from it, and a blocker that merely died is filtered out at assignment
-- time. Testing the set for emptiness instead let a creature blocked by a
-- regenerated Drudge Skeletons become unblocked.
isBlocked :: ObjectId -> GameState -> Bool
isBlocked oid gs = Map.member oid (Combat.blockers (GameState.combat gs))

-- Every creature currently IN combat: the attackers, plus everything still
-- blocking one of them. Not the keys of Combat.joinedUnder, which can outlive the
-- record it was taken for -- Pawl.Engine.Departure edits Combat.attackers
-- directly, and deliberately leaves Combat.blockers alone (CR 509.1h).
combatants :: Combat -> Set ObjectId
combatants c = Set.union (Map.keysSet (Combat.attackers c)) (Set.unions (Map.elems (Combat.blockers c)))

-- CR 506.4's two clauses whose trigger is DERIVED state -- a combatant's
-- controller changing, and an attacking or blocking creature stopping being a
-- creature -- which is why this is a sampler and not a hook. Neither has an event
-- to hang a removal on: a control-granting static ability is re-read live by the
-- projection, and even a stored SetController is installed by a resolution that
-- never announces a control change (#198); creature-ness is a CR 613 layer-4
-- answer that changes the moment the effect producing it appears or ends. The
-- same shape as Engine.checkControlContinuity's CR 302.6 scan, and
-- settleForPriority runs both at every point the board can change.
--
-- The TIMING that costs: the rules remove the permanent the instant the
-- characteristic changes, and this notices at the next settle. Nothing can see
-- the difference -- CR 117.5 makes a priority grant the coarsest moment anything
-- observes the board, and the two readers of the combat record (the CR 510 damage
-- steps, Filter.IsAttacking at targeting) both sit behind one, which settles
-- first. The window that would open it is a single resolution that changes
-- control or card types and then reads combat status in a LATER effect of the
-- same resolution; no card in the pool has one.
--
-- It only ever REMOVES, and that asymmetry is what makes the sampling sound: a
-- discrepancy proves the characteristic changed, so removing is always right,
-- while putting a creature BACK would invent a CR 506.4 the rules do not have --
-- removal from combat lasts the rest of that combat phase.
--
-- Battlefield-scoped, so this stays these two clauses and nothing else: an object
-- that has LEFT the battlefield (CR 110.1) was already removed by a separate
-- clause of CR 506.4, implemented in Pawl.Engine.Departure and Pawl.Engine.Damage.
-- Without the gate it would fail both tests here and be swept up under the wrong
-- clause. Removal itself goes through Game.removeFromCombat, so a removed ATTACKER
-- takes its blocked-ness with it while a removed BLOCKER leaves the attacker
-- blocked with nothing blocking it -- CR 509.1h's last sentence, argued there.
--
-- Creatures only, which is what `combatants` gathers: an ATTACKED planeswalker or
-- battle is not in that set, and CR 506.4's clauses about either are answered at
-- stillAttacked and stillAttackedBattle instead. CR 506.4d/e and the
-- becomes-a-battle clause are not implemented (#981); neither is the phases-out
-- clause (#929).
--
-- A combatant with no entry in Combat.joinedUnder is left alone by the CONTROL
-- clause, because there is nothing to compare it against and this only ever
-- removes. Unreachable through the engine: declareAttackers and declareBlockers
-- write the snapshot in the same update that puts the creature into the record.
-- The types clause needs no such comparand -- CR 506.3 lets only a creature be
-- declared, so "is it one now" is the whole test.
--
-- Nobody in combat short-circuits, which is most of the game: the grant list and
-- the gather each cost a whole-battlefield scan, both hoisted out of the
-- per-combatant loop. The gather is NOT shared with the one
-- Sba.performStateBasedActions makes earlier in the same settle pass: a
-- state-based action can change the board between the two, and a sample has to
-- gather against the state it is judging.
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
-- value Replay.defaultAnswer gives this very prompt. The two must agree -- a
-- diverging fallback here would be an invisible bug, since neither path can
-- observe the other.
chooseDefender :: Game ()
chooseDefender = do
  gs <- State.get
  let pid = GameState.activePlayer gs
  -- CR 800.4j: a turn whose active player has left continues without one, so the
  -- action the rules assign to the active player has no subject. CR 800.4h is the
  -- one that would hand this choice -- required of the active player by CR 507.1
  -- and CR 703.4h -- to the next player in turn order. pawl skips it, which is an
  -- unobservable divergence rather than a vacuous case (#181); the argument is on
  -- Pawl.Types.Combat's defender field.
  --
  -- Engine.runTurnBasedActions binds the identical test before calling this, so
  -- on the engine's path this guard is redundant. Do NOT delete it: it is the
  -- copy a DIRECT caller depends on -- a spec, or a second combat phase spliced
  -- by an effect, neither of which goes through Engine's guard.
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
-- creature put onto the battlefield attacking -- the same question, so the same
-- function and the same prompt (Prompt.ChooseAttackTarget says why one and not
-- two).
--
-- Not prompted with one candidate, which is CR 508.1b's own condition: the rule
-- calls for an announcement only when the defending player controls a
-- planeswalker or battle, or the game allows attacking multiple players, and
-- attackTargets returns a lone defending player exactly when none of those holds.
--
-- An answer outside the candidate list is a broken interpreter, not a game state,
-- and degrades to the first candidate -- the defending player, always a legal
-- thing to attack. chooseDefender's posture and Replay.defaultAnswer's value for
-- this prompt, which must agree with it for chooseDefender's reason.
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
-- triggers watch. CR 508.1g's OPTIONAL costs to attack (exert, enlist) are not
-- implemented (#597).
--
-- CR 508.1's preamble is the one clause that costs this function its shape: a
-- declaration the active player cannot comply with is illegal, and the game
-- returns to the moment before it. A cost to attack is the first step pawl can
-- fail to comply with AFTER the board has been written to, so the entry state is
-- captured and restored. See the payment below.
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
        chosen <- Game.choose (Prompt.DeclareAttackers decider pid candidates)
        -- Filtered, not trusted: an interpreter cannot attack with a creature
        -- that is not legally an attacker.
        let isCandidate oid = List.elem oid candidates
            offered = filter isCandidate chosen
            -- CR 508.1c's set-shaped restrictions and CR 508.1d's maximization,
            -- taken ONCE for all three questions below -- one battlefield walk
            -- each, against the same candidate list the prompt was built from.
            -- The restriction set feeds the ceiling as well as the check beside
            -- it, so the two cannot judge different boards.
            alone = CombatRestriction.cantAttackAlone candidates gs
            bound = attackCeilingGiven alone candidates gs
            -- Declining to attack is NOT always legal: with a CR 508.1d
            -- requirement on the board (Curse of the Nightly Hunt), "no attacks"
            -- can itself be the illegal answer, so the filtered answer is not a
            -- state this can always accept. It degrades to forcedAttackDeclaration
            -- instead -- always legal, and EQUAL to the filtered answer whenever
            -- no requirement is in force.
            --
            -- The whole answer is replaced rather than repaired, which is
            -- declareBlockers' posture: a declaration is illegal AS A WHOLE, and
            -- unioning the missing creatures into the player's answer is unsound
            -- now that a restriction can forbid a declaration for its SIZE -- a
            -- Bonded Construct added to an empty answer would manufacture the
            -- attacks-alone illegality it was meant to remove, which is exactly
            -- the trap declareBlockers describes for menace. Nor is it
            -- re-prompted -- a pure `Prompt r -> r` returns the identical wrong
            -- answer -- and this is NOT CR 733's rewind, which is about human
            -- error at a table rather than an engine check.
            --
            -- It is not the engine choosing for the player: an enforcing
            -- interpreter never arrives here, and the player's answer was taken
            -- and rejected before this ran. Where a requirement leaves several
            -- legal declarations -- any SUPERSET of the required creatures that
            -- CR 508.1c allows also attains the maximum -- this takes the
            -- SMALLEST, on both of attackCeiling's paths: the closed form's
            -- answer is the required creatures that attack freely, and the
            -- enumeration's is the first in candidateAttackDeclarations' order,
            -- which no proper subset of attains. That is the least the rules can
            -- be said to have forced. It is a real choice among distinguishable
            -- declarations, and it is why nothing but a broken interpreter may
            -- reach it; the same is true of forcedBlockDeclaration.
            attacking =
              if attackDeclarationAllowed alone (Set.fromList offered) && obeysAttackRequirements bound offered
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
            -- creature in `attacking` (CR 508.1a), so this cannot disagree with
            -- the legality check.
            joined = Map.fromList (fmap (\oid -> (oid, pid)) attacking)
            -- CR 508.1b's candidates, taken ONCE for the whole declaration and
            -- from the state it is judged against: a pure `Prompt r -> r` cannot
            -- change the board between two announcements.
            targets = attackTargets defender gs
        -- CR 508.1b: the announcement, one question per chosen creature. Taken
        -- here rather than beside the CR 508.1a prompt because that is the rule's
        -- own order, and asked of `attacking` rather than of `chosen` so that a
        -- creature the CR 508.1d degradation dropped is never announced.
        recorded <- fmap Map.fromList (Monad.mapM (\oid -> fmap ((,) oid) (announceAttackTarget pid oid targets)) attacking)
        -- UNIONED into the record, not written over it. Nothing in the pool can
        -- have joined combat before this runs, but "the record is mine alone" is
        -- exactly the assumption CR 508.8's second clause breaks, and replacing
        -- the map would silently remove such a creature from combat.
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
        -- CR 508.1f, and ONLY 508.1f. Tapping is separate from the record-writing
        -- because the rules put a step between them: 508.1f taps, 508.1h-j
        -- determine and pay, and only 508.1k makes the creatures attacking. The
        -- order is observable -- a Birds of Paradise just declared as an attacker
        -- is tapped, so it is no longer a mana source for the very cost its attack
        -- incurred.
        State.modify' (\g -> List.foldl' tapIt g attacking)
        gs1 <- State.get
        -- CR 508.1h: the total cost to attack is determined once and then LOCKED
        -- IN. That is this `let`, and nothing more elaborate is needed: the total
        -- is computed from the finished declaration and from the board as of CR
        -- 508.1f, and nothing runs between that determination and the payment it
        -- is handed to. Asking AttackCost.totalCost a second time is exactly what
        -- the rule forbids, which is why that function leaves the locking to its
        -- caller.
        let owed = AttackCost.totalCost recorded gs1
        -- CR 508.1i's mana-ability window and CR 508.1j's all-costs-or-nothing
        -- payment are both Mana.payCost: it prompts for which source to tap until
        -- the pool covers the cost, and restores the entry state rather than
        -- spending half of it. Skipped outright at {0}, so a combat with no
        -- Ghostly Prison in it reaches no mana code at all.
        --
        -- NO "will you pay?" PROMPT, and that is a rules reading rather than an
        -- elision. CR 508.1j is unconditional once the creatures are chosen, and
        -- CR 508.1d's excuse from paying is exercised one step earlier, by NOT
        -- DECLARING the creature -- which is what attackCeiling's cost clause
        -- keeps legal even under an attacking requirement. So declining IS
        -- reachable, at the CR 508.1a prompt where the rules put it. The same
        -- shape a cast has: Cast.castSpell does not ask whether the caster wants
        -- to pay after they have announced the spell, because announcing it was
        -- the choosing.
        paid <-
          if null (ManaCost.unwrap owed)
            then pure True
            else Mana.payCost pid owed
        if not paid
          then
            -- CR 508.1's preamble: the declaration is illegal and the game returns
            -- to the moment before it. Reachable by an ordinary player --
            -- declaring more attackers than they can pay for is a mistake the
            -- rules catch here.
            --
            -- What the rules then expect, and pawl cannot do, is a fresh
            -- declaration: a pure `Prompt r -> r` returns the identical answer, so
            -- re-prompting would loop. The active player therefore attacks with
            -- nothing, which can leave a CR 508.1d requirement unobeyed that a
            -- smaller declaration would have obeyed (#600).
            State.put before
          else
            -- CR 508.1k: each chosen creature becomes an attacking creature. After
            -- the payment, which is the rules' own order.
            do
              State.modify' attach
              -- CR 508.2b: the declaration is what abilities trigger on, and CR
              -- 508.3a scopes them to a creature DECLARED as an attacker -- so one
              -- event per creature chosen HERE, and none at all for a creature put
              -- onto the battlefield attacking. Recorded after the record is
              -- written, so the board a trigger's intervening-if clause reads (CR
              -- 603.4) already has these creatures attacking.
              --
              -- The event names the creature and not what it was announced as
              -- attacking, so CR 508.3a's attacks-a-permanent form, CR 508.3b and
              -- CR 508.3e are unavailable (#538).
              State.modify' (\g -> List.foldl' (\h oid -> Event.recordEvent (GameEvent.AttackerDeclared oid) h) g attacking)

-- CR 508.4: a creature put onto the battlefield attacking has its controller
-- choose what it is attacking as it enters. Resolve calls this for each permanent
-- an effect's EntryRiders say is attacking -- a token its Create arm minted
-- (Hanweir Garrison's), or a card its MoveToZone arm returned to the battlefield
-- (Meandering Towershell's); nothing else does.
--
-- The creature was never DECLARED, and this function's whole difference from
-- declareAttackers follows from that. It records no GameEvent.AttackerDeclared,
-- so CR 508.3a's exclusion of these creatures from attack triggers holds by
-- construction rather than by a filter. It taps nothing, because CR 508.1f taps
-- what is declared (a token's own tapped status comes from the creating effect).
-- And it asks none of canAttack's questions, per CR 508.4c, so summoning sickness
-- (CR 302.6) and defender (CR 702.3b) do not reach it.
--
-- What it does check is the three ways the rules say the creature enters WITHOUT
-- being an attacking creature: CR 506.3a's noncreature permanent, CR 506.3b's
-- creature entering under anyone but the attacking player (CR 506.2's active
-- player), and CR 506.3c / CR 508.4a's attack on a player not in the game. Each
-- is a silent no-op rather than a failure -- the permanent is already on the
-- battlefield and stays there, which is precisely what those rules say.
--
-- Combat.defender being Nothing is the fourth way, and it is CR 506.3c's clause
-- again rather than a fallback: outside the combat phase there is no defending
-- player at all (see Pawl.Types.Combat's defender field).
--
-- CR 508.4's CHOICE is prompted per permanent, over the same candidates CR
-- 508.1b's declaration offers, and both Hanweir Garrison's and Meandering
-- Towershell's rulings say it must be -- the tokens need not all attack the same
-- thing, and a returning creature need not attack whom it attacked before. Elided
-- at one candidate, which is every board with no planeswalker on the defending
-- player's side and no battle they protect.
--
-- CR 508.4a's remaining clauses need no check of their own: attackTargets derives
-- the offer from the board AT THIS MOMENT, so a candidate it lists satisfies all
-- three, and the degradation for an out-of-list answer is the defending player,
-- whom the guard below has already checked is in the game.
--
-- CR 508.4d's unblocked-on-entry rule is not implemented: every source in the
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
          -- CR 508.4's chooser is the creature's controller, and by the guard
          -- above that is the attacking player -- the same player CR 508.1b asks,
          -- which is what lets the two share one prompt.
          target <- announceAttackTarget controller oid (attackTargets defender gs)
          State.put
            gs
              { GameState.combat =
                  c
                    { Combat.attackers = Map.insert oid target (Combat.attackers c),
                      -- CR 506.4's comparand, for the same reason declareAttackers
                      -- takes one: this is where the creature joins combat.
                      Combat.joinedUnder = Map.insert oid controller (Combat.joinedUnder c),
                      -- CR 508.8's SECOND clause. Written inside the guards, and
                      -- so here rather than in Resolve's Create arm: CR 506.3a-c
                      -- and CR 508.4a each let the permanent enter while never
                      -- becoming an attacking creature, and one that never became
                      -- an attacker cannot answer this rule.
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
-- CR 310.8c -- "a battle's protector may block creatures attacking that battle
-- with creatures they control; creatures controlled by other players can't block
-- those attackers" -- needs no clause of its own, and the proof is two rules
-- meeting rather than an accident. This function asks the defending player and
-- nobody else, and attackableBattles admits a battle only when the defending
-- player is its protector, so every creature attacking a battle is being blocked
-- by that battle's protector or by nobody. Proved by Pawl.BattleSpec's pair of CR
-- 310.8c cases, which move one Piker between two opponents' sides. That equality
-- of "defending player" and "protector" is a fact about a single defending
-- player, and CR 802's option is again what would break it (#175).
--
-- No still-playing guard: at three or more seats, a defending player who left the
-- game has had every object they owned removed by CR 800.4a, so legalBlockers
-- finds nothing for them and the inner Monad.unless short-circuits. At two seats
-- CR 800.4a never runs at all -- CR 800.1 gates it on more than two players
-- (Departure.continuesAfterDeparture) -- but CR 104.2a ends the game the instant
-- a player's last opponent leaves, which Sba.checkSba records as
-- GameState.result and Engine.playGame's loop reads before ever calling
-- declareBlockers again.
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
        chosen <- Game.choose (Prompt.DeclareBlockers decider pid candidates attacking)
        -- CR 509.1b: an illegal declaration is illegal AS A WHOLE. It is NOT
        -- filtered down to its legal entries -- that is unsound, not merely
        -- inelegant: under menace, dropping one blocker from a pair leaves an
        -- illegal single block, so the filter would manufacture the illegality it
        -- was meant to remove. CR 510.1e's assignment check has the same shape.
        --
        -- This is NOT CR 733's rewind. An enforcing engine never offers an
        -- illegal declaration, so only a broken interpreter arrives here, and
        -- re-prompting a pure `Prompt r -> r` returns the identical wrong answer.
        --
        -- Declining to block is NOT always legal: with a CR 509.1c requirement on
        -- the board (Lure), "no blocks" can itself be the illegal answer, so doing
        -- nothing is not a state this can fall back to. It degrades to
        -- forcedBlockDeclaration instead -- always legal, and equal to "no blocks"
        -- whenever no requirement is in force. The same posture chooseDefender
        -- takes: degrade TOTALLY rather than fail, and never re-prompt.
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
              -- controllerOf == Just pid (CR 509.1a) on both the accepted and the
              -- forced path. Unioned rather than replacing, since the attackers'
              -- entries are already in this map.
              joined = Map.union (Map.fromList (fmap (\b -> (b, pid)) (Map.keys declaration))) (Combat.joinedUnder (GameState.combat gs1))
          State.modify' $ \g -> g {GameState.combat = (GameState.combat g) {Combat.blockers = merged, Combat.joinedUnder = joined}}
