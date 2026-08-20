module Pawl.Engine.Activate where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.Binding as Binding
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Condition as Condition
import qualified Pawl.Engine.Cost as Cost
import qualified Pawl.Engine.Decide as Decide
import qualified Pawl.Engine.Detain as Detain
import qualified Pawl.Engine.EffectZone as EffectZone
import qualified Pawl.Engine.Event as Event
import qualified Pawl.Engine.Filter as Filter
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Keyword as Keyword
import qualified Pawl.Engine.ManaAbility as ManaAbility
import qualified Pawl.Engine.Modal as Modal
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.SplitSecond as SplitSecond
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivationRestriction as ActivationRestriction
import qualified Pawl.Types.Card as Card
import Pawl.Types.Cost (Cost)
import qualified Pawl.Types.DuringPhase as DuringPhase
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import Pawl.Types.Keyword (Keyword)
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.Modal as Modal.Type
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Payment as Payment
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.RevealCause as RevealCause
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.StackObjectKind as StackObjectKind
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

-- CR 302.6: a creature's {T}-cost ability can't be activated while summoning
-- sick. The whole reading lives in Pawl.Engine.Cost, because the mana window
-- needs the same one and cannot come through here -- CR 605.3b is why
-- activatableGiven refuses a mana ability outright. All that is left on this
-- side is handing over the ability's cost.
sicknessOk :: PlayerId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Card -> GameState -> Bool
sicknessOk = sicknessOkGiven Map.empty

sicknessOkGiven :: Map.Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Card -> GameState -> Bool
sicknessOkGiven pcs pid srcId ability =
  Cost.sicknessOkGiven pcs pid srcId (ActivatedAbility.cost ability)

-- The abilities to consider activating, which depends on WHERE the object is --
-- the one place that zone question is asked, so no caller repeats it.
--
-- On the battlefield: the PROJECTION's, so Humility (layer 6) strips them. In a
-- hand: the ones rule 702 mints for the card's printed keywords, which is
-- cycling (CR 702.29a) and reinforce (CR 702.77a) today, read off the PRINTED
-- card, which misses an effect that granted one there (#1859); CR 113.6b is the rule
-- that lets an ability name its own zone -- PLUS the card's own printed
-- abilities that name the hand, per CR 113.6j and CR 113.6m (Faerie Macabre's
-- "Discard this card: ..."). In a graveyard: the PRINTED abilities
-- whose own cost or effect names the graveyard, per CR 113.6m -- both zones
-- through zoneAbilitiesOf. Anywhere else: nothing -- flashback and rule 702's other
-- zone abilities are CASTING permissions (CR 702.34a), so they reach
-- Pawl.Engine.Cast instead. The first ability ACTIVATED from a fourth zone adds
-- an arm here: CR 113.6j reaches "any zone in which its cost can be paid", and
-- Cost.zoneOfComponent names only the hand and the graveyard, so no cost in the
-- vocabulary is payable from a library or from exile. CR 113.6m's EFFECT half
-- could name another zone through a MoveToZone's `origin`, and every such
-- `origin` in `data/cards/` states the graveyard -- Jarad, Golgari Lich Lord,
-- Reassembling Skeleton and Squee, Goblin Nabob, swept 2026-08-18.
--
-- CR 702.29b and CR 702.77b are why this gates ACTIVATION and not existence: a
-- cycling or reinforce ability keeps existing in every zone, so an effect
-- depending on objects having activated abilities sees it. That second question
-- is asked of Pawl.Engine.Projection.abilitiesGiven -- which mints those
-- abilities on the battlefield too, and is what Tsabo's Web reads through
-- Filter.HasNonManaActivatedAbility -- and this function then withholds them here
-- through functionsIn.
--
-- This is the LONE-QUERY convenience wrapper: it precomputes nothing, so it
-- reaches Projection.project for itself, as do sicknessOk above and
-- activatable's membership check when called the same way. The ENUMERATION path
-- goes through abilitiesForGiven instead.
abilitiesFor :: ObjectId -> GameState -> [ActivatedAbility.ActivatedAbility Card.Card]
abilitiesFor = abilitiesForGiven Map.empty

-- The ...Given half of the pair, and the one the enumeration calls:
-- Action.legalActions hands it the board it projected once, so nothing here
-- re-derives a projection per object. Only the battlefield arm reads that board
-- at all -- a hand or graveyard object's absence from it is not a miss (#1859; see
-- Projection.projectGiven).
abilitiesForGiven :: Map.Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> GameState -> [ActivatedAbility.ActivatedAbility Card.Card]
abilitiesForGiven pcs oid gs = case fmap Object.zone (Game.lookupObject oid gs) of
  -- Filtered by CR 113.6m's "functions ONLY in that zone", exactly as the
  -- graveyard arm below is. For an ability whose COST names another zone the
  -- filter changes nothing observable -- such a cost is unpayable here
  -- (Cost.canPayComponent asks the zone), so `activatable`'s cost conjunct was
  -- already withholding it, which is what a Loxodon Surveyor ON the battlefield
  -- proves in Pawl.SpeedSpec. For an ability whose EFFECT names it there is no
  -- such second gate: Reassembling Skeleton's "{1}{B}: Return this card from
  -- your graveyard to the battlefield" is payable by a Skeleton standing on the
  -- battlefield, and only this filter stops it being offered there.
  Just Zone.Battlefield -> filter (functionsIn Zone.Battlefield) (Projection.abilitiesGiven pcs oid gs)
  -- CR 113.6j: the MINTED abilities rule 702 gives the printed keywords, plus the
  -- card's own AUTHORED ones that name the hand. The two are disjoint by
  -- construction -- handAbilitiesOf reads Face.keywords and zoneAbilitiesOf reads
  -- Face.activatedAbilities -- so nothing is offered twice.
  Just Zone.Hand -> case Game.faceOf oid gs of
    Nothing -> []
    Just face -> Keyword.handAbilitiesOf (Face.keywords face) <> zoneAbilitiesOf Zone.Hand oid gs
  Just Zone.Graveyard -> zoneAbilitiesOf Zone.Graveyard oid gs
  _ -> []

-- CR 113.6j + CR 113.6m + CR 702.178b: the AUTHORED abilities a card outside the
-- battlefield offers from the zone it is in. Two zones ask it today -- the
-- graveyard, and the hand for Faerie Macabre's "Discard this card: Exile up to
-- two target cards from graveyards" -- and the zone is a parameter because
-- nothing in the reading below is about which zone it is: CR 113.6j says an
-- ability functions "from any zone in which its cost can be paid", and
-- functionsIn is the same question asked of whichever zone the card is in.
--
-- CR 113.6m -- "an ability whose cost or effect specifies that it moves the
-- object it's on out of a particular zone functions only in that zone" -- is why
-- Loxodon Surveyor's "{3}, Exile this card from your graveyard: Draw a card" and
-- Reassembling Skeleton's "{1}{B}: Return this card from your graveyard to the
-- battlefield tapped" function in the graveyard at all. One states the zone in
-- its cost and the other in its effect, which is the rule's two halves and the
-- two the zoneFunctionedFrom below reads. DERIVED from the ability either way,
-- never declared by the card, so no card file teaches the closed half a rule it
-- already has.
--
-- CR 702.178b is a second rule, and the reason the CONDITION is re-asked here:
-- "if an ability granted by a max speed ability states which zones it functions
-- from, the max speed ability that grants that ability functions from those
-- zones". The Surveyor's ability is granted by a max speed ability (CR 702.178a
-- spells that grant as ActivatedAbility.condition), and it states the graveyard,
-- so the GRANT functions in the graveyard too -- which is exactly this gate being
-- asked of a card that is not on the battlefield.
--
-- The PRINTED abilities, not the projection's (#1859), the Face.castingPermissions
-- precedent. Not a claim about the rules -- CR 613.1f does reach a card outside
-- the battlefield, and Cast.instantSpeed reads rule 702.8a's keyword there -- and
-- observationally identical while nothing can rewrite a graveyard card's text.
--
-- The condition's perspective is the OWNER. CR 109.5's "your" is the ability's
-- controller, and the Max Speed glossary entry says which player that is for a
-- card that is not on the battlefield: "that permanent's controller (or that
-- card's owner, if it isn't on the battlefield)". CR 108.4 leaves such a card
-- with no controller to ask about, so activatorOf below answers the owner for the
-- same reason and the two cannot disagree.
--
-- The VIEW is Projection.fullView, matching Projection.abilitiesGiven: nothing
-- here is inside the layer fold, so there is no circularity to bound against.
zoneAbilitiesOf :: Zone.Zone -> ObjectId -> GameState -> [ActivatedAbility.ActivatedAbility Card.Card]
zoneAbilitiesOf zone oid gs = case (Game.faceOf oid gs, Game.lookupObject oid gs) of
  (Just face, Just obj) ->
    let functionsHere = functionsIn zone
        granted ability = case ActivatedAbility.condition ability of
          Nothing -> True
          Just cond -> Condition.holds (Projection.fullView gs) (Filter.contextFor (Just (Object.owner obj)) (Just oid)) gs oid cond
     in filter (\ability -> functionsHere ability && granted ability) (Face.activatedAbilities face)
  _ -> []

-- CR 113.6m in full: "an ability whose cost OR EFFECT specifies that it moves
-- the object it's on out of a particular zone functions only in that zone".
-- Nothing for an ability that specifies neither, which leaves CR 113.6's own
-- default in place -- an instant or sorcery spell's abilities function on the
-- stack, everything else's on the battlefield.
--
-- The two halves are read by the two modules that own the two shapes:
-- Pawl.Engine.Cost walks the cost's components, Pawl.Engine.EffectZone
-- classifies one effect. Neither is a case on which ability this is, and this
-- function is not one either -- it asks the same question of both halves of the
-- rule's own sentence and takes whichever answers.
--
-- The COST answer wins a disagreement, and the choice is arbitrary because the
-- disagreement is: an ability whose cost names one zone and whose effect names
-- another functions in neither, since the cost is unpayable outside the first
-- and CR 113.6m gives one zone. No printing writes such an ability, and pawl
-- reports the cost's zone rather than inventing a "functions nowhere" answer no
-- reader has a use for.
--
-- ALL MODES, and their effects in printed order: CR 700.2 makes a modal
-- ability's modes alternatives, so a zone stated by any of them is a zone the
-- ability can move its object out of. No modal ability in the pool states one.
--
-- This is the ACTIVATED reading of a rule that says "an ability";
-- Pawl.Engine.Event.zoneFunctionedFrom is the triggered one, and has only the
-- effect half to fold, CR 603.1 giving a triggered ability no cost.
--
-- Not implemented: CR 113.6m's "a previous part of its cost or effect specifies
-- that the object is put into that zone" clause, which would need this fold to
-- be ORDER-sensitive across the cost and the effects, and its
-- delayed-triggered-ability sentence (#819). The clause's Aura half needs a
-- trigger condition and so belongs to the triggered reading alone.
zoneFunctionedFrom :: ActivatedAbility.ActivatedAbility Card.Card -> Maybe Zone.Zone
zoneFunctionedFrom ability =
  case Cost.zoneFunctionedFrom (ActivatedAbility.cost ability) of
    Just zone -> Just zone
    Nothing ->
      Maybe.listToMaybe
        (Maybe.mapMaybe EffectZone.zoneFunctionedFrom (Modal.allEffects (ActivatedAbility.modal ability)))

-- CR 113.6m's "functions only in that zone", asked of one zone: does this
-- ability function from there? True for an ability that names no zone at all,
-- which CR 113.6's default puts on the battlefield -- so this is only ever asked
-- of a zone abilitiesForGiven has already decided is a zone abilities are read
-- from, and never of a library or a stack.
functionsIn :: Zone.Zone -> ActivatedAbility.ActivatedAbility Card.Card -> Bool
functionsIn zone ability = case zoneFunctionedFrom ability of
  Nothing -> zone == Zone.Battlefield
  Just named -> zone == named

-- CR 602.2: only an object's controller, or its owner if it has no controller,
-- may activate its activated ability. Both halves of that parenthetical are live
-- here, which is why this cannot simply be Projection.controllerOf: a card in a
-- hand or a graveyard has no controller at all (CR 108.4), and CR 400.3 puts
-- every card in either in its owner's.
--
-- Nothing for every other zone, matching abilitiesFor's silence there.
activatorOf :: ObjectId -> GameState -> Maybe PlayerId
activatorOf oid gs = activatorOfGiven (Projection.controlGrants gs) oid gs

-- activatorOf with the control-grant list PRECOMPUTED, so an enumeration over
-- every permanent walks the battlefield for control-granting statics once rather
-- than once per permanent (#200).
activatorOfGiven :: [Projection.ControlGrant] -> ObjectId -> GameState -> Maybe PlayerId
activatorOfGiven grants oid gs = case Game.lookupObject oid gs of
  Nothing -> Nothing
  Just obj -> case Object.zone obj of
    Zone.Battlefield -> Projection.controllerOfGiven grants Set.empty oid gs
    Zone.Hand -> Just (Object.owner obj)
    Zone.Graveyard -> Just (Object.owner obj)
    _ -> Nothing

-- CR 602.5: does every clause of this ability's printed "activate only ..."
-- rider permit activating it right now?
--
-- ALL of them, which is what CR 602.5's "prohibited from being activated" means:
-- one clause failing is a prohibition in force, and a card printing two joins
-- them with "and". The empty list is CR 602.2's default and passes vacuously.
--
-- Priority is not re-checked here: the only caller is Action.legalActions, which
-- the priority loop asks only of the player who has priority.
--
-- CASTING PROHIBITIONS ARE NOT CONSULTED, by any clause. CR 307.5 says so for the
-- sorcery-speed rider, and no rule extends Pawl.Engine.Cast's CR 601.3 list to
-- an activation either, since CR 601.3 is about beginning to CAST a spell. That
-- is why this reads the game state directly rather than reaching for
-- Pawl.Types.CastingRestriction, whose arms are spelled the same way and answer
-- a different question.
--
-- This gate makes the ability un-OFFERED. Engine.priorityLoop is what makes that
-- binding: it rejects an action the interpreter was not offered (#219).
restrictionsOk :: PlayerId -> ActivatedAbility.ActivatedAbility Card.Card -> GameState -> Bool
restrictionsOk pid ability gs = all (restrictionMet pid gs) (ActivatedAbility.restrictions ability)

-- Does the game state satisfy this one printed clause?
--
-- Casing on the arms is a classification, not an effect's identity:
-- Pawl.Engine.Activate is the sole reader of Pawl.Types.ActivationRestriction,
-- exactly as Pawl.Engine.Cast is of Pawl.Types.CastingRestriction.
restrictionMet :: PlayerId -> GameState -> ActivationRestriction.ActivationRestriction -> Bool
restrictionMet pid gs restriction = case restriction of
  -- CR 307.5's three conjuncts, and Turn.sorcerySpeedWindow is that window shared
  -- with CR 307.1's casting gate: two rules, the same three facts, one copy.
  ActivationRestriction.SorcerySpeed -> Turn.sorcerySpeedWindow pid gs
  -- CR 500.1's phases and steps: Turn.inWindow asks whether GameState.phase
  -- falls inside the window the rider names. CONTAINMENT rather than equality,
  -- because a rider may name a phase that has steps -- Jade Statue's "only
  -- during combat" is live in all five of CR 506.1's combat steps, while
  -- Desert's names one of them. Pawl.Engine.Cast reads the same
  -- Pawl.Types.DuringPhase bundle off CastingRestriction.DuringPhase;
  -- deliberately duplicated rather than shared, since the two gates differ in
  -- what else they may read.
  --
  -- CR 102.1 supplies the second conjunct, a genuinely separate fact: Desert's
  -- rider names no turn (EachTurn, and the step alone decides), while Llanowar
  -- Augur's "only during your upkeep" names alice's upkeep and not bob's. CR
  -- 109.5 is why `pid` answers "your" -- for an activated ability that is the
  -- player who activated it, which `activatable` has already pinned to
  -- activatorOf -- so a stolen permanent's rider follows the thief.
  ActivationRestriction.DuringPhase (DuringPhase.MkDuringPhase window scope) ->
    Turn.inWindow window (GameState.phase gs)
      && Event.turnScopeAdmits scope (GameState.activePlayer gs) pid
  -- CR 508.3b's question, asked of the ACTIVATING player, and the same reader the
  -- casting side's clause of this name uses -- see Combat.attackedThisStep for
  -- why it is the declaration record and not Combat.attacked.
  ActivationRestriction.AttackedThisStep -> Combat.attackedThisStep pid gs
  -- CR 506.7b through CR 506.7g, which says an "activate only" rider naming one
  -- of CR 506.7's points is governed exactly as the same words on a cast are --
  -- so this is Combat.afterBlockersDeclared verbatim, the reader
  -- Pawl.Engine.Cast's arm of this name uses. Trap Runner is the card.
  ActivationRestriction.AfterBlockersDeclared -> Combat.afterBlockersDeclared gs

-- CR 606.3 (CR 306.5d says the same for planeswalkers): a loyalty ability may be
-- activated only with priority and an empty stack during a main phase of its
-- controller's turn, and only if no player has already activated a loyalty
-- ability of that permanent this turn.
--
-- Vacuously true for every ability that is not a loyalty ability, which is what
-- makes this a conjunct rather than an arm of restrictionsOk: CR 606.3 is a rule
-- about what a COST contains (CR 606.2), not a clause a card prints, so it is
-- derived through Pawl.Engine.Cost.isLoyaltyCost and never read off
-- Pawl.Types.ActivationRestriction, whose arms are all printed text.
--
-- The window is Turn.sorcerySpeedWindow verbatim, not a near-copy: CR 606.3's
-- first clause and CR 307.5's restricted case are the same three facts, and
-- priority is not re-checked because only the priority holder is asked.
--
-- The once-per-turn clause is a fold over the CR 608.2i log rather than a stamp,
-- and it is keyed on the PERMANENT and not on the player: an opponent who
-- somehow activated it first has used the permanent's one activation.
loyaltyOk :: PlayerId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Card -> GameState -> Bool
loyaltyOk pid srcId ability gs =
  not (Cost.isLoyaltyCost (ActivatedAbility.cost ability))
    || (Turn.sorcerySpeedWindow pid gs && not (loyaltyActivatedThisTurn srcId gs))

loyaltyActivatedThisTurn :: ObjectId -> GameState -> Bool
loyaltyActivatedThisTurn srcId gs = elem (GameEvent.LoyaltyAbilityActivated srcId) (fmap snd (GameState.events gs))

-- CR 602.2b's routing of an activation cost through CR 601.2b, at the X=0 FLOOR:
-- an ability is affordable when its activation cost is payable with X=0, since
-- the activating player may always choose 0. `activatable` conjoins this
-- predicate and `affordableX` climbs it, so what activatability measures and
-- what the bound reports cannot drift apart.
--
-- The substitution is not decoration. A ManaSymbol.Variable that reaches payment
-- demands nothing at all (Mana.waysOf), so leaving it in place would answer the
-- same as X=0 by accident rather than by rule -- the accident that made the {X}
-- free (#544).
payableCost :: PlayerId -> ObjectId -> GameState -> Cost Keyword -> Bool
payableCost = payableCostAt 0

-- The same predicate on a board the caller already walked -- see
-- Cost.canPaySomeCompletionGiven.
payableCostGiven :: [ObjectId] -> Map.Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> ObjectId -> GameState -> Cost Keyword -> Bool
payableCostGiven sources pcs = payableCostAtGiven sources pcs 0

-- The same question asked at some OTHER value of X -- `payableCost` is this at
-- the floor and `affordableX` is this climbed.
--
-- CR 601.2f's TOTALLING against the ACTIVATION adjustments (CR 602.2b routes an
-- activation cost through rule 601.2b-i), which is Heartstone and Training
-- Grounds reaching an ability's cost (#90). Not the spell's adjustments and not
-- Cost.total: those gather the constructors whose sentences say "spells", so
-- Thalia's tax cannot arrive here however her Filter reads
-- (Pawl.Engine.PlayerEffect.activationCostAdjustments).
--
-- BOTH halves of the totalling: the mana arithmetic rides in as a function, for
-- the reason below, and CR 601.2f's additional non-mana components are appended
-- to the cost before it is measured (Cost.plusComponents) -- so Brutal
-- Suppression's "Sacrifice a land" is a reason this gate can answer False, and
-- the cost it measures is the cost `activateAbility` will pay.
--
-- Cost.canPaySomeCompletion and not Cost.canPay so that the two gates ask ONE
-- predicate, in the same shape Cost.announce's `total` parameter gives the two
-- offers: CR 601.2b's completion comes before CR 601.2f's totalling, so a {2/R}
-- totalled while still spelled {2/R} would hide the generic reduction the
-- announcement exposes.
payableCostAt :: Natural -> PlayerId -> ObjectId -> GameState -> Cost Keyword -> Bool
payableCostAt x pid srcId gs cost =
  let adjustments = Cost.activationAdjustments pid srcId gs
   in Cost.canPaySomeCompletion Nothing ManaSpending.AsProduced pid srcId (Cost.totalManas adjustments) (Cost.plusComponents adjustments (Cost.substituteX x cost)) gs

-- The same predicate on a board the caller already walked -- see
-- Cost.canPaySomeCompletionGiven.
payableCostAtGiven :: [ObjectId] -> Map.Map ObjectId PC.ProjectedCharacteristics -> Natural -> PlayerId -> ObjectId -> GameState -> Cost Keyword -> Bool
payableCostAtGiven sources pcs x pid srcId gs cost =
  let adjustments = Cost.activationAdjustments pid srcId gs
   in Cost.canPaySomeCompletionGiven Nothing ManaSpending.AsProduced sources pcs pid srcId (Cost.totalManas adjustments) (Cost.plusComponents adjustments (Cost.substituteX x cost)) gs

-- CR 601.2b via 602.2b: the greatest X this player could actually pay for, which
-- is what Prompt.ChooseX carries. The climb itself is Cost.greatestPayableX,
-- shared with Cast.affordableX; only the predicate differs, and only by WHICH
-- adjustments CR 601.2f's totalling reads. Advisory, never a clamp -- see
-- Prompt.ChooseX.
affordableX :: PlayerId -> ObjectId -> GameState -> Cost Keyword -> Natural
affordableX pid srcId gs cost = Cost.greatestPayableX (\x -> payableCostAt x pid srcId gs cost) cost

-- CR 602.2/602.5: the ability is a member of the source's abilities
-- (abilitiesFor), it is not a mana ability, the whole activation cost is payable
-- at CR 601.2b's X=0 floor (CR 118.3), the {T} sickness gate holds, the
-- ability's timing rider permits it now (CR 307.5), and enough modes are
-- fillable to satisfy the selection (CR 700.2a/602.2b). The cost is CR 601.2f's
-- total of the printed one (payableCostAt).
--
-- The mana-ability conjunct is about the STACK and not about permission: CR
-- 605.3b keeps such an ability off it, so an Action.Activate has nothing to do
-- with one. CR 605.3a's windows are served elsewhere -- by
-- Action.ActivateManaAbility with priority, and by Cost.payMana inside a
-- payment -- and both of them go through Cost.tapForMana.
--
-- activatableGiven is the half Action.legalActions wants: `grants` is one
-- control-grant walk, `pcs` one whole-board projection and `sources` one sweep
-- of this player's mana sources, each taken once for the enumeration instead of
-- once per permanent per ability (#200, #316, #1073). EVERY conjunct is given
-- that board, the last two included. They ask about OTHER objects -- the target
-- pool, the mana sources -- but the board is a whole-board snapshot rather than
-- this object's own, so it answers those questions too, and the plain wrappers
-- they used to call (Target.fillableModes, Cost.canPaySomeCompletion) build
-- exactly this from exactly this `gs`.
--
-- `pools` and `sources` are the last two conjuncts' own hoists, and they are the
-- residual #716 could not reach: threading the PROJECTIONS in stopped the target
-- and cost gates projecting the board per permanent, but each still built a
-- whole battlefield structure of its own per ability -- the target gate a base
-- recipient set (Target.Pools), the cost gate this player's mana sources. Both
-- are functions of `gs` alone, so both are the same for every permanent in one
-- enumeration (#1073). Build `sources` with Cost.activationManaSourcesGiven,
-- which is the one place that pairs the sweep with the capacity this gate reads.
--
-- Threading them buys the SHAPE of the loop: those wrappers hoist per CALL, and
-- the caller is a loop over the battlefield, so an ability that reached them
-- cost a whole-board sweep per permanent. Pawl.PerformanceSpec's Prodigal
-- Sorcerer ceiling is what holds that line.
--
-- `activatable` keeps Map.empty deliberately. It has no engine caller -- only
-- tests -- so the slower per-object Projection.projectGiven fallback costs
-- nothing, and it makes the plain path a genuinely independent computation that
-- PerformanceSpec's differential test can hold the threaded one against -- its
-- `sources` is built off that same Map.empty for the same reason.
activatable :: PlayerId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Card -> GameState -> Bool
activatable pid srcId ability gs =
  let grants = Projection.controlGrants gs
   in activatableGiven grants Map.empty (Target.poolsGiven Map.empty gs) (Cost.activationManaSourcesGiven grants Map.empty pid gs) pid srcId ability gs

activatableGiven :: [Projection.ControlGrant] -> Map.Map ObjectId PC.ProjectedCharacteristics -> Target.Pools -> [ObjectId] -> PlayerId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Card -> GameState -> Bool
activatableGiven grants pcs pools sources pid srcId ability gs =
  activatorOfGiven grants srcId gs == Just pid
    && elem ability (abilitiesForGiven pcs srcId gs)
    && not (ManaAbility.isManaAbility ability)
    -- CR 702.61a's other limb -- "players can't ... activate abilities that
    -- aren't mana abilities" -- and it sits AFTER the mana conjunct on purpose:
    -- CR 702.61b's exemption for mana abilities is then the same fact CR 605.3b
    -- already established here, rather than a second reading of the rule. The
    -- windows that do serve a mana ability (Action.ActivateManaAbility,
    -- Cost.payMana) never reach this function, so neither is gated.
    && not (SplitSecond.inForce gs)
    -- CR 701.35a's third clause. UNLIKE split second one line up, this reaches a
    -- mana ability too -- rule 701.35a says "its activated abilities" with no
    -- carve-out where CR 702.61b writes one -- so Cost.manaActivations carries the
    -- same conjunct for CR 605.3a's windows, exactly as sickness below is asked in
    -- both places.
    && not (Detain.detained srcId gs)
    && sicknessOkGiven pcs pid srcId ability gs
    && restrictionsOk pid ability gs
    && loyaltyOk pid srcId ability gs
    && Modal.selectionPossible
      (Target.fillableModesGiven pcs grants pools (Just pid) Map.empty srcId Map.empty (ActivatedAbility.modal ability) gs)
      (Modal.Type.selection (ActivatedAbility.modal ability))
    && payableCostGiven sources pcs pid srcId gs (ActivatedAbility.cost ability)

-- CR 602.2a: an ability activated from a hidden zone reveals the card that has
-- it (CR 701.20a). Note what the rule does NOT say: there is no qualifier about
-- the cost. The "cost that can't be paid while the object is on the battlefield"
-- clause people remember is CR 113.6j, which is about where an ability FUNCTIONS.
--
-- The revealer is the activating player. CR 602.2a is worded passively and names
-- nobody, but the hidden zone being read is theirs -- CR 400.3 puts every card in
-- a hand in its owner's, and Activate.activatorOf gives a card in a hand to that
-- owner precisely because CR 108.4 leaves it with no controller.
--
-- Reaches exactly the hand today: abilitiesFor serves the battlefield, the hand
-- and the graveyard and answers [] elsewhere -- and of those three only a hand is
-- hidden (CR 400.2 makes a graveyard a public zone), so the library offers
-- nothing to activate, and mana abilities never reach this function (CR 605.3b
-- keeps them off the stack).
--
-- CR 701.20a's duration -- revealed until the ability leaves the stack -- is not
-- modeled, and is vacuous for every card in `data/cards/`: every ability a hand
-- offers there discards the card as a cost, whether rule 702 minted that cost
-- (CR 702.29a's cycling, CR 702.77a's reinforce) or the card authored it (Faerie
-- Macabre, CR 113.6j), so the card is in a public
-- graveyard a moment later. A forecast ability (CR 702.57a) is the shape that
-- would make the duration observable; none is in the pool (#282).
revealIfHidden :: PlayerId -> ObjectId -> Game ()
revealIfHidden pid srcId = do
  gs <- State.get
  case fmap Object.zone (Game.lookupObject srcId gs) of
    Just zone | Game.isHiddenZone zone -> Event.reveal RevealCause.Ordinary pid srcId
    _ -> pure ()

-- CR 602.2: announce the activation, revealing the card if it is coming from a
-- hidden zone (602.2a), put the ability on the stack (a fresh OfAbility object),
-- then walk CR 601.2b-i as CR 602.2b sends it -- choose modes, announce the value
-- of X, announce the hybrid and Phyrexian symbols (CR 118.13a), stamp targets,
-- pay -- and keep priority (117.3c). Reject-not-repair on an illegal mode or
-- target answer.
--
-- An announced X can lose the activation all by itself: enumeration measures the
-- cost at CR 601.2b's X=0 floor, and the value the player names is theirs to
-- name freely. See the gate below.
--
-- `before` is the pre-announcement state and is the ONLY thing the rejection
-- paths restore to, which is what puts CR 602.2a's reveal inside the rollback:
-- an activation the engine refused revealed nothing, and must leave no reveal
-- in the log claiming otherwise. Everything else reads `gs`, the state as of the
-- announcement.
activateAbility :: PlayerId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Card -> Game ()
activateAbility pid srcId ability = do
  before <- State.get
  -- CR 602.2a's own order: the reveal is part of announcing, and so precedes
  -- the ability becoming an object on the stack (the rest of that same rule).
  revealIfHidden pid srcId
  gs <- State.get
  let (abilId, gs1) = Game.freshObjectId gs
      (ts, gs2) = Game.freshTimestamp gs1
      obj =
        Object.MkObject
          { Object.owner = pid,
            Object.enteredUnder = Nothing,
            Object.source = Source.OfAbility srcId ability,
            Object.zone = Zone.Stack,
            Object.tapped = TapState.Untapped,
            Object.facing = Facing.FaceUp,
            Object.exiledFaceDown = False,
            Object.damage = 0,
            Object.sickness = Sickness.Settled pid,
            Object.bindings = Map.empty,
            Object.counters = Map.empty,
            Object.counterTimestamps = Map.empty,
            Object.attachedTo = Nothing,
            Object.chosenColor = Nothing,
            Object.chosenSubtype = Nothing,
            Object.chosenNames = Set.empty,
            Object.chosenPlayer = Nothing,
            Object.timestamp = ts,
            Object.face = Nothing,
            Object.turnedOverAt = Nothing,
            Object.worldSince = Nothing,
            Object.playableFromExile = Nothing,
            Object.plotted = Nothing,
            Object.foretold = Nothing,
            Object.ringBearerFor = Nothing,
            Object.protector = Nothing,
            Object.ventureRoom = Nothing,
            Object.classLevel = Nothing,
            Object.unlockedHalves = Set.empty,
            Object.designations = Set.empty,
            Object.kicked = False,
            Object.announcedX = Nothing,
            Object.detainedUntil = Set.empty,
            Object.doesNotUntapNext = False,
            Object.exertedBy = Set.empty
          }
      onStack =
        gs2
          { GameState.objects = Map.insert abilId obj (GameState.objects gs2),
            GameState.stack = abilId : GameState.stack gs2
          }
      decider = Decide.deciderFor pid gs
      -- CR 602.2b/700.2a, mirroring Cast.castSpell's mode block: a selection with
      -- one answer is FORCED, unprompted -- every single-mode ability is exactly
      -- {ModeIndex 0}. A real choice issues ChooseModes. Modal.forcedSelection is
      -- what tells the two apart, CR 700.2d's exception included.
      legal = Target.fillableModes (Just pid) Map.empty srcId Map.empty (ActivatedAbility.modal ability) gs
      selection = Modal.Type.selection (ActivatedAbility.modal ability)
  State.put onStack
  -- Sorted on the way in, for the reason Cast.castProposed gives: printed order
  -- (CR 608.2c), with a repeated mode's instances adjacent (CR 700.2d).
  chosenModes <- case Modal.forcedSelection legal selection of
    Just forced -> pure forced
    Nothing -> fmap Seq.sort (Game.choose (Prompt.ChooseModes decider pid abilId legal selection))
  -- Reject-not-repair: an answer that does not satisfy the printed instruction
  -- makes the whole activation a no-op, guarding every step below.
  if not (Modal.selectionSatisfiedBy legal selection chosenModes)
    then State.put before
    else do
      -- CR 602.2b routes the rest of the activation through CR 601.2b-i, so CR
      -- 601.2b's announcement of a variable cost (CR 107.3) governs an activation
      -- cost's {X} too, and it is asked HERE -- after the modes, before CR
      -- 118.13a's announcement and before CR 601.2c's targets, which is 601.2b's
      -- own order.
      --
      -- Cinder Elemental exercises it. Not asking was not a missing question but
      -- a free {X}: a ManaSymbol.Variable that survives to payment demands
      -- nothing (Mana.waysOf), so the engine was announcing 0 on the player's
      -- behalf (#544).
      --
      -- The bound rides the PRINTED cost, which is what `activatable` gated on:
      -- both go through payableCostAt, so CR 601.2f's totalling is applied by the
      -- predicate rather than baked into the cost handed to it. Nothing filters
      -- the answer against the bound (see Prompt.ChooseX).
      let printedCost = ActivatedAbility.cost ability
      mAmount <-
        if Cost.hasVariable printedCost
          then fmap Just (Game.choose (Prompt.ChooseX decider pid abilId (affordableX pid srcId gs printedCost)))
          else pure Nothing
      let announcedAtX = maybe printedCost (\x -> Cost.substituteX x printedCost) mAmount
      -- CR 602.2: an activation a player cannot comply with is illegal, and the
      -- game returns to the moment before it started. The X just named is where
      -- that can first become true: `activatable` measured the cost at CR
      -- 601.2b's X=0 floor, the only value it can know before an announcement
      -- exists.
      --
      -- Asked with the same predicate that floor was asked with, so a gate and an
      -- announcement cannot disagree about what a cost is. That matters beyond
      -- tidiness: CR 118.13a's announcement below runs on this cost, and an X
      -- large enough to leave neither of CR 107.4f's routes payable would leave
      -- Mana.announce with no offer to make. This gate is what keeps that arm out
      -- of reach from here (#417).
      --
      -- Reject-not-repair, the posture every other step here takes: the
      -- announcement is NOT clamped to affordableX -- CR 601.2b lets the player
      -- announce the value freely -- it is honoured and then loses the ability.
      -- Asked unconditionally rather than only when there is an {X}, which buys
      -- one predicate over one cost instead of two spellings of when the gate
      -- applies.
      if not (payableCost pid srcId gs announcedAtX)
        then State.put before -- reject: the whole activation is a no-op
        else do
          -- CR 118.13a's announcement, which names an activated ability's
          -- activation cost, happens here at 601.2b's position and not when the
          -- cost is paid. Moltensteel Dragon exercises it; the rule's other two
          -- clauses -- a cost paid during a resolution, or for a special action --
          -- are still unreached (#373).
          --
          -- Measured through the SAME totalling payableCost gated on, off the
          -- same adjustments -- against the printed cost instead, a reduction
          -- could hide a route and Mana.announce would elide the prompt and pay
          -- life on the player's behalf (#416, for the spell that named it).
          --
          -- Run on the cost carrying the ANNOUNCED value, which is CR 601.2b's own
          -- order (the value of X precedes the hybrid and Phyrexian
          -- announcements).
          --
          -- CR 601.2f's additional components are on the cost by this point
          -- (Cost.plusComponents), which is what payableCost measured and what
          -- Cost.pay will charge. It matters to the announcement itself: the
          -- offers are filtered against the claims a component makes on a zone,
          -- so a Phyrexian symbol offered without the added "Sacrifice a land"
          -- in view would be offered against a board that has one land too many.
          let gathered = Cost.activationAdjustments pid srcId gs
          announcedCost <- Cost.announce Nothing ManaSpending.AsProduced pid srcId (Cost.totalManas gathered) (Cost.plusComponents gathered announcedAtX)
          let slots = Modal.modesTargetSlots chosenModes (ActivatedAbility.modal ability)
              sets = Target.legalSets (Just pid) Map.empty srcId slots gs
          chosen <- Target.chooseTargets decider pid abilId slots sets
          if not (Target.selectionLegal (Just pid) srcId slots sets chosen gs)
            then State.put before -- reject: the whole activation is a no-op
            else do
              -- CR 113.7: bind the source permanent under the reserved self slot, so
              -- an activated ability that refers to "this creature" (Longtusk Cub)
              -- resolves the reference as a slot read -- exactly as
              -- Engine.placeBorne does for a TRIGGERED ability's source.
              --
              -- CR 109.5 binds the controller under the reserved you slot in the
              -- same breath: "The words 'you' and 'your' on an object refer to the
              -- object's controller ... For an activated ability, this is the
              -- player who activated the ability." That player is `pid`, the one
              -- CR 602.2 lets activate this ability at all -- so Brothers of Fire's
              -- "and 1 damage to you" reaches a player, exactly as
              -- Engine.placeBorne does for a TRIGGERED ability's controller.
              --
              -- CR 601.2b's announced X is stamped alongside, onto the ABILITY object
              -- and not the source permanent -- Cinder Elemental sacrifices that
              -- permanent to pay, so the ability is the only holder still there to
              -- read at resolution (Quantity.evaluateFor). CR 113.7a is why the
              -- ability keeps all three once its source is gone: "Once activated or
              -- triggered, an ability exists on the stack independently of its
              -- source."
              State.modify' (\g -> g {GameState.objects = Map.adjust (\o -> o {Object.bindings = Binding.setYou pid (Binding.setTriggerSource srcId (Binding.fromChoices chosen mAmount chosenModes))}) abilId (GameState.objects g)})
              -- CR 601.2f, at the position CR 602.2b gives it and in Cast.castSpell's
              -- own order: the reductions that apply to this activation are announced
              -- (CR 118.7e's choice of half) and then applied to the announced cost.
              -- The record announced is the record applied, so the cost paid is the
              -- cost the gates measured -- one reduction cannot be gathered twice
              -- from two states.
              --
              -- CR 118.7e asks nothing today: every activation-cost reducer in the
              -- pool reduces by generic mana (Heartstone's {1}), which has no halves
              -- to choose between. The seam is here rather than skipped so that the
              -- one that does cannot arrive at a path that never asks.
              --
              -- CR 601.2f's ORDER is asked at the same seam and does reach a board:
              -- Heartstone's floor beside Blossoming Tortoise's absence of one on an
              -- animated Mishra's Foundry is two orders at two prices. The ANNOUNCED
              -- cost goes in because the order is chosen against the cost it will be
              -- applied to.
              adjustments <- Cost.announceReductions pid srcId gs announcedCost gathered
              let paidCost = Cost.totalWith adjustments announcedCost
              -- CR 601.2g/h via Pawl.Engine.Cost.pay: the mana window, then the
              -- components. The gates above prove SOME sequence of choices pays for
              -- this ability -- but Unpaid is reachable all the same, because the
              -- mana window then asks the player to make those choices and a
              -- mis-tapped colour is a choice the engine must honour (Cost.payMana).
              -- Reject-not-repair restores the whole activation, including the
              -- ability object this function put on the stack.
              payment <- Cost.pay Nothing ManaSpending.AsProduced pid srcId paidCost
              case payment of
                -- CR 606.3: record that a loyalty ability of THIS PERMANENT was
                -- activated, which is the whole of the once-per-turn limit's storage
                -- (see loyaltyOk above). Every path that rejects the activation
                -- restores `before`, and the log lives in that state, so no rejected
                -- activation can leave a record behind.
                Payment.Paid bound -> do
                  -- CR 608.2h: the slots the PAYMENT bound -- the permanent a
                  -- Sacrifice component put in a graveyard -- folded onto the
                  -- ability object, so "the sacrificed creature's power" has a
                  -- name to read at resolution (Jarad, Golgari Lich Lord). After
                  -- the payment because that is when the payment knows them, and
                  -- onto the ABILITY for the reason CR 113.7a gives above: the
                  -- source may itself be what was sacrificed.
                  State.modify' (\g -> g {GameState.objects = Map.adjust (\o -> o {Object.bindings = Binding.setPaid bound (Object.bindings o)}) abilId (GameState.objects g)})
                  Monad.when
                    (Cost.isLoyaltyCost (ActivatedAbility.cost ability))
                    (State.modify' (Event.recordEvent (GameEvent.LoyaltyAbilityActivated srcId)))
                  -- CR 601.2c through CR 602.2b: each chosen object became a
                  -- target of this ability, which is what CR 702.21a's ward
                  -- watches -- and an activated ability is the half
                  -- GameEvent.SpellCast could never carry.
                  --
                  -- The ABILITY object and not its source permanent: rule 702.21a
                  -- counters "that spell or ability", and CR 113.7a makes the
                  -- ability the thing on the stack. Its controller is `pid` (CR
                  -- 113.8), the player rule 702.21a offers the cost to.
                  --
                  -- After the payment for Cast.castSpell's reason: everything
                  -- above can still restore `before` and unwind the activation.
                  Event.becameTarget abilId StackObjectKind.Ability pid chosen
                Payment.Unpaid -> State.put before
