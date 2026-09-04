module Pawl.Engine.Activate where

import qualified Control.Monad as Monad
import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Pawl.Engine.ActivationRestriction as ActivationRestriction
import qualified Pawl.Engine.Binding as Binding
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
import qualified Pawl.Engine.Resolve as Resolve
import qualified Pawl.Engine.SplitSecond as SplitSecond
import qualified Pawl.Engine.Target as Target
import qualified Pawl.Engine.Turn as Turn
import qualified Pawl.Engine.Vanguard as Vanguard
import qualified Pawl.Types.AbilityKind as AbilityKind
import qualified Pawl.Types.AbilityName as AbilityName
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.ActivatedAbilitySource as ActivatedAbilitySource
import qualified Pawl.Types.Activator as Activator
import qualified Pawl.Types.Card as Card
import Pawl.Types.Cost (Cost)
import qualified Pawl.Types.CostAdjustments as CostAdjustments
import qualified Pawl.Types.Face as Face
import qualified Pawl.Types.Facing as Facing
import Pawl.Types.Game (Game)
import qualified Pawl.Types.GameEvent as GameEvent
import Pawl.Types.GameState (GameState)
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import Pawl.Types.Keyword (Keyword)
import qualified Pawl.Types.KeywordFamily as KeywordFamily
import qualified Pawl.Types.LoggedEvent as LoggedEvent
import qualified Pawl.Types.Mana as Mana
import qualified Pawl.Types.ManaSpending as ManaSpending
import qualified Pawl.Types.Modal as Modal.Type
import qualified Pawl.Types.ModeIndex as ModeIndex
import qualified Pawl.Types.Object as Object
import Pawl.Types.ObjectId (ObjectId)
import qualified Pawl.Types.Payment as Payment
import qualified Pawl.Types.PaymentMoment as PaymentMoment
import qualified Pawl.Types.PaymentSubject as PaymentSubject
import Pawl.Types.PlayerId (PlayerId)
import qualified Pawl.Types.ProjectedCharacteristics as PC
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.RevealCause as RevealCause
import qualified Pawl.Types.Sickness as Sickness
import Pawl.Types.SlotName (SlotName)
import qualified Pawl.Types.Source as Source
import qualified Pawl.Types.StackObjectKind as StackObjectKind
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.Zone as Zone

-- CR 302.6: a creature's {T}-cost ability can't be activated while summoning
-- sick. The whole reading lives in Pawl.Engine.Cost, because the mana window
-- needs the same one and cannot come through here -- CR 605.3b is why
-- activatableGiven refuses a mana ability outright. All that is left on this
-- side is handing over the ability's cost.
sicknessOk :: PlayerId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Card (GrantedAbility.GrantedAbility Card.Card) -> GameState -> Bool
sicknessOk = sicknessOkGiven Map.empty

sicknessOkGiven :: Map.Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Card (GrantedAbility.GrantedAbility Card.Card) -> GameState -> Bool
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
-- through zoneAbilitiesOf. In the COMMAND zone: the same reader again, whose CR
-- 113.6p limb keeps an emblem's (CR 114.4) and a face-up vanguard card's (CR
-- 902.7) rows that state no zone at all. Anywhere else: nothing -- flashback and
-- rule 702's other zone abilities are CASTING permissions (CR 702.34a), so they reach
-- Pawl.Engine.Cast instead. The first ability ACTIVATED from a fifth zone adds
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
abilitiesFor :: ObjectId -> GameState -> [ActivatedAbility.ActivatedAbility Card.Card (GrantedAbility.GrantedAbility Card.Card)]
abilitiesFor = abilitiesForGiven Map.empty

-- The ...Given half of the pair, and the one the enumeration calls:
-- Action.legalActions hands it the board it projected once, so nothing here
-- re-derives a projection per object. Only the battlefield arm reads that board
-- at all -- a hand or graveyard object's absence from it is not a miss (#1859; see
-- Projection.projectGiven).
abilitiesForGiven :: Map.Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> GameState -> [ActivatedAbility.ActivatedAbility Card.Card (GrantedAbility.GrantedAbility Card.Card)]
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
  --
  -- The delayed map is the HOST's own face while the abilities are the PROJECTED
  -- ones, so an ability granted by another object that arms a delayed trigger the
  -- GRANTOR declared resolves to no name here and takes CR 113.6's battlefield
  -- default. Nothing in data/cards/ grants an ability that arms one; the CR
  -- 113.6m readings that matter -- Reassembling Skeleton, Prized Amalgam -- are
  -- all a card's own text. Pawl.Engine.Event.battlefieldAbilitiesOf carries the
  -- same pairing and the same note.
  Just Zone.Battlefield -> filter (functionsIn (Game.delayedAbilitiesOf oid gs) Zone.Battlefield) (Projection.abilitiesGiven pcs oid gs)
  -- CR 113.6j: the MINTED abilities rule 702 gives the printed keywords, plus the
  -- card's own AUTHORED ones that name the hand. The two are disjoint by
  -- construction -- handAbilitiesOf reads Face.keywords and zoneAbilitiesOf reads
  -- Face.activatedAbilities -- so nothing is offered twice.
  Just Zone.Hand -> case Game.faceOf oid gs of
    Nothing -> []
    Just face -> Keyword.handAbilitiesOf (Face.keywords face) <> zoneAbilitiesOf Zone.Hand oid gs
  Just Zone.Graveyard -> zoneAbilitiesOf Zone.Graveyard oid gs
  -- CR 114.4 and CR 902.7's third limb, "its activated abilities may be
  -- activated". The narrowing to the objects rule 113.6p names is inside
  -- zoneAbilitiesOf, so a commander or a dungeon card sharing this zone offers
  -- only a row that STATES it (CR 113.6b) -- the split Pawl.Engine.Projection's
  -- fromCommandZone and Pawl.Engine.Event's inCommand make with the same test.
  Just Zone.Command -> zoneAbilitiesOf Zone.Command oid gs
  _ -> []

-- CR 702 / CR 601.2f: WHICH RULE minted this ability of `oid`, as the family
-- designator Pawl.Types.ReduceActivationCost.grantedBy compares -- Fluctuator's
-- "cycling abilities you activate" against the ability actually being activated.
-- Nothing for an ability the card itself prints.
--
-- The zone split is abilitiesForGiven's above, and for its reason: the hand arm
-- reads PRINTED keywords because rules 702.29a and 702.77a state the hand, and
-- the battlefield arm reads POST-LAYER ones because CR 613.1f can take a keyword
-- away. Kept in step with that function by hand -- an arm here that named a
-- different keyword source than the arm that MINTED the ability would classify it
-- as printed, which reads as "no reduction" rather than as an error.
--
-- Nothing for every other zone, the command zone included, where
-- abilitiesForGiven does offer abilities: no rule-702 keyword mints an activated
-- ability outside a hand or the battlefield, so a command-zone offer is always a
-- printed one and always classifies as printed here.
familyGrantingGiven :: Map.Map ObjectId PC.ProjectedCharacteristics -> ObjectId -> GameState -> ActivatedAbility.ActivatedAbility Card.Card (GrantedAbility.GrantedAbility Card.Card) -> Maybe KeywordFamily.KeywordFamily
familyGrantingGiven pcs oid gs ability = case fmap Object.zone (Game.lookupObject oid gs) of
  Just Zone.Battlefield -> Keyword.familyGranting (Projection.keywordsGiven pcs oid gs) ability
  Just Zone.Hand -> case Game.faceOf oid gs of
    Nothing -> Nothing
    Just face -> Keyword.familyGranting (Map.fromSet (const 1) (Face.keywords face)) ability
  _ -> Nothing

-- familyGrantingGiven off this object's own board -- `activatable`'s pairing, and
-- activateAbility's, neither of which is inside an enumeration.
familyGranting :: ObjectId -> GameState -> ActivatedAbility.ActivatedAbility Card.Card (GrantedAbility.GrantedAbility Card.Card) -> Maybe KeywordFamily.KeywordFamily
familyGranting = familyGrantingGiven Map.empty

-- CR 113.6j + CR 113.6m + CR 702.178b: the AUTHORED abilities a card outside the
-- battlefield offers from the zone it is in. Three zones ask it today -- the
-- graveyard, the hand for Faerie Macabre's "Discard this card: Exile up to
-- two target cards from graveyards", and the command zone for Barrin's
-- "Sacrifice a permanent: Return target creature to its owner's hand" -- and the
-- zone is a parameter because nothing in the reading below is about which zone
-- it is: CR 113.6j says an
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
zoneAbilitiesOf :: Zone.Zone -> ObjectId -> GameState -> [ActivatedAbility.ActivatedAbility Card.Card (GrantedAbility.GrantedAbility Card.Card)]
zoneAbilitiesOf zone oid gs = case (Game.faceOf oid gs, Game.lookupObject oid gs) of
  (Just face, Just obj) ->
    let delayed = Face.delayedAbilities face
        -- CR 113.6p, the limb beside CR 113.6b that reads the OBJECT rather than
        -- the ability: an emblem's abilities function in the command zone (CR
        -- 114.4) and a face-up vanguard card's do too (CR 902.7), so a row of one
        -- that states no zone functions here instead of taking rule 113.6's
        -- battlefield default. functionsIn keeps the default and this disjunct
        -- overrides it, rather than the two readings being written twice;
        -- Vanguard.functionsFromCommandZone is rule 113.6p's own list, shared with
        -- the static, replacement, triggered and combat-restriction walks over
        -- this zone, so a commander's or a dungeon card's unstated row is left
        -- functioning on the battlefield where rule 113.6 puts it.
        functionsHere ability =
          functionsIn delayed zone ability
            || ( zone == Zone.Command
                   && Maybe.isNothing (zoneFunctionedFrom delayed ability)
                   && Vanguard.functionsFromCommandZone oid gs
               )
        granted ability = case ActivatedAbility.condition ability of
          Nothing -> True
          Just cond -> Condition.holds (Projection.fullView gs) (Filter.contextFor (Game.teams gs) (Just (Object.owner obj)) (Just oid)) gs oid cond
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
-- CR 113.6m's "a previous part of its cost or effect specifies that the
-- object is put into that zone" clause needs no order-sensitivity here, and
-- gets none: it can cancel no answer this fold gives. CR 400.7 mints a fresh
-- id on every zone change, so a part that puts the object into a zone leaves
-- CR 113.7a's source nothing to move -- a later part has to name the ARRIVAL
-- instead (Pawl.Engine.Binding.became, or a slot the move itself minted, as
-- Meandering Towershell's "exiled" is), and Pawl.Engine.EffectZone answers
-- Nothing for every ref but the reserved source slot. So first-answer-wins and
-- the rule's order-sensitive reading agree on every ability that can be
-- written, and a card whose later part moved the reserved source slot out of a
-- zone an earlier part put it into is what would refute that; see #2501.
--
-- CR 113.6m's final sentence is read by the fold too, Pawl.Engine.EffectZone's
-- ArmDelayedTrigger arm answering off the `delayed` map this carries. The one
-- card in data/cards/ whose ACTIVATED ability arms a delayed trigger is Grist,
-- the Hunger Tide, and what it arms destroys a target rather than moving Grist,
-- so the walk answers Nothing there. This reading is therefore a regression
-- fence; the sentence itself is proved through the TRIGGERED one, on Prized
-- Amalgam in Pawl.ZoneTriggerSpec.
zoneFunctionedFrom :: Map.Map AbilityName.AbilityName (TriggeredAbility.TriggeredAbility Card.Card (GrantedAbility.GrantedAbility Card.Card)) -> ActivatedAbility.ActivatedAbility Card.Card (GrantedAbility.GrantedAbility Card.Card) -> Maybe Zone.Zone
zoneFunctionedFrom delayed ability =
  case Cost.zoneFunctionedFrom (ActivatedAbility.cost ability) of
    Just zone -> Just zone
    Nothing ->
      Maybe.listToMaybe
        (Maybe.mapMaybe (EffectZone.zoneFunctionedFrom delayed) (Modal.allEffects (ActivatedAbility.modal ability)))

-- CR 113.6m's "functions only in that zone", asked of one zone: does this
-- ability function from there? True for an ability that names no zone at all,
-- which CR 113.6's default puts on the battlefield -- so this is only ever asked
-- of a zone abilitiesForGiven has already decided is a zone abilities are read
-- from, and never of a library or a stack.
functionsIn :: Map.Map AbilityName.AbilityName (TriggeredAbility.TriggeredAbility Card.Card (GrantedAbility.GrantedAbility Card.Card)) -> Zone.Zone -> ActivatedAbility.ActivatedAbility Card.Card (GrantedAbility.GrantedAbility Card.Card) -> Bool
functionsIn delayed zone ability = case zoneFunctionedFrom delayed ability of
  Nothing -> zone == Zone.Battlefield
  Just named -> zone == named

-- CR 602.2's DEFAULT activator: an object's controller, or its owner if it has
-- no controller. Both halves of that parenthetical are live here, which is why
-- this cannot simply be Projection.controllerOf: a card in a hand or a graveyard
-- has no controller at all (CR 108.4), and CR 400.3 puts every card in either in
-- its owner's.
--
-- The command zone answers the owner as well, and its two rules say so
-- themselves rather than leaning on rule 602.2's fallback: CR 902.6 makes the
-- controller of a face-up vanguard card its owner, and CR 114.2 makes an emblem
-- both owned and controlled by the player it was created for. Rule 400.3 puts
-- every other card in that zone in its owner's, as it does a hand and a
-- graveyard. Pawl.Engine.Event's command-zone trigger walk takes the owner for
-- the same two rules, so the two cannot disagree about who a vanguard's
-- abilities belong to.
--
-- The default only. CR 602.2's own "unless the object specifically says
-- otherwise" is read one function down, in mayActivateGiven, so what this
-- answers stays a fact about the OBJECT and its zone rather than about one
-- ability's printed instructions.
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
    Zone.Command -> Just (Object.owner obj)
    _ -> Nothing

-- CR 602.2's whole permission conjunct: its default, and the "unless the object
-- specifically says otherwise" that CR 602.1b lets an ability write.
--
-- The AnyPlayer arm keeps the ZONE gate rather than dropping activatorOfGiven
-- altogether. That function answers Nothing for a zone abilities are not read
-- from at all -- abilitiesForGiven's silence there -- so keeping it means a
-- printed "any player" widens WHO and nothing else. `stillPlaying` is the other
-- half: CR 800.4 takes a departed player out of the game, and "any player" is
-- the players in it.
--
-- CR 602.1a is untouched by either arm: the cost is paid by whoever is
-- activating, which is `pid` at every conjunct of activatableGiven and at every
-- payment activateAbility makes.
mayActivateGiven :: [Projection.ControlGrant] -> PlayerId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Card (GrantedAbility.GrantedAbility Card.Card) -> GameState -> Bool
mayActivateGiven grants pid srcId ability gs = case ActivatedAbility.activator ability of
  Activator.Controller -> activatorOfGiven grants srcId gs == Just pid
  Activator.AnyPlayer -> Maybe.isJust (activatorOfGiven grants srcId gs) && elem pid (Game.stillPlaying gs)

-- The objects whose activated abilities `pid` may be offered, which is the
-- candidate half of CR 602.2 as mayActivateGiven above is the permission half:
-- what this player controls, plus their own hand, graveyard and command zone (CR
-- 400.3, and CR 902.6 / CR 114.2 for the two objects rule 113.6p names there),
-- plus CR 602.1b's exception -- a permanent someone else controls whose ability
-- says any player may activate it.
--
-- ONE reader for both. Action.legalActions and Pawl.ActivateSpec's differential
-- reference each used to build this list themselves, and the widening is exactly
-- the kind of second candidate list that then goes stale: a gate that admitted
-- the opponent's Glittering Lion would have offered it to nobody, because no
-- enumeration named the Lion.
--
-- The exception arm reads the BATTLEFIELD only. Every printing of the clause is
-- on a permanent (Scryfall o:"any player may activate" game:paper, 2026-09-02,
-- 41 cards), and another player's hand, graveyard or command zone offers nothing
-- any card asks for, so those three arms stay this player's own.
--
-- That is where the DIVISION OF LABOUR between this list and mayActivateGiven
-- lies, and it is not symmetric. On the battlefield the two agree by
-- construction: the `activator` filter drops exactly the objects
-- mayActivateGiven's Controller arm would refuse, which is why
-- Pawl.ActivateSpec's Withered Wretch negative goes red only when BOTH are
-- neutralized. Off it they do not: mayActivateGiven's AnyPlayer arm asks only
-- that the object HAVE an activator and that `pid` still be playing, and
-- activatorOfGiven answers the OWNER for a hand, a graveyard and the command
-- zone -- so an "any player may activate" ability printed on a card in someone
-- else's hand, graveyard or command zone would pass the permission and is kept
-- out by this enumeration's zone scoping alone. No printing writes one, per the
-- search above.
activationSources :: PlayerId -> GameState -> [ObjectId]
activationSources pid gs = activationSourcesGiven (Projection.controlGrants gs) Map.empty pid gs

activationSourcesGiven :: [Projection.ControlGrant] -> Map.Map ObjectId PC.ProjectedCharacteristics -> PlayerId -> GameState -> [ObjectId]
activationSourcesGiven grants pcs pid gs =
  let own = Projection.controlsGiven grants pid gs
      mine = Set.fromList own
      openToAnyone oid = any ((== Activator.AnyPlayer) . ActivatedAbility.activator) (abilitiesForGiven pcs oid gs)
      others = filter (\oid -> not (Set.member oid mine) && openToAnyone oid) (Set.toList (GameState.battlefield gs))
   in own <> others <> Game.zoneMembers Zone.Hand pid gs <> Game.zoneMembers Zone.Graveyard pid gs <> Game.zoneMembers Zone.Command pid gs

-- CR 606.3 (CR 306.5d says the same for planeswalkers): a loyalty ability may be
-- activated only with priority and an empty stack during a main phase of its
-- controller's turn, and only if no player has already activated a loyalty
-- ability of that permanent this turn.
--
-- Vacuously true for every ability that is not a loyalty ability, which is what
-- makes this a conjunct rather than an arm of
-- ActivationRestriction.restrictionsOk: CR 606.3 is a rule about what a COST
-- contains (CR 606.2), not a clause a card prints, so it is
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
loyaltyOk :: PlayerId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Card (GrantedAbility.GrantedAbility Card.Card) -> GameState -> Bool
loyaltyOk pid srcId ability gs =
  not (Cost.isLoyaltyCost (ActivatedAbility.cost ability))
    || (Turn.sorcerySpeedWindow pid gs && not (loyaltyActivatedThisTurn srcId gs))

loyaltyActivatedThisTurn :: ObjectId -> GameState -> Bool
loyaltyActivatedThisTurn srcId gs = elem (GameEvent.LoyaltyAbilityActivated srcId) (fmap LoggedEvent.event (GameState.events gs))

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
payableCost :: [Map.Map SlotName (Set.Set ObjectId)] -> Maybe KeywordFamily.KeywordFamily -> PlayerId -> ObjectId -> GameState -> Cost Keyword -> Bool
payableCost aimable = payableCostAt aimable 0

-- The same predicate on a board the caller already walked -- see
-- Cost.canPaySomeCompletionGiven.
payableCostGiven :: [Map.Map SlotName (Set.Set ObjectId)] -> [ObjectId] -> Map.Map ObjectId PC.ProjectedCharacteristics -> Maybe KeywordFamily.KeywordFamily -> PlayerId -> ObjectId -> GameState -> Cost Keyword -> Bool
payableCostGiven aimable sources pcs = payableCostAtGiven aimable sources pcs 0

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
payableCostAt :: [Map.Map SlotName (Set.Set ObjectId)] -> Natural -> Maybe KeywordFamily.KeywordFamily -> PlayerId -> ObjectId -> GameState -> Cost Keyword -> Bool
payableCostAt aimable x family pid srcId gs cost =
  aimingSomewhere (Cost.readsBoundSlot (Cost.substituteX x cost)) aimable family pid srcId gs (\slots adjustments -> Cost.canPaySomeCompletion slots (PaymentSubject.Activating srcId) ManaSpending.AsProduced pid srcId (Cost.totalManas adjustments) (Cost.plusComponents adjustments (Cost.substituteX x cost)) gs)

-- The same predicate on a board the caller already walked -- see
-- Cost.canPaySomeCompletionGiven.
payableCostAtGiven :: [Map.Map SlotName (Set.Set ObjectId)] -> [ObjectId] -> Map.Map ObjectId PC.ProjectedCharacteristics -> Natural -> Maybe KeywordFamily.KeywordFamily -> PlayerId -> ObjectId -> GameState -> Cost Keyword -> Bool
payableCostAtGiven aimable sources pcs x family pid srcId gs cost =
  aimingSomewhere (Cost.readsBoundSlot (Cost.substituteX x cost)) aimable family pid srcId gs (\slots adjustments -> Cost.canPaySomeCompletionGiven slots (PaymentSubject.Activating srcId) ManaSpending.AsProduced sources pcs pid srcId (Cost.totalManas adjustments) (Cost.plusComponents adjustments (Cost.substituteX x cost)) gs)

-- CR 601.2f's totalling asked where CR 601.2c's targets do not exist yet: the
-- predicate holds if SOME aiming this activation could still take leaves the
-- cost payable. Dwarven Mauler's "equip abilities you activate that target this
-- creature" is the reducer that makes the two answers differ.
--
-- The lookahead is the rules' own order rather than a shortcut past it: CR
-- 601.2e's legality check sits after 601.2c, so no rule measures an activation
-- cost without the targets in hand. Both gates that call this run earlier than
-- that -- one enumerating what to offer, one at CR 601.2b's position -- so
-- neither may refuse an activation that some legal choice of targets completes,
-- which is what CR 602.2 makes the test of a legal activation.
--
-- `aimable` is what CR 601.2c could still bind, one slot map per fillable mode,
-- and TWO searches read it because two different things read the targets. Which
-- one runs is `slotReading`: whether the COST's own criteria name a slot at all
-- (Cost.readsBoundSlot). A cost that names none answers the same under every
-- aiming, so the cheap search below is exact for it.
--
-- The COST search (slotReading) is over whole announcements -- Target.aimings --
-- because a criterion reading a slot is only answerable against one: a "creature
-- other than the target" cost measured with nothing bound admits the target
-- itself and offers an activation CR 601.2h then refuses. The empty aiming is
-- NOT among them unless a fillable mode really has no target slot, since a
-- player who must choose a target cannot choose none.
--
-- The ADJUSTMENT search is the old one, unchanged: only ReduceActivationCost
-- reads the targets (Pawl.Engine.PlayerEffect.activationCostAdjustmentsGiven),
-- and each candidate is tried ON ITS OWN rather than all at once, since handing
-- the whole set in would let two reducers wanting two different targets both
-- apply where no one choice satisfies both. One at a time is exact for an
-- ability with a single target slot -- which is every ability such a reducer
-- names in the pool -- and for more slots it is STRICTER than the rules rather
-- than weaker, since a real selection is a superset of the singleton, the
-- criterion is asked of ANY target, and a reduction only reduces.
--
-- The empty aiming comes first there: a player may always choose a target that
-- reduces nothing, and it is the only aiming an ability with no target slot has.
-- The union gather then guards the climb -- the gather is monotone in the target
-- set, so a union that adjusts nothing leaves every singleton adjusting nothing
-- too -- which keeps every board without a target-naming reducer at one gather
-- and one payability search, as before.
--
-- Not implemented: `slotReading` is read off the PRINTED cost, so a criterion
-- that arrives on a component CR 601.2f's adjustments add is not seen here and
-- its cost takes the cheap search (#2959). No cost adjustment in `data/cards/`
-- adds a component with a criterion naming a slot.
aimingSomewhere :: Bool -> [Map.Map SlotName (Set.Set ObjectId)] -> Maybe KeywordFamily.KeywordFamily -> PlayerId -> ObjectId -> GameState -> (Map.Map SlotName (Set.Set ObjectId) -> CostAdjustments.CostAdjustments -> Bool) -> Bool
aimingSomewhere slotReading aimable family pid srcId gs payable =
  -- CR 605.1a's kind is AbilityKind.NonManaAbility at all three sites in this
  -- module, and CR 605.3b is why: activatableGiven refuses a mana ability
  -- outright and the cost conjunct this gate serves sits after that refusal,
  -- while activateAbility below puts the ability on the stack, which no mana
  -- ability does. So neither Suppression Field's "unless they're mana abilities"
  -- nor Zirda's "that aren't mana abilities" ever turns an adjustment away on
  -- this path -- the mana window is where they do
  -- (Cost.manaActivationAdjustments).
  let gather aimedAt = Cost.activationAdjustments aimedAt family AbilityKind.NonManaAbility pid srcId gs
      candidates = Set.unions (concatMap Map.elems aimable)
      blind = gather Set.empty
   in if slotReading
        then any (any (\aiming -> payable aiming (gather (Set.unions (Map.elems aiming)))) . Target.aimings) aimable
        else
          payable Map.empty blind
            || (gather candidates /= blind && any (payable Map.empty . gather . Set.singleton) (Set.toList candidates))

-- The objects a chosen or offered target set names. CR 601.2c lets a player be a
-- target too, and Recipient.objectOf drops those: a reduction's criterion is
-- matched against an OBJECT's projection, so a player recipient is not something
-- it could answer about.
recipientObjects :: Set.Set Recipient.Recipient -> Set.Set ObjectId
recipientObjects = Set.fromList . Maybe.mapMaybe Recipient.objectOf . Set.toList

-- What CR 601.2c could still bind, for a gate that has to measure the cost
-- before it asks: one slot map per fillable mode, each slot holding every object
-- that mode could name for it. One mode at a time and kept apart, rather than
-- Modal.modesTargetSlots over the whole fillable set at once: two modes may
-- print the same slot name, and a union of the SLOT MAPS would drop one of them
-- -- and an announcement chooses one mode, so an aiming drawn across two is not
-- one a player could make.
--
-- Only the FILLABLE modes (CR 700.2a), which is the set activatableGiven's mode
-- conjunct measures: a slot belonging to a mode this board cannot choose is not
-- a target this activation could name.
candidateSlotsGiven :: Map.Map ObjectId PC.ProjectedCharacteristics -> [Projection.ControlGrant] -> Target.Pools -> PlayerId -> ObjectId -> Modal.Type.Modal Card.Card (GrantedAbility.GrantedAbility Card.Card) -> Set.Set ModeIndex.ModeIndex -> GameState -> [Map.Map SlotName (Set.Set ObjectId)]
candidateSlotsGiven pcs grants pools pid srcId modal fillable gs =
  let slotsOf mi = Modal.modesTargetSlots (Seq.singleton mi) modal
      -- CR 601.2b's announcement has NOT been made at this point, which is what
      -- `unannounced` says: a slot's CR 202.3 computed bound reading the X states
      -- no bound here rather than an unmeetable one, so the gate is measured
      -- against every recipient the announcement could still reach (Blighted
      -- Nightmare's graveyard slot). This call and activateAbility's PRE-X one
      -- agree, which they must: one is the offer gate and the other is what
      -- affordableX measures the cost against, and a gate and an announcement may
      -- not disagree about what a cost is.
      setsOf slots = Target.legalSetsGiven pcs grants pools (Just pid) True Map.empty srcId slots gs
   in fmap (fmap recipientObjects . setsOf . slotsOf) (Set.toList fillable)

-- CR 601.2b via 602.2b: the greatest X this player could actually pay for, which
-- is what Prompt.ChooseX carries. The climb itself is Cost.greatestPayableX,
-- shared with Cast.affordableX; only the predicate differs, and only by WHICH
-- adjustments CR 601.2f's totalling reads. Advisory, never a clamp -- see
-- Prompt.ChooseX.
--
-- `mCeiling` is CR 101.1's, evaluated off the ABILITY being activated rather than
-- off the face (Cost.ceilingOf over ActivatedAbility.maximumX) and passed straight
-- through: it bounds the search as well as the announcement, which is what makes
-- Blighted Nightmare's blight route terminate -- CostComponent.BlightX's demand
-- never grows (Cost.demandGrowsWithX), so the climb has no other ground to stop
-- on. Cast.affordableX takes the same argument off Face.maximumX.
affordableX :: Maybe Natural -> [Map.Map SlotName (Set.Set ObjectId)] -> Maybe KeywordFamily.KeywordFamily -> PlayerId -> ObjectId -> GameState -> Cost Keyword -> Natural
affordableX mCeiling aimable family pid srcId gs cost = Cost.greatestPayableX mCeiling (\x -> payableCostAt aimable x family pid srcId gs cost) cost

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
-- enumeration (#1073). Build `sources` with Cost.supplyManaSourcesGiven, which
-- is the one place that pairs the sweep with the capacity this gate reads --
-- NOT Cost.activationManaSourcesGiven, which is CR 605.3a's wider offer.
--
-- Threading them buys the SHAPE of the loop: those wrappers hoist per CALL, and
-- the caller is a loop over the battlefield, so an ability that reached them
-- cost a whole-board sweep per permanent. Not implemented: nothing asserts
-- that line -- an allocation ceiling held it until measuring bytes was judged
-- too compiler-specific to keep (gap #578).
--
-- `activatable` keeps Map.empty deliberately. It has no engine caller -- only
-- tests -- so the slower per-object Projection.projectGiven fallback costs
-- nothing, and it makes the plain path a genuinely independent computation a
-- differential test could hold the threaded one against -- its `sources` is
-- built off that same Map.empty for the same reason.
activatable :: PlayerId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Card (GrantedAbility.GrantedAbility Card.Card) -> GameState -> Bool
activatable pid srcId ability gs =
  let grants = Projection.controlGrants gs
   in activatableGiven grants Map.empty (Target.poolsGiven Map.empty gs) (Cost.supplyManaSourcesGiven grants Map.empty pid gs) pid srcId ability gs

activatableGiven :: [Projection.ControlGrant] -> Map.Map ObjectId PC.ProjectedCharacteristics -> Target.Pools -> [ObjectId] -> PlayerId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Card (GrantedAbility.GrantedAbility Card.Card) -> GameState -> Bool
activatableGiven grants pcs pools sources pid srcId ability gs =
  let modal = ActivatedAbility.modal ability
      fillable = Target.fillableModesGiven pcs grants pools (Just pid) Map.empty srcId Map.empty modal gs
      -- CR 601.2c's targets do not exist at an offer, so the cost conjunct is
      -- handed the ones this activation could still name -- see
      -- aimingSomewhere. Shared with the mode conjunct's own `fillable` rather
      -- than taken twice: they are the same question (CR 700.2a).
      aimable = candidateSlotsGiven pcs grants pools pid srcId modal fillable gs
   in mayActivateGiven grants pid srcId ability gs
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
        -- same conjunct for CR 605.3a's windows, exactly as sickness below and the
        -- printed rider two lines down are asked in both places.
        && not (Detain.detained srcId gs)
        && sicknessOkGiven pcs pid srcId ability gs
        && ActivationRestriction.restrictionsOk pid srcId (ActivatedAbility.restrictions ability) gs
        && loyaltyOk pid srcId ability gs
        && Modal.selectionPossible fillable (Modal.Type.selection modal)
        && payableCostGiven aimable sources pcs (familyGrantingGiven pcs srcId gs ability) pid srcId gs (ActivatedAbility.cost ability)

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
-- Reaches exactly the hand today: abilitiesFor serves the battlefield, the hand,
-- the graveyard and the command zone and answers [] elsewhere -- and of those
-- four only a hand is hidden (CR 400.2 names the graveyard and the command zone
-- among the public zones), so the library offers
-- nothing to activate, and mana abilities never reach this function (CR 605.3b
-- keeps them off the stack).
--
-- CR 701.20a's duration -- revealed until the ability leaves the stack -- is not
-- modeled, and is vacuous for every card in `data/cards/`: every ability a hand
-- offers there discards the card as a cost, whether rule 702 minted that cost
-- (CR 702.29a's cycling, CR 702.77a's reinforce) or the card authored it (Faerie
-- Macabre, CR 113.6j), so the card is in a public
-- graveyard a moment later. A forecast ability (CR 702.57a) is the shape that
-- would make the duration observable; none is in the pool (#1408).
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
activateAbility :: PlayerId -> ObjectId -> ActivatedAbility.ActivatedAbility Card.Card (GrantedAbility.GrantedAbility Card.Card) -> Game ()
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
            Object.source =
              Source.OfAbility
                ActivatedAbilitySource.MkActivatedAbilitySource
                  { ActivatedAbilitySource.source = srcId,
                    ActivatedAbilitySource.ability = ability
                  },
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
            Object.kicked = Map.empty,
            Object.bestowed = False,
            Object.phyrexianLifePaid = 0,
            Object.manaSpent = Mana.MkMana [],
            Object.announcedX = Nothing,
            Object.castFrom = Nothing,
            Object.detainedUntil = Set.empty,
            Object.goadedBy = Set.empty,
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
      -- CR 601.2f's reductions can name the KIND of ability (Fluctuator's
      -- "cycling abilities"), so every totalling below is asked with this
      -- ability's provenance -- the two gates and the payment alike, or a
      -- reduction the gate withheld could still be applied when the cost is
      -- paid. Read off `gs`, the pre-stack board, because the ability's source is
      -- still in the zone whose keywords minted it.
      family = familyGranting srcId gs ability
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
          -- CR 601.2c's slots and their legal recipients, taken HERE rather than
          -- below where they are answered: the gate one step down has to measure
          -- a cost the targets can still change, so it is handed what they could
          -- be (aimingSomewhere). Read off `gs`, the same pre-stack board
          -- Target.chooseTargets is offered from, so one binding serves both.
          slots = Modal.modesTargetSlots chosenModes (ActivatedAbility.modal ability)
          -- The PRE-X map, and it says so: `unannounced` is True, so a slot's CR
          -- 202.3 computed bound reading the X states no bound rather than an
          -- unmeetable one, and the lookahead measures the cost against every
          -- recipient the announcement could still reach. candidateSlotsGiven's
          -- call is the same one, for the reason its own note gives.
          --
          -- NO GAMEPLAY OBSERVER today, and the flag is kept anyway because CR
          -- 601.2b has not happened yet where it is read: this map is consulted
          -- only by a cost that READS A BOUND SLOT (aimingSomewhere's slotReading),
          -- and no ability in `data/cards/` has both such a cost and a slot with a
          -- computed bound -- the whole suite stays green with this True flipped to
          -- False.
          --
          -- The map CR 601.2c is ANSWERED from is the second one below, taken once
          -- the X exists.
          unannouncedSets = Target.legalSets (Just pid) True Map.empty srcId slots gs
          aimableUnannounced = [fmap recipientObjects unannouncedSets]
          -- CR 101.1: the ceiling this ABILITY's own words put on the value about
          -- to be announced -- Blighted Nightmare's "X can't be greater than the
          -- greatest toughness among creatures you control". Read HERE and once,
          -- the timing CR 601.2b fixes through 602.2b, so a creature that leaves in
          -- response cannot shrink it.
          --
          -- Off the SOURCE permanent (`srcId`) rather than the ability object: CR
          -- 113.7 makes that permanent the ability's source, and it is the object
          -- every other quantity on this road is evaluated against. It is still in
          -- its zone here, the activation cost being unpaid until below.
          mCeiling = Cost.ceilingOf pid srcId (ActivatedAbility.maximumX ability) gs
      mAmount <-
        if Cost.hasVariable printedCost
          then fmap Just (Game.choose (Prompt.ChooseX decider pid abilId (affordableX mCeiling aimableUnannounced family pid srcId gs printedCost)))
          else pure Nothing
      let announcedAtX = maybe printedCost (\x -> Cost.substituteX x printedCost) mAmount
          -- CR 101.1, and CR 101.2 for its direction, exactly as Cast.castProposed
          -- reads a face's: the ability's printed sentence overrides the rule that
          -- would otherwise leave X free, and a "can't" beats the permission.
          -- REJECT rather than clamp, which is this whole step's posture.
          overCeiling = case (mCeiling, mAmount) of
            (Just c, Just x) -> x > c
            _ -> False
          -- CR 601.2c's slots and their legal recipients as the announcement
          -- ACTUALLY made can reach them, seeded with CR 601.2b's X: a slot's own
          -- CR 202.3 computed bound reads it (Blighted Nightmare's "creature card
          -- with mana value X or less"), and `unannounced` is False because the
          -- value exists by now. Cast.castProposed's shape exactly, and
          -- Binding.fromChoices is the same writer CR 601.2i stamps the ability
          -- object with below, so the offer and the record cannot spell one X two
          -- ways.
          --
          -- The SAME seed reaches Target.selectionLegal's joint check below, so a
          -- slot offered against the announced X is not re-judged against no X.
          --
          -- `unannounced` is False because the announcement is MADE; it changes
          -- nothing while the seed carries the value, which is what makes flipping
          -- it here leave the suite green (Filter.boundUnannounced answers only
          -- where the seed cannot supply the number).
          seed = Binding.fromChoices Map.empty mAmount Seq.empty
          sets = Target.legalSets (Just pid) False seed srcId slots gs
          aimable = [fmap recipientObjects sets]
      -- CR 602.2: an activation a player cannot comply with is illegal, and the
      -- game returns to the moment before it started. The X just named is where
      -- that can first become true: `activatable` measured the cost at CR
      -- 601.2b's X=0 floor, the only value it can know before an announcement
      -- exists. Both gates look ahead to CR 601.2c's candidate targets the same
      -- way (aimingSomewhere); what the player actually aims at is charged
      -- below, and a choice that reduces nothing loses the ability at the
      -- payment rather than here.
      --
      -- Measured against the POST-X map, which the earlier gates could not be:
      -- once the value is named the candidate set is exact, so a cost reading a
      -- bound slot is priced against what CR 601.2c will actually offer rather
      -- than against the wider pre-announcement lookahead.
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
      if overCeiling || not (payableCost aimable family pid srcId gs announcedAtX)
        then State.put before -- reject: the whole activation is a no-op
        else do
          -- CR 118.13a's announcement, which names an activated ability's
          -- activation cost, happens here at 601.2b's position and not when the
          -- cost is paid. Moltensteel Dragon exercises it; rule 118.13b's cost
          -- paid during a resolution announces at its own site
          -- (Pawl.Engine.Resolve.payGatePaidBy), and rule 118.13c's special
          -- action at each of its own (Pawl.Engine.FaceDown.turnFaceUp and
          -- its five siblings).
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
          -- TARGET-BLIND, unlike the gate above, and deliberately: that gate
          -- asks whether SOME aiming pays and may look ahead, while this is a
          -- CHOICE the player makes against one definite cost, and CR 601.2c's
          -- targets are not announced until below. What that costs is nothing
          -- here -- CR 118.13a's announcement is a choice of halves, every
          -- activation-cost reducer in the pool reduces by GENERIC mana, and a
          -- target-aware reduction therefore cannot change which nonhybrid
          -- equivalent or Phyrexian half a player would announce. The reductions
          -- themselves are gathered again below, once the targets exist.
          let gathered = Cost.activationAdjustments Set.empty family AbilityKind.NonManaAbility pid srcId gs
          -- The Phyrexian life record is DISCARDED here: CR 702.150a reads what
          -- the player who CAST a spell announced, and no rule asks the same of
          -- an activation cost.
          (announcedCost, _) <- Cost.announce (PaymentSubject.Activating srcId) ManaSpending.AsProduced pid srcId (Cost.totalManas gathered) (Cost.plusComponents gathered announcedAtX)
          chosen <- Target.chooseTargets decider pid abilId (Maybe.fromMaybe 0 mAmount) slots sets
          if not (Target.selectionLegal (Just pid) seed srcId (Maybe.fromMaybe 0 mAmount) slots sets chosen gs)
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
              State.modify' (\g -> g {GameState.objects = Map.adjust (\o -> o {Object.bindings = Binding.setThisAbility abilId (Binding.setYou pid (Binding.setTriggerSource srcId (Binding.fromChoices chosen mAmount chosenModes)))}) abilId (GameState.objects g)})
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
              -- CR 601.2c's announced targets, gathered a SECOND time now that
              -- they exist: Dwarven Mauler's "equip abilities you activate that
              -- target this creature" is a reduction the pre-target gather above
              -- cannot see, and CR 601.2f's position after 601.2c is what makes
              -- reading them here the rule's own order rather than a shortcut.
              -- Only the REDUCTIONS can differ between the two records --
              -- ReduceActivationCost is the one arm carrying a target criterion
              -- -- so the increases and the CR 601.2f components the announcement
              -- above measured are the same ones charged below.
              let aimedAt = Set.unions (fmap recipientObjects (Map.elems chosen))
                  targeted = Cost.activationAdjustments aimedAt family AbilityKind.NonManaAbility pid srcId gs
              adjustments <- Cost.announceReductions pid srcId gs announcedCost targeted
              let paidCost = Cost.totalWith adjustments announcedCost
              -- CR 601.2g/h via Pawl.Engine.Cost.pay: the mana window, then the
              -- components. The gates above prove SOME sequence of choices pays for
              -- this ability -- but Unpaid is reachable all the same, because the
              -- mana window then asks the player to make those choices and a
              -- mis-tapped colour is a choice the engine must honour (Cost.payMana).
              -- Reject-not-repair restores the whole activation, including the
              -- ability object this function put on the stack.
              payment <- Cost.pay Resolve.performManaAbility PaymentMoment.OutsideResolution (PaymentSubject.Activating srcId) (Just abilId) ManaSpending.AsProduced pid srcId paidCost
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
                -- CR 733.1's last sentence, Cost.keepingLibraryActions' reason:
                -- a mana ability tapped in the window this payment opened may
                -- have shuffled or revealed, and this reject-not-repair
                -- restore must not undo that too.
                Payment.Unpaid -> Cost.restoreKeepingLibraryActions before
