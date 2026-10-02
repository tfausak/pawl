{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: CR 702.122 crew -- Pawl.Types.Keyword's Crew arm, the ability
-- Pawl.Engine.Keyword.crew mints from it and the route
-- Pawl.Engine.Projection.abilitiesGiven takes to offer it; CR 702.122a's cost,
-- Pawl.Types.CostComponent's TapForTotalPower and the two arms
-- Pawl.Engine.Cost gives it; and CR 208.3 / CR 301.7a-b, the gate
-- Pawl.Engine.Projection.noncreaturePT puts on a noncreature permanent's printed
-- power and toughness.
--
-- Gameplay-level throughout. Consulate Dreadnought is the fixture: a {1} Artifact
-- -- Vehicle, 7/11, whose ENTIRE printed text is "Crew 6", so nothing else it
-- prints can make a case pass. Its crew 6 is high enough that the threshold is
-- reached by a SET rather than by one creature, which is what the any-number
-- choice exists for.
--
-- The arithmetic is deliberately non-degenerate. Hill Giant is 3/3 and
-- Blind-Spot Giant is 4/3, so the crewing pair totals 7 -- which is not 6 (the
-- threshold), not 2 (how many creatures were tapped), and not either creature's
-- own power. Only one route through the numbers reaches the answer, so a case
-- cannot pass by summing the wrong thing.
--
-- THREE SEATS, not two. CR 702.122a's "you control" is a real narrowing, and on a
-- two-seat board "an opponent" and "the other player" coincide -- so the case that
-- proves an opponent's creatures cannot crew gives one power-4 creature to bob and
-- one power-3 creature to carol, a pair that would pay the cost if control were
-- not being read.
--
-- CR 702.122e has its own fixture, Mobilizer Mech: rule 702.122a's plain crew
-- ability reaches nothing that reads the crewing, so data/scenarios/crew adds a
-- second Vehicle rather than another case on this one.
--
-- CR 702.122e's RIDER has a fixture of its own again, Mighty Servant of Leuk-o:
-- the rider is an intervening "if" that counts the creatures which paid that
-- activation's cost, and neither Vehicle above prints one, so crewedByRiderSpec
-- below adds a third.
--
-- CR 702.122b has its own fixture too, Gearshift Ace: rule 702.122b is asked of
-- the CREWER, which neither Vehicle above can be, so data/scenarios/crew adds a
-- creature that reads the relation from that side.
--
-- CR 702.122d has its own fixture as well, Revoke Privileges: the prohibition is
-- another permanent's static ability, so cantCrewSpec below adds an Aura rather
-- than another case on the Dreadnought.
--
-- CR 702.122c has its own fixture too, Subterranean Schooner: the relation is
-- read LATER IN THE TURN, by a trigger of the declare attackers step rather than
-- by the crew ability, so data/scenarios/crew runs that whole step instead of
-- watching one resolution.
module Pawl.CrewSpec where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Engine.Action as Action
import qualified Pawl.Engine.Activatable as Activatable
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.TriggerCondition as TriggerCondition
import qualified Pawl.Types.TriggeredAbility as TriggeredAbility
import qualified Pawl.Types.Zone as Zone

-- The crew ability, taken from the PROJECTION rather than from the card's face.
-- That is the wiring under test as much as anything else: rule 702.122a's ability
-- is minted by Pawl.Engine.Keyword and appended by
-- Pawl.Engine.Projection.abilitiesGiven, so a card file that declares no
-- activatedAbilities still offers one.
crewAbility :: ObjectId.ObjectId -> GameState.GameState -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
crewAbility oid gs = case Projection.abilitiesOf oid gs of
  ability : _ -> Just ability
  [] -> Nothing

-- Alice's board: one Consulate Dreadnought and one creature per printing in
-- `crewers`, all Settled and untapped, with alice holding priority in her
-- precombat main phase. Three seats, for the reason the module header gives.
board :: Printing.Printing -> [Printing.Printing] -> (ObjectId.ObjectId, [ObjectId.ObjectId], GameState.GameState)
board dreadnought crewers =
  let (vehicleId, gs0) = S.addPermanent dreadnought S.alice S.threePlayerGame
      add (ids, g) p = let (oid, g1) = S.addPermanent p S.alice g in (ids <> [oid], g1)
      (crewIds, gs1) = List.foldl' add ([], gs0) crewers
   in (vehicleId, crewIds, gs1 {GameState.priority = Just S.alice})

-- Activate the Vehicle's crew ability and resolve it. Returns the state
-- unchanged if the permanent offers no ability at all, so a case that expects
-- crewing to have happened asserts on the board and not on this returning Just.
crewWith :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
crewWith answer vehicleId gs = case crewAbility vehicleId gs of
  Nothing -> gs
  Just ability ->
    let activated = S.runPure answer gs (Activate.activateAbility S.alice vehicleId ability)
     in S.runPure answer activated Stack.resolveTop

-- Is this permanent a creature right now, after the layer fold?
isCreature :: ObjectId.ObjectId -> GameState.GameState -> Bool
isCreature oid gs = Set.member CardType.Creature (Projection.cardTypesOf oid gs)

-- Can alice activate the Vehicle's crew ability on this board?
crewable :: ObjectId.ObjectId -> GameState.GameState -> Bool
crewable vehicleId gs = case crewAbility vehicleId gs of
  Nothing -> False
  Just ability -> Activatable.activatable S.alice vehicleId ability gs

-- Tap one permanent in place, without paying anything for it.
tap :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
tap oid gs =
  gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) oid (GameState.objects gs)}

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Crew" $ do
  printedPowerSpec s registry
  crewCostSpec s registry
  crewedVehicleSpec s registry
  crewedByRiderSpec s registry
  cantCrewSpec s registry

-- CR 208.3 and CR 301.7a: the printed numbers are on the card and are not the
-- permanent's characteristics until it is a creature.
printedPowerSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
printedPowerSpec s registry = Spec.describe s "PrintedPower" $ do
  Spec.it s "CR 208.3 an uncrewed Vehicle on the battlefield has no power or toughness" $ do
    dreadnought <- S.printingOf s registry "Consulate Dreadnought"
    hillGiant <- S.printingOf s registry "Hill Giant"
    let (vehicleId, crewIds, gs) = board dreadnought [hillGiant]
    Spec.assertEqWith
      s
      "no power or toughness"
      (Projection.powerOf vehicleId gs, Projection.toughnessOf vehicleId gs)
      (Nothing, Nothing)
    -- The positive control: the SAME read point answers for an ordinary creature
    -- on the SAME board, so a Nothing above is CR 208.3 and not a broken fixture.
    case crewIds of
      giantId : _ ->
        Spec.assertEqWith
          s
          "Hill Giant still reports 3/3"
          (Projection.powerOf giantId gs, Projection.toughnessOf giantId gs)
          (Just 3, Just 3)
      [] -> Spec.assertFailure s "fixture should have a crewer"
  -- CR 208.3's SECOND sentence: off the battlefield the printed numbers stand.
  Spec.it s "CR 208.3 a Vehicle in a hand keeps its printed power and toughness" $ do
    dreadnought <- S.printingOf s registry "Consulate Dreadnought"
    let (gs, cardId) = S.handOne dreadnought (Setup.emptyGame S.bothPlayers)
    Spec.assertEqWith s "in a hand" (fmap Object.zone (Game.lookupObject cardId gs)) (Just Zone.Hand)
    Spec.assertEqWith
      s
      "7/11 in a hand"
      (Projection.powerOf cardId gs, Projection.toughnessOf cardId gs)
      (Just 7, Just 11)

-- CR 702.122a's cost: which creatures are candidates, and when the threshold is
-- out of reach.
crewCostSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
crewCostSpec s registry = Spec.describe s "CrewCost" $ do
  Spec.it s "CR 702.122a crew 6 is out of reach at total power 4 and payable at 7" $ do
    dreadnought <- S.printingOf s registry "Consulate Dreadnought"
    hillGiant <- S.printingOf s registry "Hill Giant"
    blindSpot <- S.printingOf s registry "Blind-Spot Giant"
    let (aloneId, _, alone) = board dreadnought [blindSpot]
        (pairId, _, pair) = board dreadnought [blindSpot, hillGiant]
    Spec.assertBool s (not (crewable aloneId alone)) "power 4 alone cannot crew 6"
    Spec.assertBool s (crewable pairId pair) "power 4 and power 3 together can"
  Spec.it s "CR 702.122a a tapped creature is not a candidate" $ do
    dreadnought <- S.printingOf s registry "Consulate Dreadnought"
    hillGiant <- S.printingOf s registry "Hill Giant"
    blindSpot <- S.printingOf s registry "Blind-Spot Giant"
    let (vehicleId, crewIds, gs) = board dreadnought [blindSpot, hillGiant]
    case crewIds of
      -- Tapping the power-4 creature leaves power 3 untapped, which is short of
      -- 6 -- and short by a different amount than the case above, so the two
      -- cannot both be passing on one arithmetic accident.
      bigId : _ -> Spec.assertBool s (not (crewable vehicleId (tap bigId gs))) "3 untapped power cannot crew 6"
      [] -> Spec.assertFailure s "fixture should have two crewers"
  -- CR 702.122a's "you control". Three seats, so the creatures that would pay the
  -- cost are spread across two OTHER players and neither is "the other player".
  Spec.it s "CR 702.122a creatures an opponent controls cannot crew" $ do
    dreadnought <- S.printingOf s registry "Consulate Dreadnought"
    hillGiant <- S.printingOf s registry "Hill Giant"
    blindSpot <- S.printingOf s registry "Blind-Spot Giant"
    let (vehicleId, gs0) = S.addPermanent dreadnought S.alice S.threePlayerGame
        (_, gs1) = S.addPermanent blindSpot S.bob gs0
        (_, gs2) = S.addPermanent hillGiant S.carol gs1
        gs = gs2 {GameState.priority = Just S.alice}
    Spec.assertBool s (not (crewable vehicleId gs)) "bob's 4 and carol's 3 are not alice's to tap"

-- CR 702.122a's effect, and what CR 301.7b and CR 302.6 then say about the
-- permanent it lands on.
crewedVehicleSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
crewedVehicleSpec s registry = Spec.describe s "CrewedVehicle" $ do
  Spec.it s "CR 514.2 the Vehicle stops being a creature at cleanup" $ do
    dreadnought <- S.printingOf s registry "Consulate Dreadnought"
    hillGiant <- S.printingOf s registry "Hill Giant"
    blindSpot <- S.printingOf s registry "Blind-Spot Giant"
    let (vehicleId, _, gs) = board dreadnought [blindSpot, hillGiant]
        crewed = crewWith S.identityAnswer vehicleId gs
        afterCleanup = S.runPure S.identityAnswer crewed (Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup))
    Spec.assertBool s (isCreature vehicleId crewed) "a creature during the turn"
    Spec.assertBool s (not (isCreature vehicleId afterCleanup)) "and not one afterwards"
    Spec.assertEqWith
      s
      "CR 208.3 takes the printed numbers back with it"
      (Projection.powerOf vehicleId afterCleanup, Projection.toughnessOf vehicleId afterCleanup)
      (Nothing, Nothing)

-- alice's board for CR 702.122e and CR 702.122b: one permanent per printing, the
-- first being the card that READS the crewing -- Mobilizer Mech for rule 702.122e
-- and Gearshift Ace for rule 702.122b -- then one per printing in `vehicles` and
-- one per printing in `crewers`, all Settled and untapped, with
-- alice holding priority in her precombat main phase. Three seats, for the reason
-- the module header gives.
crewReaderBoard :: Printing.Printing -> [Printing.Printing] -> [Printing.Printing] -> (ObjectId.ObjectId, [ObjectId.ObjectId], [ObjectId.ObjectId], GameState.GameState)
crewReaderBoard reader vehicles crewers =
  let (readerId, gs0) = S.addPermanent reader S.alice S.threePlayerGame
      add (ids, g) p = let (oid, g1) = S.addPermanent p S.alice g in (ids <> [oid], g1)
      (vehicleIds, gs1) = List.foldl' add ([], gs0) vehicles
      (crewIds, gs2) = List.foldl' add ([], gs1) crewers
   in (readerId, vehicleIds, crewIds, gs2 {GameState.priority = Just S.alice})

-- Crew `vehicleId`, then let CR 603.3 gather what that resolution triggered onto
-- the stack (Engine.settleForPriority) and resolve it. Both states are returned:
-- the one with the trigger waiting, and the one after it resolved.
crewAndSettle :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> (GameState.GameState, GameState.GameState)
crewAndSettle answer vehicleId gs =
  let crewed = crewWith answer vehicleId gs
      onStack = S.runPure answer crewed Engine.settleForPriority
   in (onStack, S.runPure answer onStack Stack.resolveTop)

-- CR 702.122e's rider: an intervening "if" that refers to the crewing creatures
-- means only the ones that paid the cost of the activation that caused the
-- trigger. Mighty Servant of Leuk-o is the fixture -- {3} Artifact -- Vehicle
-- 6/6, crew 4, "whenever this Vehicle becomes crewed for the FIRST TIME EACH
-- TURN, if it was crewed by EXACTLY TWO creatures, it gains 'whenever this
-- creature deals combat damage to a player, draw two cards' until end of turn".
--
-- Crew 4 is what makes the pair of boards differ in ONE thing. Blind-Spot Giant
-- is 4/3, so it pays the cost alone; adding Hill Giant's 3 pays the same cost
-- with two creatures. So the positive and negative boards are the same board,
-- the same Vehicle and the same cost, tapping one creature or two.
--
-- The arithmetic is non-degenerate for the module header's reason: the rider's
-- threshold is 2 (a COUNT), the cost's is 4 (a total POWER), and the two boards
-- total 4 and 7. No single number reaches the answer twice, so a condition that
-- had counted power or summed the count would miss on both boards.
--
-- Four crewers, two of each printing, so the third case can crew twice in one
-- turn with a fresh pair each time and never reuse a tapped creature.
crewedByRiderSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
crewedByRiderSpec s registry = Spec.describe s "CrewedByRider" $ do
  Spec.it s "CR 702.122e crewed by exactly two creatures grants the ability" $ do
    servant <- S.printingOf s registry "Mighty Servant of Leuk-o"
    hillGiant <- S.printingOf s registry "Hill Giant"
    blindSpot <- S.printingOf s registry "Blind-Spot Giant"
    let (servantId, _, crewIds, gs) = crewReaderBoard servant [] [hillGiant, blindSpot]
    case crewIds of
      [giantId, blindId] -> do
        let (onStack, after) = crewAndSettle (crewingWith [giantId, blindId]) servantId gs
        Spec.assertBool s (drawsOnCombatDamage servantId after) "the Vehicle gained the combat-damage trigger"
        Spec.assertBool s (not (drawsOnCombatDamage servantId onStack)) "and had not gained it while the trigger waited"
        Spec.assertBool s (isCreature servantId after) "with the crewing itself having animated it"
      _ -> Spec.assertFailure s "fixture should have two crewers"

-- Does this permanent carry the triggered ability Mighty Servant of Leuk-o's
-- rider grants -- "whenever this creature deals combat damage to a player, draw
-- two cards"? Read off the PROJECTION, hasFirstStrike's shape one ability kind
-- over, so a grant that never reached layer 6 answers False.
drawsOnCombatDamage :: ObjectId.ObjectId -> GameState.GameState -> Bool
drawsOnCombatDamage oid gs =
  any
    ((\condition -> case condition of TriggerCondition.SelfDealsCombatDamageToPlayer _ -> True; _ -> False) . TriggeredAbility.condition)
    (Projection.triggeredAbilitiesOf oid gs)

-- Taps `tappers` to pay CR 702.122a's cost and answers nothing else: Gearshift
-- Ace's trigger targets nothing, so crewingAt's target answers would go unasked.
crewingWith :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
crewingWith tappers p = case p of
  Prompt.ChooseTapsForTotalPower {} -> Set.fromList tappers
  _ -> S.identityAnswer p

-- CR 702.122d: "can't crew Vehicles" -- an effect that forbids TAPPING a
-- creature to pay a crew cost, carried by Pawl.Types.CrewRestriction and
-- gathered by Pawl.Engine.CrewRestriction. Revoke Privileges {2}{W} is the
-- pool's printing; its other two clauses ("can't attack, block") are Pacifism's
-- and are proved where Pacifism's are.
--
-- THREE creatures on the positive board, one of them enchanted, because the
-- reading this has to be told apart from is "the Aura stops the crew" rather than
-- "the Aura stops THIS creature crewing": with the enchanted creature out, the
-- other two still reach crew 6, so the Vehicle is crewed and the case can assert
-- WHICH creatures paid.
--
-- The arithmetic is non-degenerate for the module header's reason. Blind-Spot
-- Giant is 4/3 and Hill Giant is 3/3, so 4+3 = 7 pays crew 6 while either alone
-- falls short -- and where three creatures are wanted the enchanted one is a
-- SECOND Blind-Spot Giant, so the two power-4 creatures differ in the Aura and in
-- nothing else.
cantCrewSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
cantCrewSpec s registry = Spec.describe s "CantCrew" $ do
  -- The gate (CR 118.3): with the only power-4 creature enchanted, 3 untapped
  -- power is left and crew 6 is out of reach. Both boards carry the same two
  -- creatures, so the refusal is the Aura's and not an arithmetic accident.
  Spec.it s "CR 702.122d an enchanted creature's power no longer reaches the threshold" $ do
    dreadnought <- S.printingOf s registry "Consulate Dreadnought"
    hillGiant <- S.printingOf s registry "Hill Giant"
    blindSpot <- S.printingOf s registry "Blind-Spot Giant"
    revoke <- S.printingOf s registry "Revoke Privileges"
    let (vehicleId, crewIds, gs) = board dreadnought [blindSpot, hillGiant]
    case crewIds of
      [bigId, _] -> do
        let (auraId, unattached) = S.addPermanent revoke S.alice gs
            enchanted = S.attach auraId bigId unattached
        Spec.assertBool s (crewable vehicleId gs) "4 and 3 together pay crew 6"
        Spec.assertBool s (crewable vehicleId unattached) "and an unattached Revoke Privileges changes nothing"
        Spec.assertBool s (not (crewable vehicleId enchanted)) "with the 4 enchanted, 3 is short of 6"
      _ -> Spec.assertFailure s "fixture should have exactly two crewers"
  -- CR 702.122d names the CREW cost and nothing else, so the same cost component
  -- printed outside a crew ability is untouched. Synthetic Crewed Battery's "{T},
  -- tap another untapped creature you control, tap any number of other untapped
  -- creatures you control with total power 2 or greater: add {C}" is the pool's
  -- only such printing, and the enchanted Goblin Piker is the only creature on
  -- the board whose power can reach that 2 -- Ornithopter's is 0, and it pays the
  -- exact-count component instead. A prohibition that reached every
  -- TapForTotalPower would leave the {1} spell uncastable.
  Spec.it s "CR 702.122d the same component outside a crew ability is untouched" $ do
    battery <- S.printingOf s registry "Synthetic Crewed Battery"
    ornithopter <- S.printingOf s registry "Ornithopter"
    piker <- S.printingOf s registry "Goblin Piker"
    drum <- S.printingOf s registry "Springleaf Drum"
    revoke <- S.printingOf s registry "Revoke Privileges"
    let (_, gs0) = S.addPermanent battery S.alice (Setup.emptyGame S.bothPlayers)
        (_, gs1) = S.addPermanent ornithopter S.alice gs0
        (pikerId, gs2) = S.addPermanent piker S.alice gs1
        (auraId, gs3) = S.addPermanent revoke S.alice gs2
        enchanted = S.attach auraId pikerId gs3
    Spec.assertBool s (offersCast drum gs2) "the Battery pays for a {1} spell off the Piker's power 2"
    Spec.assertBool s (offersCast drum enchanted) "and still does with the Piker enchanted"
    Spec.assertBool s (not (offersCast drum gs1)) "where Ornithopter's power 0 alone cannot"

-- Would alice be offered a cast of this printing out of her hand? Pawl.ManaSpec's
-- shape, duplicated rather than hoisted (docs/adding-a-module.md).
offersCast :: Printing.Printing -> GameState.GameState -> Bool
offersCast printing gs =
  let (withSpell, oid) = S.handOne printing gs
   in any (S.isCastOf oid) (Action.legalActions S.alice withSpell)
