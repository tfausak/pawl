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
-- Blind-Spot Giant is 4/1, so the crewing pair totals 7 -- which is not 6 (the
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
-- ability reaches nothing that reads the crewing, so becomesCrewedSpec below adds
-- a second Vehicle rather than another case on this one.
--
-- CR 702.122b has its own fixture too, Gearshift Ace: rule 702.122b is asked of
-- the CREWER, which neither Vehicle above can be, so crewsVehicleSpec below adds
-- a creature that reads the relation from that side.
--
-- Not covered here, because no card in the pool reaches them: rule 702.122c's
-- relation read LATER IN THE TURN, which Subterranean Schooner's "target creature
-- that crewed it this turn" wants and which outlives the resolution these
-- fixtures watch, and CR 702.122d's "can't crew Vehicles".
module Pawl.CrewSpec where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Combat as Combat
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Setup as Setup
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.BeginningStep as BeginningStep
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardType as CardType
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Keyword as Keyword
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.Sickness as Sickness
import qualified Pawl.Types.TapState as TapState
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
      (crewIds, gs1) = foldl add ([], gs0) crewers
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

isArtifact :: ObjectId.ObjectId -> GameState.GameState -> Bool
isArtifact oid gs = Set.member CardType.Artifact (Projection.cardTypesOf oid gs)

tapStateOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe TapState.TapState
tapStateOf oid gs = fmap Object.tapped (Game.lookupObject oid gs)

-- Can alice activate the Vehicle's crew ability on this board?
crewable :: ObjectId.ObjectId -> GameState.GameState -> Bool
crewable vehicleId gs = case crewAbility vehicleId gs of
  Nothing -> False
  Just ability -> Activate.activatable S.alice vehicleId ability gs

-- Tap one permanent in place, without paying anything for it.
tap :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
tap oid gs =
  gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) oid (GameState.objects gs)}

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Crew" $ do
  printedPowerSpec s registry
  crewCostSpec s registry
  crewedVehicleSpec s registry
  becomesCrewedSpec s registry
  crewsVehicleSpec s registry

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
  -- CR 702.122a's "other". Once crewed, the Vehicle is an untapped 7/11 creature
  -- its controller controls -- every word of the criterion but that one -- so if
  -- "other" were dropped it could pay for its own second crew off its own power.
  Spec.it s "CR 702.122a a crewed Vehicle cannot crew itself" $ do
    dreadnought <- S.printingOf s registry "Consulate Dreadnought"
    hillGiant <- S.printingOf s registry "Hill Giant"
    blindSpot <- S.printingOf s registry "Blind-Spot Giant"
    let (vehicleId, _, gs) = board dreadnought [blindSpot, hillGiant]
        crewed = crewWith S.identityAnswer vehicleId gs
    Spec.assertBool s (isCreature vehicleId crewed) "it is a creature now"
    Spec.assertEqWith s "and untapped" (tapStateOf vehicleId crewed) (Just TapState.Untapped)
    Spec.assertEqWith s "with power 7" (Projection.powerOf vehicleId crewed) (Just 7)
    Spec.assertBool s (not (crewable vehicleId crewed)) "but it is not a candidate for its own crew cost"
  -- The engine never makes this choice: paying the cost asks, and the answer is
  -- what gets tapped. An interpreter that names only the power-3 creature falls
  -- short of 6, so the payment is Unpaid -- and CR 601.2h's all-or-nothing makes
  -- that a complete no-op, with nothing tapped and nothing on the stack.
  Spec.it s "CR 702.122a an answer short of the threshold pays nothing" $ do
    dreadnought <- S.printingOf s registry "Consulate Dreadnought"
    hillGiant <- S.printingOf s registry "Hill Giant"
    blindSpot <- S.printingOf s registry "Blind-Spot Giant"
    let (vehicleId, crewIds, gs) = board dreadnought [blindSpot, hillGiant]
    case crewIds of
      [_, smallId] ->
        let stingy :: Prompt.Prompt r -> r
            stingy p = case p of
              Prompt.ChooseTapsForTotalPower {} -> Set.singleton smallId
              _ -> S.identityAnswer p
            after = crewWith stingy vehicleId gs
         in do
              Spec.assertBool s (not (isCreature vehicleId after)) "the Vehicle did not become a creature"
              Spec.assertEqWith s "and the creature named was not tapped" (tapStateOf smallId after) (Just TapState.Untapped)
      _ -> Spec.assertFailure s "fixture should have exactly two crewers"

-- CR 702.122a's effect, and what CR 301.7b and CR 302.6 then say about the
-- permanent it lands on.
crewedVehicleSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
crewedVehicleSpec s registry = Spec.describe s "CrewedVehicle" $ do
  Spec.it s "CR 702.122a crewing taps the chosen creatures and CR 301.7b gives the Vehicle its printed P/T" $ do
    dreadnought <- S.printingOf s registry "Consulate Dreadnought"
    hillGiant <- S.printingOf s registry "Hill Giant"
    blindSpot <- S.printingOf s registry "Blind-Spot Giant"
    let (vehicleId, crewIds, gs) = board dreadnought [blindSpot, hillGiant]
        crewed = crewWith S.identityAnswer vehicleId gs
    Spec.assertBool s (isCreature vehicleId crewed) "artifact creature: creature"
    Spec.assertBool s (isArtifact vehicleId crewed) "artifact creature: artifact"
    Spec.assertEqWith
      s
      "7/11, its printed numbers"
      (Projection.powerOf vehicleId crewed, Projection.toughnessOf vehicleId crewed)
      (Just 7, Just 11)
    Spec.assertEqWith
      s
      "both crewers tapped"
      (fmap (`tapStateOf` crewed) crewIds)
      [Just TapState.Tapped, Just TapState.Tapped]
    -- The Vehicle itself is NOT tapped: rule 702.122a's cost taps the crew, and
    -- the tap symbol is nowhere in it.
    Spec.assertEqWith s "the Vehicle is not tapped" (tapStateOf vehicleId crewed) (Just TapState.Untapped)
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
  -- CR 302.5 with CR 208.3: attacking is what a Vehicle is crewed FOR, and the
  -- negative half is the point -- an uncrewed one is not a creature, so CR 508.1a
  -- never reaches it.
  Spec.it s "CR 508.1a an uncrewed Vehicle cannot attack and a crewed one can" $ do
    dreadnought <- S.printingOf s registry "Consulate Dreadnought"
    hillGiant <- S.printingOf s registry "Hill Giant"
    blindSpot <- S.printingOf s registry "Blind-Spot Giant"
    let (vehicleId, _, gs) = board dreadnought [blindSpot, hillGiant]
        crewed = crewWith S.identityAnswer vehicleId gs
    Spec.assertBool s (not (Combat.canAttack S.alice vehicleId gs)) "uncrewed: cannot attack"
    Spec.assertBool s (Combat.canAttack S.alice vehicleId crewed) "crewed: can attack"
    Spec.assertBool s (elem vehicleId (Combat.legalAttackers S.alice crewed)) "and is offered as an attacker"
  -- CR 302.6's second sentence reads the PERMANENT's history, not the creature's:
  -- a Vehicle that arrived this turn is a creature the moment it is crewed and
  -- still cannot attack, because it has not been controlled continuously since
  -- the turn began. The same board one untap step later can.
  Spec.it s "CR 302.6 a Vehicle that arrived this turn cannot attack even when crewed" $ do
    dreadnought <- S.printingOf s registry "Consulate Dreadnought"
    hillGiant <- S.printingOf s registry "Hill Giant"
    blindSpot <- S.printingOf s registry "Blind-Spot Giant"
    let (vehicleId, _, gs) = board dreadnought [blindSpot, hillGiant]
        arrived = gs {GameState.objects = Map.adjust (\o -> o {Object.sickness = Sickness.Sick}) vehicleId (GameState.objects gs)}
        crewed = crewWith S.identityAnswer vehicleId arrived
        untapped = S.runPure S.identityAnswer crewed (Engine.runTurnBasedActions (Phase.Beginning BeginningStep.Untap))
    Spec.assertBool s (isCreature vehicleId crewed) "crewed all the same"
    Spec.assertBool s (not (Combat.canAttack S.alice vehicleId crewed)) "but summoning sick"
    Spec.assertBool s (Combat.canAttack S.alice vehicleId untapped) "and able once the untap step has settled it"

-- CR 702.122e: "whenever this Vehicle becomes crewed" IS "whenever a crew ability
-- of this Vehicle resolves".
--
-- Mobilizer Mech {1}{U} Artifact -- Vehicle 3/4: "Flying / Whenever this Vehicle
-- becomes crewed, up to one other target Vehicle you control becomes an artifact
-- creature until end of turn. / Crew 3" (data/cards/mobilizer-mech.json; Oracle
-- text checked against api.scryfall.com, 2026-09-07).
--
-- THE TARGET IS ANOTHER VEHICLE, and that is what makes the assertion
-- discriminating: on a one-Vehicle board an implementation that animated the Mech
-- a second time would agree with one that fired the trigger, since the Mech is
-- already a creature off its own crew ability. Every case below asserts on a
-- Consulate Dreadnought's identity instead.
--
-- The tap set and the target are answered by a test-local interpreter rather than
-- by Pawl.Support's script harness, which has no vocabulary for rule 702.122a's
-- ChooseTapsForTotalPower. The target is FILTERED out of what the engine offered,
-- so a board that never offered it fails rather than being repaired.
becomesCrewedSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
becomesCrewedSpec s registry = Spec.describe s "BecomesCrewed" $ do
  Spec.it s "CR 702.122e crewing the Mech animates the other Vehicle its trigger targeted" $ do
    mech <- S.printingOf s registry "Mobilizer Mech"
    dreadnought <- S.printingOf s registry "Consulate Dreadnought"
    hillGiant <- S.printingOf s registry "Hill Giant"
    let (mechId, vehicleIds, crewIds, gs) = crewReaderBoard mech [dreadnought] [hillGiant]
    case (vehicleIds, crewIds) of
      ([dreadId], [giantId]) -> do
        let (onStack, after) = crewAndSettle (crewingAt [giantId] dreadId) mechId gs
        Spec.assertBool s (not (isCreature dreadId onStack)) "the Dreadnought is no creature while the trigger waits"
        Spec.assertBool s (isCreature dreadId after) "and is one once the trigger has resolved"
        Spec.assertBool s (isArtifact dreadId after) "an ARTIFACT creature, both card types"
        Spec.assertEqWith s "with the Dreadnought's own printed 7/11" (Projection.powerOf dreadId after, Projection.toughnessOf dreadId after) (Just 7, Just 11)
        Spec.assertEqWith s "off one trigger on the stack" (length (GameState.stack onStack)) 1
      _ -> Spec.assertFailure s "fixture should have one other Vehicle and one crewer"
  -- CR 702.122e is per RESOLUTION, not once a turn: two crew activations in one
  -- turn are two triggers, aimed at two different Vehicles so that the second
  -- cannot be read off the first.
  Spec.it s "CR 702.122e a second crew ability resolving in the same turn triggers again" $ do
    mech <- S.printingOf s registry "Mobilizer Mech"
    dreadnought <- S.printingOf s registry "Consulate Dreadnought"
    hillGiant <- S.printingOf s registry "Hill Giant"
    blindSpot <- S.printingOf s registry "Blind-Spot Giant"
    let (mechId, vehicleIds, crewIds, gs) = crewReaderBoard mech [dreadnought, dreadnought] [hillGiant, blindSpot]
    case (vehicleIds, crewIds) of
      ([firstId, secondId], [giantId, blindId]) -> do
        let (_, once) = crewAndSettle (crewingAt [giantId] firstId) mechId gs
            (_, twice) = crewAndSettle (crewingAt [blindId] secondId) mechId once
        Spec.assertBool s (isCreature firstId once) "the first crewing animated the first Dreadnought"
        Spec.assertBool s (not (isCreature secondId once)) "and not the second"
        Spec.assertBool s (isCreature secondId twice) "the second crewing animated the second Dreadnought"
        Spec.assertBool s (isCreature firstId twice) "with the first still animated"
        Spec.assertEqWith
          s
          "each crewing tapped its own creature"
          (tapStateOf giantId twice, tapStateOf blindId twice)
          (Just TapState.Tapped, Just TapState.Tapped)
      _ -> Spec.assertFailure s "fixture should have two other Vehicles and two crewers"
  -- The negative, one board away from the first case: the crew ability that
  -- resolves belongs to a DIFFERENT Vehicle, so rule 702.122e's "of [this
  -- Vehicle]" withholds the trigger. The Dreadnought's crew 6 needs both
  -- creatures where the Mech's crew 3 needed one, which is the cards' arithmetic
  -- and not a second variable: the assertion is about the third Vehicle, which
  -- neither crewing tapped and neither cost could reach.
  Spec.it s "CR 702.122e crewing a different Vehicle does not fire the Mech's trigger" $ do
    mech <- S.printingOf s registry "Mobilizer Mech"
    dreadnought <- S.printingOf s registry "Consulate Dreadnought"
    hillGiant <- S.printingOf s registry "Hill Giant"
    blindSpot <- S.printingOf s registry "Blind-Spot Giant"
    let (_, vehicleIds, crewIds, gs) = crewReaderBoard mech [dreadnought, dreadnought] [hillGiant, blindSpot]
    case (vehicleIds, crewIds) of
      ([crewedId, bystanderId], [giantId, blindId]) -> do
        let (onStack, after) = crewAndSettle (crewingAt [giantId, blindId] bystanderId) crewedId gs
        Spec.assertBool s (isCreature crewedId after) "the Dreadnought that was crewed is a creature"
        Spec.assertBool s (not (isCreature bystanderId after)) "and the other Vehicle was never animated"
        Spec.assertEqWith s "with nothing triggered onto the stack" (length (GameState.stack onStack)) 0
      _ -> Spec.assertFailure s "fixture should have two other Vehicles and two crewers"

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
      (vehicleIds, gs1) = foldl add ([], gs0) vehicles
      (crewIds, gs2) = foldl add ([], gs1) crewers
   in (readerId, vehicleIds, crewIds, gs2 {GameState.priority = Just S.alice})

-- Crew `vehicleId`, then let CR 603.3 gather what that resolution triggered onto
-- the stack (Engine.settleForPriority) and resolve it. Both states are returned:
-- the one with the trigger waiting, and the one after it resolved.
crewAndSettle :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> (GameState.GameState, GameState.GameState)
crewAndSettle answer vehicleId gs =
  let crewed = crewWith answer vehicleId gs
      onStack = S.runPure answer crewed Engine.settleForPriority
   in (onStack, S.runPure answer onStack Stack.resolveTop)

-- Taps `tappers` to pay CR 702.122a's cost and aims CR 702.122e's trigger at
-- `target`, announcing the printed "up to one" as one.
crewingAt :: [ObjectId.ObjectId] -> ObjectId.ObjectId -> Prompt.Prompt r -> r
crewingAt tappers target p = case p of
  Prompt.ChooseTapsForTotalPower {} -> Set.fromList tappers
  Prompt.AnnounceTargets _ _ _ offers -> fmap (const 1) offers
  Prompt.ChooseTargets _ _ _ sets -> fmap (\(_, candidates) -> Set.filter ((== Just target) . Recipient.objectOf) candidates) sets
  _ -> S.identityAnswer p

-- CR 702.122b: the crewer's side of rule 702.122c's relation, read by Gearshift
-- Ace's "whenever this creature crews a Vehicle, that Vehicle gains first strike
-- until end of turn".
--
-- TWO Dreadnoughts on every board here, and a creature TAPPED FOR ANOTHER REASON
-- on both: the readings this has to be told apart from are "any tapped creature
-- crewed" and "any Vehicle gains it", and one Vehicle with one tapped creature
-- distinguishes neither.
--
-- The arithmetic is non-degenerate for the module header's reason: crew 6 is paid
-- by the Ace (2) and Blind-Spot Giant (4) in the positive case and by Hill Giant
-- (3) and Blind-Spot Giant (4) in the negative, so no single creature and no
-- other pair reaches the threshold.
crewsVehicleSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
crewsVehicleSpec s registry = Spec.describe s "CrewsVehicle" $ do
  Spec.it s "CR 702.122b the Ace gives first strike to the Vehicle it crewed" $ do
    ace <- S.printingOf s registry "Gearshift Ace"
    dreadnought <- S.printingOf s registry "Consulate Dreadnought"
    hillGiant <- S.printingOf s registry "Hill Giant"
    blindSpot <- S.printingOf s registry "Blind-Spot Giant"
    let (aceId, vehicleIds, crewIds, gs) = crewReaderBoard ace [dreadnought, dreadnought] [hillGiant, blindSpot]
    case (vehicleIds, crewIds) of
      ([crewedId, bystanderId], [giantId, blindId]) -> do
        -- Hill Giant is tapped where it stands, so the board holds a tapped
        -- creature that crewed nothing.
        let (onStack, after) = crewAndSettle (crewingWith [aceId, blindId]) crewedId (tap giantId gs)
        Spec.assertBool s (not (hasFirstStrike crewedId onStack)) "the Vehicle has no first strike while the trigger waits"
        Spec.assertBool s (hasFirstStrike crewedId after) "and has it once the trigger has resolved"
        Spec.assertBool s (not (hasFirstStrike bystanderId after)) "the Vehicle the Ace did not crew gains nothing"
      _ -> Spec.assertFailure s "fixture should have two Vehicles and two other crewers"
  -- The negative, one board away: the same Vehicle is crewed by the OTHER two
  -- creatures while the Ace is tapped for a reason of its own. Rule 702.122b asks
  -- what was tapped to PAY, so a tapped Ace that paid nothing grants nothing.
  Spec.it s "CR 702.122b an Ace tapped for another reason has crewed nothing" $ do
    ace <- S.printingOf s registry "Gearshift Ace"
    dreadnought <- S.printingOf s registry "Consulate Dreadnought"
    hillGiant <- S.printingOf s registry "Hill Giant"
    blindSpot <- S.printingOf s registry "Blind-Spot Giant"
    let (aceId, vehicleIds, crewIds, gs) = crewReaderBoard ace [dreadnought, dreadnought] [hillGiant, blindSpot]
    case (vehicleIds, crewIds) of
      ([crewedId, _], [giantId, blindId]) -> do
        let (onStack, after) = crewAndSettle (crewingWith [giantId, blindId]) crewedId (tap aceId gs)
        Spec.assertBool s (isCreature crewedId after) "the Vehicle was crewed all the same"
        Spec.assertBool s (not (hasFirstStrike crewedId after)) "and gained no first strike"
        Spec.assertEqWith s "with nothing triggered onto the stack" (length (GameState.stack onStack)) 0
      _ -> Spec.assertFailure s "fixture should have two Vehicles and two other crewers"

-- Does this permanent have first strike right now, after the layer fold?
hasFirstStrike :: ObjectId.ObjectId -> GameState.GameState -> Bool
hasFirstStrike = Projection.hasKeyword Keyword.FirstStrike

-- Taps `tappers` to pay CR 702.122a's cost and answers nothing else: Gearshift
-- Ace's trigger targets nothing, so crewingAt's target answers would go unasked.
crewingWith :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
crewingWith tappers p = case p of
  Prompt.ChooseTapsForTotalPower {} -> Set.fromList tappers
  _ -> S.identityAnswer p
