{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Covers: CR 702.171 saddle -- Pawl.Types.Keyword's Saddle arm, the ability
-- Pawl.Engine.Keyword.saddle mints from it, CR 702.171b's designation
-- (Pawl.Types.Designation's Saddled and the sweep that ends it,
-- Pawl.Engine.Expiry.clearedSaddles) and the trigger that reads it back,
-- Pawl.Types.TriggerCondition's SelfAttacksWhileSaddled.
--
-- Gameplay-level throughout. Bridled Bighorn {3}{W} Creature -- Sheep Mount 3/4
-- is the fixture: vigilance, "Whenever this creature attacks while saddled,
-- create a 1\/1 white Sheep creature token", and saddle 2.
--
-- The arithmetic is non-degenerate. Saddle 2 is paid by Hill Giant alone at
-- power 3 -- which is not 2 (the threshold), not 1 (how many creatures were
-- tapped) and not the Bighorn's own 3\/4 toughness -- and Goblin Piker 2\/1
-- stands by untapped so a case can say WHICH creature paid.
--
-- CR 702.171c's "saddles" relation has its own fixture, Giant Beaver: see
-- saddledThisTurnSpec.
module Pawl.SaddleSpec where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Pawl.Engine.Activatable as Activatable
import qualified Pawl.Engine.Activate as Activate
import qualified Pawl.Engine.Engine as Engine
import qualified Pawl.Engine.Game as Game
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Engine.Stack as Stack
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CardName as CardName
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.Designation as Designation
import qualified Pawl.Types.EndingStep as EndingStep
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.Object as Object
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Printing as Printing
import qualified Pawl.Types.Prompt as Prompt
import qualified Pawl.Types.Recipient as Recipient
import qualified Pawl.Types.TapState as TapState
import qualified Pawl.Types.Zone as Zone

-- The saddle ability, taken from the PROJECTION rather than from the card's
-- face: rule 702.171a's ability is minted by Pawl.Engine.Keyword and appended by
-- Pawl.Engine.Projection.abilitiesGiven, so the card file declares no
-- activatedAbilities at all.
saddleAbility :: ObjectId.ObjectId -> GameState.GameState -> Maybe (ActivatedAbility.ActivatedAbility Card.Type.Card (GrantedAbility.GrantedAbility Card.Type.Card))
saddleAbility oid gs = case Projection.abilitiesOf oid gs of
  ability : _ -> Just ability
  [] -> Nothing

-- alice's board: the Bighorn and one creature per printing in `others`, all
-- Settled and untapped, with alice holding priority in her precombat main phase
-- -- which is the window CR 602.5d's "only as a sorcery" admits.
board :: Printing.Printing -> [Printing.Printing] -> (ObjectId.ObjectId, [ObjectId.ObjectId], GameState.GameState)
board bighorn others =
  let (mountId, gs0) = S.addPermanent bighorn S.alice S.threePlayerGame
      add (ids, g) p = let (oid, g1) = S.addPermanent p S.alice g in (ids <> [oid], g1)
      (otherIds, gs1) = List.foldl' add ([], gs0) others
   in (mountId, otherIds, gs1 {GameState.phase = Phase.PrecombatMain, GameState.priority = Just S.alice})

-- Activate the Mount's saddle ability and resolve it. Returns the state
-- unchanged if the permanent offers no ability at all, so a case that expects
-- the mark asserts on the board and not on this returning Just.
saddleWith :: (forall r. Prompt.Prompt r -> r) -> ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
saddleWith answer mountId gs = case saddleAbility mountId gs of
  Nothing -> gs
  Just ability ->
    let activated = S.runPure answer gs (Activate.activateAbility S.alice mountId ability)
     in S.runPure answer activated Stack.resolveTop

-- Name the creatures that pay rule 702.171a's cost; everything else is the
-- default answer.
tappingFor :: [ObjectId.ObjectId] -> Prompt.Prompt r -> r
tappingFor tappers p = case p of
  Prompt.ChooseTapsForTotalPower {} -> Set.fromList tappers
  _ -> S.identityAnswer p

-- Can alice activate the Mount's saddle ability on this board?
saddleable :: ObjectId.ObjectId -> GameState.GameState -> Bool
saddleable mountId gs = case saddleAbility mountId gs of
  Nothing -> False
  Just ability -> Activatable.activatable S.alice mountId ability gs

isSaddled :: ObjectId.ObjectId -> GameState.GameState -> Bool
isSaddled oid gs = maybe False (Set.member Designation.Saddled . Object.designations) (Game.lookupObject oid gs)

tapStateOf :: ObjectId.ObjectId -> GameState.GameState -> Maybe TapState.TapState
tapStateOf oid gs = fmap Object.tapped (Game.lookupObject oid gs)

-- Tap one permanent in place, without paying anything for it.
tap :: ObjectId.ObjectId -> GameState.GameState -> GameState.GameState
tap oid gs =
  gs {GameState.objects = Map.adjust (\o -> o {Object.tapped = TapState.Tapped}) oid (GameState.objects gs)}

sheepTokens :: GameState.GameState -> Int
sheepTokens = S.countOnBattlefieldByName (CardName.MkCardName (Text.pack "Sheep Token")) S.alice

-- Declare `attacker` and nothing else.
attackingWith :: ObjectId.ObjectId -> Prompt.Prompt r -> r
attackingWith attacker p = case p of
  Prompt.DeclareAttackers _ _ ids -> filter (== attacker) ids
  _ -> S.aggressiveAnswer p

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Saddle" $ do
  saddleCostSpec s registry
  saddledDesignationSpec s registry
  attacksWhileSaddledSpec s registry
  saddledThisTurnSpec s registry
  returnSaddlersSpec s registry

-- CR 702.171a's cost and its rider.
saddleCostSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
saddleCostSpec s registry = Spec.describe s "SaddleCost" $ do
  -- Rule 702.171a's "other". The Bighorn is an untapped power-3 creature alice
  -- controls -- every word of the criterion but that one -- so without "other" it
  -- would pay for its own saddle 2 off its own power.
  Spec.it s "CR 702.171a a Mount is no candidate for its own saddle cost" $ do
    bighorn <- S.printingOf s registry "Bridled Bighorn"
    hillGiant <- S.printingOf s registry "Hill Giant"
    let (aloneId, _, alone) = board bighorn []
        (pairId, _, pair) = board bighorn [hillGiant]
    Spec.assertEqWith s "the Mount's own power is 3" (Projection.powerOf aloneId alone) (Just 3)
    Spec.assertBool s (not (saddleable aloneId alone)) "which does not pay its own saddle 2"
    Spec.assertBool s (saddleable pairId pair) "where another creature's power 3 does"
  -- Rule 702.171a's "Activate only as a sorcery", which crew does not print. The
  -- same board twice, differing in the phase alone.
  Spec.it s "CR 702.171a saddle is not activatable outside a main phase" $ do
    bighorn <- S.printingOf s registry "Bridled Bighorn"
    hillGiant <- S.printingOf s registry "Hill Giant"
    let (mountId, _, gs) = board bighorn [hillGiant]
        inCombat = gs {GameState.phase = Phase.Combat CombatStep.DeclareAttackers}
    Spec.assertBool s (saddleable mountId gs) "alice's precombat main phase admits it"
    Spec.assertBool s (not (saddleable mountId inCombat)) "her declare attackers step does not"

-- CR 702.171b's designation: what the resolution writes, and when it ends.
saddledDesignationSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
saddledDesignationSpec s registry = Spec.describe s "Saddled" $ do
  Spec.it s "CR 702.171b the saddle ability taps the creature named and marks the Mount saddled" $ do
    bighorn <- S.printingOf s registry "Bridled Bighorn"
    hillGiant <- S.printingOf s registry "Hill Giant"
    piker <- S.printingOf s registry "Goblin Piker"
    let (mountId, otherIds, gs) = board bighorn [hillGiant, piker]
    case otherIds of
      [giantId, pikerId] -> do
        let after = saddleWith (tappingFor [giantId]) mountId gs
        Spec.assertBool s (isSaddled mountId after) "the Mount is saddled"
        Spec.assertEqWith s "the creature named is tapped" (tapStateOf giantId after) (Just TapState.Tapped)
        Spec.assertEqWith s "the creature not named is not" (tapStateOf pikerId after) (Just TapState.Untapped)
        -- Rule 702.171a's cost carries no tap symbol, so the Mount itself never
        -- taps -- the whole reason the threshold is paid by OTHER creatures.
        Spec.assertEqWith s "and the Mount itself is untapped" (tapStateOf mountId after) (Just TapState.Untapped)
      _ -> Spec.assertFailure s "fixture should have two other creatures"
  -- Rule 702.171b's clock: "until the end of the turn". The other ending, a zone
  -- change, is CR 400.7's new object and needs no sweep of its own.
  Spec.it s "CR 702.171b the cleanup step ends the designation" $ do
    bighorn <- S.printingOf s registry "Bridled Bighorn"
    hillGiant <- S.printingOf s registry "Hill Giant"
    let (mountId, otherIds, gs) = board bighorn [hillGiant]
    case otherIds of
      [giantId] -> do
        let saddled = saddleWith (tappingFor [giantId]) mountId gs
            after = snd (Engine.runGamePure S.identityAnswer saddled (Engine.runTurnBasedActions (Phase.Ending EndingStep.Cleanup)))
        Spec.assertBool s (isSaddled mountId saddled) "it was saddled before the step"
        Spec.assertBool s (not (isSaddled mountId after)) "and is not after it"
      _ -> Spec.assertFailure s "fixture should have one other creature"

-- CR 702.171b read back by a card: "whenever this creature attacks while
-- saddled". Two boards differing in the designation and in nothing else -- the
-- Hill Giant is tapped on both, on the negative board by hand rather than by
-- paying the cost.
attacksWhileSaddledSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
attacksWhileSaddledSpec s registry = Spec.describe s "AttacksWhileSaddled" $ do
  Spec.it s "CR 702.171b an attacking Mount that is saddled creates the token" $ do
    bighorn <- S.printingOf s registry "Bridled Bighorn"
    hillGiant <- S.printingOf s registry "Hill Giant"
    case S.combatBoardOf [bighorn, hillGiant] [] of
      (gs, [mountId, giantId], []) -> do
        let saddled = saddleWith (tappingFor [giantId]) mountId gs
            after = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) (attackingWith mountId) saddled
        Spec.assertBool s (isSaddled mountId saddled) "the Mount is saddled as it is declared"
        Spec.assertEqWith s "and its trigger made a Sheep" (sheepTokens after) 1
      _ -> Spec.assertFailure s "fixture should have a Mount and one other creature"
  Spec.it s "CR 702.171b an attacking Mount that is not saddled creates none" $ do
    bighorn <- S.printingOf s registry "Bridled Bighorn"
    hillGiant <- S.printingOf s registry "Hill Giant"
    case S.combatBoardOf [bighorn, hillGiant] [] of
      (gs, [mountId, giantId], []) -> do
        let after = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) (attackingWith mountId) (tap giantId gs)
        Spec.assertBool s (not (isSaddled mountId after)) "the Mount was never saddled"
        Spec.assertEqWith s "so no Sheep was made" (sheepTokens after) 0
      _ -> Spec.assertFailure s "fixture should have a Mount and one other creature"

-- CR 702.171c: "a creature 'saddles' a permanent as it's tapped to pay the cost
-- to activate a permanent's saddle ability" -- the relation read back later in
-- the turn, by which time the saddle ability has resolved and left.
--
-- Giant Beaver {3}{G} Creature -- Beaver Mount 4/4: "Vigilance / Whenever this
-- creature attacks while saddled, put a +1/+1 counter on target creature that
-- saddled it this turn. / Saddle 3" (data/cards/giant-beaver.json; Oracle text
-- checked against api.scryfall.com, 2026-09-25).
--
-- TWO Beavers, each saddled by a different creature, and only one declared as an
-- attacker: that parts "saddled IT" from "saddled a Mount". The third board taps
-- a creature where it stands instead, which parts "saddled it" from "is tapped".
-- The saddlers are Hill Giant 3/3 and Jedit Ojanen 5/5, each paying saddle 3
-- alone, so the counter lands on distinct numbers -- 4/4 and 6/6.
--
-- The target is FILTERED out of what the engine offered rather than built, so a
-- board that never offered it takes another creature and the assertions say so.
saddledThisTurnSpec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
saddledThisTurnSpec s registry =
  let fixture = do
        beaver <- S.printingOf s registry "Giant Beaver"
        hillGiant <- S.printingOf s registry "Hill Giant"
        jedit <- S.printingOf s registry "Jedit Ojanen"
        case S.combatBoardOf [beaver, beaver, hillGiant, jedit] [] of
          (gs, [attackerId, otherId, giantId, jeditId], []) -> pure (Just (attackerId, otherId, giantId, jeditId, gs))
          _ -> pure Nothing
      throughDeclaration = S.runToStep (Phase.Combat CombatStep.DeclareBlockers)
   in Spec.describe s "SaddledThisTurn" $ do
        -- The proving case: the Giant saddled the Beaver that attacks, Jedit
        -- saddled the other one, and the counter lands on the Giant.
        Spec.it s "CR 702.171c the attacking Beaver's counter goes on the creature that saddled it" $ do
          saddleBoard <- fixture
          case saddleBoard of
            Just (attackerId, otherId, giantId, jeditId, gs) -> do
              let saddled = saddleWith (tappingFor [jeditId]) otherId (saddleWith (tappingFor [giantId]) attackerId gs)
                  after = throughDeclaration (aimingWith attackerId giantId) saddled
              Spec.assertEqWith s "the creature that saddled the attacker took the counter" (S.powerToughnessOf giantId after) (Just (4, 4))
              Spec.assertEqWith s "and the creature that saddled the other Beaver is its printed 5/5" (S.powerToughnessOf jeditId after) (Just (5, 5))
              Spec.assertEqWith s "with the stack empty, so the trigger resolved" (GameState.stack after) []
            Nothing -> Spec.assertFailure s "fixture should have two Beavers and two saddlers"
        -- One board away: the saddlers swap Beavers, so the Giant saddled a Mount
        -- and not THIS one, and Jedit takes the counter instead.
        Spec.it s "CR 702.171c a creature that saddled the other Mount is no target" $ do
          saddleBoard <- fixture
          case saddleBoard of
            Just (attackerId, otherId, giantId, jeditId, gs) -> do
              let saddled = saddleWith (tappingFor [giantId]) otherId (saddleWith (tappingFor [jeditId]) attackerId gs)
                  after = throughDeclaration (aimingWith attackerId giantId) saddled
              Spec.assertEqWith s "the creature that saddled the other Beaver is still its printed 3/3" (S.powerToughnessOf giantId after) (Just (3, 3))
              Spec.assertEqWith s "where the creature that saddled the attacker grew to 6/6" (S.powerToughnessOf jeditId after) (Just (6, 6))
            Nothing -> Spec.assertFailure s "fixture should have two Beavers and two saddlers"
        -- One board away again, in what the Giant was tapped FOR: tapped where it
        -- stands, it saddled nothing.
        Spec.it s "CR 702.171c a creature tapped for another reason saddled nothing" $ do
          saddleBoard <- fixture
          case saddleBoard of
            Just (attackerId, _, giantId, jeditId, gs) -> do
              let saddled = saddleWith (tappingFor [jeditId]) attackerId (tap giantId gs)
                  after = throughDeclaration (aimingWith attackerId giantId) saddled
              Spec.assertEqWith s "the tapped creature is still its printed 3/3" (S.powerToughnessOf giantId after) (Just (3, 3))
              Spec.assertEqWith s "where the creature that saddled the attacker grew to 6/6" (S.powerToughnessOf jeditId after) (Just (6, 6))
            Nothing -> Spec.assertFailure s "fixture should have two Beavers and two saddlers"

-- Declares `attacker` and nothing else, and aims the trigger's target at
-- `wanted` where the engine offered it -- and otherwise at the first candidate it
-- did offer, so a board that admitted the wrong creature says so.
aimingWith :: ObjectId.ObjectId -> ObjectId.ObjectId -> Prompt.Prompt r -> r
aimingWith attacker wanted p = case p of
  Prompt.DeclareAttackers _ _ ids -> filter (== attacker) ids
  Prompt.ChooseTargets _ _ _ sets -> fmap (aimAt . snd) sets
  _ -> S.aggressiveAnswer p
  where
    aimAt candidates =
      let asked = Set.filter ((== Just wanted) . Recipient.objectOf) candidates
       in if Set.null asked then Set.fromList (take 1 (Set.toAscList candidates)) else asked

-- CR 702.171c read by a choice rather than a target. Rambling Possum {2}{G}
-- Creature -- Possum Mount 3/3: "Whenever this creature attacks while saddled,
-- it gets +1/+2 until end of turn. Then you may return any number of creatures
-- that saddled it this turn to their owner's hand. / Saddle 1"
-- (data/cards/rambling-possum.json; Oracle text checked against
-- api.scryfall.com, 2026-09-25).
--
-- Hill Giant and Goblin Piker both pay the one saddle 1, and Jedit Ojanen is
-- tapped where it stands, so the answer -- every creature offered -- returns
-- exactly the two that saddled it.
returnSaddlersSpec :: (Monad m) => Spec.Spec m n -> Registry.Registry m -> n ()
returnSaddlersSpec s registry = Spec.describe s "ReturnSaddlers" $ do
  Spec.it s "CR 702.171c the attacking Possum returns the creatures that saddled it, and no other" $ do
    possum <- S.printingOf s registry "Rambling Possum"
    hillGiant <- S.printingOf s registry "Hill Giant"
    piker <- S.printingOf s registry "Goblin Piker"
    jedit <- S.printingOf s registry "Jedit Ojanen"
    case S.combatBoardOf [possum, hillGiant, piker, jedit] [] of
      (gs, [mountId, giantId, pikerId, jeditId], []) -> do
        let saddled = saddleWith (tappingFor [giantId, pikerId]) mountId (tap jeditId gs)
            after = S.runToStep (Phase.Combat CombatStep.DeclareBlockers) (returningAll mountId) saddled
            onBattlefield oid = Set.member oid (GameState.battlefield after)
            handSize g = length (Game.zoneMembers Zone.Hand S.alice g)
        Spec.assertBool s (not (onBattlefield giantId)) "the Giant, which saddled it, left the battlefield"
        Spec.assertBool s (not (onBattlefield pikerId)) "and so did the Piker"
        Spec.assertBool s (onBattlefield jeditId) "Jedit, tapped for no saddle cost, stayed"
        Spec.assertEqWith s "and the Giant and the Piker are in alice's hand" (handSize after) (handSize saddled + 2)
        Spec.assertEqWith s "and the Possum got +1/+2" (S.powerToughnessOf mountId after) (Just (4, 5))
      _ -> Spec.assertFailure s "fixture should have a Possum and three other creatures"

-- Declares `attacker` and nothing else, and returns every creature the engine
-- offered.
returningAll :: ObjectId.ObjectId -> Prompt.Prompt r -> r
returningAll attacker p = case p of
  Prompt.DeclareAttackers _ _ ids -> filter (== attacker) ids
  Prompt.ChooseAnyNumberOfPermanents _ _ _ candidates -> Set.fromList candidates
  _ -> S.aggressiveAnswer p
