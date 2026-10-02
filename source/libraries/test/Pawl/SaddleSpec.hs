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
-- data/scenarios/saddle.
module Pawl.SaddleSpec where

import qualified Data.List as List
import qualified Pawl.Engine.Activatable as Activatable
import qualified Pawl.Engine.Projection as Projection
import qualified Pawl.Registry as Registry
import qualified Pawl.Spec as Spec
import qualified Pawl.Support as S
import qualified Pawl.Types.ActivatedAbility as ActivatedAbility
import qualified Pawl.Types.Card as Card.Type
import qualified Pawl.Types.CombatStep as CombatStep
import qualified Pawl.Types.GameState as GameState
import qualified Pawl.Types.GrantedAbility as GrantedAbility
import qualified Pawl.Types.ObjectId as ObjectId
import qualified Pawl.Types.Phase as Phase
import qualified Pawl.Types.Printing as Printing

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

-- Can alice activate the Mount's saddle ability on this board?
saddleable :: ObjectId.ObjectId -> GameState.GameState -> Bool
saddleable mountId gs = case saddleAbility mountId gs of
  Nothing -> False
  Just ability -> Activatable.activatable S.alice mountId ability gs

spec :: (Monad m, Monad n) => Spec.Spec m n -> Registry.Registry m -> n ()
spec s registry = Spec.describe s "Saddle" $ do
  saddleCostSpec s registry

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
